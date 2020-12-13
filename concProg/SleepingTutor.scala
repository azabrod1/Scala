package concProg

import io.threadcso._
import java.util.concurrent.locks.ReentrantLock
import org.omg.CORBA.Object
import io.threadcso.semaphore.BooleanSemaphore
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import io.threadcso.semaphore.FastFlag

/**
        A solution to the problem will extend this class, which simply
        provides a standard test-rig.
*/

trait SleepingTutor 
{
  /** Protocol for tutor to wait for students to arrive */
  def TutorWait: Unit 
  /** Protocol for student to arrive and wait for tutorial */
  def Arrive: Unit  
  /** Protocol for students to receive tutorial */
  def ReceiveTute: Unit  
  /** Protocol for tutor to end tutorial */
  def EndTeach: Unit
  
  val start = nanoTime
  def now   = (nanoTime-start)/1E9 // in seconds
  def log(me: String, item: String) =  
      println(f"$now%6.5f $me%-5s $item%s")

  val random = new scala.util.Random

  def Student(me: String) = proc(s"Student $me") {
    while (true) {
      sleep(random.nextInt(3)*Sec)
      log(me, "arrives"); Arrive
      log(me, "ready");   ReceiveTute
      log(me, "leaves")
    }
  }

  def Tutor = proc("Tutor") {
    while (true) {
      log("Tutor", "waiting");   TutorWait
      log("Tutorial", "starts"); 
      sleep(random.nextInt(3)*Sec)
      log("Tutorial", "ends");   EndTeach; 
      sleep(random.nextInt(3)*Sec)
    }
  }

  val System = Tutor || Student("Ada") || Student("Bob")

  def main(args: Array[String]) = 
      System()
}

/**
     We use lock based syncronization to build a monitor. If the tutor or student must wait for others before proceeding we use 
     condition variables to wait
     
*/

object MonitoredTutor extends SleepingTutor{ 
  val lock            = new ReentrantLock()
  val students_ready  = lock.newCondition()
  val classDismissed  = lock.newCondition()
  var numStudents     = 0
  
  
  //When the tutor arrives, they wait on the students_ready conditon until there are two students ready to proceed
  def TutorWait ={
    lock.lock();
    while(numStudents != 2)
      students_ready.await
     lock.unlock();
  }
  
  /*When the first student arrives they wait on the second student. The second student, on arrival, wakes up the first student and tutor 
   * We use a students_ready condition to wake both the first student and tutor 
   */
  def Arrive = {
		  lock.lock();
		  numStudents += 1
				  if(numStudents == 2)
					  students_ready.signalAll; 
				  else{
					  while(numStudents != 2) students_ready.await;
				  } 
		  lock.unlock();
  }

  //Students wait on class dismissed condition until the tutor dismisses the class
  def ReceiveTute{
	  lock.lock(); while(numStudents != 0) classDismissed.await(); //Tutor sets num students back to 0 at the end
	  lock.unlock()
  }

  /*When the tutor finishes the tutorial, they signal to the students that the class is over and set numStudents to 0 to 
   * prepare for next round
   */
  def EndTeach = {
		  lock.lock
		  numStudents = 0; classDismissed.signal(); classDismissed.signal(); 
		  lock.unlock
  }
}
/**
 * The semaphore based class uses boolean semaphores and an atomicInteger which students can poll to see if they are first or second to arrive
 * 
 */
object SemaphoreTutor extends SleepingTutor{

	var tutorial             = new FastFlag();

	val concensusObject      = new AtomicInteger();
	var studentFlags         = Array(new FastFlag(), null, new FastFlag(), new FastFlag());


	def TutorWait ={
			tutorial.acquire //Tutor acquires boolean semaphore upon arrival to wait for students to release it when ready
	}

	/*We use an atomic integer as a standard consensous object, students poll it to see which one of them arrives first
	 * The first one to arrive just waits on a flag while the second to arrive releases the first
	 */
	def Arrive = {
			val studentNumber = concensusObject.getAndIncrement;
			if(studentNumber == 0) //I am first so I must wait
				studentFlags(0).acquire
				else
					studentFlags(0).release //If i am second, I must release the first

	}

	/*Once again the sudents use the consensous object to see who is ready first, the one to be ready second lets the tutor know they are ready to start */
	def ReceiveTute = {
			val studentNumber = concensusObject.getAndIncrement;
			if(studentNumber == 3){ //Let the tutor know both students ready
				tutorial.release; 
			}

			studentFlags(studentNumber).acquire

	}  
/*At the end of the tutorial, the tutor must reset the flags and release students waiting on the boolean semaphores*/
	def EndTeach          = {
			studentFlags(2).release; studentFlags(3).release;
			resetFlags()

	}
//Reset flags 
	private def resetFlags() = {
			studentFlags      = Array(new FastFlag(), null, new FastFlag(), new FastFlag())
			concensusObject.set(0)
			tutorial          = new FastFlag 


	}

}
