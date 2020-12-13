package conc
import io.threadcso._
import java.util.ArrayList
import java.util.concurrent.ConcurrentLinkedQueue
import io.threadcso.semaphore.BooleanSemaphore

import java.util.concurrent.atomic.AtomicInteger

object ThreeSemaphoreMonitor extends Coordinator{
  var sum = 0
  val queue = new BooleanSemaphore(false)
  val mutex = new BooleanSemaphore(true)

  def enter(id : Int) : Int = {
    var done = false

    do{
      mutex.acquire
    
      if(sum % 3 == 0){
        sum += id
        mutex.release
        done = true
      }
      else{ 
        mutex.release
        queue.acquire
      }
   
    }while(!done);
    
  sum   
  }
  
  def exit(id : Int) : Int = {
    mutex.acquire
    sum -= id
    
    if(sum %3 == 0)
      queue.release
    
    mutex.release
    sum
  }
  

	val log = new ArrayList[Int]()


	def Tutor(me: Int) = proc("Tutor" + me) {

		for(i <- 0 until 100){
			log.add(enter(me) - me); exit(me);
		}

	}

	val System = Tutor(1) || Tutor(2) || Tutor(3) || Tutor(3) ||Tutor(1) || Tutor(1) || Tutor(2) || Tutor(1) || Tutor(3) || Tutor(9) || Tutor(1) || Tutor(2) || Tutor(2) || Tutor(3) || Tutor(3) ||Tutor(1) || Tutor(1) || Tutor(2) || Tutor(1) || Tutor(3) || Tutor(9) || Tutor(1) || Tutor(2) 
	

	def main(args: Array[String]) = {
	  System();
	  for(num <- log.toArray()){
	    print(num + " ");
	  }
	    
  
	}
  
  
  
 
}