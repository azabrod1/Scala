// Dining Philosophers
package lastpracs
import io.threadcso._
import scala.language.postfixOps


object TimeOutPhil
{
  // Philosophers' actions 
  abstract class Action {}
  case class Pick(who: Int)  extends Action
  case class Drop(who: Int)  extends Action

  
  val x = OneOne[Int]


  
  val N = 5 // Number of philosophers

  val random = new scala.util.Random

  // Simulate basic actions
  def Eat   = sleep(500*milliSec)
  def Think = sleep((200+random.nextInt(300))*milliSec) 
  def Pause = sleep((200+random.nextInt(300))*milliSec)
 
  val report = ManyOneBuf[String](20, "report")

  // A single philosopher
  def Phil(me: Int, left: ![Action], right: ![Action]) = proc("Phil"+me) {
    
    repeat {
      report!(s"$me sits");
      Think
      left!Pick(me);  report!(me+" picks up left fork");  Pause
      
      //If the right fork is not available after a certain timeout period; we drop the left fork and wait a bit
      if (right.writeBefore(500000)(Pick(me))) { report!(me+" picks up right fork"); Pause
      //In case it aquires right fork, proceed normally
      report ! (me+" eats"); Eat
      left!Drop(me);  report!(me+" drops left fork"); Pause
      right!Drop(me); report!(me+" drops right fork"); Pause
      report!(s"$me gets up"); Pause } 
      
      else {  left!Drop(me); println("Drops left fork due to contention");} //Drop left fork in case of failure to aquire right fork
      
    }
  }

  // A single fork
  def Fork(me: Int, left: ?[Action], right: ?[Action]) = proc("Fork"+me) {
    var owner: String="Nobody"
    val state = new Debuggable
    {   override def toString = s"Fork ${me} is with philosopher $owner" 
        register
    }
    serve
    {(
        left  =?=> { case Pick(x) => owner=s"$x"; left  ? { case Drop(y) => assert(y==x); case _ => print();  owner="nobody"} }
     |  right =?=> { case Pick(x) => owner=s"$x"; right ? { case Drop(y) => assert(y==x);  case _ => ; print(); owner="nobody"} }
    )}
    println(s"FORK $me DIED: ${state}")
  }

  // Channels to pick up and drop the forks:
  val philToLeftFork  = 
      for (i<-0 until N) yield 
          OneOne[Action]  (s"Phil($i) to Fork($i)")
  val philToRightFork = 
      for (i<-0 until N) yield 
          OneOne[Action]  (s"Phil($i) to Fork(${(N+i-1)%N})")
  // philToLeftFork(i)  is from Phil(i) to Fork(i);
  // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)


  // Put the components together
  val AllPhils: PROC = || ( 
    for (i <- 0 until N) yield 
      Phil( i, philToLeftFork(i), philToRightFork(i) ) 
  )

  val AllForks: PROC = || ( 
    for (i <- 0 until N) yield 
      Fork( i, philToRightFork((i+1)%N), philToLeftFork(i) ) 
  )

  val System: PROC = AllPhils || AllForks || component.console(report)

  // And run it
  def main(args : Array[String]) = 
  { println(debugger)
    System()
  } 
}

  