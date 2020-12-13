// Dining Philosophers
package lastpracs
import io.threadcso._
import scala.language.postfixOps


object Butler
{
  
  // Philosophers' actions 
  abstract class Action {}
  case class Pick(who: Int)  extends Action
  case class Drop(who: Int)  extends Action

  val N = 5 // Number of philosophers

  val random = new scala.util.Random

  // Simulate basic actions
  def Eat   = sleep(500*milliSec)
  def Think = sleep((200+random.nextInt(300))*milliSec) 
  def Pause = sleep((200+random.nextInt(300))*milliSec)
 
  val report    = ManyOneBuf[String] (20, "report")
  
  type Permit = Boolean;
  
  val PermitReturn = ManyOneBuf[Permit](N-1, "Workers to Butler");
  val PermitRation  = OneManyBuf[Permit](N-1, "Butler to Workers");
  
  /** The butler has N-1 sitting permits to hand out when a phil wants to sit. When a phil gets up, they hand back
   *  the permit to the butler so they can ration it to someone else
   */
  for(p <- 0 until N-1) //Give the butler four permits to hand out
    PermitReturn!true;

 
  // A single philosopher
  def Phil(me: Int, left: ![Action], right: ![Action]) = proc("Phil"+me) {
    repeat {
      PermitRation?; //Ask to be seated
      report!(s"$me sits")
      Think
      left!Pick(me);  report!(me+" picks up left fork");  Pause
      right!Pick(me); report!(me+" picks up right fork"); Pause
      report ! (me+" eats"); Eat
      left!Drop(me);  report!(me+" drops left fork"); Pause
      right!Drop(me); report!(me+" drops right fork"); Pause
      PermitReturn!true;
      report!(s"$me gets up"); Pause 
    }
    PermitRation.closeIn(); PermitReturn.closeOut();
  }

  //Butler gives permits to phils and waits to get them back. Omly when permits are available in buffer can it give them out
  def Butler = proc("Butler") { repeat{PermitRation!(PermitReturn?)}; PermitRation.closeOut(); PermitReturn.closeIn(); }
  

  // A single fork
  def Fork(me: Int, left: ?[Action], right: ?[Action]) = proc("Fork"+me) {
    var owner: String="Nobody"
    val state = new Debuggable
    {   override def toString = s"Fork ${me} is with philosopher $owner" 
        register
    }
    serve
    {(
        left  =?=> { case Pick(x) => owner=s"$x"; left  ? { case Drop(y) => assert(y==x); owner="nobody"} }
     |  right =?=> { case Pick(x) => owner=s"$x"; right ? { case Drop(y) => assert(y==x); owner="nobody"} }
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

  val System: PROC = Butler || AllPhils || AllForks || component.console(report)

  // And run it
  def main(args : Array[String]) = 
  { println(debugger)
    System()
  } 
}

  