package conc
import io.threadcso._
import io.threadcso.semaphore.BooleanSemaphore
import java.util.concurrent.atomic.AtomicBoolean
import io.threadcso.semaphore.FastFlag


object PairMonitor {
  private val committMale         = new BooleanSemaphore(true); private val committFemale = new BooleanSemaphore(true);
  private val ordering            = new AtomicBoolean(false)  ; private var semaWait      = new FastFlag;	
	
  
  def manEnter   = {committMale.acquire;   matchMe}
  def womanEnter = {committFemale.acquire; matchMe}

  private def matchMe = {
		  val IWasHereFirst = ordering.compareAndSet(false, true); //This returns true to the first thread executing it since the second CAS fails

		  if(IWasHereFirst){
			  semaWait.acquire //Wait for second thread to arrive 
			  ordering.set(false) //Reset consensus object
			  semaWait = new FastFlag; //Create new waiting flag (boolean Semaphore that only can be used once
			  committMale.release; committFemale.release //Allow next pair of male and female in now that everything is reset 
		  }
		  else
			  semaWait.release;
		}
  var f, m = 0;
  
  def Female(me: Int) = proc(s"Female $me") {
	  for(me <- 0 until 100){
		  womanEnter; f+=1; if(f%5 == 0) println("women:  " + f + " men: " + m + " 	ID:  " + me)
	  }
  }

  def Male(me: Int) = proc(s"Male $me") {
	  for(me <- 0 until 100){
		  manEnter; m+=1; if(f%5 == 0) println("women:  " + f + " men: " + m + " 	ID:  " + me)
	  }
  }


  val System = Female(0) || Female(1)  || Female(2)|| Female(3) || Female(4) || Male(0) || Male(1)  || Male(2)|| Male(3) || Male(4);
  

  def main(args: Array[String]) = 
      System()

}