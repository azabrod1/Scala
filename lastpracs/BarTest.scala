package lastpracs
import io.threadcso._


object BarTest {
  val N = 10;
  
  
  def barrierTest(barrier : MsgBarrier, me : Int, delay : Boolean) = proc{
    val start = System.currentTimeMillis()
    if(delay) sleep(1000* milliSec)

    barrier.sync(me)
    println(me+ ": "+ (System.currentTimeMillis() - start));
    
  }
  
  
  
  
  def main(args : Array[String]){
    
    val barrier = new MsgBarrier(N)
    val progs1 = || (for(t <- 0 until N/2) yield barrierTest(barrier, t, false));
    val progs2 = || (for(t <- N/2 until N) yield barrierTest(barrier, t, true));

    (progs1 || progs2)();
    
    barrier.delete
    
    
  
  }
  
  
}