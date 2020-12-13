package lastpracs
import io.threadcso._
import java.util.concurrent.ThreadLocalRandom

object PI {
  
  def main(args : Array[String]){
    (new Calc_Pi(1000000000 ,5,1000)).run
  }
  
 
  
  class Calc_Pi(TTL_CALCS: Long = 1000000, numWorkers : Int = 5, loadSize : Int = 1000){
     
  /*We use buffered channels so that workers/boss will not have to context switch every assignment (if not alot of CPUs), 
   * boss can deposit several at once */  
    
  val toWorkers = OneManyBuf[Int](numWorkers*2, "Boss to Workers")
  val toBoss    = ManyOneBuf[Long](numWorkers, "Workers to Boss" )
    
  
  def run = {
		  val workers = ||(for(q <- 0 until numWorkers) yield worker)
		  val organization = workers || boss
		  organization()

  }
    

  def boss = proc{
    
		val start = System.currentTimeMillis()
	  var calculated : Long = 0

  //Assign tasks to workers
     while(calculated < TTL_CALCS){
	     toWorkers!loadSize
	     calculated += loadSize
       } 
	  toWorkers.closeOut; //This lets the workers know its time to report back results
      
      var qualifying : Long = 0; 
      
      /*Now get the results from workers */
      for(w <- 1 to numWorkers)
        qualifying += (toBoss?);
           
      val pi = qualifying.asInstanceOf[Double] / calculated.asInstanceOf[Double]
      
      printf("Estimated value of Pi: %.6f\n", pi*4 );  print("Total time taken:  " + (System.currentTimeMillis() - start))
      
       toBoss.closeIn; 


    
    }
  
  def worker = proc{
    
    val rand = ThreadLocalRandom.current() 

    
    var loadSize = -1;
    var ttlHits = 0 : Long //The total calculations such that x^2 + y^2 < 1
    var X,Y = 0.asInstanceOf[Double];
    
    repeat{
      loadSize = toWorkers?;
      
      for(calc <- 0 until loadSize){ //Generate x and y randomly
        X = rand.nextDouble(); Y = rand.nextDouble();
        if(X*X + Y*Y < 1) ttlHits += 1;
 
      }
    }
    toBoss!ttlHits;
    
    toBoss.closeOut(); toWorkers.closeIn()
    
    
  }
    
    
    
    
  }
  
  
  
  
  
  
  
}