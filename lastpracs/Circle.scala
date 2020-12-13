package lastpracs
import io.threadcso._

object Circle {
  
  /*One node gets *special* status in that it passes its value on first to start the ring. 
   * The rest first read the value of the last thread in the ring before outputting the next function result
   */
  
  def startNode[T](last: ?[T], next: ![T], v : T) = proc {
    next!v  //Let the second node in the ring have its value 
    
    val result = last?; //After the first loop is done, we get the result of f(f(f(f(...)
    println(result) 
    next!result         //Pass it on to second node
    
    last?; //Just let the last node spit out its value
    last.closeIn(); next.closeOut()
    
    
  }

  def node[T](last: ?[T], next: ![T], v : T, func : (T,T) => T) = proc {
    next!func(last?, v) //Read the last value in the chain, apply f to it, and send it along
  
    val result  = last?;
    println(result)
    next!result
    
    last.closeIn(); next.closeOut()
  
  }
  

  def main(args : Array[String]){

	  val N = 20;

	  val channels = for (i←0 until N) yield OneOne[Int](s"chan$i");

		val accumulater = (x : Int, y: Int) => x + y;

	  val circle : PROC = (startNode(channels(N-1), channels(0), 0)) || 
	  || (for(n <- 1 until N) yield node(channels(n-1), channels(n), n, accumulater))
	  
	  circle()


  
  
}
  
  
}