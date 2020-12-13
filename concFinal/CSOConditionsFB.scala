package concFinal
import io.threadcso._

class CSOConditionsFB[T] extends FilterBuffer[T]{
  private val monitor   = new Monitor
  private var value : T = null.asInstanceOf[T]
  private var empty     = true
  private val isEmpty, isFull = monitor.newCondition
 
  def put(datum: T): Unit = monitor withLock {
    while(!empty)
      isEmpty.await()
    value = datum
    empty = false
      
      /* We signal ALL waiting consumers because some of their filters may not allow them
       *  to consume the value deposited. This way we are guaranteed if there exists a consumer waiting
       *  that can consume the value, it will be consumed by some process. 
       */
      
    isFull.signalAll(); //We signal All waiting consumers because some of their filters may not
                          //Allow them to consume the value deposited
                          //This way all consumers can try to consume the value
      
  }
  
  
  def get(filter: T ⇒ Boolean): T = monitor withLock {
    
     /*The while condition below first checks whether the buffer is empty, if the buffer is empty, the
      * while loop short circuits without even evaluating the second condition and evaluates to true. If 
      * the buffer is not empty, the buffer checks if filter(value) is true. Thus, the buffer makes the 
      * thread go to sleep and wait until the buffer is  not empty and filter(value) = true
      */
	  while(empty || !filter(value))
		  isFull.await()

	  empty = true
	  isEmpty.signal //We only need to wake up one waiting producer since the buffer size is one 

    return value
    
  } 
  
  
}