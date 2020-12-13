package concFinal
import io.threadcso._
import java.util.HashSet

/**The purpose of this class is to provide an implementation that guards against 
 * the possibility that a producer deposits a value that no thread's
 * filter will accept. It does this by having each consumer ititially register its filter
 * the first time it calls get(). Then, producers and consumers can make sure to discard values
 * that will not be accepted by any filter.
 */

class DeadLockFreeCSOConditionsFB[T](C : Int) extends FilterBuffer[T] {
  private val monitor   = new Monitor
  private var value : T = null.asInstanceOf[T]
  private var empty     = true
  private val isEmpty, isFull = monitor.newCondition
  private var waiting   =  0
  private val threadIDs = new HashSet[Long]
  private val filters   = new HashSet[T => Boolean]
  
  //If all consumers have registered their filter, a producer can discard a value it knows will not
  //be accepted by any filters
  
  def put(datum: T): Unit = monitor withLock {

    while(!empty)
      isEmpty.await()
    /*If a producer's value has no chance of being consumed, its deposit is thrown away */
    if(filters.size() == C && isJunk(datum)){ 
      isEmpty.signal(); return; //Awaken another producer before returning
    }
    
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
    
     
    /*The first time a consumer calls get(), it registers its filter with the buffer 
     * To make sure the consumer does not register twice, we record its threadID*/
    
    if(threadIDs.size() != C && !threadIDs.contains(Thread.currentThread().getId)){
      filters.add(filter); 
      threadIDs.add(Thread.currentThread().getId) }
    
    var clear = false
    while(!clear){ 
      clear = true
      if(empty){//Wait if the buffer is empty
        clear = false
        isFull.await
      }
      else if(!filter(value)){
         clear = false
         waiting += 1
         if(waiting == C && isJunk(value)){ //If all consumers can't consume the value, remove it and ask a 
           empty = true                   //producer for a replacement
           isEmpty.signal
         }
         isFull.await
         waiting -= 1
      }
    }
    
	  empty = true
	  isEmpty.signal //We only need to wake up one waiting producer since the buffer size is one 
	  return value
  }
  
  /*tests if current value cannot pass any filters*/
  private def isJunk(deposit : T) : Boolean = {
    val it = filters.iterator()
    
    while(it.hasNext()){ //Would any of the filters accept the current value??
      val filter = it.next()
      if(filter(deposit))
        return false
    }
    true
  }
  
  

}