package concFinal
import io.threadcso._
import io.threadcso.semaphore.BooleanSemaphore
import java.util.LinkedList
import io.threadcso.semaphore.FastFlag


class DeadLockFreeSemaphoreFB[T](M: Int) extends FilterBuffer[T] {
  
  type request = ((T ⇒ Boolean), FastFlag)
  private val waitQ             = new LinkedList[request] //LinkedList instead of ArrayList because deletions are O(1)
  private val producePermit     = new BooleanSemaphore(true);
  private val consumePermit     = new BooleanSemaphore(false);
  private var value             = null.asInstanceOf[T]
  private val filters           = new LinkedList[(T => Boolean)]
  private val threadIDs         = new LinkedList[Long]
  
  def put(datum: T): Unit = {
   
    producePermit.acquire  //Linearization point for put(datum) is successfully acquiring this semaphore

    value = datum
    
    //Iterate through the linkedList to find a blocked consumer whose filter will accept the new datum
    //This way we do not wake up no more than one extra thread, leading to efficiency gains 
    val iterator = waitQ.iterator(); var done = false;
    while(iterator.hasNext() && !done){
      val (func, flag) = iterator.next();
      if(func(value)){
        flag.release(); done = true;
        iterator.remove()
      }
    }
    
    //If all consumers have already registered their filters, we can check if any of the consumer's filters
    //would be able to accept the datum. If not, we discard the datum. Note that the below if statement only executes 
    //if the producer was not able to find a consumer in the queue whose filter accepts datum, because then the 
    //if condition would be redundent.

    if(!done && filters.size() == M && isJunk(datum)){
      producePermit.release(); return; //Ask another producer to deposit a new value
    }  


    //Allow either the released consumer or another non blocked one to consume the new value
    //Notice that we do not release the producer semaphore, the consumer will do that after consumption

    consumePermit.release() 

  }
  
  def get(filter: T ⇒ Boolean): T = {
	 
    consumePermit.acquire
	  
	   /*The first time a consumer calls get(), it registers its filter with the buffer 
     * To make sure the consumer does not register twice, we record its threadID*/
    
    if(threadIDs.size() != M && !threadIDs.contains(Thread.currentThread().getId)){
      filters.add(filter); 
      threadIDs.add(Thread.currentThread().getId) }
	

	  ////Linearization point for a consumer's get() is when while loop condition registers true
	  while(!filter(value)){  
	    val flag = new FastFlag       //Effectively a boolean semaphore but more efficient (and can't be reused)

	    waitQ.addLast((filter, flag)) //Ask the producer to release it when a value passing its filter is deposited
	    
	    //If all consumers are on the queue, the value can't be valid or the last producer would have released it!
	    if(waitQ.size() == M) 	      
	      producePermit.release 
	    else 
	      consumePermit.release         //Allow other another thread to try consume this value
	   
	 	  flag.acquire                  //Await release
	    consumePermit.acquire
	  }
	  
	  
    val toReturn = value
    producePermit.release() //Now we release a producer to deposit the next value
    
    toReturn
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