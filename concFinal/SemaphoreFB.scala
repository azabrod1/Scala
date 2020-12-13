package concFinal
import io.threadcso._
import io.threadcso.semaphore.BooleanSemaphore
import java.util.LinkedList
import io.threadcso.semaphore.FastFlag

  /**The most obvious way to try implementing this is to use a boolean semaphore in place of a condition variable 
   * where a producer deposits releases the consumption semaphore when it deposits a value. Then the awakened
   * consumer attempts consumption, if the filter does not allow consumption, the consumer passes on the semaphore to
   * another consumer by a release. The PROBLEM with this is that two consumers that whose filter prevents
   * consumption can pass the semaphore to each other for a very long time instead of passing it to an able consumer
   * or just sleeping, hence we need a more clever solution.
   * 
   * Our implementation operates like this:
   * 
   * waitQ: A linkedQueue that consumers place themselves on to wait for a deposited value that their filter
   * accepts. 
   *    
   *
   * The program runs in a cycle like this:
   *
   * PRODUCER starts Put(). When producePermit is acquired 1) no consumers are running because the last consumer to call
   * acquire() and successfully retrive a value does not release consumePermit 2) no other producers are running because
   * because put() does not release producePermit anywhere in the call and as mentioned no consumers are running and hence
   * cant release producePermit. Thus, we have mutual exclusion in Put(). The producer deposits its value
   * and then chooses the first thread in the queue whose filter will accept the new value and releases it. We achieve some sense
   * of fairness because threads that went on the queue first are considered first. 
   * The producer ends by releasing the consumePermit. Notice that producePermit is not released! This ensures that
   * consumers run get() without a producer running at the same time. Put() ends...
   * 
   * CONSUMER starts get(): The critical section of consumer can't run at the same time as a producer, as explained above. A 
   * consumer's critical section is also protected from other consumers by the consumePermit semaphore. That is how we achieve
   * mutal exclusion in get(). Once a consumer acquires consumePermit, it checks if the value is accepted by its filter.
   * If yes, the consumer takes the value and starts up a new producer (but does not release another consumer) by releasing 
   * producePermit. If the filter returns false, the consumer puts itself on the queue and waits for a producer to
   * release it when it deposits a value acceptable to its filter. Before suspending itself, the thread releases the consumePermit
   *  so that another consumer can try to take the value. Notice that when a consumer is released from the queue, it must reevaluate the
   *  filter because another consumer may have acquired consumePermit first and taken the value first and the current one may not
   *  be acceptable.
   *  
   *  Efficiency considerations: 
   *  
   *  PROS: a) interestingly the permits act as both condition variables and mutexes at the same time;
   *  saving some overhead (one can see this by readng the above or considering an execution example)
   *  
   *  b) We only wake one consumer following a value being deposited and we make sure the filter would evaluate true for it
   *  c) We only wake one producer at a time
   *  d) The queue is scanned in a way so earlier arriving consumers are considered first
   *  
   *  CONS: a) Storing a linked list which contains both function pointers and Flags may be expensive 
   *  
   */

class SemaphoreFB[T] extends FilterBuffer[T] {
  
  type request = ((T ⇒ Boolean), FastFlag)
  private val waitQ             = new LinkedList[request] //LinkedList instead of ArrayList because deletions are O(1)
  private val producePermit     = new BooleanSemaphore(true);
  private val consumePermit     = new BooleanSemaphore(false);
  private var value             = null.asInstanceOf[T]

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
    
    //Allow either the released consumer or another non blocked one to consume the new value
    //Notice that we do not release the producer semaphore, the consumer will do that after consumption

    consumePermit.release() 

  }
  def get(filter: T ⇒ Boolean): T = {
  
	  consumePermit.acquire
    
	  ////Linearization point for a consumer's get() is when while loop condition registers true
	  while(!filter(value)){  
	    val flag = new FastFlag       //Effectively a boolean semaphore but more efficient (and can't be reused)

	    waitQ.addLast((filter, flag)) //Ask the producer to release it when a value passing its filter is deposited

	    consumePermit.release         //Allow other another thread to try consume this value
	    flag.acquire                  //Await release

	    consumePermit.acquire
	  }
    val toReturn = value
    producePermit.release() //Now we release a producer to deposit the next value
    
    return toReturn
  }
  
  
}