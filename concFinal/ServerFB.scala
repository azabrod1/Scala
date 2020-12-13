package concFinal
import io.threadcso._
import java.util.LinkedList
import java.util.concurrent.atomic.AtomicBoolean

class ServerFB[V] extends AnyRef with FilterBuffer[V] {

	abstract class Reply
  case class  Received(toReturn: V)                      extends Reply
  case object Deposited                                  extends Reply
  case class  Termination(successful : Boolean)          extends Reply

  abstract class Request
  case class Put(reply: ![Reply], datum: V)              extends Request
  case class Get(reply: ![Reply], filt: (V => Boolean))  extends Request 
  case class ShutDown(reply: ![Reply])                   extends Request

  private var toServer =  ManyOne[Request]
	private var server   = fork(Server(toServer))
	@volatile private var serverON = true

	
  def get(filter: V ⇒ Boolean): V = {
	  if(!serverON) startServer //The server shuts down after 2 seconds, so we may need restart it
	 
    val reply = OneOne[Reply]

    toServer!Get(reply, filter)
    reply? {case Received(toReturn) => reply.close(); toReturn} 
  }
	
	
  def put(datum: V): Unit = {
    if(!serverON) startServer

    val reply = OneOne[Reply]
    toServer!Put(reply, datum)
    reply? {case Deposited => reply.close(); ()}
  }

 
  def Server(requests : Chan[Request]) = proc{
	 	
	  //queues for waiting consumers & producers
	  val produceQueue = new LinkedList[Put] //LinkedList instead of ArrayList because deletions are O(1)
	  val consumeQueue = new LinkedList[Get] 

	  var value   = null.asInstanceOf[V]
	  var isEmpty = true
	  
	  /* Helper Function: Matching()
	   * Precondition- buffer is empty 
	   * 
	   * Purpose-      For each enqueued producer, see if a consumer exists that can take its value.
	   * 						   If yes, deposit that producer's value and immediately let that consumer take it
	   * 						 	 If no,  leave that producer in the queue for now and try the same with next one
	   * 							 The idea is to try to match all the pairs of waiting producers-consumers we can.
	   * 							 This is a more advanced version of a buffer with conditions because here we 
	   * 						   use our stored knowledge to wake up the right threads instead of just
	   * 						   letting randomness dictate which thread is awakened (as the case with conditions)
	   * 
	   * Postcondition- None of producers and consumers remaining on the queues or in the buffer can
	   * 								possibly match. No more meaningful progress can be made. If there are values on
	   * 								the producer queue, there must also be a value in the buffer (as we must adhere
	   * 								to the abstract interface we are implementing!) 
	   */
	  
	   @inline def matching(){
	     val produceIt = produceQueue.iterator();
	     var matched   = false
	     
	     while(produceIt.hasNext() && !consumeQueue.isEmpty()){ //For each producer, try find a matching consumer
	       val producer    = produceIt.next()
	       val consumeIt   = consumeQueue.iterator()
	       matched = false

	       while(consumeIt.hasNext() && !matched){
	    	   val get = consumeIt.next()
	    			   if(get.filt(producer.datum)){
	    				   matched = true
	    				   produceIt.remove()
	    				   producer.reply!Deposited //The producer effectively deposits its value into the buffer
	    				 
	    			  	 consumeIt.remove()       //The consumer immediately consumes the deposited value
	    				   get.reply!Received(producer.datum)
	    			 }
	       }
	       
	     }
	     /*At this point, we have the invariant that none of the queued consumers' filters would accept any of the 
	      * enqueued producers' values. We dequeue a producer off the queue and let it deposit a value
	      */
	     if(!produceQueue.isEmpty()){
	       val toAdd = produceQueue.remove()
	       isEmpty   = false
	       value     = toAdd.datum
	       toAdd.reply!Deposited
	     }
	     
	  }
	  
	  /** Serve loop:
	   *  
	   *  Invariants: a) If there are producers on the producer queue, there must also be a value in the buffer, otherwise we would have
	   *  	let a queued up producer deposit its value b) If a value  was consumed in the last loop iteration, none of the consumers 
	   *    currently queued up can accept the values from any of the producers currently queued up. This second rule is maintained by
	   *  	the matching() function when a consumer takes a value
	   *  
	   *  
	   *  Consumer: An arriving consumer can't do anything if the current value on the buffer is not accepted by its filter or 
	   *  if the buffer is empty (and by invariant a, producer queue is empty also). The consumer thus can only
	   *  make progress if the buffer has a value it can consume. In this case, it consumes the value and we call the matching function
	   *  which makes sure to select a producer from the queue whose value can be consumed by one of the consumers on the queue. If
	   *  on such producer is found, the producer deposits its value and the value is quickly consumed by the chosen consumer. We repeat this
	   *  until no meaningful progress can be made (invariant b holds). At the end of matching(), if producers exist that could not be matched,
	   *  the first one on the queue is allowed to deposit its value anyway to maintain invariant a). 
	   *  
	   *  Producer: If a producer arrives and there is already a value on the buffer, no meaningful progress can be made
	   *  If there is no value on the buffer, by invariant a) there are also no producers on the producer queue. Thus
	   *  the producer can put its value in the buffer and check if there are any consumers queued up that would 
	   *  accept its value. Thus, the producer deposits its value if and only if the buffer is empty when it arrives
	   *  
	   *  
	   */
	  
	  
	  serve(
	      requests =?=> {
	        
	          case consumer @ Get(reply, filter)  =>{
	          if(!isEmpty && filter(value)){ //Does the buffer contain a value the consumer's filter accepts
	              isEmpty  = true              
	              reply!Received(value)        //Remove the value from buffer and send it along
	              
	              if(!produceQueue.isEmpty())
	                matching()
	            }
	            
	            else
	              consumeQueue.add(consumer)
	          }
	          
	        case producer @ Put(reply, datum) => {
	          if(isEmpty){ //the producer deposits its value if and only if buffer is empty. Otherwise it queues up
	            reply!Deposited  //producer effectively deposits value here
	            
	            var matched = false
	            val it = consumeQueue.iterator()
	            
	            while(it.hasNext() && !matched){ //Try to match the newly deposited value with a consumer on the queue
	              val consumer = it.next()
	              if(consumer.filt(datum)){ //Wake up the first consumer whose filter accepts datum
	                it.remove()  //remove consumer off the consumeQueue and let it take the new datum
	                consumer.reply!Received(datum);
	                matched = true
	              }
	            }
	            //If a consumer that accepts the datum can't be found, store the datum on the buffer anyway to maintain invariant a)
	            if(!matched){ 
	              isEmpty = false
	              value   = datum 
	            }
	          }
	          else  produceQueue.add(producer)
	        }
	        
	      } 
	      /*if the server is inactive for 2 seconds and the buffer is empty, as are the queues, the server is shut down
	       * The server is restarted if requests start coming in again after shutdown
	       */
	      | after(2 * Sec) ==> {
	          val inactive = produceQueue.isEmpty() && consumeQueue.isEmpty() && isEmpty 
	          if(inactive){
	            requests.close
	            serverON = false
	            stop;
	          }
	      }
	      
	      )
	  
	}
  /*Restart the server if requests start coming in after it suspended. It is synchronized so that
   * two threads do not try to restart server at the same time!*/
  
 private def startServer : Unit = synchronized{
    if(serverON) return ()
    toServer =  ManyOne[Request]
    server = fork(Server(toServer))
    serverON = true
  }
}