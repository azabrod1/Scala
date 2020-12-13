package concFinal
import io.threadcso._
import java.util.concurrent.CountDownLatch
import PubSub.Msg
import java.util.ArrayList
import java.util.concurrent.atomic.AtomicInteger


/**Test rig for the Publisher-Subscriber problem */
object testPubSub {
  
  type Id = String 

  
  def main(args: Array[String]){
    testProperClose
    
    oneToOne(300000)
    modulotest(100, 10000)
    modulotest2(100, 10000)
    testOverLoadFromSlowSubscriber

  
  }
  
  
  /** Modulo Test I- authors of different popularity 
   * We have:
   * K publishers with Ids from 1 to K
   * K Subscribers with Ids from 1 to k
   * 
   * Each publisher produces pubMsgs messages.
   * 
   * Each subscriber only accepts messages from publishers such that its own id is divisible by the publisher's. 
   * 
   * We can easily calculate exactly how many messages a subscriber "should" receive if our system works properly:
   * 
   * 			var shouldReceive = 0;
   *      for(i<- p to numPublishers ) if(id%p == 0) shouldReceive += pubMsgs
   *      
   *      return shouldReceive;
   * 
   * In our test, we make sure each subscriber receives exactly this amount of messages
   * 
   * The subscriber process does not exit until it receives the expected number of messages and it 
   * triggers a failed assertion if it received more messages from authors than it should have
   * Thus, if the test exits smoothly, each subscriber received exactly the amount it should
   * 
   * We believe this is a good test because:
   * 
   * Replicates real world differences in publishers' popularity; everybody subscribes to publisher 1,
   * half the subscribers read from publisher 2, one third from publisher 3... only one from publisher K.
   * 
   * Easy to calculate how much each subscriber should receive, and there many different amounts subscribers
   * receive so if there was an error it would show up 
   * 
   * Tests to make sure message objects contain what they should
   * 
   * checks that filter objects work as they are supposed to 
   * 
   * numbers not hard coded; easy to adjust number of messages sent per author and number of publishers/subscribers 
   * to see how system works under different loads
   * 
   * Note: The assertions are in the code for the subscribers, not in the main test function!
   * 
   */
  
  
  def modulotest(K : Int, pubMsgs: Int){
    val authorReg = OneOneBuf[(Id, ![![String]])](50)
    val clientReg = OneOneBuf[(Id, Msg => Boolean, ![?[Msg]])](50)
    
    val latch = new CountDownLatch(K)
    
    fork(PubSub.pubsub(authorReg, clientReg))
    val publishers = ||(for(p <- 1 to K) yield simplePublisher(p, pubMsgs, "modulo", authorReg))
        
    val clients = ||(for(c <- 1 to K) yield moduloSubscriber(c,  clientReg, msgsToExpect(c, pubMsgs, K), latch))
    
    val start = System.currentTimeMillis
    
    fork(clients);

    latch.await()
    
    publishers()
    
    authorReg.closeOut; clientReg.closeOut
    
    if(K != 1)
      println("Assertions Passed for ModuloTest\tTime taken: " + (System.currentTimeMillis - start))
      
    else
      println("Assertions Passed for One Pub-One Sub\tTime taken: " + (System.currentTimeMillis - start))

  }
  
   /**This test is similar to the first one, but instead we have some subscribers that subscribe to many
    * publishers and others that subscribe to very little.
    * 
    * A subscriber accepts a publisher's message if and only if the publisher's id
    * is divisible by the subscriber's
    * 
    * For instance, subscriber 1 subscribes to every publisher, subscriber 2 to every other publisher and so on
    * 
    * This is similar to the first test, but the purpose is to see how the system handles some subscribers
    * having very large loads (we have a subscriber getting every single message!)
    * 
    * Note: Server takes a while to shut down in this case, probably due to large channels used
    * but it eventually does
    */
  def modulotest2(K : Int, pubMsgs: Int){
    val authorReg = OneOneBuf[(Id, ![![String]])](50)
    val clientReg = OneOneBuf[(Id, Msg => Boolean, ![?[Msg]])](50)
    
    val latch = new CountDownLatch(K)
    
    fork(PubSub.pubsub(authorReg, clientReg))
    val publishers = ||(for(p <- 1 to K) yield simplePublisher(p, pubMsgs, "modulo", authorReg))
        
    val clients = ||(for(c <- 1 to K) yield moduloSubscriber(c, clientReg, msgsToExpect2(c, pubMsgs, K), latch, true))

    
    val start = System.currentTimeMillis
    
    val server = fork(clients);

    latch.await()
    
    publishers()
    
    authorReg.closeOut; clientReg.closeOut
        
    println("Assertions Passed for ModuloTest 2\tTime taken: " + (System.currentTimeMillis - start))
    
    
  }
  
  /**Multi-threaded programs are usually designed with the intention of working well when there are many threads
   * running concurrently. It is thus good to make sure the program also works in the base case, 
   * one publisher and one subscriber
   * 
   * Here oen publisher sends 10,000 texts to a subscriber
   */
  def oneToOne(pubMsgs : Int = 10000) =  modulotest(1, pubMsgs);
  
  
    
  def testOverLoadFromSlowSubscriber{
    val authorReg = OneOneBuf[(Id, ![![String]])](1)
    val clientReg = OneOneBuf[(Id, Msg => Boolean, ![?[Msg]])](1)
    
    val latch = new CountDownLatch(1)
    
    val server = fork(PubSub.pubsub(authorReg, clientReg))
    val publisher =  simplePublisher(1, PubSub.SUBSCRIBER_BUF * 2, "modulo", authorReg);
        
    val client =  slowSubscriber(clientReg, latch);
        
    fork(client);

    latch.await()
    
    publisher()
       
    authorReg.closeOut; clientReg.closeOut
    
    server.join() //if we get passed the join, the server closed which means it figured out one of the subscribers needed to be removed

    println("Server successfully eliminated slow reading subscriber ");
    
  } 
 
  
  //How many messages should subscriber from modulo test 1 expect
  //Each subscriber reads from publisher such that the publisher's id divides into its own
  @inline def msgsToExpect(div : Int, pubMsgs : Int, numPublishers : Int) : Int ={
    var toExpect = 0;
    for(p<- 1 to numPublishers )
      if(div%p == 0) toExpect += pubMsgs
    
      toExpect
  }
  
    //How many messages should subscriber from modulo test 2 expect
    //Subscribe to publishers if your id divides evenly into the publishers
    //Different than above**
    @inline def msgsToExpect2(div : Int, pubMsgs : Int, numPublishers : Int) : Int ={
    var toExpect = 0;
    for(p<- 1 to numPublishers )
      if(p%div == 0) toExpect += pubMsgs
    
      toExpect
  }
  /*Represents an author with Id id (converted to string), it sends the message "id" K times, with a given prefix */
  def simplePublisher(id : Int, K: Int, prefix : String = "", authorReg: ![(Id, ![![String]])] ) = proc{
    val strID  = id.toString()
    val reply = OneOne[![String]]
    authorReg!(strID, reply)
    val myChan = reply?;
    reply.close
    
    //Send the message prefix + Id over and over
    for(k <-0 until K)
      myChan!(prefix + strID)
      
     myChan.closeOut //Do not forget to close the ports!
    
  }
  
  //Subscribers to all authors whose ids are divisible by Div
  def moduloSubscriber(id: Int, clientReg: ![(Id, Msg => Boolean, ![?[Msg]])], 
      ttlMsgs : Int, latch: CountDownLatch, inverse : Boolean = false ) = proc{
       
        val reply  = OneOne[?[Msg]]
        
        val filter = if(!inverse){msg : Msg => id%(msg.author).toInt == 0} else {msg : Msg => (msg.author).toInt%id == 0} 
          
        clientReg!(id.toString(), filter, reply)
        
        val inbox = reply?;
        reply.close
        latch.countDown()
        
        var received = null.asInstanceOf[Msg]
        
       //Will never exit the loop if less than expected number of messages sent to the subscriber
        for(x <- 0 until ttlMsgs)
          received = inbox?
          
          //If the PubSub works as it should, there should be NO more values to receive 
          //This assertion makes sure of that
          
        assert(inbox.readBefore(100*milliSec) == None)
        
        //We check to make sure the text and id of messages are processed properly
        //In other words, we make sure the publisher servers are wrapping texts into
        //message objects correctly
        assert(filter(received) && received.text.startsWith("modulo"))
        
        
        //If we get here, we subscriber received the expect number of messages
        inbox.closeIn
  }

  //Simulates a subscriber that stops reading its messages
   def slowSubscriber(clientReg: ![(Id, Msg => Boolean, ![?[Msg]])], 
       latch: CountDownLatch) = proc{
       
        val reply  = OneOne[?[Msg]]
        
        val filter = {msg : Msg => true} 
          
        clientReg!(1.toString(), filter, reply)
        
        val inbox = reply?;
        reply.close
        latch.countDown()
        
        var received = null.asInstanceOf[Msg]
        
       //Will never exit the loop if less than expected number of messages sent to the subscriber
          received = inbox?
          
  }
  
  
  /*Here we test that the server closes itself properly when there are no subscribers left and the
   * channel to add subscribes closes. 
   */
  def testProperClose{
    val authorReg = OneOneBuf[(Id, ![![String]])](50)
    val clientReg = OneOneBuf[(Id, Msg => Boolean, ![?[Msg]])](50)
    
    val server = fork(PubSub.pubsub(authorReg, clientReg))
    
    val getInbox = OneOne[?[Msg]]
    
    clientReg!("sample Id", _ => true, getInbox)
    
    val inbox = getInbox?; getInbox.close;
    inbox.closeIn()
    
    clientReg.close(); authorReg.close()
    
    server.join //Wont get past this line if server does not shut down
    
    print("Test Self Closing Feature: Server Successfully Shut Itself Down\n")
   
    
  }
  
}