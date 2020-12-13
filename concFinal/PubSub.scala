package concFinal
import io.threadcso._
import java.util.LinkedList

/** 							***The journey of a message in our PubSub implementation**
 * 
 * A publisher writes a text message and sends it to channel associated with that publisher ->
 * 
 * The text message is picked up by the publisher's own server. Each publisher is associated with their own server
 * as soon as the publisher signs up by sending their ID and channel to authorReg. The server wraps the text into a message
 * object which also contains the publisher's ID. The message object is now sent to msgBox, the channel containing all the
 * publisher's messages that have not been processed yet. Next, the distributer, after processing the messages received before our
 * message, finds our message in msgBox. The distributer then goes through its list of subscribers and sends our message to each 
 * subscriber whose filter accepts it. The subscribers get the message on their own personal channel they received when they subscribed
 * 
 */

object PubSub { 
  
  val PUBLISHER_BUF_SIZE = 10000; val SUBSCRIBER_BUF = 10000; val MSG_BOX_SIZE = 5000000
  
  type Id = String
  case class Msg(author : Id, text: String)
  
  case class Subscriber(filter : Msg => Boolean, channel : ![Msg])
  
 
  def pubsub(authorReg: ?[(Id, ![![String]])], clientReg : ?[(Id, Msg => Boolean, ![?[Msg]])]) = proc{
     
    val msgBox  = ManyOneBuf[Msg](MSG_BOX_SIZE) //Messages from the authors that are yet to be distributed kept here
   
    /*The publisherRegister sets up one server per publisher to read in their messages
    The distributer is in charge of feeding the messages to the correct subscribers */
    
    val system = publisherRegister(msgBox, authorReg)||distributor(authorReg, clientReg, msgBox)
    
    system()
    msgBox.close
  
  }
  
  
  private def publisherRegister(msgBox: ![Msg], authorReg: ?[(Id, ![![String]])]) = proc{

	  //Repeatedly enlist new publishers that want to join the system
	  repeat{
		  val (id, reply) = authorReg?; //get new publisher's details 
		  val pubPort = OneOneBuf[String](PUBLISHER_BUF_SIZE, id.toString())  //Construct a personal port for them to communicate into 
		  reply!pubPort  //Send the publisher its own dedicated port

			/*Give each publisher their own server to read their texts, wrap them in Msg objects, and send them to be distributed*/
		   val publisher = fork(pubServer(id, pubPort, msgBox, pubPort)); 
	  }
	  authorReg.closeIn;  msgBox.closeOut

  }

    /*Each publisher gets their own server which reads their messages, tags them, and sends them to main Msg Channel 
     * Each author is responsible for closing the output object on their own port when they are done inputting*/
  private def pubServer(id : Id, pubPort : ?[String], msgBox: ![Msg], inbox : ?[String]) = proc{
      
     repeat{ //Read in the publisher's message, and send it to the main msgBox, along with the author's ID
       msgBox!(Msg(id, inbox?)) 
     }
      pubPort.closeIn(); msgBox.closeOut(); 
      
    }
    /*Repeatedly read in messages that the publishers have sent and forward them to each subscriber that 
     * accepts the message 
     */
  private def distributor(authorReg: ?[(Id, ![![String]])], clientReg : ?[(Id, Msg => Boolean, ![?[Msg]])], msgBox: ?[Msg]) = proc{
      
      val subscribers = new LinkedList[Subscriber]; 

      //We alternate between adding new subscribers and forwarding messages to existing subscribers
      serve(

    		  clientReg =?=> {case (id, filter, channel) => 
    		    {
    			  val subPort = OneOneBuf[Msg](SUBSCRIBER_BUF) //Create port for subscriber to read from
    			  subscribers.add(Subscriber(filter, subPort)); //Add the subscriber and its filter to list of active subs
    			  channel!subPort;                              //Give the subscriber a reference to the port it'll read from
    			  channel.closeOut
    			  }
    		  }
    		  |
    		  msgBox =?=> {Msg => 
    		    { /*After processing a new message, we feed it to the input ports of each active subscriber whose filter
    		    * accepts the message 
    		    */
    	      val it = subscribers.iterator
    	      while(it.hasNext){
    	        val subscriber = it.next
    	        if(!subscriber.channel.canOutput){ //If the subscriber is no longer active, remove their connection
    	          it.remove; subscriber.channel.closeOut();
    	        }
    	        else if(subscriber.filter(Msg)){  //Check if Msg passes filter
    	         //To avoid a slow subscriber slowing down the system, we unsubscribe the ones whose buffer fills up
    	          //Alternatively we could have just flushed their buffer and started to write to them again or skipped them
    	          if(!subscriber.channel.writeBefore(2*Sec)(Msg)){ 
    	            it.remove(); subscriber.channel.closeOut();          
    	          }
    	        }
    	      }
    		      
           }  
    		 }
    		 
    		    | after(5 * Sec) ==> { //Every 5 seconds of non activity, check if there are subscribers left
    		    purgeSubscribers(subscribers);
    		    
    		    if(!clientReg.canInput && subscribers.size == 0) { //The Server shuts down as no more subscribers will ever join
	           msgBox.closeIn; authorReg.closeIn; clientReg.closeIn //this will close publisherRegister as well as it depends on authorReg port
	           stop;
	          }
	      } 
    		 
          )
      
      
    }

  /*remove from the list of subscribers any that have closed their input stream*/
  private def purgeSubscribers(subscribers : LinkedList[Subscriber]){
	  val it = subscribers.iterator;
	  while(it.hasNext){
		  val subscriber = it.next;
		  if(!subscriber.channel.canOutput){ //If the subscriber is no longer active, remove their connection
			  it.remove; subscriber.channel.closeOut();
		  }
	  }

  }
  
}