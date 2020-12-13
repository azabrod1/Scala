package lastpracs
import io.threadcso._


class MsgBarrier(N : Int) {
  
  //To communicate up and down the heap
  val up = for (i←0 until N)   yield OneOne[Unit](s"up$i") 
  val down = for (i←0 until N) yield OneOne[Unit](s"down$i")
  
  def Node(me : Int) = proc{
    val child1 = 2*me+1; val child2 = 2*me+2;
    
    if(child1 < N){
      up(child1)?; // Wait for first child to be ready if it exists
      
      if(child2 < N) up(child2)?; //Is there also a second child?
    }
    
    if(me != 0) up(me)!() //Tell parent the thread and its offspring are ready
    
    /* Wait till all threads have synced on barrier */
    
    if(me != 0) down(me)? //Wait for parent's notification that all threads 
    
     if(child1 < N){
      down(child1)!(); // Wait for first child to be ready if it exists
      
      if(child2 < N) down(child2)!(); //Is there also a second child?
    }
    
  }
  
  def sync(me : Int) = {
    Node(me)();  
  }
  
  def delete(){
    for(c <- 0 until N){
      up(c).close; down(c).close()
    }
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
}