package conc
import io.threadcso._
import io.threadcso.semaphore.BooleanSemaphore

class OneConsumerBuffer[T](capacity : Int) {
  private val buffers   =  Array.ofDim[Int](2, capacity)
  private val mutex     =  new BooleanSemaphore(true)
  private val cWait     =  new BooleanSemaphore(false)
  private val pWait     =  new BooleanSemaphore(false)
  private var active    =  0  //Which array is currently being filled
  private var size      =  0
  

 def get : Array[Int] = {
  
   mutex.acquire
   while(size != capacity){ //Wait till the buffer is all filled up
    mutex.release; cWait.acquire
   } 
   mutex.acquire
   val toReturn = buffers(active)
   active = active ^ 1 //Switch active buffer with XOR
   if(size == capacity)
     pWait.release
   
   mutex.release
   toReturn   
 }
  
def put(item : Int)  {
  
  mutex.acquire
  if(size < capacity){
    buffers(active)(size) = item
    size += 1
    if(size == capacity) cWait.release //Release the consumer 
    mutex.release
      
  }
  
  else{
    mutex.release
    pWait.acquire
    put(item)                           //Retry insertion after being woken up
    if(size < capacity) pWait.release   //After insertion, release the next put. Occasional spurious wakeup happens, but guarded against

   }


 }

}


