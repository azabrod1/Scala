package concFinal

//Filter buffer implemented with JVM style monitors

class MonitorFB[T] extends FilterBuffer[T] {
  
  private var value : T = null.asInstanceOf[T]
  private var empty = true //Alternatively, we could use value = null to mean the buffer is empty, but that would 
                    //limit producers from depositing nulls which we would prefer not to do
  
  def put(datum : T) = synchronized {
    
    /*We first wait until the buffer is empty, if we get past the while(!empty) condition, this means 
     * the buffer is empty and since the thread owns the lock on this monitor, we do not have to worry about another producer 
     * depositing its item ahead of this producer thanks to the syncronized call
     */
    while(!empty) wait()
    
    empty = false
    value = datum
    
    
    /*Notify the consumers that a new value has been deposited into the buffer. We need to do notifyAll
     *  because a notify() call may wake up a producer instead of a consumer, leading to possible deadlock
     */
    notifyAll()
    
  }
  
   def get(filter : T => Boolean) : T = synchronized{
     
     /*The while condition below first checks whether the buffer is empty, if the buffer is empty, the
      * while loop short circuits without even evaluating the second condition and evaluates to true. If 
      * the buffer is not empty, the buffer checks if filter(value) is true. Thus, the buffer makes the thread
      * go to sleep and wait until the buffer is  not empty and filter(value) = true
      */
     
     while(empty || !filter(value))
       wait()
     
     empty = true
     notifyAll(); //Use NotifyAll() to notify sleeping producers that they can deposit into the buffer now 
     return value
     
   }

  
  
  
  
  
  
}