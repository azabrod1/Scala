package practical2

import java.util.concurrent.CyclicBarrier

import io.threadcso._
import io.threadcso.channels.OneOne
import io.threadcso.channels.OneOneBuf


import com.sun.xml.internal.ws.spi.db.OldBridge

class GameOfMessages3(maxRounds : Int, private var prevBoard : Array[Boolean], T: Int, timeBetweenFrames : Long) {
   private val N                  = Math.sqrt(prevBoard.length).asInstanceOf[Int]
      
   private var board              =  new Array[Boolean](N*N)
         
   private var workersToManager   =  new Array[OneOne[Boolean]](T)

   private var managerToWorkers   =  new Array[OneOneBuf[Boolean]](T)
   
   for(t <- 0 until T){
     workersToManager(t)          = new OneOne[Boolean]
     managerToWorkers(t)          = new OneOneBuf[Boolean](2)
   }
   
  
   /* We use a message passing control structure here. Thread 0 tells thread 1 when it is ok to proceed; thread 2 tells thread 3 and so on. This works because
    * only immediate neighbors affect a threads computation of next generation cell values
    */
   
   
   def play(){
     //Define channel t to be the channel that thread t reads to and thread t+1 writes to where 0<t<T-1 channels
     
    
     val rows_per = N/T

     val gameProcessor  = || (for(t <- 0 until T - 1) yield worker(t * rows_per, (t + 1) * rows_per - 1, t) ) || worker((T-1)*rows_per, N-1, T-1) || manager();
     
     if(timeBetweenFrames > 0) Thread.sleep(timeBetweenFrames)
     
     gameProcessor()
         
   }

   
   private def manager() = proc{
     var display = null.asInstanceOf[Display]
     if(timeBetweenFrames >= 0) display =  new Display(N, prevBoard);

     var stable   = true //If all rows are stable, we can stop
     var round    = 0;
     
     for(t <- 0 until T) stable = (workersToManager(t)?) && stable  //Handle first round a bit differently: Do not need release workers here
     round += 1;
     stable = stable || (round >= maxRounds);

     while(!stable){

       //Update board
    	 val tempBoard = prevBoard
    	 prevBoard  = board
    	 board      = tempBoard 
  
       for(t <- 0 until T) managerToWorkers(t)!false; //Allow threads to proceed. 
                             
    	 /*All threads to do work*/
       
       if(timeBetweenFrames >= 0){
    	   display.replaceBoard(prevBoard);
    	   display.draw;
    	   if(timeBetweenFrames > 0) Thread.sleep(timeBetweenFrames)
       }
  
       stable = true
       
       for(t <- 0 until T){stable = (workersToManager(t)?) && stable } //Here the manager waits for workers to finish
       
       round += 1;
       stable = stable || (round >= maxRounds);
       
     }
     
     for(t <- 0 until T) managerToWorkers(t)!true; //Tell threads the job is done

     
     print("Total rounds taken: " + round);
     
     if(timeBetweenFrames < 0) display =  new Display(N, prevBoard);


   }
   
   private def worker(start : Int, end : Int, me : Int ) = proc {
     var rnd = -1
     var stable  = true
     
     do{
       rnd += 1
       
       stable = true
       /*First step we do work */
       for(row <- start to end)  
			   for(col <- 0 until N) 
				   stable = updateCell(row, col) && stable;
       
       /*Report that the worker is finished with this round's duties and comment on whether its specific chunk is stable*/
       
       workersToManager(me)!stable       
       
     }while(!(managerToWorkers(me)?)); //Pause till we are allowed to continue. Manager will let us know if more work is required 
       
       
     }
 
     /*1. Any live cell with fewer than two live neighbours dies, as if by needs caused by underpopulation.
			*2. Any live cell with more than three live neighbours dies, as if by overcrowding.
			*3. Any live cell with two or three live neighbours lives, unchanged, to the next generation.
			*4. Any tile with exactly three live neighbours cells will be populated with a living cell.
      */
     
    
      @inline def updateCell(r : Int, c : Int) : Boolean = {
        
        val startRow = (r+N-1) % N;   val endRow = (r+1) % N;
        val startCol = (c+N-1) % N;   val endCol = (c+1) % N;
       
        var neighbours = 
          X(prevBoard(startRow * N + startCol)) + X(prevBoard(startRow * N + c)) + X(prevBoard(startRow * N + endCol))                 +
          X(prevBoard(r * N + startCol))                                         + X(prevBoard(r * N + endCol))                        +
          X(prevBoard(endRow * N + startCol))   + X(prevBoard(endRow * N + c))   + X(prevBoard(endRow * N + endCol))                   ;
                
        board(r*N + c) =  (neighbours == 3) || (neighbours == 2 && prevBoard(r*N + c))
        
        board(r*N + c) == prevBoard(r*N+c)
        
      }
     
    @inline private  def X(v : Boolean) : Int = if (v) 1 else 0  
   

  
}