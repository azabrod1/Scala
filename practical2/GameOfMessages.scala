package practical2

import java.util.concurrent.CyclicBarrier
import io.threadcso._
import io.threadcso.channels.OneManyBuf
import io.threadcso.channels.ManyOneBuf

import com.sun.xml.internal.ws.spi.db.OldBridge

class GameOfMessages(maxRounds : Int, private var prevBoard : Array[Boolean], T: Int, interval : Int) {
   private val N       = Math.sqrt(prevBoard.length).asInstanceOf[Int];
   
   private var display            = null.asInstanceOf[Display];//new Display(N, prevBoard)
   
   private var board              = new Array[Boolean](N*N)
         
   private var workersToManager   =  new ManyOneBuf[Boolean](T)

   private var managerToWorkers   =  new OneManyBuf[Boolean](T)
   
   
  
   /* We use a message passing control structure here. Thread 0 tells thread 1 when it is ok to proceed; thread 2 tells thread 3 and so on. This works because
    * only immediate neighbors affect a threads computation of next generation cell values
    */
   
   
   def play(){
     //Define channel t to be the channel that thread t reads to and thread t+1 writes to where 0<t<T-1 channels
    
     val rows_per = N/T

     val gameProcessor  = || (for(t <- 0 until T - 1) yield worker(t * rows_per, (t + 1) * rows_per - 1) ) || worker((T-1)*rows_per, N-1) || manager();
     
     gameProcessor()
     
     display =  new Display(N, prevBoard);
    
   }

   
   private def manager() = proc{
     
     var stable   = true //If all rows are stable, we can stop
     var round    = 0;
     
     for(t <- 0 until T) stable = (workersToManager?) && stable   //Handle first round a bit differently: Do not need release workers here
     round += 1;
     stable = stable || (round >= maxRounds);

     while(!stable){

       //Update board
    	 val tempBoard = prevBoard
    	 prevBoard  = board
    	 board      = tempBoard 

    	 println("manager begins round " + round)
    	 
       for(t <- 0 until T) managerToWorkers!false; //Allow threads to proceed. 
                             
    	 /*All threads to work*/
    	 
       stable = true
       
       println("manager waits at round " + round)

       for(t <- 0 until T){ stable = (workersToManager?) && stable }//Here the manager waits for workers to finish
       
       round += 1;
       stable = stable || (round >= maxRounds);
       
     }
     
     for(t <- 0 until T) managerToWorkers!true; //Allow workers to exit
     
     print("Total rounds taken: " + round);

   }
   
   private def worker(start : Int, end : Int ) = proc {
     var rnd = -1
     var stable  = true
     
     do{
       rnd += 1
       println("worker " + (start/(N/T)) + "   round: " + rnd + " ")

       stable = true
       /*First step we do work */
       for(row <- start to end)  
			   for(col <- 0 until N) 
				   stable = updateCell(row, col) && stable;
       
       /*Report that the worker is finished with this round's duties and comment on whether its specific chunk is stable*/
       
       workersToManager!stable       
       
     }while(!(managerToWorkers?)); //Pause till we are allowed to continue. Manager will let us know if more work is required 
       
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