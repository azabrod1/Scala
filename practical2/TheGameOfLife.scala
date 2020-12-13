package practical2
import java.util.concurrent.CyclicBarrier

class TheGameOfLife(totalRounds : Int, private var prevBoard : Array[Boolean], T: Int, interval : Int) {
   private val N       = Math.sqrt(prevBoard.length).asInstanceOf[Int];
   println("This is N :"  +   N);

   private val display = new Display(N, prevBoard)
   private val barrier = new CyclicBarrier(T)
   private var round   = 0
   
   private var board = new Array[Boolean](N*N);
   
   def play(){
     val workers = loadWorkers()
     for(thread <- workers) thread.start
     
     for(thread <- workers) thread.join
    
   }
  
   private def  loadWorkers() : Array[Thread] = {
     val rows_per = N/T
     val workers  = new Array[Thread](T)
     for(t <- 0 until T - 1)
       workers(t) = new Thread(new Worker(t * rows_per, (t + 1) * rows_per - 1))
     workers(T-1) = new Thread(new Worker((T-1)*rows_per, N-1))

     workers
   }
   
   private class Worker(start : Int, end : Int) extends Runnable{
   
     
     def run(){
       if(interval != 0) Thread.sleep(interval)
    	 do{  
    		 for(row <- start to end)
    			 for(col <- 0 until N)
    				 updateCell(row, col)

    		 barrier.await

    		 if(start == 0){ //If this is thread 0, do some book keeping and update the visual
    		   prevBoard = board
    			 board = display.replaceBoard(board) //Switch boards
    			 display.draw
    		   round += 1
    		 }
    		 
    		 barrier.await
    		 
    		 if(interval != 0) Thread.sleep(interval)

    	 }while(round < totalRounds);

    	 
    	}
    

     /*1. Any live cell with fewer than two live neighbours dies, as if by needs caused by underpopulation.
			*2. Any live cell with more than three live neighbours dies, as if by overcrowding.
			*3. Any live cell with two or three live neighbours lives, unchanged, to the next generation.
			*4. Any tile with exactly three live neighbours cells will be populated with a living cell.
      */
     
   
     @inline def updateCell(r : Int, c : Int) : Unit = {
        
        val startRow = (r+N-1) % N;   val endRow = (r+1) % N;
        val startCol = (c+N-1) % N;   val endCol = (c+1) % N;
        
        var neighbours = 
          X(prevBoard(startRow * N + startCol)) + X(prevBoard(startRow * N + c)) + X(prevBoard(startRow * N + endCol))                 +
          X(prevBoard(r * N + startCol))                                         + X(prevBoard(r * N + endCol))                        +
          X(prevBoard(endRow * N + startCol))   + X(prevBoard(endRow * N + c))   + X(prevBoard(endRow * N + endCol))                   ;
                
        board(r*N + c) =  (neighbours == 3) || (neighbours == 2 && prevBoard(r*N + c))
        
        
      }
     
     @inline def X(v : Boolean) : Int = if (v) 1 else 0   
     
   }
   
 
   
   
 
   
   

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}