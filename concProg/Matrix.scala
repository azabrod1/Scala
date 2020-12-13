package concProg

import io.threadcso._

class Matrix(val N : Int, val M: Int) {
  val matrix = new Array[Int](N*M);
  
  def v(n : Int, m : Int) : Int = return matrix(n*M + m);
  
  def set(n : Int, m : Int, newValue : Int) = {
    matrix(n*M + m) = newValue;
  }
  
  def display = {
    
    for(n <- 0 until N){
      for(m <- 0 until M)
        print(matrix(n*M + m) + " ")
      println
      
    }
  }
  
   def multiply(that : Matrix) : Matrix = {
     
    (new Multiplier(this, that)).getResult
   
  }
   
   def *(that : Matrix) : Matrix = {
     
    (new Multiplier(this, that)).getResult
   
  }
   
   
class Multiplier(A : Matrix, B : Matrix){
  type Task = (Int, Int)
  val result = new Matrix(A.N, B.M)
  
  val numWorkers    = 5;
  val rowsPerBundle = 3

 
  val toWorkers = N2NBuf[Task](numWorkers, 1, numWorkers, "Boss to Workers")
  val toControl = N2NBuf[Boolean](numWorkers, numWorkers, 1, "Workers to Boss" )
  
  def getResult : Matrix = {
    val workers = || (for (w <- 1 to numWorkers) yield worker);
    
   (workers || boss )()
      
    result
    
  }
  
  
  def boss = proc {
    var rowsDone = 0
    for(task <- 0 until A.N/rowsPerBundle){
      toWorkers!(task*rowsPerBundle, rowsPerBundle)
    }
    val remainder = A.N % rowsPerBundle
    toWorkers!((A.N/rowsPerBundle)*rowsPerBundle, remainder)
    toWorkers.closeOut 
    
    for(w <- 0 until numWorkers)
      toControl?
      
     toControl.close()
     
  }
  
  def worker = proc {
          var sum = 0

    repeat{
      val (startRow, rowsToCompute) = toWorkers?;
      for(Arow <- startRow until rowsToCompute + startRow){
        for(Bcol <- 0 until B.M){
          sum = 0
          for(Acol <- 0 until A.M) 
            sum += A.v(Arow, Acol) * B.v(Acol, Bcol);
          
          result.set(Arow, Bcol, sum)
        }
      }
    }
          toWorkers.closeIn
    toControl!true //Let the boss know that the worker will retire
    

  }

  
}
  
}