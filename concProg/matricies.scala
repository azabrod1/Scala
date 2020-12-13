package concProg

object matricies {
 
  
	def main(args: Array[String]): Unit = {
			val N = 10
			val A = new Matrix(N,N)
			val B = new Matrix(N,N)
			
			for(i <- 0 until N){
			  A.set(i, i, 3);
			  B.set(i, i, 2) 
			}
			
			val result = A*B
			
			result.display
			

	}
}