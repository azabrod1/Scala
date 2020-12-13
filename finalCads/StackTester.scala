package finalCads

import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.ConcurrentLinkedDeque;

object StackTester  {

	val THREADS = 5;
	var debug = 0;
  val N = 10000;
	val DIGITS = N*THREADS;
	
	def main(args: Array[String]) : Unit = {
	  val start = System.currentTimeMillis()
	  
	  for(x <- 0 until 10){
	    
	    val stack = new CombiningTreeStack[Int](THREADS);
	    val numbers = new Array[Int](2*DIGITS);
	    
	    for(i <- (-(THREADS*N)+1) until DIGITS )
	      numbers(THREADS*N + i-1) = i;
	    
	    shuffleArray(numbers);
	    
	    val t = new Array[Thread](THREADS);
			val results = new Array[Array[Int]](THREADS);
			
		
			for(i <- 0 until THREADS){
				results(i) = new Array[Int](2*N);
				Arrays.fill(results(i), -1);
				t(i) = new Thread(new StackOps(stack, i, 2*N*i, 2*N*(i+1), results(i), numbers));
			}
			
			for(thrd <- t)			
			  thrd.start();
			try{
				for(thrd <- t)
					thrd.join();
			}catch{
		      case exception : InterruptedException => exception.printStackTrace()
			}
			
			val stats = new Array[Int](DIGITS);
			
			for(array <- results){
				for(i <- 0 until (2*N)){
					if(array(i) != -1)
						stats(array(i)) += 1;
				}
			}
			var toAdd = stack.pop(0);
			while(toAdd != None){
				stats(toAdd.get) += 1; toAdd = stack.pop(0); debug += 1;
			}
			for(i <- 1 until stats.length){
				//assert(stats[i] == 1);
				if(stats(i) != 1) System.out.print("Index: " + i + " Freq: "+  stats(i) + ",  ");
				//if(i%5 == 4)
				//	System.out.println();
			}
			}
		 System.out.println("Time Taken:  " + (System.currentTimeMillis() - start));
 
	}
	
		def shuffleArray(array : Array[Int]) : Unit = {
		var index, temp : Int = -1;
		val random = new Random();
		
		for(i <- array.length -1 until 0){
			index = random.nextInt(i + 1);
			temp = array(index);
			array(index) = array(i);
			array(i) = temp;
		}
	}

}
