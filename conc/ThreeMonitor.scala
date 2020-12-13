package conc
import io.threadcso._
import java.util.ArrayList

import java.util.concurrent.atomic.AtomicInteger

object ThreeMonitor extends Coordinator{
	val sum = new AtomicInteger(0);


	def enter(id : Int) : Int = {
	    var currSum : Int = -1
			while(true){
				currSum = sum.get
				
				/*If the sum is not divisible by three, we sleep and hope when we wake up the sum is divisible by 3
				 *If the current sum is divisible by three, we try to add 'id' to currSum to make our 
				 * entrance into the critical section official with a compareAndSet. If the CAS operation fails,
				 * we sleep to wait for thread traffic to clear up
				 */
						if(currSum % 3 != 0 || !sum.compareAndSet(currSum, currSum + id)) 
							 Thread.sleep(0)

						  	else return currSum + id; //Return what the id sum was when we did the CAS operation
 
			}
			0
	}

	def exit(id : Int) : Int = sum.addAndGet(-id);

	val log = new ArrayList[Int]()


	def Tutor(me: Int) = proc("Tutor" + me) {

		for(i <- 0 until 100){
			log.add(enter(me)); exit(me);
		}

	}





	val System = Tutor(1) || Tutor(2) || Tutor(3) || Tutor(3) ||Tutor(1) || Tutor(1) || Tutor(2) || Tutor(1) || Tutor(3) || Tutor(9) || Tutor(1) || Tutor(2) || Tutor(2) || Tutor(3) || Tutor(3) ||Tutor(1) || Tutor(1) || Tutor(2) || Tutor(1) || Tutor(3) || Tutor(9) || Tutor(1) || Tutor(2) 
	

	def main(args: Array[String]) = {
	  System();
	  for(num <- log.toArray()){
	    print(num + " ");
	  }
	    
  
	}
  
  
  
 
}