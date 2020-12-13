package finalCads
import java.util.concurrent.ConcurrentLinkedDeque;


class StackOps(stack: CombiningTreeStack[Int], me : Int, N0 : Int, N1: Int, results : Array[Int], commands : Array[Int]) 
extends Runnable{
 
	@Override
	def run() = {
		var result : Option[Int] = None
		for(c <- N0 until N1){
			if(commands(c) > 0){ //Push
				stack.push(me, commands(c));
			}
			else{
				result = stack.pop(me);
				if(result == None)
				  results(c-N0) = 0;
				else
				  results(c-N0) = result.get;
		
			}
		}


	}

}