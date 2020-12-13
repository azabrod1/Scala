
/*
 * We represent intended pushes and pops as instances of the Operation class
 * For a push, the item field holds the object the operation is trying to place
 * onto the stack. Operations are not necessarily completed by the threads that called them.
 * Thus, a thread completing a pop operation deposits the result into a 'result' field so that
 * the thread that called the operation can return its value 
 * 
 * In case a messenger thread is carrying out an operation on behalf of another thread, we use the 
 * 'completed' field to let the original thread know when the operation can safely return (we can't
 * return before that or linearizability will be lost)
 * 
 * During traversal of the combining tree, a messenger thread carries a bundle of operations. This bundle is represented
 * as a linked list, hence the 'next' field. 
 * 
 * 
 */

package finalCads;

abstract class Operation[A](private val isPush : Boolean)  {
  	@volatile protected  var  next : Operation[A] = null
    private  var completed = false


			/* This method is used to combine two linked lists of operations of the same type by attaching one to the tail of the other 
			 * 
			 * For collections of Pushes, we put the bundle that arrived second (bundle delivered by 2nd thread) on top of the bundle
			 * delivered by the first tread to be in accord with the intuition of a stack: items pushed first to the stack appear on the stack first.
			 * 
			 * For pops, we place the bundle of pops that arrived first (with the initial thread visiting the node)
			 * on the top (head) of the linked list followed by the later bundle. This is to go with the intuition that if  a messenger picked up
			 * pop()_1 before pop()_2, then pop()_1 should be executed first.
			 * 
			 * The purpose of this is to have a well ordered structure in the event that operations are linearized 'simultaniously', 
			 * this allows us to give an ordering to operations that were linearized in the same moment of program execution. We picked orderings
			 * that seem to be intuitive.
			 */
			def combine(firstArrival : Operation[A], secondArrival: Operation[A]) : Operation[A] = {

					var top, bottom : Operation[A] = null

							if(firstArrival.isPush){
								top = secondArrival; bottom = firstArrival
							} else{
								top = firstArrival; bottom  = secondArrival
							}

					var curr = top;

					//Find tail of the linked list of operations 
					while(curr.next != null){
						curr = curr.next
					}
					curr.next = bottom

					return top;
}

/*Notifies the thread that spawned the operation that the operation is complete and the thread can now return
 * from its function call
 */
def finish() : Unit = {
	    	this.completed = true
				this.synchronized  {
					  this.notifyAll()
				}
}

/*Returns the size of the linked list that starts at this operation*/
def  size() : Int = {
		var size = 0
				var op : Operation[A] = this
				while(op != null){
					size += 1; op = op.next
				}
		return size
}

def getNextOP() : Operation[A] = this.next

/*We eliminate as many operations of different types as possible and let the threads that spawned the
 * operations know they are finished. End result is a bundle of operations of the same type or empty operations bundle
 */

def eliminate( pushes : Push[A],  pops : Pop[A]) : Operation[A] = {
  var currPush = pushes; var currPop = pops
		while(currPush != null && currPop != null){

			currPop.finish(Some(currPush.toPush))
			currPush.finish();

			currPush = currPush.getNext(); currPop =  currPop.getNext();
		}

		if(currPush != null)
			return currPush

		else
			return currPop
}

def isDone() : Boolean = completed 

def opIsPush() : Boolean = isPush

}


