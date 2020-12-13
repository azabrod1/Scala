package finalCads
import java.util.ArrayDeque;
import java.util.concurrent.atomic.AtomicReference;


class CombiningTreeStack[A](private var width : Int) {
  
  private val tree = new Array[Cell[A]](width-1)
  private var top = new AtomicReference[Node](null)  
  
  if(width > 1)
    this.tree(0) = new Cell() //Initialize root

  //initialize rest of the cells
  for(cell <- 1 until (width - 1))
	    tree(cell) = new Cell[A](tree((cell - 1)/2))
	    


  //So threads can store nodes they visit for later backtracking
  private val visited = new Array[ArrayDeque[Cell[A]]](width) 
  
  
	//Initializes combining tree with 'width' nodes, including (width + 1)/2 leaves. Each leaf is shared 
	//by two threads. In this implementation, width is the number of threads using the stack
  
  for(thd <- 0 until width)
    visited(thd) = new ArrayDeque[Cell[A]](width)


	/* operation to push item onto the stack
	 *  me represents thread identifier, where 0 <= me < numThreads (width) */
    

	 def push(me : Int, item : A) : Unit = {
	   
	   //We want our data structure to perform well when there is low contention
  	// So we first attempt a 'plain vanilla' lock free push, if it fails because of high contention,
    //we enter the operation into the combining tree
		
		if(vanillaPush(item)) 
			return;
		

		val push : Operation[A] = new Push[A](item);

		try {
			operate(push, me)
		} catch  {
		case exception : Exception => exception.printStackTrace()
		}
	   
	 }
  
  def pop(me : Int) : Option[A] = {
    /*First try 'vanilla' pop, if contention is low, this is all we need */
    val currTop : Node = top.get()
    if(currTop == null) //linearization point for pop if stack is empty and emptiness discovered by 'vanilla' pop
			return None
    
    if(top.compareAndSet(currTop, currTop.next)) //linearization point for vanilla pop if stack is not empty 
			return Some(currTop.item)
    
		val pop : Operation[A] = new Pop[A]()

		try {
			operate(pop, me)
		} catch  {
		case exception : Exception => exception.printStackTrace()
		}  
		return pop.asInstanceOf[Pop[A]].getResult

  }
  
  	/*Attempts to push item onto the stack without using the combining tree. In a very high contention environment
	 * it may be wise to disable this feature entirely as it could add contention. This feature greatly outperforms the variant that 
	 * tries to use the combining tree right away.  
	 *   */
	
	private def vanillaPush(item : A) : Boolean = {

		val newTop =  new Node(item)
    val oldTop =  top.get()

		newTop.next =  oldTop

		return top.compareAndSet(oldTop, newTop) //if CAS succeeds: Linearization point for push completed the 'plain vanilla' way 
	}

	private def multiPop(bundle : Pop[A]) : Unit = {

		var willPop : Int = 0;  val bundleSize = bundle.size(); 
		var done   = false;
		var oldTop, curr : Node = null

		do{
			oldTop = top.get(); curr = oldTop
			willPop = 0  //number of nodes to remove from stack = min(bundle.size, stack.size))
			while((willPop < bundleSize) && curr != null){
				curr = curr.next; willPop += 1
			}

			//the 'Or' short-circuits the conditional if there are no nodes to pop
			if(willPop == 0 || top.compareAndSet(oldTop, curr)) //All pops belonging to bundle are linearized here
				done = true

		}while(!done);

		var op = bundle; curr = oldTop;

		var popped = 0
		/*First send results to the pops that did not receive nulls*/
		while(popped < willPop){ 
			op.finish(Some(curr.item))
			op = op.getNext(); curr = curr.next; popped += 1
		}

		/*Now send the results that are null, pops that return null because stack did not contain enough items*/
		while(popped < bundleSize){
			op.finish(None)
			op = op.getNext()
			popped += 1
		}
	}

	private def multiPush(bundle : Push[A]) : Unit = {
		var op                = bundle;
		val bundleHead        = new Node(op.toPush);
		var bundleTail        = bundleHead;

		//Put all the pushes into a linked list of Nodes
		while(op.getNext() != null){
			op = op.getNext();
			bundleTail.next = new Node(op.toPush);
			bundleTail = bundleTail.next;
		}

		//Attempt to add the bundle of Nodes to the stack
		var oldTop : Node = null ; var done = false;

		do{
			oldTop = top.get();
			bundleTail.next = oldTop;

			if(top.compareAndSet(oldTop, bundleHead)){ //All the pushes linearized here, 'top' of bundle being the 'last' one
				done = true; //Successful push, we are done
			}

		}while(!done);

		op = bundle;

		//Notify waiting threads that their push was successful
		while(op != null){
			op.finish(); op = op.getNext();
		}
	}

	

  /* Under high contention (simple push/pop fails) the stack inserts the operation onto the 
   * combining tree in an attempt to eliminate contention. This function oversees the entirety of a thread's 
   * interaction with the combining tree. 
   * 
   * Function input: Operation op - this object represents the intended push/pop operation. More info about the object
   * at Operation.java. 
   * 
   * A thread's execution of this function is something along these lines:
   * 
   * A thread starts  as a messenger at the leaf node associated with its thread ID carrying just its own
   * operation. 
   * The thread then traverses up the tree, registering its presence and intent to pass
   * its operation up the tree via the REGISTER function. If it reaches a cell while the cell is IDLE 
   * (no other thread currently using it), it marks the cell status as "Single", which
   * guarantees that it will be the one to carry its operation and any
   * obtained along the way to the next cell on the way up the tree. If the thread arrives at a cell while it has already
   * been registered for use (status : SINGLE), it marks its status as "PAIRED_WAIT", which announces that the first
   * thread will be a Messenger for the operations both threads are carrying, and the first thread will wait till this thread
   * deposits the operations to be delivered. 
   * 
   * After the register phase, the thread traverses the tree from its leaf to the node it hands off its operations to
   * (either main stack or a node with another messenger thread). Some of the cells the thread traverses will hold
   * 
   * 
   * Some of the cells encountered may hold bundles of operations that threads arriving at the cell second left for the 
   * current thread to deliver on their behalf. Before proceeding, the thread picks up those operations and tries to cancel them out
   * with one another if they are the same type or combine the sets if they are the same type. 
   * 
   * 
   * A thread that drops its operations off to a messenger waits for its operation to be completed as is notified of its completion
   * by the messenger thread completing the operation. A thread that made it all the way to the root deposits its operation
   * bundle to the central stack via the multi-push or multi-pop method*
   *
   * 
   * * Important to note that the messenger always carries a bundle of operations of same type (otherwise it would cancel them out)*
   * 
   * 
   */
	private def operate(op : Operation[A], me: Int ) = {

		var curr = tree(tree.length - me/2 - 1)
 
		while(curr.precombine){
			curr = curr.parent
		}
			
		val stop = curr; curr =  tree(tree.length - me/2 - 1)
		var bundle = op

		while(curr != stop){
			bundle = curr.merge(bundle); visited(me).push(curr); 
			curr = curr.parent
		}

		/*If this thread stops at the root node, it performs the operations that made it 
		 * through the tree on the main stack. As an invariant, if a thread is a messenger (has not handed off its bundle of 
		 * operations to another thread yet ) and its own operation has not yet been completed then it must carry its own operation.
		 * This is easy to see because each thread starts out as a messenger carrying a bundle of operations containing just its own op.
		 * Then, with each node the thread progresses as a messenger, it either gives up messenger status or
		 * keeps its own bundle and possibly merges the bundle with another collection of operations left at that node. This merge either 
		 * adds new operations to the bundle without removing any or completes some of the operations and possibly adds some more.
		 * 
		 * This is important because it means a thread that gets all the way too the root does not need to wait for its own op to complete!!
		 * */

		if(stop.isRoot()){
			if(bundle != null){
				if(bundle.opIsPush)
					multiPush(bundle.asInstanceOf[Push[A]])

				else multiPop(bundle.asInstanceOf[Pop[A]])
			}
		} 

		else{                     //deposit operation for messenger to collect and 
			//wait for your op to be completed
			stop.store(bundle); 
			op.synchronized {
				while(!op.isDone()){
					op.wait(); 
				}
			}

		}

		//Reset all nodes the thread visited except the one it stopped at
		curr = visited(me).pollFirst();
		while(curr != null){
			curr.refresh()
			curr = visited(me).pollFirst()
		}
	}
	/*Container for objects stored on the main stack*/
	private class Node(val item : A){
		 var next : Node = null

	}
}
