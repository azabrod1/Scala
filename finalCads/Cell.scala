package finalCads;


/*This class represents a cell of the binary combining tree which threads traverse to meet eachother and 
 * combine their operations. */
class Cell[A](val parent: Cell[A]) {

	// Node statuses
	private val ROOT = -1;  private val IDLE = 0;         private val SINGLE = 1;
	private val SOLO = 2 ;  private val PAIRED_WAIT = 3;  private val PAIRED = 4;

	private  val KEEP_EXPLORING = true
	private var status  = IDLE
	private var secondOP : Operation[A] = null

	def this() = { this(null); status = ROOT }


	def precombine : Boolean = synchronized{

		while(status != IDLE && status != SINGLE && status != ROOT)  wait();

		status match{

		case IDLE   =>	{status = SINGLE;      return KEEP_EXPLORING}

		case SINGLE =>  {status = PAIRED_WAIT; return !KEEP_EXPLORING}

		case ROOT   =>                         return !KEEP_EXPLORING;

		case default => throw new Exception("Unexpected Cell Status Achieved");
		}
	}


	/* We combine linked list of operations by stacking the list added second on top of the list added by the first thread. 
	 * This is to reflect the fact pushes added earlier would be further a long a stack
	 */	
	 def merge(op : Operation[A] ) : Operation[A] = synchronized{

		while(status == PAIRED_WAIT) wait();

		status match{	
		case SINGLE => {status = SOLO; return op}  

		case PAIRED => {
			//Did one or both ops already get eliminated in combining?
			if(op == null)
				return secondOP;
			if(secondOP == null)
				return op;

			if(op.opIsPush == secondOP.opIsPush){ //test if the operations are same type or different types

				return op.combine(op, secondOP);
			}	

			if(op.opIsPush)
				return op.eliminate(op.asInstanceOf[Push[A]], secondOP.asInstanceOf[Pop[A]]);
			else
				return op.eliminate(secondOP.asInstanceOf[Push[A]],op.asInstanceOf[Pop[A]]);

		}

		case default =>
		throw new Exception("Cell has taken an unexpected state during a merge");
		}
	}
	 

	/*Function for a thread to deposit its value so that a messenger can merge it with its own ops and
	 * move the op forward. Afterward we notify the messenger that we have stored the op and it can proceed.
	 */
	 def store(op: Operation[A] ) : Unit = synchronized{
		//If the thread has already canceled out its operations, it can stop advancing. This cancels its pairing
		if(op == null){ 
			status = SINGLE;
			notify(); //Wake up waiting thread
			return;
		}

		secondOP = op;
		status   = PAIRED;
		notify();

	}

	def isRoot() : Boolean =  status == ROOT;

	def  refresh() : Unit = synchronized {
		status = IDLE;
		notify();
	}


}

