package concFinal
import io.threadcso._

/**Test Rig for question 1, the object automatically executes most of the tests. The tests are described in the comments
 * and in the response to question 1
 * 
 */


object FilterBufferTest {
   val N = 100000; //Assume number  divisible by both total producers and consumers (to simplify test implementation) 
  def main(args : Array[String]) = {
       
      normalTests() //Runs the many tests we implemented for the filter buffer implementations
      
       // testDeadLockFree() //This tests deadlock free variations where the buffer takes # of Consumers as parameter
                              //Uncomment to run the tests

  }

 
  def normalTests(){
    	  val x = new ServerFB[Int]

	  OneVOne(new MonitorFB, "JVM Monitor Filter Buffer")
	  OneVOne(new CSOConditionsFB, "CSO Conditions Filter Buffer")
	  OneVOne(new SemaphoreFB, "Semaphore Filter Buffer")
	  OneVOne(x, "Server Filter Buffer")
	  println()
	  
	  GameOfN(new SemaphoreFB, "Semaphore Filter Buffer", 10,10)
	  GameOfN(new CSOConditionsFB, "CSO Conditions Filter Buffer", 10,10)
	  GameOfN(new MonitorFB, "JVM Monitor Filter Buffer", 10,10)
	  GameOfN(x, "Server Filter Buffer", 10,10)
	  println()


	  AllYouCanEat(new CSOConditionsFB, "CSO Conditions Filter Buffer",10)
	  AllYouCanEat(new MonitorFB, "JVM Monitor Filter Buffer",10)
	  AllYouCanEat(new SemaphoreFB, "Semaphore Filter Buffer",10)
	  AllYouCanEat(x, "Server Filter Buffer",10)
	  println()



  }
  
  //test the "dead lock free implementations, that is, test the implementations that discard values no consumer can match with
  def testDeadLockFree(){
    val C = 10; val P = 10
    
    DeadLockFreeTest(new DeadLockFreeCSOConditionsFB(C), 10, 10)
    DeadLockFreeTest(new DeadLockFreeSemaphoreFB(C), 10, 10)
    
  }
  
  

  
  /*We test execution first with one consumer and one producer. Many concurrent programs that work with many threads 
   * surprisingly break down when the minimum are provided because so much time is spent on imagining as much concurrency as possible and 
   * additionally arrays are sometimes not indexed carefully enough. Also, testing with just one consumer and producer helps catch
   * simpler errors before having to face more difficult ones when we add producers and consumers*/
  
  def OneVOne(buf : FilterBuffer[Int], bufName : String){
   val supply  = loadArray()
   val storage = new Array[Int](N)
   
   
   val oneVone = consumeRange(buf, 0, N, storage, {x => true}) || produce(buf, 0, N, supply)
   val start = System.currentTimeMillis()
   oneVone()
   val duration =  System.currentTimeMillis() - start
   assert(check(storage, 1));
   println("Time taken for One Consumer One Producer using implementation:  "+ bufName + ":  " + duration)
   
  }
  
  /*We test execution with P Producers and C consumers running at the same time. 
   *To test how well the implementation works with filters, we make it that 
   * consumer 0 can consume all numbers x such that x%N == 0, consumer 1 can consume all
   * numbers x%N = 1 and so on. Producers each produce N/P values.
   * 
   * The purpose of this test is to see how each buffer implementation 
   * in a solidly demanding running environment: many producers and consumers
   * reasonably high contention, and filter functions playing a role
   */
  
  def GameOfN(buf : FilterBuffer[Int], bufName : String, C : Int, P: Int){
   val supply  = loadArray()
   val storage = new Array[Int](N);
   val consumers = || (for(c <- 0 until C) yield consumeRange(buf, (c*N)/C,((c+1)*N)/C , storage, {x => (x%C) == c}))
   val producers = || (for(p <- 0 until P) yield produce(buf, (p*N)/P,((p+1)*N)/P , supply))
   
   
   val start = System.currentTimeMillis()

   (consumers||producers)()
   
   val duration =  System.currentTimeMillis() - start
   assert(check(storage, 1));
   
   println("Time taken for Many Consumer and Producers test using implementation:  "+ bufName + ":  "  + duration)

  }
  
  
  /*Now consumers can compete with eachother for values. Consumers can take either even or odd values
   * hence there will be a lot of overlap in allowed domains.
   * 
   * The purpose of this test is to see how the buffers perform in a more hectic environment where
   * where consumers acceptable domains overlap. Another purpose of this test is to see if all consumers consume roughly the same value
   * The purpose of this is to make sure our implementations do not have weird bugs where some consumers will sit and not do anything
   * for the entire execution. In other words, we are checking for STARVATION FREEDOM AND FAIRNESS.
   * 
   * 
   * How do we test? Fill the first half the supply array with negatives and the second half with positives
   * 
   * If the sum of all consumed is not 0, clearly the implementation is wrong
   * 
   * How do we test fairness? All consumers eat the same amount of values but if one thread hogged the input
   * for most of the time until its storage was full, the sum of the values in its storage would be a large negative
   * If the consumer was starved until another thread filled up its storage, it would have a large positive number
   * 
   * We find that the CSO Conditions implentation and JVM Monitor implementation is not consistently fair (depends)
   * while the semaphore implementation is always fair (the sum is close to zero of each thread's storage)
   * 
   */
  
  
  def AllYouCanEat(buf : FilterBuffer[Int], bufName : String, P: Int){
   val supply  = new Array[Int](N)
   
   for(i<- 0 until N/2)
     if(i%2 == 0) supply(i) = -1 else supply(i) = -2
     
   for(i<- N/2 until N)
     if(i%2 == 0) supply(i) = 1 else supply(i) =  2
   
  
   
   val storage = new Array[Int](N);
   val consumersA = || (for(c <- 0 until 2) yield consumeRange(buf, (c*N)/4,((c+1)*N)/4 , storage, {x => ((x==1) || (x== -1)) }))
   val consumersB = || (for(c <- 2 until 4) yield consumeRange(buf, (c*N)/4,((c+1)*N)/4 , storage, {x => ((x==2) || (x== -2)) }))

   val producer = produce(buf, 0,N, supply)
   
   
   val start = System.currentTimeMillis()

   (consumersA || consumersB||producer)()
   
   val duration =  System.currentTimeMillis() - start
   println("Time taken for All You Can Eat test using implementation:  "+ bufName + ": " + duration)

   assert(AllYouCanEatTest(storage))
   
    
  }
  /*The purpose of this function is to test the implementations which guard against deadlock
   * caused by a producer depositing a value which will pass none of the C consumers' filters
   * 
   * --It Should NOT be used with the normal implementations--
   * 
   * We test these implementations by making some of the numbers in the supply to large for any consumer
   * to accept 
   */
  
  def DeadLockFreeTest(buf : FilterBuffer[Int], C : Int, P : Int){
   
   val supplySize  = (N*1.1).asInstanceOf[Int]; val cutOff = N //All numbers above cutoff don't pass any filters
   val supply = loadArray(supplySize)
   
   val storage = new Array[Int](cutOff);
   val consumers = || (for(c <- 0 until C) yield consumeRange(buf, (c*N)/C,((c+1)*N)/C , storage,
       {x => ((x%C) == c && x < cutOff)}))
       
   val producers = || (for(p <- 0 until P) yield produce(buf, (p*supplySize)/P,((p+1)*supplySize)/P , supply))
   
   
   val start = System.currentTimeMillis()

   (consumers||producers)()
   
   val duration =  System.currentTimeMillis() - start
   assert(check(storage, 1, cutOff));
   
   println("Time taken for DeadLock Free test using implementation:  "+ buf.getClass + ":  "  + duration)
    
  }
  

 
  /*Loads array of ints from 0 to N-1 and returns it shuffled randomly*/
  
  def loadArray(size : Int = N) : Array[Int] = {
    val array = new Array[Int](size)
    
    for(i <- 0 until size)
      array(i) = i;
    
    shuffleArray(array) //Randomly shuffle array
    
    array
    
  }
  
  @inline def printArray(array : Array[Int]) = {print(array.mkString("\n"));}
  
  
  /** The following are functions we will use to design tests of the following structure:
   *  
   *  1) Build up a supply array with values 0 to N-1 and shuffle it
   *  2) Designate X producers to each distribute portions of the supply array to the buffer
   *  3) Designate Y Consumers to consume some of these values and store them in a storage array
   *  4) Run producers and consumers in parallel
   *  5) We check to see if all of the N values are in storage and only they are in storage: this ensures that
   *  each value that is produced is consumed exactly once
   *  
   */
  
  
 
  /*Inputs: the buffer, the range of values to add to the store (consumes hi-lo values), where to store result, filter
   * Purpose: Consumers hi-lo values from the buffer and stores them in storage
   */
  
  def consumeRange(buf :  FilterBuffer[Int], lo : Int, hi : Int, storage: Array[Int], func : Int => Boolean) = proc {
  
    for(c <- lo until hi)
      storage(c) = buf.get(func)
     
  }
  /*Inputs: the buffer, the range of values to deposit from supply array (produces hi-lo values), the supply
   * Outputs: provides consumer with the values from the randomly shuffled supply array from lo to hi
   */
  
  def produce(buf :  FilterBuffer[Int], lo : Int, hi : Int, supply: Array[Int]) = proc {
  
    for(c <- lo until hi)
      buf.put(supply(c))
     
  }  
 
  
  
  /*We check if consumers managed to consume all the values 0 to N-1
   * Inputs: The result array of the test, mult: number of times each element should appear
   */
  def check(storage : Array[Int], mult : Int, MAX : Int = N) : Boolean = {
    var pass = true
    val counts  = new Array[Int](MAX); //Create array of length N
    
    //The index i of counts will refer to the number of times i appears in storage
    
    for(v <- 0 until MAX)
      counts(storage(v)) += 1 //If we saw element i, increment counts(i)
    
    for(i <- 0 until MAX) //Now check each element appeared as many times as they should have
      if(counts(i) != mult) return false
      
      return true
     
  }
  
   
  /*We check if consumers managed to consume all the values 0 to N-1
   * Inputs: The result array of the test, mult: number of times each element should appear
   */
  def AllYouCanEatTest(storage : Array[Int]) : Boolean = {
    var sum : Long = 0
    
    //The index i of counts will refer to the number of times i appears in storage
    
    
    for(v <- 0 until N/4)
      sum += storage(v)
      
      println("Sum of the first consumers values: " + sum + " out of highest possible magnitude " + N/4 + "  (Numbers closer to 0 mean the implentation is more fair)\n")
    
    
    for(v <- N/4 until N)
      sum += storage(v)
        
    return (sum ==0) //This must be true or else we have an issue 
      

  }
 
  
 
 	def shuffleArray(array : Array[Int]) : Unit = {
		var index, temp : Int = -1;
		val random = scala.util.Random		
		
		for(i <- array.length -1 to 1 by -1){
			index = random.nextInt(i + 1);
			temp = array(index);
			array(index) = array(i);
			array(i) = temp;
		}
	}
  
  
  
}