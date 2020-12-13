package concProg


import io.threadcso._

object FishFowl {
  
  val N = 100
  type Proposal = (![Int], Int)
  val tinder   =  N2N[Proposal](N,N)
  val x = 1.0000;
 
  
  def fowl(me : Int) = proc {
    val myChannel = OneOne[Int]
    tinder!(myChannel.outPort, me) //send the proposal
    val partner = myChannel?; //Return the response
    println(partner,me);
  }
  
  def fish(me: Int)  = proc {
    val (link, partner )  = (tinder?)
    link!me;
    println(partner, me)
  }
  
  def main(args: Array[String]): Unit = {
    
    val fowls  =  || (for (f <- 0 until N)  yield fowl(f))
    val fishes =  || (for (fi <- 0 until N) yield fish(fi))

   
    
    (fowls||fishes)()
    
    tinder.close
  


  }
  
 
  
}