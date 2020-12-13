package lastpracs
import io.threadcso._

import io.threadcso.component.{map,console}
//import io.threadcso.channels.OneO
import io.threadcso.channels.ManyOneBuf

object HammingNumbers {
  val bufSize = 100;
  
  //Define the Channels we use
  val  intoT3, intoMult2, intoMult3, intoMult5, outMult2, outMult3,  outMult5 = OneOneBuf[Long](bufSize);
  val merge1ToMerge2 = OneOne[Long]( "merge1ToMerge2")
  val mergeToPrefix  = OneOneBuf[Long](bufSize, "mergeToPrefix")
 
  val output, intoT1, intoT2 = OneOne[Long]();

  val X2 = new Multiplier(2)(intoMult2, outMult2);
  val X3 = new Multiplier(3)(intoMult3, outMult3);
  val X5 = new Multiplier(5)(intoMult5, outMult5);
  
  //THe circuit from the practical assignment
  val hammingGenerator = (prefix2(1.asInstanceOf[Long])(mergeToPrefix, intoT1) ||  tee(intoT1, intoT2, output) || console(output) || tee(intoT2, intoT3, intoMult2)||
      tee(intoT3, intoMult3, intoMult5) || X2.mult() || X3.mult() || X5.mult() || merge1(outMult2, outMult3, merge1ToMerge2) || 
      merge2(merge1ToMerge2, outMult5, mergeToPrefix));

  
  def main(args: Array[String]) = {
	 hammingGenerator();
	}
  
  /*Take the input of one stream and send the content to two different streams */
  def tee(in: ?[Long], out1: ![Long], out2: ![Long]) = proc
  {
    repeat{
      val input = in?;
    
    /*Output the input in parallel */
    (proc{out1!input;} || proc{out2!input;}  )();
       
    }   
    in.closeIn(); out1.closeOut(); out2.closeOut();

    
  }
  
  //Prefix function
def prefix2[Long] (v: Long)(i: ?[Long], o: ![Long]) = proc { attempt { o!v; repeat { o!(i?);  } } {}
i.closeIn; o.closeOut }  

/*We do two different merges because we want to filter out duplicates by only allowing certain combinations */
/*To avoid duplicates we make sure all numbers are of the form 2*2*2*2*2...*2 *3*3*3...*3*/
  def merge1(two : ?[Long], three: ?[Long], output: ![Long]) = proc
  {   var t = two?; var h = three?;
    repeat{
      if(t <= h){
        output!t; do{ t = two?;}while((t%3 == 0));
      }
      else{ 
        output!h; h = three?;
      }
      //print("merge1", t,h)
    }
    two.closeIn; three.closeIn; output.closeOut
  }
  /*Similar to the above*/
    def merge2(olivia : ?[Long], bob: ?[Long], output: ![Long]) = proc
  {   var o = olivia?; var b = bob?;
    repeat{
      if(o <= b){
        output!o; do{ o = olivia?;}while((o%5 == 0));
      }
      else{ 
        output!b; b = bob?;
      }
          //  print("merge2",o,b)

    }
    
    if(olivia.canInput)  repeat{ output!(olivia?)}
    
    if(bob.canInput) repeat{output!(bob?)}

    olivia.closeIn; bob.closeIn; output.closeOut
    
  }
  
}

/*Deadlock occurs because the 2*3 merger inputs numbers into the second merger faster than the 5 generater outputs numbers*/



class Multiplier(m : Long)( in : ?[Long], out : ![Long]){
  def mult() = proc{
    repeat{ out!((in?) * m) }
  }

}