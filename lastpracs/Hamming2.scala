package lastpracs;
import io.threadcso._


import io.threadcso.component.{map,console}
//import io.threadcso.channels.OneO
import io.threadcso.channels.ManyOneBuf

object HammingNumbers2 {
  
  val output, intoT1, intoT2, intoT3, intoMult2, intoMult3, intoMult5, outMult2, outMult3,  outMult5 = OneOneBuf[Long](100);
  val merge1ToMerge2 = OneOneBuf[Long](100, "merge1ToMerge2")
  val mergeToPrefix  = OneOneBuf[Long](100, "mergeToPrefix")

  val X2 = new Multiplier(2)(intoMult2, outMult2);
  val X3 = new Multiplier(3)(intoMult3, outMult3);
  val X5 = new Multiplier(5)(intoMult5, outMult5);
  
  val hammingGenerator2 = (prefix(1.asInstanceOf[Long])(mergeToPrefix, intoT1) ||  tee(intoT1, intoT2, output) || console(output) || tee(intoT2, intoT3, intoMult2)||
      tee(intoT3, intoMult3, intoMult5) || X2.mult() || X3.mult() || X5.mult() || threeWayMerge(outMult2, outMult3, outMult5, mergeToPrefix));

  
  def main(args: Array[String]) = {
	 hammingGenerator2();
	}
  
  def tee(in: ?[Long], out1: ![Long], out2: ![Long]) = proc
  {
    repeat{
      val input = in?;
    
    /*Output the input in parallel */
    (proc{out1!input;} || proc{out2!input;}  )();
       
    }   
    in.closeIn(); out1.closeOut(); out2.closeOut();

    
  }
  
def prefix[Long] (v: Long)(i: ?[Long], o: ![Long]) = proc { attempt { o!v; repeat { o!(i?);  } } {}
i.closeIn; o.closeOut }  

  def threeWayMerge(two : ?[Long], three: ?[Long], five: ?[Long], output: ![Long]) = proc
  {   var t = two?; var h = three?; var f = five?;
    repeat{
      if(t <= h && t <= f){
        output!t; do{ t = two?;}while((t%5 == 0) || (t%3 == 0))
      }
      else if(h <= f && h < t) { 
        output!h; do{h = three?;}while(h%5 == 0)
      }
      else{
        output!f; f = five?;
      }
    }
    two.closeIn; three.closeIn; five.closeIn; output.closeOut
  }



  def merge1(two : ?[Long], three: ?[Long], output: ![Long]) = proc
  {   var t = two?; var h = three?;
    repeat{
      if(t <= h){
        output!t; do{ t = two?;}while((t%3 == 0));
      }
      else{ 
        output!h; h = three?;
      }
    }
    two.closeIn; three.closeIn; output.closeOut
  }
  
    def merge2(olivia : ?[Long], bob: ?[Long], output: ![Long]) = proc
  {   var o = olivia?; var b = bob?;
    repeat{
      if(o <= b){
        output!o; do{ o = olivia?;}while((o%5 == 0));
      }
      else{ 
        output!b; b = bob?;
      }
    }
    olivia.closeIn; bob.closeIn; output.closeOut
  }
  
}


