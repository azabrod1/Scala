package lastpracs

import io.threadcso._
import io.threadcso.component.{console}



object Interleave {
   
  def Interleave(left: ?[Int], right: ?[Int], out: ![Int]) = proc {
    
    repeat{
    alt(left    =?=> {x => out!x}
         |right =?=> {y => out!y} 
         )
    }
        
        if(left.canInput)  repeat{out!(left?)}
    
        if(right.canInput) repeat{out!(right?)}

          
        left.closeIn(); right.closeIn(); out.closeOut(); 
    
  }
  
  def test(x: ![Int])= proc{
    
    for(i <- 0 until 100)
      x!i;
       
        for(i <- 100 until 200)
      x!i;
    x.closeOut();
  }

  
  
  
  def main(args : Array[String]) = 
  { 
    val left, right, out = OneOne[Int]
    
    val x = ( test(left.outPort) || test(right.outPort)  || Interleave(left.inPort, right.inPort, out.outPort)|| console(out));
    x();
       
    
  
  } 
  
}