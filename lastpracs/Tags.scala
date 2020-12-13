package lastpracs
import io.threadcso._
import io.threadcso.component.{console}

object Tags {
  val N = 20;
  
 def tagger[T](l: ?[T], r: ?[T], out: ![(Int, T)]) = proc { repeat {
  alt ( l=?=>{ vl ⇒ out!(0, vl) } | r=?=>{ vr ⇒ out!(1, vr) } )
} ; l.closeIn; r.closeIn; out.closeOut 

 }
 
 
 def detagger[T](in: ?[(Int, T)], l: ![T], r: ![T]) =  proc{
   repeat{
     val (stream, value) = in?;
     
     if(stream == 0) l!value else r!value
     
   }
   in.closeIn; l.closeOut; r.closeOut;
   
 }
 
 def Stream(output : ![Int], offset : Int) = proc{
  
   for(i <- 0 until N) output!(i + offset)
   output.closeOut();
   
 }
 
 def twoSole[T](in1: ?[T], in2: ?[T]) = proc{
   repeat{
     alt ( in1 =?=> {x => println(x)} | in2 =?=> {x => println(x)})
   }
  in1.closeIn(); in2.closeIn();
 }



def main(args : Array[String]){
 val lIn, rIn, lOut, rOut, shared =  OneOne[Int];
 val tagged = OneOne[(Int, Int)]
     val Independent = (component.copy(lIn, lOut) || component.copy(rIn, rOut))

  
 val circ = ( Stream(lIn, 0) ||Stream(rIn, N) || tagger(lIn, rIn, tagged) || detagger(tagged, lOut, rOut) || twoSole(lOut, rOut)) ;
 
 circ.apply
 
  
}

}