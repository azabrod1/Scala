package conc
import io.threadcso._

import io.threadcso.component.{prefix,map,console,tee}
import io.threadcso.channels.OneManyBuf
import io.threadcso.channels.ManyOneBuf



object Nats
{
val mid,succs, out = OneOne[Int]
val nats = OneOneBuf[Int](2, "Bush did 9/11");
nats!0; nats!0; 

val fats = new ManyOneBuf[Boolean](2);

//fats?

val circuit = ( tee(nats, out, mid) || map((x:Int) ⇒ x+1)(mid,nats) || console(out)
)

def circuit2 = proc {
  
  
 }


def main(args: Array[String]) = circuit() 

def prefix2[T] (v: T)(i: ?[T], o: ![T]) = proc { attempt { o!v; repeat { o!(i?) } } {}
i.closeIn; o.closeOut }



}
