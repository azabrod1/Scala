package lastpracs
import io.threadcso._

object Change
{ def Changer(inPound: ?[Int], change: ![Int]) =
proc {
var credit = 0 // credit in pence

serve(
     (credit >= 5 && change)    =!=> {  credit -= 5; 5}
    |(credit >= 10 && change)   =!=> {  credit -= 10; 10}
    |(credit >= 20 && change)   =!=> {  credit -= 20; 20;}
    | (credit == 0 && inPound)  =?=> { _ => credit += 100;}
    )
    
}

def Inputter(slot: ![Int], change: ?[Int]) = proc{
  slot!(1);
  var money  = 0; var c  = -1;
 while(money < 100){
   c = change?;
   money += c;
   print(c);
   
 }
       
}

def main(args: Array[String]){ // a minimal test rig { val twenty = OneOne[Unit]("twenty")

val change = OneOne[Int]("change channel") 
val pounds = OneOne[Int]("pounds") 

val c = Changer(pounds, change) || Inputter(pounds, change);
c();

}


}

