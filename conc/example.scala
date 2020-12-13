package conc
import io.threadcso._


object example {
  
  def printInt(toPrint : Int) = proc{
    for(i <- 1 to 100){
      print(toPrint); //sleep(seconds(0))
    }
    
  }
  
  
  def main(args : Array[String]){
    val printers = || (for(i <- 0 until 3) yield printInt(i)) || printInt(4) || printInt(5);
    printers()
  }
  
  
  
}