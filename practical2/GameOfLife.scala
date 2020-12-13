package practical2

import io.threadcso.`package`.NanoTime

object GameOfLife {

 def main(args: Array[String]): Unit = {
    val N = 100;
  
 val image = new Array[Boolean](N*N);
 
 val r = scala.util.Random

 
 //for(i <- 0 until N*N){
 //  r.nextInt(3)
   
   
 
 //  image(4*i) = true;
 
 
   image(18*N + 22) = true;
   image(18*N + 21) = true;
   image(19*N + 21) = true;
   image(19*N + 20) = true;

   image(20*N + 21) = true;
 
 
 
   image(20*N + 20) = true;
   image(20*N + 21) = true;
   image(20*N + 24) = true;
   image(20*N + 25) = true;
   image(20*N + 26) = true;
   image(18*N + 21) = true;
   image(19*N + 23) = true;

   
 
   // val game = new TheGameOfLife(2101, image, 5, 0)

    //game.play
    
     val v = System.currentTimeMillis()
   
    val game = new GameOfMessages3(21000, image, 5, -1)
     
     println(System.currentTimeMillis() - v)
   
   game.play

    
    
    
    
    
  }
 

  
  

  
}