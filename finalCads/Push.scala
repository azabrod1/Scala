package finalCads

class Push[A](val toPush : A) extends Operation[A](true) {
  	
  def getNext() : Push[A] = this.next.asInstanceOf[Push[A]];
  
}
