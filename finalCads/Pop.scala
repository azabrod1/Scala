package finalCads

class Pop[A] extends Operation[A](false) {
  private var result : Option[A] = null
  
  def getNext() : Pop[A] = this.next.asInstanceOf[Pop[A]];
  
  def finish(_result : Option[A]) : Unit  = {
    result = _result
    super.finish()
  }
  
  def getResult : Option[A] = result
  
}
