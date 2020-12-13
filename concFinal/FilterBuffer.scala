package concFinal

trait FilterBuffer[T] {
  def put(datum : T)
  def get(filter : T => Boolean) : T
}
