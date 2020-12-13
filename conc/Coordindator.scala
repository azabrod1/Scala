package conc

trait Coordinator
{ def enter(id: Int): Int // start using the resource
  def exit(id: Int): Int // finish using the resource }
}
