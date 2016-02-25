import fpinscala.datastructures.List
import fpinscala.datastructures.Cons
import fpinscala.datastructures.Nil
import fpinscala.datastructures.List.sum

object Hi {
  def main(args: Array[String]):Unit =
    add(1,1)
    println("Hi!")

  def add(a:Int, b: Int) =
    a + b

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}
