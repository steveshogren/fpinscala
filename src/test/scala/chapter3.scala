package hof

import fpinscala.datastructures._
import fpinscala.datastructures.List._

import collection.mutable.Stack
import org.scalatest._

class HW3P1 extends FlatSpec with Matchers {
  "compose" should "correctly compose" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x should be (3)
  }
}
