package hof

import fpinscala.datastructures._
import fpinscala.datastructures.List._

import collection.mutable.Stack
import org.scalatest._

// Implement the function tail for removing the first element of a List . Note that the
// function takes constant time. What are different choices you could make in your
// implementation if the List is Nil ? We’ll return to this question in the next chapter.
class HW3P2 extends FlatSpec with Matchers {
  "tail" should "correctly return all but the head" in {
    tail(List(1,2,3,4,5)) should be (List(2,3,4,5))
  }
  it should "correctly return Nil on Nil" in {
    tail(Nil) should be (Nil)
  }
  it should "correctly return Nil on one" in {
    tail(List(1)) should be (Nil)
  }
}

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
