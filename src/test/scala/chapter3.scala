package hof

import fpinscala.datastructures._
import fpinscala.datastructures.List._

import collection.mutable.Stack
import org.scalatest._

class HW3P5 extends FlatSpec with Matchers {
  "dropWhile" should "correctly drop while true" in {
    dropWhile(List(1, 2, 3), (a:Int) => {a == 1}) should be (List(2,3))
  }
  it should "correctly drop with other type" in {
    dropWhile(List(1, 2, 3), (a:Int) => {a <= 3}) should be (Nil)
  }
}

// Generalize tail to the function drop , which removes the first n elements from a list.
//   Note that this function takes time proportional only to the number of elements being
// dropped—we don’t need to make a copy of the entire List .
// def drop[A](l: List[A], n: Int): List[A]
class HW3P4 extends FlatSpec with Matchers {
  "drop" should "correctly drop the first character" in {
    drop(List(1, 2), 1) should be (List(2))
  }
  it should "correctly drop the first several characters" in {
    drop(List(1, 2, 3, 4), 2) should be (List(3, 4))
  }
}

// Using the same idea, implement the function setHead for replacing the first element
// of a List with a different value.
class HW3P3 extends FlatSpec with Matchers {
  "setHead" should "correctly return with a new head" in {
    setHead(List(1,2), 6) should be (List(6, 2))
  }
}

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
