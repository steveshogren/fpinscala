import fpinscala.datastructures.List
import fpinscala.datastructures.Cons
import fpinscala.datastructures.Nil
import fpinscala.datastructures.List.sum
import hof.HOF

import collection.mutable.Stack
import org.scalatest._

class HW2P2 extends FlatSpec with Matchers {
  // Implement isSorted , which checks whether an Array[A] is sorted according to a
  // given comparison function:
  // def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
  "isSorted" should "prove 1,2 sorted" in {
    HOF.isSorted(Array[Int](1,2), (a:Int, b:Int) => { a < b}) should be (true)
  }
  "isSorted" should "prove 2,1,4 not sorted" in {
    HOF.isSorted(Array[Int](2,1,3), (a:Int, b:Int) => { a < b}) should be (false)
  }
  "isSorted" should "prove 1,2,1 not sorted" in {
    HOF.isSorted(Array[Int](1, 2, 1), (a:Int, b:Int) => { a < b}) should be (false)
  }
}

class HW2P1 extends FlatSpec with Matchers {
  // Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
  //                                                                 The first two Fibonacci numbers are 0 and 1 . The nth number is always the sum of the
  //                                                               previous two—the sequence begins 0, 1, 1, 2, 3, 5 . Your definition should use a
  //                                                               local tail-recursive function.
  //                                                               def fib(n: Int): Int
  "fib" should "give fib numbers" in {
    HOF.fib(4) should be (3)
    HOF.fib(3) should be (2)
    HOF.fib(2) should be (1)
    HOF.fib(1) should be (1)
    HOF.fib(0) should be (0)
  }
}

class ExampleSpec2 extends FlatSpec with Matchers {
  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }
}
