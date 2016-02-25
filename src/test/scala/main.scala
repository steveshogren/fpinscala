import fpinscala.datastructures.List
import fpinscala.datastructures.Cons
import fpinscala.datastructures.Nil
import fpinscala.datastructures.List.sum
import hof.HOF

import collection.mutable.Stack
import org.scalatest._

class HOFSpec extends FlatSpec with Matchers {

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
