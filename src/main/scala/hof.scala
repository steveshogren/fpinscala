package hof

object HOF {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a:A) => { (b:B) => f(a,b) }


  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def g(rest: Array[A], a: A, b: A) : Boolean = {
      var o :Boolean = ordered(a,b)
      if (rest.length == 0 || rest.length == 1) o
      else o && g(rest.drop(1), b, rest(0))
    }
    g(as.drop(1), as(0), as(1))
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int) : Int =
      if (n <= 0) a
      else go(n-1, b, a+b)
    go(n, 0, 1)
  }

}
