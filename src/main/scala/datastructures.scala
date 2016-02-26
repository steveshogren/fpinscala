package fpinscala.datastructures
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] =
    as match {
      case Cons(_, x) => x
      case Nil => Nil
    }

  def setHead[A](as: List[A], newHead:A): List[A] =
    as match {
      case Cons(_, x) => Cons(newHead,x)
      case Nil => Nil
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def g(rest: List[A], n: Int) : List[A] = {
      if (n > 0) g(tail(rest), n-1)
      else rest
    }
    g(l, n)
  }

}
