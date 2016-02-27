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

  @annotation.tailrec
  def dropWhile[A](l: List[A], pred: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if (pred(x)) => dropWhile(xs, pred)
      case _ => l
    }
  }

  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def g(rest: List[A], ret: List[A]) : List[A] = {
      rest match {
        case Cons(x, Nil) => Cons(x, ret)
        case Cons(x, Cons(y, ys)) => g(Cons(y, ys), Cons(x, ret))
        case Nil => Nil
      }
    }
    g(l, Nil)
  }

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def g(rest: List[A], ret: List[A]) : List[A] = {
      rest match {
        case Cons(x, Nil) => ret
        case Cons(x, Cons(_, Nil)) => Cons(x, ret)
        case Cons(x, Cons(y, ys)) => g(Cons(y, ys), Cons(x, ret))
        case Nil => Nil
      }
    }
    reverse(g(l, Nil))
  }


}
