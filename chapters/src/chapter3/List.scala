package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ls: List[Int]): Int = foldLeft(ls, 0)(_ + _)

  def product(ls: List[Double]): Double = foldLeft(ls, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, b) => Cons(a, b))

  def tail[A](list : List[A]) : List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](list : List[A], head : A) : List[A] = Cons(head, list)

  def drop[A](list: List[A], n: Int) : List[A] = {
    def loop(i: Int, l: List[A]) : List[A] = {
      if (i == n) l
      else loop(i+1, tail(l))
    }
    loop(0, list)
  }

  def dropWhile[A](list: List[A])(p: A => Boolean) : List[A] = {
    def loop(l: List[A]) : List[A] = {
      l match {
        case Nil => Nil
        case Cons(head, xs) => if (p(head)) loop(tail(l)) else l
      }
    }
    loop(list)
  }

  def init[A](list: List[A]) : List[A] = {
    def loop(l: List[A], result: List[A]) : List[A] = {
      l match {
        case Nil => Nil
        case Cons(head, Nil) => result
        case Cons(head, xs) => loop(xs, append(result, List(head)))
      }
    }
    loop(list, Nil)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = //foldLeft(as, z)((a,b) => f(b,a))
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A,B](as: List[A]) : Int = {
    foldRight(as, 0)((a, n) => n + 1)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def loop(b: B, l: List[A]): B = l match {
      case Nil => b
      case Cons(x, xs) => loop(f(b, x), xs)
    }

    loop(z, l)
  }

  def concatenate[A](ls: List[List[A]]) : List[A] = {
    foldRight(ls, List[A]())((b, a) => append(b, a))
  }

  def map[A,B](ls: List[A])(f: A => B) : List[B] = foldRight(ls, List[B]())((i, list) => Cons(f(i), list))

  def filter[A](ls: List[A])(p: A => Boolean) = flatMap(ls)(a => if(p(a)) List(a) else Nil)

  def flatMap[A,B](ls: List[A])(f: A => List[B]) : List[B] = concatenate(map(ls)(f))

  def zipWith[A,B,C](ls1: List[A], ls2: List[B])(f: (A, B) => C) : List[C] = (ls1, ls2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, xs1), Cons(h2, xs2)) => Cons(f(h1, h2), zipWith(xs1, xs2)(f))
  }
}