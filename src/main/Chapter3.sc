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

import List._

tail(List(1, 2, 3))
tail(List(1))
tail(Nil)

setHead(List(2,3), 1)
setHead(Nil, 1)

drop(List(1, 2, 3), 0)

dropWhile(List(1, 2, 3)) (_ < 3)

init(List())

length(List(1, 2))

foldLeft(List(1, 2, 3), 0)((i, value) => i + value)

append(List(1), List(2, 3))

concatenate(List(List(1, 2), List(3, 4)))

map(List(1, 2, 3))(_ + 1)

filter(List(1, 2, 3))(_ < 3)

flatMap(List(1, 2, 3))(a => List(a, a))

flatMap(List(1, 2, 3))(a => map(List(4, 5, 6))(a + _))

zipWith(List(1, 2, 3), List(1, 2, 3))((a, b) => a+b)

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree : Tree[A]) : Int =  tree match {
    case Branch(l, r) => size(l) + size(r) + 1
    case Leaf(v) => 1
  }

  def maximum(tree: Tree[Int]) : Int = {
    tree match {
      case Branch(l, r) => maximum(l) max maximum(r)
      case Leaf(v) => v
    }
  }

  def map[A,B](tree: Tree[A])(f: A => B) : Tree[B] = tree match {
    case Branch(l, r) => Branch(map(l)(f),map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }


}

val tree = Branch(Branch(Leaf(5), Leaf(6)), Branch(Leaf(2), Leaf(3)))
Tree.size(tree)
Tree.maximum(tree)
Tree.map(tree)(_ + 1)
