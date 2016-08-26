package chapter3

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