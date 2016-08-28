package chapter4

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A]
case object None extends Option[Nothing]
case class Some[+A](v: A) extends Option[A]

object Option {
  def apply[A](as: A*): Option[A] =
    if (as.isEmpty) None
    else Some(as.head)

  def map[A,B](o: Option[A])(f: A => B) : Option[B] = o match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[A,B>:A](o: Option[A], default: => B): B = o match {
    case Some(v) => v
    case None => default
  }

  def flatMap[A,B](o: Option[A])(f: A => Option[B]): Option[B] = getOrElse(map(o)(f), None)

  def orElse[A,B>:A](o: Option[A], ob: => Option[B]): Option[B] = getOrElse(map(o)(Option(_)), ob)

  def filter[A](o: Option[A])(f: A => Boolean): Option[A] = flatMap(o)(a => if(f(a)) Option(a) else Option())

  def mean(xs: Seq[Double]) : Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]) : Option[Double] = {
    Option.flatMap(mean(xs))(m => {
      Option(xs.map(x => Math.pow(x - m, 2)).sum / xs.length)
    })
  }

  def lift[A,B](f: A => B) : Option[A] => Option[B] = Option.map(_)(f)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C) : Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Option(f(a, b))
    case (None, _) => None
    case (_, None) => None
  }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] = traverse(xs)(a => a)

  def traverse[A,B](xs: List[A])(f: A => Option[B]) : Option[List[B]] = xs.foldRight[Option[List[B]]](Some(Nil))((a, b) => f(a) match {
    case None => None
    case Some(value) => Option.flatMap(b)(xs => Some(value :: xs))
  })
}
