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
}
