package chapter4

import scala.{Option => _, Either => _, Left => _, Right => _, _}

sealed trait Either[+E,+A]
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def map[E,A,B](a: Either[E, A])(f: A => B) : Either[E, B] = a match {
    case Left(e) => Left(e)
    case Right(e) => Right(f(e))
  }

  def flatMap[E, A, EE >: E, B](a: Either[E, A])(f: A => Either[EE, B]): Either[EE, B] = a match {
    case Left(e) => Left(e)
    case Right(e) => f(e)
  }

  def orElse[A, E, EE >: E, AA >: A](a: Either[E, A])(b: => Either[EE, AA]): Either[EE, AA] = a match {
    case Left(e) => b
    case Right(v) => a
  }

  def map2[A, E, EE >: E, B, C](a:Either[E, A], b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (a, b) match {
    case (Left(a), _) => Left(a)
    case (_, Left(b)) => Left(b)
    case (Right(a), Right(b)) => Right(f(a,b))
  }


  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sys.error("todo")

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = sys.error("todo")

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}