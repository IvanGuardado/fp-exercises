package chapter5

sealed trait Stream [+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, xs: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, xs: => Stream[A]) : Stream[A] = {
    lazy val head = h
    lazy val tail = xs
    Cons(() => head, () => tail)
  }

  def empty[A] : Stream[A] = Empty

  def apply[A](sq: A*) : Stream[A] = if (sq.isEmpty) empty else cons(sq.head, apply(sq.tail : _*))

  def toList[A](st: Stream[A]) : List[A] = st match {
    case Empty => Nil
    case Cons(h, xs) => h() :: toList(xs())
  }

  def take[A](st: Stream[A], n: Int) : Stream[A] = {
    if (n == 0) empty else st match {
      case Empty => empty
      case Cons(h, xs) => cons(h(), take(xs(), n-1))
    }
  }

  def takeWhile[A](st: Stream[A])(p: A => Boolean): Stream[A] =
    foldRight(st, empty: Stream[A])((a, b) => if(p(a)) cons(a, takeWhile(b)(p)) else b)


  def drop[A](st: Stream[A], n: Int) : Stream[A] = if (n == 0) st else st match {
    case Empty => empty
    case Cons(h, xs) => drop(xs(), n-1)
  }

  def foldRight[A,B](a: Stream[A], z: => B)(f: (A, => B) => B): B =
    a match {
      case Cons(h,t) => f(h(), foldRight(t(),z)(f))
      case _ => z
    }

  def exists[A](xs: Stream[A])(p: A => Boolean): Boolean =
    foldRight(xs, false)((a, b) => p(a) || b)

  def forall[A](xs: Stream[A])(p: A => Boolean) : Boolean =
    foldRight(xs, true)((a,b) => p(a) && b)

  def headOption[A](xs: Stream[A]) : Option[A] = foldRight(xs, None: Option[A])((a,b) => Some(a))

  def map[A,B](xs: Stream[A])(f: A => B): Stream[B] = foldRight(xs, empty: Stream[B])((a,b) => cons(f(a), b))

  def filter[A](xs: Stream[A])(p: A => Boolean): Stream[A] =
    foldRight(xs, empty: Stream[A])((a, b) => if(p(a)) cons(a, b) else b)

  def append[A, B>:A](xs1: Stream[A], xs2: => Stream[B]): Stream[B] =
    foldRight(xs1, xs2)((a,b) => cons(a, b))

  def flatMap[A,B](xs: Stream[A])(f: A => Stream[B]): Stream[B] =
    foldRight(xs, empty: Stream[B])((a, b) => append(f(a), b))

  def constant[A](v: A) : Stream[A] = unfold(v)(a => Some(a, a))

  def from(v: Int) : Stream[Int] = unfold(v)(a => Some(a, a + 1))

  def fibs(): Stream[Int] = unfold((0, 1))(s => {
    val (a, b) = s
    Some(a, (b, a + b))
  })

  def unfold[A, S](z: S)(f: S => Option[(A, S)]) : Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def zipWith[A,B,C](s1: Stream[A], s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((s1, s2))(_ match {
    case (Cons(x, xs), Cons(x2, xs2)) => Some(f(x(), x2()), (xs(), xs2()))
    case _ => None
  })

  def zipAll[A,B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A],Option[B])] =  unfold((s1, s2))(_ match {
    case (Cons(x, xs), Cons(x2, xs2)) => Some((Some(x()), Some(x2())), (xs(), xs2()))
    case (Cons(x, xs), _) => Some((Some(x()), None), (xs(), empty))
    case (_, Cons(x, xs)) => Some((None, Some(x())), (empty, xs()))
    case _ => None
  })

  def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean = forall(zipWith(s1, s2)((a, b) => a == b))(_ == true)

  def tails[A](a: Stream[A]): Stream[Stream[A]] = unfold(a)(s => s match {
    case Cons(x, xs) => Some((s, xs()))
    case _ => None
  })

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean = exists(tails(s1))(startsWith(_, s2))

  def scanRight[A,B](a: Stream[A], v: B)(f: (A, => B) => B): Stream[B] =
    unfold(a)(s => s match {
      case Cons(x, xs) => Some((foldRight(s, v)(f), xs()))
      case Empty => None
    })
}
