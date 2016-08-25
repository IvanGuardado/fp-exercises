def fib1(nth : Int) : Int = {

  def loop(i : Int, p1 : Int, p2 : Int) : Int = {
    if (i == nth) p1 + p2
    else loop(i + 1, p2, p1 + p2)
  }

  nth match {
    case 0 => 0
    case 1 => 1
    case _ => loop(2, 0, 1)
  }

}


def isOrdered[A](as : Array[A], f: (A, A) => Boolean) : Boolean = {
  val length = as.length;

  def loop(i : Int) : Boolean = {
    if (i + 1 == length) f(as(i), as(i - 1))
    else f(as(i), as(i - 1)) match {
      case true => loop(i + 1)
      case false => false
    }
  }

  length match {
    case 0 => true
    case 1 => true
    case _ => loop(1)
  }
}

def isGreater(a : Int, b : Int) = a < b

val numbers = Array()
isOrdered(numbers, isGreater)

def curry[A,B,C](f: (A,B) => C) : A => (B => C) = a => b => f(a, b)

val isGreatherThan2 = curry(isGreater)(2)
isGreatherThan2(3)

def uncurry[A,B,C](f: A => (B => C)) : (A,B) => C = (a, b) => f(a)(b)

val _isGreaterThan = uncurry(curry(isGreater))
_isGreaterThan(2, 4)

def compose[A,B,C](f: B => C, g: A => B) : A => C = a => f(g(a))