package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue) (Math.abs(i+1), rng2)
    else (Math.abs(i), rng2)
  }

  def double: Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue.toDouble)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (i, rng2) = rng.nextInt
      val (l, rng3) = ints(count -1)(rng2)
      (i :: l, rng3)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a =>
      flatMap(rb)(b => unit(f(a,b)))
    )

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight((Nil : List[A], rng))((rand, b) => {
      val (i, r2) = rand(b._2)
      (i :: b._1, r2)
    })
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (i, r) = f(rng)
    g(i)(r)
  }
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a =>
      sb.flatMap(b => State.unit(f(a,b)))
    )

  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B](s => {
      val (i, r) = run(s)
      f(i).run(r)
    })
}
object State {
  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] = State(rng => {
    fs.foldRight((Nil : List[A], rng))((rand, b) => {
      val (i, r2) = rand.run(b._2)
      (i :: b._1, r2)
    })
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

//object State {
//  type Rand[A] = State[RNG, A]
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
//}