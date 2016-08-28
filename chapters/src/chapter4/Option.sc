import chapter4._

Option.map(Some(1))(1 + _)
Option.map(None : Option[Int])(1 + _)

Option.getOrElse(Some(1), 2)
Option.getOrElse(None : Option[Int], 2)

Option.flatMap(Some(1))(a => Some(3))
Option.flatMap(None)(a => Some(2))

Option.orElse(Some(1), Some(2))
Option.orElse(None, Some(2))

Option.filter(Some(1))(_ > 1)
Option.filter(Some(1))(_ < 10)

Option.orElse(Some(1), Some(2))
Option.orElse(None, Some(2))


Option.mean(Seq(8.0, 9.0, 10.0))
Option.variance(Seq(1, 2, 3, 4, 5, 6))

Option.map2(Some(1), Some(2))(_ + _)
Option.map2(None : Option[Int], Some(2))(_ + _)
Option.map2(Some(1), None: Option[Int])(_ + _)

Option.sequence(List(Some(1), Some(2), Some(3)))
Option.sequence(List(Some(1), None, Some(3)))

Option.sequence(Nil)

Option.traverse(List(1, 2))(a => if (a > 2) None else Some(a))