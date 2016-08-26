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
