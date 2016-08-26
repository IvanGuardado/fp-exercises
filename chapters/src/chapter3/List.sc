import chapter3.List
import chapter3.List._

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