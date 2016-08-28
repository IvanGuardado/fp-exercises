import chapter4.Either._

map(Try("unsafe".toInt))(a => a + 1)
map(Try(1))(a => a + 1)

flatMap(Try(1))(a => Try(a.toInt))
flatMap(Try("unsafe"))(a => Try(a.toInt))

orElse(Try(1))(Try(1))
orElse(Try("unsafe".toInt))(Try(2))

map2(Try(1), Try(2))(_ + _)
map2(Try("unsafe".toInt), Try(2))(_ + _)
map2(Try(2), Try("unsafe".toInt))(_ + _)