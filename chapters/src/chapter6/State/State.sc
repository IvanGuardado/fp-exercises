import chapter6.RNG.Simple
import chapter6.RNG._

val s = unit(1)

map(s)(a => a + 2)(Simple(2))

nonNegativeInt(Simple(Int.MinValue))

double(Simple(100))

intDouble(Simple(1))

ints(5)(Simple(1))

map2(unit(1), unit(2))(_ + _)(Simple(1))