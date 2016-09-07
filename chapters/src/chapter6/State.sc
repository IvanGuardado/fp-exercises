import chapter6.RNG.Simple
import chapter6.RNG._
import chapter6._

val s = unit(1)

map(s)(a => a + 2)(Simple(2))

nonNegativeInt(Simple(Int.MinValue))

double(Simple(100))

intDouble(Simple(1))

ints(5)(Simple(1))

map2(unit(1), unit(2))(_ + _)(Simple(1))

sequence(List(unit(1), unit(2)))(Simple(1))

flatMap(unit(2))(a => if (a == 1) unit(2) else unit(3))(Simple(1))

State(unit(1)).flatMap(a => if (a == 1) State.unit(2) else State.unit(3)).run(Simple(1))

State(unit(1)).map(_ + 1).run(Simple(100))

State(unit(1)).map2(State(unit(2)))(_+_).run(Simple(100))

val l = List[State[Int, Int]](State.unit(1), State.unit(2))
State.sequence(l).run(100)

val inputs = List[Input](Coin, Turn, Turn, Coin, Turn)

Machine.simulateMachine(inputs).run(Machine(true, 10, 0))
