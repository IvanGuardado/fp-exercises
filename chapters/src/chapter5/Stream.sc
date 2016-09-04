import chapter5.Stream
import chapter5.Stream._

val st = cons(1, cons(2, empty))
toList(st)

toList(drop(st, 0))
toList(drop(st, 1))
toList(drop(st, 3))

toList(take(st, 0))
toList(take(st, 1))
toList(take(st, 3))

toList(takeWhile(st)(_ < 0))
toList(takeWhile(st)(_ < 2))
toList(takeWhile(st)(_ < 3))

forall(st)(a => true)
forall(st)(a => a < 3)
forall(st)(a => a > 1)

headOption(st)
headOption(empty)

toList(map(st)(_ + 1))

toList(filter(st)(_ < 1))
toList(filter(st)(_ < 2))
toList(filter(st)(_ < 3))

toList(append(st, cons(3, empty)))

toList(flatMap(st)(a => cons(a, cons(a+1, empty))))

val st2 = cons(1,cons(2, cons(3, cons(4, empty))));
def _map(a: Int) : Int = {
  println("map")
  a + 10
}

def _filter(a: Int) : Boolean = {
  println("filter")
  a % 2 == 0
}
val res = filter(map(st2)(_map))(_filter)

toList(take(res, 1))

val ones = constant(1)
toList(take(ones, 3))

toList(take(from(5), 5))

toList(take(fibs, 10))

toList(take(unfold(0)(s => Some(s, s+1)), 5))

toList(zipWith(Stream(1, 2, 3, 4), Stream(1, 2, 3, 4, 5))(_+_))

toList(zipAll(Stream(1, 2, 3, 4), Stream(1, 2, 3, 4, 5)))
toList(zipAll(Stream(1, 2, 3, 4, 5), Stream(1, 2, 3, 4)))

startsWith(Stream(1, 2, 3, 4, 5), Stream(1, 2, 3))


hasSubsequence(Stream(1, 2, 3, 4, 5), Stream(2, 4))


toList(scanRight(Stream(1,2,3), 0)(_+_))