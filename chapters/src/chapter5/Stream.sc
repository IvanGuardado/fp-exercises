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