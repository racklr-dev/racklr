// Multi-line bracket continuations, operator-at-end-of-line continuations and
// leading-dot chaining.
module fixtures/continuations

val matrix = [ [1, 2, 3],
               [4, 5, 6],
               [7, 8, 9] ]

fun between( i : int ) : total bool
  ((i >= 0x0300 && i <= 0x036F) ||
   (i >= 0x1AB0 && i <= 0x1AFF) ||
   (i >= 0x1DC0 && i <= 0x1DFF))

fun long-sum( x : int ) : total int
  x +
    2 +
    3

fun chained( x : int ) : total int
  x
    .inc
    .inc
