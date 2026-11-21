// Top-level val declarations (typed and inferred) and local val/var bindings.
module fixtures/bindings

val greeting : string = "hello"
val numbers = [1, 2, 3]

fun counter( start : int ) : total int
  var n := start
  n := n + 1
  val step = 2
  n + step

fun local-bindings( x : int ) : total int
  val bumped = x + 1
  val doubled = bumped * 2
  doubled
