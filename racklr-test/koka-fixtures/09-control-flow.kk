// if/elif/else, a braced match expression, and a return in an if.
module fixtures/control

fun sign( n : int ) : total int
  if n < 0 then 0 - 1
  elif n == 0 then 0
  else 1

fun braced( x : int ) : total int
  match x {
    0 -> 1
    _ -> 2
  }

fun early( xs : list<int> ) : total int
  if xs.is-empty then return 0
  xs.head
