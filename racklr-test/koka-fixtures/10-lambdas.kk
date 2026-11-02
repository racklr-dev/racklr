// fn lambdas: expression bodies, braced bodies, typed parameters and trailing
// lambda arguments.
module fixtures/lambdas

fun compose( x : int ) : total int
  val add-one = fn(i) i + 1
  val twice = fn(i){ add-one(add-one(i)) }
  twice(x)

fun typed( f : int -> int, x : int ) : total int
  f(x)

fun map-twice( xs : list<int> ) : total list<int>
  xs.map( fn(i) i * 2 ).map( fn(i) i + 1 )

fun trailing( xs : list<int> ) : total list<int>
  xs.map fn(i)
    i * 3
