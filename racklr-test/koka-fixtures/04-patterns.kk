// match: constructor, literal, wildcard, as, tuple and list patterns, and
// boolean guards. (Koka 3.x has no or-patterns; `|` introduces a guard.)
module fixtures/patterns

type shape
  Circle( radius : int )
  Rect( width : int, height : int )

fun area( s : shape ) : total int
  match s
    Circle(r) -> 3 * r * r
    Rect(w,h) -> w * h

fun classify( x : maybe<int> ) : total int
  match x
    Just(0) -> 0
    Just(v) | v > 0 -> v
    Just(v) -> 0 - v
    Nothing -> 0 - 1

fun named-whole( x : maybe<int> ) : total int
  match x
    whole as Just(v) -> v
    whole as Nothing -> 0

fun first-two( xs : list<int> ) : total int
  match xs
    [a,b] -> a + b
    _ -> 0

fun pair-sum( p : (int,int) ) : total int
  match p
    (a,b) -> a + b
