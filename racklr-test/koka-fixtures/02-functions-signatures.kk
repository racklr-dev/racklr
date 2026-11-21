// Function signatures: type parameters, typed parameters, result types and
// the effect-row shapes total, closed (<exn,div>) and open (e).
module fixtures/functions

pub fun swap<a,b>( pair : (a,b) ) : total (b,a)
  ( pair.snd, pair.fst )

pub fun mapped<a,b,e>( xs : list<a>, f : a -> e b ) : e list<b>
  xs.map(f)

pub fun risky( n : int ) : <exn,div> int
  if n < 0 then throw("negative") else n

pub fun chatty( msg : string ) : console ()
  println(msg)

pub fun lifted<a,e>( action : () -> e a ) : e a
  action()
