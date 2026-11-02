// A small mixed module in the shape of a real application file: imports,
// types, an effect, a handler, and a small pipeline.
module fixtures/app/pipeline

import std/core/types
import std/core/list

pub type step
  Parse
  Render( width : int )

effect trace
  fun record( msg : string ) : ()

fun describe( s : step ) : total string
  match s
    Parse -> "parse"
    Render(w) -> "render " ++ w.show

fun pipeline( steps : list<step> ) : <trace> (list<string>)
  with fun record(msg)  ()
  steps.map( fn(s) describe(s) )

pub fun main()
  pipeline([Parse, Render(80)]).map(println)
