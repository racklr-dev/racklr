// Handler expressions: with ... handler, handle(...), single-operation
// shorthands, named handlers, and resumption inside a ctl clause.
module fixtures/handlers

effect counter
  fun next() : int
  ctl bump( n : int ) : ()

named effect logger
  fun log( msg : string ) : ()

effect choose
  ctl choose() : bool

fun run-counter( action : () -> <counter|e> a ) : e (a,int)
  var total := 0
  with handler
    return(x)  (x,total)
    fun next()  total
    ctl bump(n)  total := total + n
  action()

fun run-handle( action : () -> <counter|e> a ) : e a
  handle(action)
    return(x)  x
    fun next()  0

fun all-choices( action : () -> <choose|e> a ) : e (list<a>)
  with ctl choose() -> append( resume(True), resume(False) )
  [action()]

fun run-named( action : () -> <logger|e> a ) : e a
  with h <- named handler
    fun log(msg)  ()
  action()

fun run-single( action : () -> <counter|e> a ) : e a
  with ctl bump(n)  ()
  action()
