// Effect declarations: a block form with fun and ctl operations, the one-line
// operation forms, and a named effect.
module fixtures/effects

pub effect counter
  fun next() : int
  ctl bump( n : int ) : ()

named effect logger
  fun log( msg : string ) : ()

effect fun total-count() : int
effect val current-name : string
