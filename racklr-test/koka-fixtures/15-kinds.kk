// Explicit kind annotations on abstract type declarations (the :: forms) and
// the @-prefixed identifiers the Koka 3.x front end permits.
module fixtures/kinds

pub type global-scope :: S
pub type alloc :: H -> X
pub type total-effect :: E

pub fun @default-value() : total int
  0

fun use-default() : total int
  @default-value()
