// Operator and fixity declarations, plus an operator function definition.
module fixtures/operators

pub infixr 60 (+++)
pub infixl 70 (***)
pub infix  40 (===)

pub fun (+++)( a : int, b : int ) : total int
  a + b + b

pub fun (***)( a : int, b : int ) : total int
  a * b

fun combine( a : int, b : int ) : total int
  a +++ b *** b

fun same( a : int, b : int ) : total bool
  a === b
