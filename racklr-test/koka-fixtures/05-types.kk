// Type declarations: a polymorphic ADT with field names, a struct, and
// aliases (one of them carrying an effect row).
module fixtures/types

pub type tree<a>
  Leaf
  Node( left : tree<a>, value : a, right : tree<a> )

pub struct point2d(
  x : int,
  y : int
)

pub alias pair<a> = (a, a)
pub alias parser<e,a> = () -> <parse|e> a
