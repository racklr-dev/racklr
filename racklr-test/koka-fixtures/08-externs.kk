// External imports and external declarations, including an @-prefixed
// external name (allowed by the Koka 3.x front end).
module fixtures/externs

extern import
  c  file "fixtures-ext"
  js file "fixtures-ext.js"

extern ext-add( x : int, y : int ) : int
  c  inline "((#1) + (#2))"
  js inline "((#1) + (#2))"

pub extern @ext-null( x : int ) : maybe<int>
  c  inline "(kk_integer_from_small(#1))"
  js inline "(#1)"
