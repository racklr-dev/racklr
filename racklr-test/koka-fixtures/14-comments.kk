/* Nested block comments are supported: /* an inner block comment */
   and the outer comment continues to here. */
module fixtures/comments

// A line comment before a declaration.
pub fun add( x : int, y : int ) : total int
  // A line comment inside a block.
  x + y  // and a trailing line comment

/* A block comment inside a block,
   spanning several lines. */
fun sub( x : int, y : int ) : total int
  x - y
