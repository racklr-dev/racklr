# JSX ANTLR4 grammars (authored for racklr)

`JSXLexer.g4` and `JSXParser.g4` parse the JSX element/attribute/children
syntax; the surrounding TypeScript/JavaScript is parsed by the TypeScript
grammar after `racklr/tsx-preprocess.rkt` swaps each JSX expression out for a
placeholder and records the JSX tree. They are authored for racklr (not copied
verbatim from upstream), using the same lexer-mode/brace conventions the
grammar generator supports: a mode-stack replaces upstream's
`ProcessOpenBrace`/`ProcessCloseBrace` semantic predicates, and the shared
token names (`<`, `</`, `{`) are consistent across modes.

The pair is tracked here (like the other `grammars/<lang>/` directories) so a
clean checkout runs the JSX/TSX front ends without the untracked vendored
`grammars-v4/` tree; it was previously an untracked
`grammars-v4/javascript/jsx-cleaned/` copy.
