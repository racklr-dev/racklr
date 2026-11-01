# Rust ANTLR4 grammar (vendored + locally curated)

`RustLexer.g4` and `RustParser.g4` are vendored from
[antlr/grammars-v4](https://github.com/antlr/grammars-v4) at `rust/`
(commit `b96e04db9d54bb313e542cc3e6ee913900a21520`), where they are published
under the MIT license reproduced in the file headers.

They are **not** verbatim upstream: racklr's ANTLR4 parser generator is a PEG
code generator (ordered choice is first-success, repetition is greedy, `# Alt`
labels are dropped, and Unicode property classes are unimplemented), so the
grammar was curated to the subset that generator can execute. Every local
change is listed below; the Rust front end (`racklr/frontends/rust.rkt`) is
tested against exactly this grammar (`racklr-test/lower-rust-test.rkt`).

## Local modifications

`RustLexer.g4`:

1. `XID_Start` → `[A-Za-z]` and `XID_Continue` → `[A-Za-z0-9_]`. Upstream uses
   `[\p{L}\p{Nl}]…[\p{Mn}\p{Mc}\p{Nd}\p{Pc}]`; without Unicode property support
   `i32` lexed as `i` + `32` and `read_to_string` split at the underscore.
   Non-ASCII identifiers are refused rather than mis-tokenized.
2. `WHITESPACE` `[\p{Zs}]` → `[ \t]` (same reason).

`RustParser.g4`:

1. `expression`: `structExpression` and `macroInvocation` are tried before
   `pathExpression` (first-success ordered choice would otherwise let a path
   prefix shadow a struct literal), and `structExprUnit` is removed from
   `structExpression` (a unit path is just a path expression).
2. A new `expressionNoStruct` rule — a copy of `expression` without the
   `StructExpression_` alternative, the Rust reference's "expression except
   struct expression". It is used for the condition/scrutinee positions of
   `if`, `if let`, `while`, `while let`, `for … in`, and `match`. Without it,
   `if x {}` and `Foo { x: 1 }` cannot both parse without adaptive prediction.
3. `structExprField` / `enumExprField`: alternatives reordered
   longest-first (`(identifier | tupleIndex) COLON expression` before
   `identifier`), because an inline group commits to its first success.
4. `matchArms`: right-recursive form (`matchArm FATARROW matchArmExpression
   matchArms | matchArm FATARROW expression COMMA?`); the greedy `*` form
   could not leave the optional trailing comma.
5. `tupleElements`: `expression (COMMA expression)* COMMA?` instead of
   `(expression COMMA)+ expression?` (greedy repetition without backtracking).
6. `typePathSegment`: `pathIdentSegment (PATHSEP? genericArgs | PATHSEP?
   typePathFn)?` so `io::Result<()>` does not swallow the `::`.

## Known limits (follow-up work, not part of the P2.3 proof)

- No error recovery: parsing is all-or-nothing, and a large real-world file
  can be slow on failure (the generator has no memoization).
- Semantic predicates/actions are compiled as empty success, so upstream's
  float-literal disambiguation predicates have no effect.
- Real-world `.rs` coverage is a follow-up task; the front end is tested on
  representative source (functions, structs, enums, impls, traits, modules,
  imports, control flow, calls, operators, literals, patterns).

## Refresh

```
cp grammars-v4/rust/RustLexer.g4 grammars-v4/rust/RustParser.g4 grammars/rust/
```

then re-apply the modifications above.
