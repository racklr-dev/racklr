# Haskell ANTLR4 grammar (authored pass-1 subset)

`Haskell.g4` is the tracked pass-1 grammar for the Haskell front end. It is
**authored/curated**, not a vendored copy: the upstream antlr/grammars-v4
Haskell pair cannot be executed by racklr's generator because its layout
tokens are produced by a Java base-lexer class that racklr does not run. The
acquisition attempt and its evidence are recorded below, followed by what was
written instead, the generator limits that shaped it, and how to refresh it.

The suite that loads exactly this grammar is
`racklr-test/gen-haskell-test.rkt` (tokenizer assertions, a Phase-1 fixture
set, and reject cases). Phase-1 scope comes from
`docs/plans/haskell-pipeline.md`.

## Upstream acquisition attempt (and why it was not vendored)

Source: [antlr/grammars-v4](https://github.com/antlr/grammars-v4), `haskell/`
on `master`, fetched 2026-11-01:

- `haskell/HaskellLexer.g4` — 1,742 lines
- `haskell/HaskellParser.g4` — 1,771 lines
- `haskell/Java/HaskellBaseLexer.java` — 396 lines, the layout preprocessor

Both `.g4` files are BSD-3-Clause, Copyright (c) 2020 Evgeniy Slobodkin; the
license text is reproduced at the bottom of this file. Upstream's parser uses
`options { tokenVocab = HaskellLexer; }`, and the lexer uses
`options { superClass = HaskellBaseLexer; }`.

What was tried, from the ticket checkout (all runs need `PLTCOLLECTS` set to the
repository root; see "Refresh" for why):

1. `parse-g4-file` on the upstream parser: succeeds, 257 top-level rule nodes.
2. `generate-parser-module` with `#:source-path` (so the generator loads
   `HaskellLexer.g4` through `tokenVocab`) and `#:indent-tokens? #t`:
   succeeds. It emits a 385,345-byte / 1,900-line Racket module in ~126 ms,
   and the generated module loads and exports `parse` / `tokenize`.
3. The generated lexer runs correctly on ASCII and non-ASCII Haskell:
   `module Foo where\nx = 1\n` tokenizes as
   `MODULE CONID WHERE NEWLINE VARID Eq DECIMAL NEWLINE EOF`, and `λx = 1`
   tokenizes as `VARID "λx" = 1` (upstream encodes its Unicode ranges as
   explicit `\uXXXX` ranges, which the generator's char-class unescape handles).
4. The generated parser cannot run a Haskell program. Every representative
   module fixture fails, and a parse failure surfaces as
   `match-define: no matching clause for #f` because the generated entry point
   destructures the first rule's result instead of returning `#f`:

   | input | result |
   | --- | --- |
   | `module Foo where\nx = 1\n` | error: `match-define: no matching clause for #f` |
   | `module Foo where { x = 1 }` (explicit layout braces) | error: same |
   | `module Foo where { x = 1 ; y = 2 }` | error: same |
   | `import Data.List; x = 1;\n` | tree (headerless import+decl fragment only) |
   | `f x = let y = 1 in y\n` | error: same |

The two root causes are concrete:

- **The layout tokens do not exist.** Upstream's layout rule is implemented in
  `HaskellBaseLexer.java`, the Haskell-2010 layout algorithm: it queues virtual
  `OCURLY`/`VOCURLY`/`SEMI`/`VCCURLY`/`CCURLY` tokens from declared keywords
  and indentation, and hides `NEWLINE`/`WS`/`TAB`. The lexer rules hook into it
  through six action sites (`this.processNEWLINEToken()`,
  `this.processTABToken()`, `this.processWSToken()`, and three
  `this.SetHidden()` calls on `VOCURLY`/`VCCURLY`/`SEMI`). racklr's generator
  compiles lexer actions as empty success, so every one of those calls is a
  no-op. `tokenize` on `f x = let y = 1 in y` therefore returns
  `VARID VARID Eq LET VARID Eq DECIMAL IN VARID NEWLINE EOF` — no `SEMI`, no
  `OCURLY`, no `VOCURLY` — and the parser's `open_`/`close`/`semi` rules can
  never match. (`HaskellParser.g4` has no semantic predicates and no negated
  token sets in parser rules, so those racklr limitations are *not* the
  blocker here.)
- **The upstream lexer cannot feed racklr's indent pass.** Even rewriting the
  parser to use racklr's synthetic `NEWLINE`/`INDENT`/`DEDENT` tokens would not
  help with the upstream lexer: its `NEWLINE` rule matches only the line break
  (`('\r'? '\n' | '\r')`), not the following indentation, while racklr's indent
  pass computes the block indent from the `NEWLINE` token's text and skips the
  whitespace separately. Every line therefore measures indent 0 and
  `INDENT`/`DEDENT` are never inserted — e.g. `class C a where\n  f :: a\n`
  tokenizes as `CLASS CONID VARID WHERE NEWLINE VARID DoubleColon VARID
  NEWLINE EOF` with no `INDENT`/`DEDENT`.

Vendoring ~3,500 lines that cannot execute, while the plan's Phase 1 goal is a
working tracked lexer+parser, is the wrong trade. Phase 1 therefore delivers an
authored grammar over the rule set that racklr can execute, in the same style
as `grammars/koka/Koka.g4`, and names the uncovered constructs as Phase-2 gaps.

## Chosen approach

- **One combined file.** `Haskell.g4` contains both the parser rules and the
  lexer rules (no `tokenVocab` split). It is loaded with
  `(gen-and-load-layout "../grammars/haskell/Haskell.g4")` from
  `racklr-test/`, so tests never touch `/tmp` or the untracked `grammars-v4/`
  checkout. The suite asserts the loaded path is under `grammars/haskell/` and
  never `grammars-v4/`, so it runs in a clean checkout with no vendored tree
  present.
- **Layout against racklr's token model.** The grammar uses the generator's
  synthetic `NEWLINE`/`INDENT`/`DEDENT` tokens and names its structural tokens
  exactly `OPEN_PAREN`, `CLOSE_PAREN`, `OPEN_BRACK`, `CLOSE_BRACK`,
  `OPEN_BRACE`, `CLOSE_BRACE`; the lexer's implicit-line-joining pass drops
  `NEWLINE` while one of those brackets is open, so multi-line lists, tuples and
  record braces lex as one contiguous run. `NEWLINE : ('\r'? '\n' | '\r') [ \t]*`
  captures the next line's leading indentation because that is what racklr's
  indent pass measures.
- **`NEWLINE` is the layout separator.** racklr inserts no virtual `SEMI`
  token, so every layout block is written as
  `NEWLINE INDENT item (NEWLINE item)* NEWLINE? DEDENT` (the `NEWLINE` before a
  `DEDENT` is the end-of-line of the last item). Canonical formatting is
  supported: `where`/`let`/`do`/`case … of`/`data` bodies introduced on the
  keyword's line with the body on the following, deeper-indented lines.
- **Fixed operator-precedence ladder.** `op_expr` (`||`) → `op2` (`&&`) →
  `op3` (comparisons) → `op4` (`+ - ++`) → `op5` (`* / % ^`) → `op6` (`$`) →
  `op7` (`.`) → `op8` (generic `SYMBOL`) → `op9` (unary minus / application) →
  `atom`. Declared `infixl`/`infixr`/`infix` declarations parse but are not
  applied; the plan puts precedence/fixity in the emitter's table, not the
  lowerer, so the pass-1 CST keeps the operator token as-is.
- **Rule-covered lexer, not the full token set.** 23 keywords, ASCII
  identifiers, decimal/octal/hex integers, simple floats, char/string literals,
  line and block comments, `::`/`->`/`<-`/`=>`/`=`/`|`/`\`/`@`/`.`/`,`/`;`/`:`
  /`..`, the common operators, and a `SYMBOL` fallback for other operator
  sequences.

### Phase-1 coverage (fixture set from the plan)

| Construct | Fixture shape exercised | CST rule asserted |
| --- | --- | --- |
| top-level binding + type signature | `f :: Int -> Int` / `f x = x + 1` | `type_sig`, `value_decl`, `funlhs` |
| polymorphic / constrained signatures | `map2 :: (a -> b) -> [a] -> [b]`, `foo :: Eq a => a -> Bool` | `type_sig`, `context`, `constraint` |
| `let` | inline `let y = … in …`; layout block with `in` on the next line | `let_expr`, `let_binds` |
| `where` | `where y = …` on the same line; `where` + layout block | `where_clause` |
| `if`/`then`/`else` | one line; and `if` in a body block with `then`/`else` lines | `if_expr` |
| `case` with patterns and guards | constructor patterns; `n \| n > 0 -> …` guards | `case_expr`, `case_alts`, `case_alt`, `pat` |
| `data`, positional constructors | `data Shape = Circle Double \| Square Double` | `data_decl`, `data_body`, `constr` |
| `data`, layout constructors | `data Shape` then `= …`/`\| …`/`deriving` lines | `data_decl`, `data_item` |
| `data`, named-field constructors | `Person { name :: String, age :: Int }` | `constr`, `field_decl` |
| `data` with type variables / `deriving` | `data Maybe a = … deriving (Show, Eq)` | `tyvar`, `deriving_list` |
| `newtype` | `newtype UserId = UserId Int` | `newtype_decl` |
| typeclass + instance | `class Eq a where eq :: …`; `instance Eq Shape where …` | `class_decl`, `method_sig`, `instance_decl` |
| do-notation | do block with calls, `<-` binds (including a constructor pattern), `let` statements | `do_expr`, `do_stmt` |
| module/import | `module Main where`, export list `(bar, Baz (..))`, `import qualified … as …` | `module_header`, `exports`, `import_decl` |
| lambda | `apply = \x -> x + 1` | `lambda_expr`, `apat` |
| operator expressions | precedence ladder, boolean/comparison, `.`/`$`/custom `<>`, unary `-` | `op_expr`, `rel_op`, `op6`–`op8`, `minus_expr` |
| literals, lists, tuples, fixity, comments | `42`/`2.5e3`/`'a'`/`"hi"`, `[1, 2, 3]`, `(1, "two")`, `infixl 6 <+>`, `--`/`{- -}` | tokenizer + `list_expr`, `fixity_decl` |

The suite also has 15 reject cases (missing bodies, empty `case`/`do`, unclosed
brackets/parens, malformed `data`/`instance`/`module`, stray input) so the
parser is not vacuously accepting everything.

## Generated cost (observed 2026-11-01)

Recorded so a future reader knows the authored grammar's compile cost, and can
compare it with the vendored pair that could not parse (measured with the same
`generate-parser-module` path the tests use):

| grammar | source size | rules | generated Racket module | generation |
| --- | --- | --- | --- | --- |
| `grammars/haskell/Haskell.g4` (authored) | 485 lines | 74 parser + 72 lexer | 95,328 bytes / 877 lines (~93 KB) | ~5 ms |
| upstream `haskell/` pair (not usable) | 1,742 lexer + 1,771 parser lines | 256 parser + 141 lexer + 18 fragment | 378,587 bytes / 1,917 lines (~370 KB) | ~120 ms |

The authored module is about a quarter of the vendored module because it
carries only the pass-1 rule set; the vendored figure is the size that would
have been paid for a grammar the generator cannot execute.

## Local modifications relative to upstream

There is no line-level diff: the grammar was written for racklr's generator
after the acquisition attempt, so this section lists the substantive
departures from the upstream `haskell/` pair.

Lexer:

1. `NEWLINE` includes the next line's leading spaces/tabs
   (`('\r'? '\n' | '\r') [ \t]*`); upstream's `NEWLINE` matches only the break
   and hides indentation from the layout preprocessor. This is required for
   racklr's `INDENT`/`DEDENT` computation.
2. Unicode identifier ranges collapsed to ASCII
   (`VARID : [a-z_] [a-zA-Z0-9_']*`, `CONID : [A-Z] [a-zA-Z0-9_']*`,
   `WILDCARD : '_'`). Upstream keeps thousands of explicit `\uXXXX` ranges,
   which generate and tokenize under racklr (verified), but non-ASCII
   identifiers are out of pass-1 scope.
3. Upstream keywords for pragmas, FFI, Template Haskell, deriving strategies,
   and `forall` are dropped; only the 23 keywords in the pass-1 subset remain.
4. Upstream `LCASE : '\\' (NEWLINE | WS)* 'case'` and the Template-Haskell
   quote tokens are dropped (not in scope).
5. Operator tokens are split into named rules for the precedence ladder
   (`OROR`, `AMPAMP`, `EQEQ`, `NEQ`, `LTE`, `GTE`, `LT`, `GT`, `PLUS`, `MINUS`,
   `STAR`, `SLASH`, `PERCENT`, `CARET`, `CONCAT`, `DOLLAR`) plus a generic
   `SYMBOL` fallback declared last; upstream has one rule per symbol too, but
   without a fallback for arbitrary symbols.
6. Comments are a simple `'--' ~[\r\n]* -> skip` and `'{-' .*? '-}' -> skip`.
   Upstream's block-comment rule avoids pragmas (`~[#]`); this one also swallows
   `{-# … #-}` pragmas, which is acceptable because pragmas are out of scope
   (documented below).
7. Numeric literals are reduced to `DECIMAL`, `HEX`, `OCTAL`, and a simple
   `FLOAT`; no `\uXXXX` numeric script ranges.

Parser:

1. The module/declaration grammar is re-expressed for NEWLINE/INDENT/DEDENT
   layout instead of virtual `OCURLY`/`SEMI`/`CCURLY`. Upstream's
   `module : OCURLY? semi* … CCURLY? semi? EOF` becomes
   `file : NEWLINE* module_header? NEWLINE* topdecls EOF`.
2. The expression grammar is a fixed precedence ladder over named operator
   tokens instead of upstream's `fexp → aexp → …` plus fixity resolution. This
   is a deliberate pass-1 approximation; fixity is an emitter concern in the
   plan.
3. `let`/`where`/`do`/`case`/`data` bodies are restricted to canonical
   indentation shapes (see Known limits).
4. Types are a small subset (`atype`, `btype`, right-associative `type0`,
   one-level `context`) with `hs_type` as the rule name because `type` is a
   reserved lexer-command word in racklr's `.g4` parser.
5. Patterns are constructor patterns (`CONID apat*`) over `VARID`, `CONID`,
   literal, wildcard, and parenthesised patterns.

No upstream grammar text was copied verbatim; the rule set follows the Haskell
2010 report and the upstream grammar's structure. The upstream BSD-3-Clause
license is reproduced below as attribution.

## Known limits (Phase-2 candidates, not silently accepted)

Layout:

- Only canonical block shapes are accepted: the layout keyword ends its line and
  the body is on the next, deeper-indented line (or the first item follows the
  keyword on the same line). `where`/`let` bodies aligned at the keyword's own
  column, and multi-line `if` with `then`/`else` indented deeper than `if`, are
  not accepted.
- A declaration body may be on the line after `=`, but an expression that
  continues onto a more-indented line without `let`/`where`/`do`/`case`/`of`
  (plain operator continuation) is not accepted.
- Blank/comment-only lines collapse to one `NEWLINE` (works); a block comment
  containing newlines consumes them, so indentation after a multi-line block
  comment is measured from the following real newline (usually correct, but not
  a full Haskell layout implementation).
- Explicit brace/semicolon layout (`let { … }`, `where { … }`) is not accepted;
  only NEWLINE/INDENT/DEDENT layout is.

Syntax:

- No pragmas (`{-# LANGUAGE … #-}`, `INLINE`, `SPECIALISE`, `RULES`, …); they
  are swallowed by the block-comment rule.
- No `foreign import`/FFI, Template Haskell, quasiquotes, GADTs/existentials,
  type/data families, pattern synonyms, standalone deriving, `deriving via` /
  deriving strategies, `forall`, role annotations.
- Class bodies support type signatures only (no default method bodies);
  instance bodies support simple method bindings.
- Module export lists support simple items (`VARID`, `CONID`, `CONID (..)`,
  `module M`); class/type export sub-lists are limited.
- Patterns: no list, cons, tuple, as (`@`), lazy (`~`), record, view, or
  n+k patterns. `case` guards must stay on the alternative's line(s) in the
  supported shape.
- No record-construction expressions, list comprehensions, arithmetic sequences
  (`[1..10]`), sections beyond `(op)`, or `do` with explicit braces.
- Record constructors must open the brace on the constructor's line
  (`data P = P {` newline fields … is fine; `data P = P` newline `{` is not).
- Lexer: ASCII identifiers only; decimal/octal/hex integers and simple floats
  (`2.5e3`) only, no numeric underscores or `Rational`-style forms; a line
  comment is `--` to end of line, so the operator `-->` is treated as a comment;
  block comments do not nest (same as upstream).
- `fixity` declarations are parsed but not applied.

Generator limitations encountered while building this grammar (reported, not
worked around by editing the generator):

- The generated `parse` entry raises `match-define: no matching clause for #f`
  on a failed parse instead of returning `#f`; the suite's reject cases pass
  through `check-reject`, which treats the exception as rejection.
- Greedy repetition has no backtracking, so grammar rules must be ordered and
  shaped so a repetition never has to give tokens back; the layout-block
  patterns above are written for that constraint.
- Ordered choice is first-success after a longest-parse preference
  (`longest`), not ANTLR4 adaptive prediction; the fixed operator ladder and
  keyword-first top-level alternatives avoid relying on lookahead.
- Left-recursive alternatives are rewritten to accumulator loops; rules whose
  suffix contains exactly one self-reference recurse only into the rule's
  primary alternatives (so multi-level precedence needs one rule per level,
  which this grammar has).

## Refresh

This grammar is authored, so there is no `cp` refresh step. To extend or
re-verify it:

1. Re-check the upstream rule set used as reference:

   ```bash
   curl -sS -o /tmp/hsgram/HaskellLexer.g4 \
     https://raw.githubusercontent.com/antlr/grammars-v4/master/haskell/HaskellLexer.g4
   curl -sS -o /tmp/hsgram/HaskellParser.g4 \
     https://raw.githubusercontent.com/antlr/grammars-v4/master/haskell/HaskellParser.g4
   curl -sS -o /tmp/hsgram/HaskellBaseLexer.java \
     https://raw.githubusercontent.com/antlr/grammars-v4/master/haskell/Java/HaskellBaseLexer.java
   ```

2. Edit `grammars/haskell/Haskell.g4`, adding any new construct to the
   Phase-1 table above and to `racklr-test/gen-haskell-test.rkt`.

3. Run the suite from `racklr-test/` with `PLTCOLLECTS` pointing at this
   checkout's root, because this Racket installation also links a `racklr`
   collection from a different operator checkout and would otherwise load that
   checkout's generator instead of this one:

   ```bash
   cd racklr-test
   PLTCOLLECTS="$(cd .. && pwd):" \
     flock /tmp/racklr-gen-test.lock racket gen-haskell-test.rkt
   ```

   The suite must print `14 success(es) 0 failure(s) 0 error(s)` and exit 0.
   Verify the generator path if in doubt:

   ```bash
   PLTCOLLECTS="$(cd .. && pwd):" racket -e \
     '(require racklr/gend-parser) (displayln (collection-file-path "gend-parser.rkt" "racklr"))'
   ```

## License attribution

The upstream grammar this subset was derived from is published under the
BSD-3-Clause license:

```
BSD License
Copyright (c) 2020, Evgeniy Slobodkin
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors may be
   used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
