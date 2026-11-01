# racklr Python front end: blocker catalogue and re-run history (Django 907-file scan)

Goal: effervesce the entire `django/django` tree (907 files) through
Python3 -> UIR -> effect inference -> Effervesce bridge/verify. This document
started as the catalogue of every construct racklr could not handle, measured by
a full-tree scan plus a construct-level probe battery. Those blockers are now
resolved, and the document is retained as the record of what was fixed and how
the corpus is re-measured.

**Status (2026-11-01): 0 blockers. Re-run: files=907, ok=907; every other bucket
(PARSE-FAIL, INDENT-ERROR, LOWER-ERROR, FALLBACK) is empty.** Each blocker below
is marked with its resolution; the reproducer snippets stay as regression probes.

This scan measures the front-end buckets only: parse, lower, and `?`-fallback
symbols. It does not measure effect coverage; unknown-effect elimination is
tracked separately (`LOOP_PLAN.md`, P0.3).

Measured with:

- `racket tools/scan-python.rkt django/django` (whole-tree; tracked
  `grammars/python3/` grammar, the parser the repository owns). Add `--verbose`
  for per-file failures and effect rows.
- `effervesce/tools/probe-python.rkt` (construct-level, 3-stage classification).

The historical scan below was taken with `effervesce/tools/scan-python.rkt
../django/django --verbose`; that harness reads the untracked vendored
`grammars-v4/` tree, so it needs an environment where that tree is present.

## Whole-tree totals

Original scan (before the fixes):

| bucket | files |
| ------ | ----- |
| ok (parses + lowers) | 207 |
| PARSE-FAIL | 572 |
| INDENT-ERROR | 124 |
| LOWER-ERROR | 3 |
| FALLBACK (lowers, but emits `?`-unhandled nodes) | 1 |

Files that could not be effervesced: **699 of 907 (77%)**.

Latest re-run (2026-11-01, `racket tools/scan-python.rkt django/django`):

| bucket | files |
| ------ | ----- |
| ok (parses + lowers) | 907 |
| PARSE-FAIL | 0 |
| INDENT-ERROR | 0 |
| LOWER-ERROR | 0 |
| FALLBACK (lowers, but emits `?`-unhandled nodes) | 0 |

Files that cannot be effervesced today: **0 of 907 (0%)**.

At the time of the original scan the bottleneck was the Python front end, not the
hypergraph/Effervesce layer: the Effervesce substrate was proven working end to
end on a toy Django example (clean verify, and correct `db`-effect detection on a
leaky variant).

---

## 1. NEWLINE tokens inside brackets are not suppressed (dominant PARSE-FAIL)

**Resolved.** `insert-indents` in `racklr/gend-parser/lexer.rkt` tracks bracket
depth (`(`, `[`, `{`) and drops NEWLINE inside an open bracket (implicit line
joining).

**Reproducers (all PARSE-FAIL):**

```python
from a.b import (       # paren-import-multi
    CASCADE,
    SET,
)
x = obj.method(         # call-multiline
    a,
    b,
)
d = { "a": 1, "b": 2 }  # dict-multiline  (when split across lines)
x = [1, 2]              # list-multiline
x = (1, 2)              # tuple-multiline
x = {1, 2}              # set-multiline
if (a and               # cond-multiline
        b):
    pass
x = d["k"]              # subscript-multiline
x = g(a, )              # call-trailing-comma
```

Single-line forms (`import-paren-single`, `dict` on one line, `backslash-cont`) are OK.

**Root cause.** CPython does implicit line joining: NEWLINE is never emitted while the
lexer is inside `( ) [ ] { }`. racklr's generated lexer does not track bracket depth, so
it emits NEWLINE (and, via `insert-indents`, INDENT/DEDENT) inside brackets. The grammar
rules already allow parenthesized forms (e.g. `import_from : ... '(' import_as_names ')'`
in `grammars-v4/python/python3/Python3Parser.g4:224-244`), so this is purely a lexical
gap, not a missing rule.

**Fix location.** `racklr/gend-parser/lexer.rkt` (`tokenize-raw` / `insert-indents`):
track `(` `[` `{` depth and suppress NEWLINE/INDENT/DEDENT while depth > 0.

**Impact.** This is the single largest cause of PARSE-FAIL: it breaks every multi-line
function call, dict/list/tuple/set literal, subscript, conditional, and parenthesized
import in the Django codebase.

---

## 2. Blank lines inside nested blocks break `insert-indents` (INDENT-ERROR, 124 files)

**Resolved.** The same pass collapses runs of NEWLINEs to the last one before
computing indentation, so blank lines no longer pop the indent stack.

**Reproducer (RAISE):**

```python
def f():
    if x:
        a()

        b()      # blank line above raises
    else:
        c()
```

Error: `insert-indents: unindent does not match any outer indentation level`.

**Root cause.** A blank line is tokenized at column 0, which `insert-indents` reads as an
outdent. The captured token stream shows the blank line producing `DEDENT [x2]` followed
by a re-`INDENT` — the indent stack pops two levels, and the second pop has no matching
outer level. Blank lines must be ignored entirely (no NEWLINE contribution to the indent
stack).

**Fix location.** `racklr/gend-parser/lexer.rkt` `insert-indents` (~line 134; raise at
~186): skip empty/whitespace-only lines before computing indent.

**Impact.** 124 files. Blank lines inside any nested block are ubiquitous in real Python.

---

## 3. Comment-only and whitespace-only lines inside nested blocks (PARSE-FAIL)

**Resolved.** Comment-only lines are hidden tokens and the collapsed NEWLINE run
leaves only the next real line's indentation for `insert-indents`.

**Reproducers (PARSE-FAIL):**

```python
def f():
    if x:
        a()
        # note      # comment-line-in-nested
        b()
    else:
        c()
```

```python
def f():
    if x:
        a()
                   # ws-only-line-in-nested (a line of spaces)
        b()
    else:
        c()
```

**Root cause.** Same family as #2. A comment-only line still emits NEWLINE with the
line's leading whitespace, and the trailing content of the comment disrupts the indent
machinery, so it surfaces as a parse failure rather than a raise.

**Fix location.** Same as #2: the indent pass must skip comment-only and whitespace-only
lines.

---

## 4. Non-decimal numeric literals (PARSE-FAIL)

**Resolved.** The tracked grammar's `NUMBER`/`INTEGER`/`FLOAT_NUMBER`/`IMAG_NUMBER`
rules accept the specialised tokens, and `racklr/lower-python/expr.rkt` lowers
hex/octal/binary/float/complex literals to `uir-number`.

**Reproducers (all PARSE-FAIL):**

```python
x = 0xFF        # hex
x = 0o17        # octal
x = 0b1010      # binary
x = 1.5         # float
x = 1.5e10      # float with exponent
x = 1j          # complex
```

Plain decimal (`12345`, `0`, `-5`) and `.5` are OK.

**Root cause.** The generated lexer emits distinct token types — `HEX_INTEGER`,
`OCT_INTEGER`, `BIN_INTEGER`, `FLOAT_NUMBER`, `IMAG_NUMBER` — but the parser grammar's
number rules (`Python3Parser.g4` ~lines 401-415) accept only `NUMBER` (plain decimal)
and `STRING`. The specialized integer/float/complex tokens have no parser rule.

**Fix.** Either canonicalize all numeric token types to `NUMBER` in the lexer, or extend
the parser's `atom`/number rule to accept `HEX_INTEGER | OCT_INTEGER | BIN_INTEGER |
FLOAT_NUMBER | IMAG_NUMBER`.

---

## 5. Underscore numeric literals, PEP 515 (PARSE-FAIL)

**Resolved.** The integer fragments in `grammars/python3/Python3Lexer.g4` allow `_`
between digits, for decimal/hex/octal/binary and the fraction.

**Reproducer (PARSE-FAIL):**

```python
x = 1_000        # num-underscore
x = 0xFF_FF      # underscore in hex
x = 1_000.5      # underscore before fraction
```

**Root cause.** The lexer splits `1_000` into `NUMBER="1"` + `NAME="_000"` — the integer
fragment does not allow `_` inside the literal, so `_000` lexes as an identifier.
`1_000.5` becomes `NUMBER="1" NAME="_000" NUMBER=".5"`.

**Fix.** Allow `_` in the decimal/hex/octal/binary integer fragments of
`Python3Lexer.g4`.

---

## 6. Multi-line triple-quoted strings (PARSE-FAIL)

**Resolved.** `LONG_STRING` in `grammars/python3/Python3Lexer.g4` spans newlines, so
multi-line docstrings tokenize as a single STRING. The generated lexer also
advances line/column across the literal's embedded newlines, so the tokens
after a multi-line docstring carry their real source position instead of the
line the literal opened on (`racklr-test/python-multiline-strings-test.rkt`).

**Reproducers (PARSE-FAIL):**

```python
x = """a
b"""
def f():
    """a
    b"""            # multi-line docstring
    return 1
```

Single-line `"""abc"""` is OK.

**Root cause.** The lexer tokenizes a multi-line triple-quoted string as
`UNKNOWN_CHAR` + `NAME` + `NEWLINE` + `STRING` — the triple-quoted string rule does not
span newlines.

**Fix.** `Python3Lexer.g4` `STRING_LITERAL` / triple-quote fragments must accept
multi-line content.

---

## 7. Keyword-only marker `*` in parameter lists (LOWER-ERROR, 3 files)

**Resolved.** `lower-parameters` in `racklr/lower-python/stmt.rkt` walks the
`typedargslist` for `tfpdef` nodes and attaches defaults, so the bare `*`/`/`
separators are skipped instead of being misread as parameter names.

**Reproducer (RAISE):**

```python
def f(a, *, b=None):
    return b
```

Error: `uir-typed-param-name: contract violation; expected: uir-typed-param?; given:
(uir-symbol "b")`.

**Root cause.** `lower-parameters` in `racklr/lower-python/stmt.rkt` (~line 650) does not
handle the bare `*` (STAR) separator in `typedargslist`. The following parameter `b` is
then misread as a typed param name.

**Fix.** In `lower-parameters`, treat the standalone `STAR` token as the keyword-only
separator and continue lowering subsequent params as keyword-only.

**Files in Django hitting this:** `contrib/messages/test.py`,
`core/checks/database.py`, `utils/translation/trans_null.py`.

---

## 8. Positional-only marker `/` (PARSE-FAIL)

**Resolved.** `typedargslist`/`varargslist` accept `/` in the tracked grammar and
lowering skips the marker (PR #71).

**Reproducer (PARSE-FAIL):**

```python
def f(a, /, b):
    return b
```

**Root cause.** The `typedargslist` rule in the grammar does not accept the `/` separator
(Python 3.8 positional-only parameters).

**Fix.** Add `,? '/' (',' tfpdef)*` to `typedargslist`, and handle the marker in
`lower-parameters`.

---

## 9. Walrus operator `:=` (PARSE-FAIL)

**Resolved.** `namedexpr_test` is in the tracked grammar and lowers to `uir-walrus`
(PR #71).

**Reproducer (PARSE-FAIL):**

```python
def f():
    if (n := len(x)) > 0:
        return n
```

**Root cause.** The grammar is pre-3.8 and lacks the assignment expression rule
(`namedexpr_test`).

**Fix.** Add `namedexpr_test : test [':=' test]` and wire it into `if`/`while`
conditions.

---

## 10. `type` alias statement, PEP 695 (PARSE-FAIL)

**Resolved.** The `type_alias`/`type_params` rules are in the tracked grammar and
lower to `uir-type-alias` (PR #71).

**Reproducer (PARSE-FAIL):**

```python
type T = int | str
```

**Root cause.** The grammar is pre-3.12 and has no `type` statement rule.

**Fix.** Add the `type_alias` rule (Python 3.12) and lower it.

---

## 11. Class bases with keyword arguments (PARSE-FAIL)

**Resolved.** `classdef` uses the shared `arglist`/`argument` rules, so
`class A(B, metaclass=M)` parses like a call.

**Reproducers (PARSE-FAIL):**

```python
class A(B, metaclass=M):
    pass
class A(metaclass=M):
    pass
```

Positional bases (`class A(B, C)`, `class A(*bases)`, `class A(**kw)`) are OK.

**Root cause.** The `classdef` arglist path does not accept the `name '=' test` keyword
form the way a function call does. (Reproducer verified at parse; the precise grammar
spot needs a look at the `arglist`/`argument` handling in the `classdef` context.)

---

## 12. Lowering gaps that emit `?`-unhandled nodes (FALLBACK)

**Resolved.** Chained comparisons and binary operators (`**`) fold over the
operator/operand pairs, `...` lowers to a symbol and PEP 604 unions lower as
expressions; `tasks/exceptions.py` is `ok` in the latest scan.

These parse and lower without raising, but produce `?`-prefixed unhandled symbols. They
do not block the file, but lose information for effect inference.

- `a < b < c` (chained comparison) -> `?comp_op ?op`
- `a ** b` (power) -> `?expr`
- `...` (ellipsis) -> `?atom-ELLIPSIS`
- `int | None` (PEP 604 union annotation) -> `?expr`

The single FALLBACK file in Django is `tasks/exceptions.py`.

---

## How the blockers were retired

Ranked by the blast radius the original scan measured; all items are resolved.

1. NEWLINE-inside-brackets (#1) — unlocked most of the 572 PARSE-FAIL files.
   Fixed in `racklr/gend-parser/lexer.rkt`.
2. Blank/comment/whitespace-only lines in nested blocks (#2, #3) — unlocked the 124
   INDENT-ERROR files plus a share of PARSE-FAIL. Fixed in the same pass.
3. Non-decimal + underscore numeric literals (#4, #5) — fixed in the tracked lexer
   grammar and `racklr/lower-python/expr.rkt`.
4. Multi-line triple-quoted strings (#6) — fixed in the tracked lexer grammar.
5. Keyword-only `*` (#7) — fixed in `lower-parameters` (`lower-python/stmt.rkt`).
6. Modern-grammar features (#8-#10: `/`, `:=`, `type`), class keyword bases (#11)
   and the FALLBACK lowering gaps (#12) — fixed in the tracked
   `grammars/python3/` grammar (PR #71) and the Python lowerer.

The fixes concentrate in three places: `racklr/gend-parser/lexer.rkt`
(indent/bracket/line handling), the tracked `grammars/python3/` lexer+parser
grammar, and `racklr/lower-python/stmt.rkt` / `expr.rkt` for the parameter and
expression lowering gaps. The tracked grammar replaced the vendored
`grammars-v4/python/python3/` copy as the grammar the repository owns and
measures (`racket tools/scan-python.rkt`).

## Re-running the measurement

From the repository root, with the `django/` submodule initialised:

```bash
racket tools/scan-python.rkt django/django            # aggregate
racket tools/scan-python.rkt django/django --verbose  # per-file failures + effects
```

The tool exits 0 only when every file lowers cleanly and exits 1 when any bucket
is non-zero, so the same command is the regression gate for the acceptance bar.
`racklr-test/scan-python-test.rkt` pins the bucket classification on a small
fixture tree.
