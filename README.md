# racklr: Multi-Language Transpiler Toolkit

Racklr is a parser generator and universal intermediate representation (UIR) toolkit built in Racket. It reads ANTLR4 grammar definitions (.g4 files), generates working lexer+parser modules, parses source text into Concrete Syntax Trees (CSTs), lowers CSTs to a language-independent UIR, and emits target code from UIR nodes.

**Experimental release. Six front ends parse, lower, and emit through the shared UIR: JSON, Python, JavaScript, TypeScript, TSX/React, and Clojure. Effect-row annotation, inference, and contract checking are implemented; runtime enforcement is planned.**

## Status

- **JSON:** parser, lowering, and emit; round-trips.
- **Python 3:** parser, lowering, and emit; round-trips.
- **JavaScript:** parser, lowering, and emit; round-trips.
- **TypeScript:** parser and lowering; emits through the JavaScript emitter (TS → JS, type annotations drop at emit).
- **TSX / React:** parser and lowering; emits through the JSX/HTML emitters (hooks lower to static HTML + vanilla JS).
- **Clojure:** parser, lowering, and emit; round-trips and carries effects metadata.

**Planned (not yet implemented):** Rust, HTML/CSS, Svelte, arbitrary cross-compilation between front ends, and the effect runtime-enforcement layer.

## Architecture

```
grammars-v4/*.g4          ← ANTLR4 grammar files (vendor)
        │
        ▼
  g4-lex.rkt              ← Tokenizes .g4 grammar files into token streams
  g4-parse.rkt            ← Parses token streams into grammar CSTs
        │
        ▼
  gend-parser.rkt         ← Generates Racket lexer+parser source from grammar CST
        │
        ▼
  Generated parser.rkt    ← Standalone Racket module (lexer + parser)
        │
        ├── tokenize: string → (listof token)
        └── parse:   string → cst-node
                │
                ▼
  tree.rkt                ← CST/UIR node types (node, leaf, any-tree?)
                │
                ▼
  lower-<lang>.rkt        ← CST → UIR lowering pass (per language)
  emit-<lang>.rkt         ← UIR → language text code generator (per language)
                │
                ▼
  uir.rkt                 ← UIR struct definitions (Tier 0 + Tier 1 types)
                 │
                 ▼
  effects/                ← effect-row annotation, inference, contract checks
```

## Component Reference

### g4-lex.rkt
Grammar lexer. Reads ANTLR4 grammar source text, produces tokens (identifiers, keywords, operators, strings, char-classes, comments). Handles ANTLR4-specific syntax: `->` commands, `?*+` suffixes, `..` ranges, `~` negation, `<` `>` element options, `#` alternative labels, `=` and `+=` element labels.

### g4-parse.rkt
Grammar parser. Consumes token stream from g4-lex, produces a grammar CST with nodes for: `parser-rule`, `lexer-rule`, `fragment-rule`, `mode`, `options`, `alternative`, and element types (`literal`, `token-ref`, `rule-ref`, `char-class`, `star`, `plus`, `optional`, `group`, `negated`, `labeled`, `append-labeled`, `action`).

### gend-parser.rkt
Parser generator. Takes a grammar CST and produces a complete Racket module source string. The generated module provides `token` struct (type, value, start, end), `tokenize` (lexer), and `parse` (parser).

Supports: lexer modes, tokenVocab imports (auto-loads lexer grammar from same directory), left-recursion detection (warns and rewrites to accumulator loops), INDENT/DEDENT injection for Python-style layout languages.

### tree.rkt
Core tree types: `source-pos` struct (line, col, offset), `cst-node` (tag + children + range), `cst-leaf` (tag + text + range), and `any-tree?` predicate.

### uir.rkt
Universal Intermediate Representation. Defines 40+ node types across Tier 0 (computational) and Tier 1 (OOP/module): `uir-null`, `uir-bool`, `uir-number`, `uir-string`, `uir-symbol`, `uir-list`, `uir-record`, `uir-var`, `uir-fn`, `uir-call`, `uir-if`, `uir-block`, `uir-return`, `uir-set!`, `uir-let`, `uir-while`, `uir-for-each`, `uir-try`, `uir-throw`, `uir-with`, `uir-class`, `uir-define-class`, `uir-get`, `uir-import`, `uir-export`, `uir-decorated`, `uir-fstring`, `uir-paren`, `uir-walrus`, `uir-type-alias`, `uir-ternary`, `uir-spread`, `uir-new`, `uir-unary-op`, and 14 match/case pattern types. Schema documented in `docs/uir-schema.md`.

### lower-python.rkt / emit-python.rkt (1,744 + 470 lines)
Python 3 lowering and emit. Handles: functions (def, lambda, async, decorated), classes, for/while loops, try/except/finally, with, if/elif/else, match/case, return/yield/await, assignments (including augmented), imports, list/set/dict comprehensions, generators, f-strings, ternary expressions, all operators.

Modern Python 3 syntax is supported by the tracked grammar in `grammars/python3/`
(the vendored `grammars-v4` Python3 grammar predates it): assignment expressions
(`:=`, PEP 572), positional-only parameters (`/`, PEP 570) — in both `def` and
`lambda` — and PEP 695 `type` alias statements, including generic aliases
(`type ListOrSet[T] = list[T] | set[T]`).

Known limitations: the positional-only marker is not represented in UIR, so it
drops at emit (call shape is preserved; the emitted parameters are
positional-or-keyword). Type-parameter bounds/defaults (`[T: int]`, `[T = int]`)
parse and lower but only the parameter names survive to emit. Match/case class
patterns and dotted-name value patterns hit parser edge cases.

### lower-javascript.rkt / emit-javascript.rkt (1,058 + 399 lines)
JavaScript (ES2020) lowering and emit. Handles: function/arrow/async/generator functions, classes with methods/getters/setters, object/array literals, spread/rest, for/for-in/for-of/while/do-while, try/catch/throw, switch/case, break/continue, import/export, ternary, unary operators, `new`/`this`.

Known limitations: template literals and tagged templates not yet implemented; `debugger`, `with`, and labeled statements not yet lowered.

### lower-json.rkt / emit-json.rkt
JSON pipeline proof-of-concept: parse JSON → lower to UIR → emit JSON text → parse again.

### Effects (racklr/effects/)

Effect rows give generated code a readable boundary: a function declares which capabilities it may use, how often, and in what scope. Four pieces are implemented and tested:

- `infer.rkt`: effect inference; composes rows up the call graph from annotations and known effects, and defers higher-order calls whose callee is not statically known.
- `contract.rkt`: contract checking; subtyping between rows, budgets as upper bounds, and prohibitions.
- `provenance.rkt` + `graph/provenance.rkt`: every inference run records the
  evidence for each effect edge (classifying call site, source tokens, receiver
  type, call chain) as structured data, projected into the typed hypergraph
  substrate; `provenance-paths` answers "why is this function `db`" with the
  exact caller→callee chain, and the projection refuses an effect edge with no
  ground.
- `python-annot.rkt`: Python annotation surface (`__effects__` attribute / decorator) extracted into a UIR effect row.
- `clojure-annot.rkt`: Clojure annotation surface (`^{:effects ...}` metadata) extracted into a UIR effect row.

Both emitters attach the row to the emitted function so it survives round-trips: Python via a `__effects__` attribute, Clojure via `:__effects__` metadata on the `defn`. The Clojure effects test executes the emitted code through the [Jolt](https://github.com/jolt-lang/jolt) runtime and reads the row back off the var.

**Planned:** runtime enforcement. Static checks run wherever the call graph is resolvable; a runtime guard covers the rest. Jolt is the Clojure runtime for this, and effect-ts is the chosen direction for JavaScript/TypeScript emission.

## Generated Parser Design

### Lexer (tokenize)
Recursive descent with character-level matching. Supports lexer modes with a mode stack. Whitespace and newlines update position tracking (line/col/offset). For Python, a post-processing pass injects INDENT/DEDENT tokens based on NEWLINE + leading whitespace.

### Parser (parse)
Recursive descent with backtracking. Each grammar rule becomes a `parse-XXX` function returning `(list new-pos cst-node)` or `#f`.

### Known Limitations
- `~(...)` negated token sets in parser rules are skipped (treated as always matching)
- Char-class `\uXXXX` codepoint escapes not handled in generated `cc-match`
- Element labels (`id = element`, `id += element`) are parsed but not used in generated code
- Unicode property classes (`\p{XX}`) in char-classes may have limited support
- Left-recursive alternatives are rewritten to accumulator loops with a warning

## Adding a New Language

1. Find or create ANTLR4 `.g4` grammar files. Pristine vendored grammars live in the untracked `grammars-v4/<lang>/` checkout; authored or modified grammars are tracked in `grammars/<lang>/` (see `grammars/koka/` and `grammars/python3/`)
2. If using separate lexer/parser grammars, ensure `tokenVocab = <LexerName>` is set in the parser grammar
3. Generate and test: `(parse-g4-file "grammars-v4/<lang>/<Lang>Parser.g4")` → `(generate-parser-module cst #:source-path path)`
4. Write lowering pass: CST → UIR
5. Write code generator: UIR → target language text

## Testing

Run each suite directly from inside `racklr-test/`. `raco test racklr-test/*.rkt` is unreliable in this toolchain, and tests that generate-and-load grammars resolve their grammar paths relative to the working directory.

```bash
cd racklr-test
racket effects-infer-test.rkt        # effect inference
racket provenance-test.rkt           # effect-edge provenance + graph query
racket effects-contract-test.rkt     # contract checking
racket effects-python-annot-test.rkt # Python effect annotations
racket effects-clojure-test.rkt      # Clojure effects + Jolt runtime round-trip
racket lower-clojure-test.rkt        # Clojure lowering + emit
racket lower-typescript-test.rkt     # TypeScript lowering
racket tsx-integration-test.rkt      # TSX/React end-to-end
racket lower-python-test.rkt         # Python lowering + emit
racket modern-python-test.rkt        # modern Python: walrus, positional-only, type alias
racket lower-javascript-test.rkt     # JavaScript lowering + emit
racket gend-parser-test.rkt          # parser generator
racket g4-parse-test.rkt             # grammar parser
racket uir-test.rkt                  # UIR types + serialization
```

Other suites cover parser generation per language (`gen-*-test.rkt`), JSON/HTML emitters, the esbuild integration, and core tree/range operations.

Provenance completeness can also be checked over a real corpus (same submodule
and grammar prerequisites as the shared baseline):

```bash
racket tools/provenance-corpus.rkt                 # django/django/core
racket tools/provenance-corpus.rkt django/django   # wider probe
```

It prints the effect-edge and provenance counts and exits non-zero if any
effect edge has no reachable ground; it never re-pins the baseline.

## Dependencies

- Racket 9.3
- No external Racket packages required
- [Jolt](https://github.com/jolt-lang/jolt) (Clojure runtime) for the Clojure effects round-trip test; the suite skips its Jolt checks if the binary is not on `PATH`
- ANTLR4 grammar files from [grammars-v4](https://github.com/antlr/grammars-v4) (vendored in `grammars-v4/`)

## Setup

```bash
# Clone the repo
git clone https://github.com/racklr-dev/racklr.git
cd racklr

# Install the package (linked mode)
raco pkg install --link .

# Fetch grammar files
git clone https://github.com/antlr/grammars-v4.git

# Run all tests
raco test racklr-test/*.rkt
```

## Roadmap

**Shipped:**
- Effect-row annotation (Python, Clojure), inference, and contract checking
- Six front ends through the shared UIR, including Clojure and TypeScript/TSX

**Next, in priority order:**
- Clojure effects runtime enforcement via Jolt, then the same for the JS/TS side via effect-ts
- Arbitrary cross-compilation between any two front ends (each pair becomes two small passes over the shared UIR)
- Rust: lowering + emit (parser generation is already tested)
- HTML/CSS and Svelte: Tier 2 UIR lowering + emit
- Universal transpilation pipeline with a language registry
