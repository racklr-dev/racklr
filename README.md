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
  emit-contract.rkt       ← the contract every target implements: UIR + effect row → source
  emit.rkt                ← single dispatch over every emit-contract target
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

Effect rows give generated code a readable boundary: a function declares which capabilities it may use, how often, and in what scope. The pieces implemented and tested:

- `infer.rkt`: effect inference; composes rows up the call graph from annotations and known effects, and defers higher-order calls whose callee is not statically known.
- `contract.rkt`: contract checking; subtyping between rows, budgets as upper bounds, and prohibitions.
- `origin.rkt` + `provenance.rkt` + `graph/provenance.rkt`: every inference run
  records the evidence for each effect edge (classifying call site, source
  tokens, receiver type, call chain) as structured data, projected into the
  typed hypergraph substrate; `provenance-paths` answers "why is this function
  `db`" with the exact caller→callee chain, and the projection refuses an
  effect edge with no ground. Each site is keyed by a stable origin
  (`file::owner@position:receiver-chain`, from source structure only, never an
  inferred value), so the readable call expression is a display atom and
  `provenance-origins` is a run-to-run drift fingerprint.
- `labels.rkt` + `graph/profile.rkt`: each effect edge carries a confidence
  (`inferred` / `declared` / `unknown`) and a coverage (`universal` /
  `census-param` / `local-literal`) label, combined by the weakest link;
  `transitive-profile` exposes every function's effect closure with the exact
  witness chains and the unresolved leaves as data (`effect-profile-obligations`)
  rather than a boolean flag.
- `python-annot.rkt`: Python annotation surface (`__effects__` attribute / decorator) extracted into a UIR effect row.
- `clojure-annot.rkt`: Clojure annotation surface (`^{:effects ...}` metadata) extracted into a UIR effect row.

Both emitters attach the row to the emitted function so it survives round-trips: Python via a `__effects__` attribute, Clojure via `:__effects__` metadata on the `defn`. The Clojure effects test executes the emitted code through the [Jolt](https://github.com/jolt-lang/jolt) runtime and reads the row back off the var.

### Effect-row spelling (emit-effects.rkt)

`emit-contract.rkt` decides whether a target can carry a row; `emit-effects.rkt`
owns how that row is spelled. It is the one place that renders a
`uir-effect-row` into each target's native effect vocabulary; the emitters own
where the row is placed, so a row cannot be dropped silently.

| Target | Native effect expression | Placement |
|---|---|---|
| Python | object literal plus a `# @effects {...}` documented comment | `@__withEffects(row)` decorator above the `def` |
| JavaScript | object literal plus a `/** @effects {...} */` documented annotation | `__withEffects(row, fn)` guard around the declaration |
| TypeScript | emitted through the JS emitter (type annotations drop at emit), so the JS object/guard form is its emitted native form | as JavaScript |
| Clojure | keyword-keyed map, e.g. `{:llm {:modality :may :max 5}}` | `defn` attr-map metadata `{:__effects__ ...}`, which the reader keeps on the var |
| Koka | typed effect row, e.g. `: <llm, db> ret` (or `total` when none) | function return type |

Fields without native syntax are documented, not discarded. A Koka typed row
spells names only, so budgets, scopes, ops and prohibitions are recorded in a
`// @effects {...}` comment next to the typed row, and a `must-not` entry is
never granted as a typed effect. (Koka has no way to distinguish `may` from
`must` in the typed row, so both render as `<db>` and the modality distinction
is only in the comment.)

The Python/JS runtime guards enforce a scope by classifying the performed op as
a read or a write. That vocabulary is rendered from the classifier's DATA verb
tables (`racklr/effects/guard-vocabulary.rkt`, backed by
`racklr/effects/imports/library-config.rkt`), not hand-written in the emitters,
so a new lexicon verb is enforced by construction. A scope value that is not
`reads=...` or `writes=...` has no enforcement branch and is rejected at guard
construction instead of silently skipping enforcement. Koka's grammar only accepts `<effects>` in
front of a result type, so a function with a known row but no declared result
type carries it as the documented comment alone. The JSON target has no
functions or effect vocabulary, so function rows do not appear there.
`unknown` entries are dropped: an unclassified effect is a tracked inference
obligation, not something a target guard can enforce. `uir-method` has no
effect-row slot yet, so method rows are not emitted by any target.

Per-target mapping tests live in `racklr-test/effects-{python,ts,jsdoc,clojure,koka}-annot-test.rkt`.

**Planned:** runtime enforcement. Static checks run wherever the call graph is resolvable; a runtime guard covers the rest. Jolt is the Clojure runtime for this, and effect-ts is the chosen direction for JavaScript/TypeScript emission.

### emit-contract.rkt / emit.rkt

One contract for every target: `render : UIR × (or/c #f uir-effect-row?) → string`. `emit-contract.rkt` defines that call shape, the row-reification strategies a target may declare, and the shared rule that an explicit row is attached to the unit's entry function (`attach-root-row`) or refused. `emit.rkt` is the single dispatch and the target table: `(emit target uir row)` renders any supported target from the same UIR-plus-row input — `python`, `javascript`, `jsx`, `koka`, `clojure`, `dom-js`, `html`, `json`, `effervesce` — and forwards target-specific keyword options (`#:indent` for JSON, `#:title`/`#:lang`/`#:external-js` for HTML), so it is a drop-in replacement for the direct emitter calls.

A supplied row is rendered in the target's native vocabulary where one exists (Python `@__withEffects({...})`, JavaScript/HTML `__withEffects({...}, fn)`, Koka `fun f() : <llm, db>` / `: total`, Clojure `:__effects__` metadata) and refused with an error where it does not (JSX/DOM/JSON/Effervesce), so it is never silently dropped. Omitting the row keeps the pre-contract one-argument call shape.

### Self-emit: the compiler emits itself into a target (P6.2)

`racklr/self-emit.rkt` is the reflexive step on top of the emit contract: a compiler is a UIR program (a "self-image"), and self-emit takes that program, renders it through `(emit target uir row)`, writes the generated source to disk, runs it when the target has a runtime here, and reports whether the effect rows survived in the target's native vocabulary. It is image-agnostic by design — the self-image is a registry entry (`register-self-image!`) or a serialized UIR file (`load-self-image-file`, s-expression or JSON), so the tower's own lowered UIR plugs into the same path without a driver change.

`racklr/self/seed/` is the runnable self-image that proves the path today: a compiler written entirely as a UIR program — `tokenize`, `parse`, `token_atom`, `emit_expr`, `emit_program`, `compile_source`, `main` — with an explicit effect row on every function: seven named functions, six of them pure stages and `main` carrying the `io` row. Emitted into Python it is a standalone compiler: it reads a small s-expression program, prints a runnable Python program, and that compiled program runs. The rows travel with it: `@__withEffects({})` on the pure stages, `@__withEffects({"io": {"modality": "may"}})` on `main`, and the io calls become `__perform(...)` runtime-guarded operations.

```bash
# Emit the seed compiler into Python, then run the emitted compiler on a sample.
racket tools/emit-self.rkt --image kernel --target python --out /tmp/self-emit \
        --run --stdin '(+ 1 (* 2 3))'
# emitted python -> /tmp/self-emit/seed-compiler.py
# rows-native?: #t
# run: ok (exit 0)
# output:
# print((1 + (2 * 3)))
```

Python is the runnable target wired up today (`python3`); JavaScript/JSX are runnable under `node` for programs whose semantics are portable, and the other targets emit through the same contract but their toolchains are not wired up. The honest loss model lives in `docs/self-emit.md`. This change ships the P6.2 emit half of Phase 6 — verified end-to-end on a compiler-as-UIR program — plus the serialized-UIR seam that the P6.1 self-image (the tower's own modules lowered to UIR, a separate ticket) plugs into without a driver change; the P6.1 lowering itself is not part of this change.

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
3. If the language has layout or template-literal lexing conventions, add a `language-policy` value (see `racklr/gend-parser/language-policy.rkt`) and pass it as `#:policy`
4. Generate and test: `(parse-g4-file "grammars-v4/<lang>/<Lang>Parser.g4")` → `(generate-parser-module cst #:source-path path #:policy policy)`
5. Write lowering pass: CST → UIR
6. Write a host lexicon under `racklr/effects/lexicons/`: token tables (`effect-tokens`), the host dialect (implicit receiver names, exception/constructor conventions, front-end construct encodings), import-tag rules, and the operation vocabulary. The default is the Python/Django lexicon; `infer-effects` / `infer-effects-all` / `classify-call` take it as `#:lexicon`. No engine edits.
7. Write code generator: UIR → target language text

## Testing

Run each suite directly from inside `racklr-test/`. `raco test racklr-test/*.rkt` is unreliable in this toolchain, and tests that generate-and-load grammars resolve their grammar paths relative to the working directory.

```bash
cd racklr-test
racket effects-infer-test.rkt        # effect inference
racket effects-algebra-property-test.rkt # effect algebra laws + fixpoint monotonicity
racket effects-algebra-prohibitions-test.rkt # prohibition / declared-row contract-order agreement
racket effects-subtyping-handlers-test.rkt # subtyping laws + row-valued handler interpretation + bridge equivalence
racket provenance-test.rkt           # effect-edge provenance + graph query
racket provenance-origin-test.rkt    # stable site origins: stability, drift diff
racket effect-profile-test.rkt       # confidence/coverage labels + transitive profiles
racket effects-contract-test.rkt     # contract checking
racket emit-contract-test.rkt        # emit contract + single dispatch
racket effects-python-annot-test.rkt # Python effect annotations
racket effects-clojure-test.rkt      # Clojure effects + Jolt runtime round-trip
racket lower-clojure-test.rkt        # Clojure lowering + emit
racket lower-typescript-test.rkt     # TypeScript lowering
racket tsx-integration-test.rkt      # TSX/React end-to-end
racket lower-python-test.rkt         # Python lowering + emit
racket modern-python-test.rkt        # modern Python: walrus, positional-only, type alias
racket python-multiline-strings-test.rkt # multi-line triple-quoted strings: lexing, positions, emit
racket lower-javascript-test.rkt     # JavaScript lowering + emit
racket gend-parser-test.rkt          # parser generator
racket g4-parse-test.rkt             # grammar parser
racket uir-test.rkt                  # UIR types + serialization
racket scan-python-test.rkt          # full-tree scanner bucket classification
racket self-seed-test.rkt            # seed compiler as a UIR program (emit + run)
racket self-emit-test.rkt            # self-emit driver, CLI, native rows, P6.1 seam
racket self-emit-integration-test.rkt # P6.2 end-to-end: emit compiler, run it, run its output
```

Other suites cover parser generation per language (`gen-*-test.rkt`), JSON/HTML emitters, the esbuild integration, and core tree/range operations.

Provenance completeness can also be checked over a real corpus (same submodule
and grammar prerequisites as the shared baseline):

```bash
racket tools/provenance-corpus.rkt                 # django/django/core
racket tools/provenance-corpus.rkt django/django   # wider probe
racket tools/provenance-corpus.rkt --profiles      # also transitive profiles
racket tools/provenance-corpus.rkt --origins       # dump the stable site origins
```

It prints the effect-edge and provenance counts (and, with `--profiles`, the
transitive-profile label report), the distinct-origin count and a sha256
fingerprint of the sorted site origins (`--origins` prints the ids themselves so
two runs diff directly; origins use repo-relative component names), and exits
non-zero if any effect edge has no reachable ground or the graph's site origins
disagree with the record. It never re-pins the baseline.

### Full-corpus measurement

`tools/scan-python.rkt` scans a directory tree through the Python3 front end
using the tracked `grammars/python3/` grammar and reports, per file, which
pipeline stage fails (`ok`, `INDENT-ERROR`, `PARSE-FAIL`, `LOWER-ERROR`,
`FALLBACK`). From the repository root, with the `django/` submodule
initialised:

```bash
racket tools/scan-python.rkt django/django            # aggregate
racket tools/scan-python.rkt django/django --verbose  # per-file failures + effects
```

It exits 0 only when every file lowers cleanly. The latest Django run is
907/907 `ok` with 0 blockers; see `docs/django-frontend-catalogue.md`.

### Real host-code bridge sample

`fastapi/` vendors the backend of the official FastAPI
full-stack template (release 0.10.0; 27 Python files, 1,611 lines) with an
Effervesce spec and runs the bridge over it. The tree parses and lowers with the
tracked `grammars/python3/` grammar and every inferred effect (`db`, `io`,
`time`) is covered by the spec's declared needs; calls the inference cannot
resolve (e.g. `jwt`, `pwdlib`, `jinja2`) are reported as `bridge-unverified`
instead of being counted as covered. See `fastapi/README.md`.

```bash
./fastapi/check.sh
```

The harness is `tools/bridge-python.rkt`, the tracked-grammar counterpart of
`effervesce/tools/verify-dir.rkt`; the suite is
`racklr-test/bridge-fastapi-sample-test.rkt`.

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
