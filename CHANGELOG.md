# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Tracked modern Python 3 grammar in `grammars/python3/` (`Python3Lexer.g4`,
  `Python3Parser.g4`). Adds assignment expressions (`:=`, PEP 572),
  positional-only parameters (`/`, PEP 570) in `def` and `lambda`, and PEP 695
  `type` alias statements, including generic aliases.
- UIR nodes `uir-walrus` and `uir-type-alias`, with Python lowering, emission
  and sexp/JSON serialization. `emit-python` renders `(name := value)` and
  `type Name[T] = value`.
- `racklr-test/modern-python-test.rkt` covering parse, lower and emit
  round-trips for the new syntax.
- One emit contract (`racklr/emit-contract.rkt`) and a single dispatch
  (`racklr/emit.rkt`): every target emitter takes UIR plus an optional explicit
  effect row, and `(emit target uir row)` renders any supported target from
  that same input, forwarding target-specific keyword options. An explicit row
  is reified in the target's native vocabulary (Python/JavaScript guards, Koka
  effect annotation, Clojure metadata) or refused, never silently dropped;
  omitting the row keeps the previous one-argument call shape.
- `racklr-test/emit-contract-test.rkt` covering the target table, the native
  row rendering for every row-capable target, refusal for the targets without a
  native row vocabulary, and the unchanged row-less call shape.

### Changed

- Python front-end test suites load the tracked `grammars/python3/` grammar
  instead of the vendored `grammars-v4/python/python3/` copy, so the parser the
  tests exercise is the one the repository owns.
- Emitted effect rows now have one owner of their spelling
  (`racklr/emit-effects.rkt`): Python/JS/TS object literals, Clojure `defn`
  metadata, and Koka typed effect rows, with a documented `@effects`
  annotation for fields a target cannot spell. Python and JS gain a
  `# @effects {...}` / `/** @effects {...} */` annotation next to their runtime
  guard; Koka and Clojure emit inferred rows instead of discarding them.
- The Python/JS runtime guards' scope read/write vocabulary is rendered from the
  classifier's DATA verb tables (`racklr/effects/guard-vocabulary.rkt`) instead
  of hand-written lists, and a scope encoding other than `reads=...` or
  `writes=...` now fails at guard construction rather than silently no-oping.

### Notes

- Koka's typed effect row is names-only: `may` and `must` both render as `<db>`
  and the modality distinction survives only in the documented `// @effects`
  comment, so a Koka reader should not read the typed row as stronger than the
  contract row. A row on a Koka function with no declared result type is emitted
  as the documented comment alone, because the grammar only accepts `<effects>`
  in front of a result type.

## [0.1.0rc1] - 2026-06-23

### Added

- JSX→React.createElement transpilation pipeline:
  - Cleaned JSX grammar (`grammars-v4/javascript/jsx-cleaned/`)
  - `lower-jsx.rkt` — CST→UIR lowering (7 lowering functions)
  - `emit-jsx.rkt` — UIR→React.createElement emitter
  - 4 new UIR struct types: `uir-element`, `uir-attribute`, `uir-jsx-expr`, `uir-text-node`
  - 42 JSX tests (`gen-jsx-test.rkt`) covering tokenize, parse, lower, and emit

## [0.1.0] - 2025-06-14

### Added

- Standard Racket collection layout: source in `racklr/`, tests in `racklr-test/`
- Package metadata (`info.rkt`) with multi-collection declaration
- GPL-3.0-or-later license (`LICENSE`)
- `.gitignore` entries for generated parser artifacts, Racket bytecode, and vendored grammars
- `CHANGELOG.md` (this file)

### Changed

- Branch renamed from `master` to `main`
- All inter-module `(require "file.rkt")` references converted to collection-style `(require racklr/file)` paths
- Generated parser template now emits `(require racklr/tree)` instead of `(require "tree.rkt")`
- Grammar file paths in tests now use `../grammars-v4/` prefix (tests moved one level deeper)

### Removed

- Flat-file project structure (all `.rkt` files previously in repository root)
