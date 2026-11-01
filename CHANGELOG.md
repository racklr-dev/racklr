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
- Effect algebra (`racklr/effects/algebra/`): provenance-carrying rows as sets
  of (effect, evidence) pairs, the `join` / `sequence` / `choose` / `scope`
  combinators, a generic monotone fixpoint driver, and row subtyping with
  handler interpretation (the bridge checker's capability table generalized to
  rows).
- `racklr-test/effects-algebra-property-test.rkt`: property tests for
  associativity and idempotence of join, monotonicity and least-fixpoint laws,
  the subtyping laws (reflexivity, transitivity, bottom, join as LUB), and
  handler interpretation.
- `racklr-test/effects-algebra-prohibitions-test.rkt`: the declared-row
  prohibition agreement. Algebra rows canonicalize `must-not` to absence, so
  `declared-prohibited-effects` reads a prohibition from the UIR row and the
  tests pin that `row-subtype?` still agrees with `effect-subtype?` for rows
  carrying `must-not` on either side of `≤`.

### Changed

- Effect inference's fixpoint is now expressed in the algebra:
  `infer.rkt` and `whole-program.rkt` run `algebra/least-fixpoint` over
  `algebra/join`, and `rows.rkt` is the algebra adapter. The refactor is
  behavior-preserving: the `type-graph.rkt django/django/core` baseline is
  unchanged (`sha256:14bac7860833329849c4d81740ecccd01870b72cb8355f947f70956e179623ba`).
- Python front-end test suites load the tracked `grammars/python3/` grammar
  instead of the vendored `grammars-v4/python/python3/` copy, so the parser the
  tests exercise is the one the repository owns.

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
