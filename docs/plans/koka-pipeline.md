# Plan: Koka Lowering & Raising Pipeline

## Overview

Add Koka as a source and target language: `lower-koka.rkt` (Koka CST to UIR) and `emit-koka.rkt` (UIR to Koka source), with tests. Enables Koka round-tripping and cross-compilation through the UIR.

Koka is the one language in this trio with genuine first-class typed effects, which makes it the natural "native" target for racklr's effect-row work. Its `effect` declarations, `with` handlers, and `resume` map directly onto the effect layer's `uir-effect-row` and `uir-effect-entry`, but the current UIR has no representation of effect operations or handlers. This plan adds those.

## Status (verified 2026-11-01)

**Phases 1-3 landed in earlier PRs and were re-verified for this ticket.** The
tracked pass-1 grammar is `grammars/koka/Koka.g4`
(`racklr-test/gen-koka-test.rkt`, observed 2/2, exit 0); lowering and emission
are `racklr/lower-koka.rkt` and `racklr/emit-koka.rkt`, round-tripped by
`racklr-test/lower-koka-test.rkt` (observed 10/10, exit 0). Effect
declarations, handlers, and `uir-fn` effect rows lower there. Deeper
effect-layer fixtures beyond the current round-trip suite remain open. The
`Current State` section below records the pre-work survey.

## Effect-System Status (read this first)

Koka is a strongly typed functional language whose central feature is effect typing. Every function type records its effects, effects are row-polymorphic, and effect handlers are a core language feature rather than a library.

- A function's type includes an effect row. `total` means pure; `<exn, div>` means it may throw or diverge.
- Effects are polymorphic: `map : (xs : list<a>, f : (a) -> e b) -> e list<b>` threads the effect of `f` through.
- `effect` declares a new effect with operations; `with ... handler` installs a handler; `resume` continues the computation.
- This is exactly the "absence is prohibition, presence is permission" model racklr's effect rows already express in `uir-effect-entry` with `may`/`must`/`must-not` modalities.

Implication: Koka is the strongest fit for the effects proposal, and it is the only one of the three where the "first-class typed effects" motivation is fully correct.

Caveats:

- Koka v3 is a research language. Its own site states it is "not ready for production use." Treat Koka support as a research-grade front end, not a production target.
- Koka is layout-sensitive (indentation-based), which the generated lexer must handle the same way the Python and TSX lexers already do.

## Current State

- **Grammar**: None exists. There is no Koka ANTLR4 grammar anywhere public: not in the vendored `grammars-v4/` checkout, not in the upstream `antlr/grammars-v4` repo, and not in `koka-lang/koka` (whose compiler is written in Haskell and parses with a hand-written Haskell parser). The only reusable material is a third-party tree-sitter grammar and Koka's formal grammar in its documentation. `KokaLexer.g4` and `KokaParser.g4` must be authored from scratch, which is the first and largest cost.
- **Lowering**: None. `lower-koka.rkt` does not exist.
- **Emission**: None. `emit-koka.rkt` does not exist.
- **UIR**: The effect-row structs exist (`uir-effect-row`, `uir-effect-entry`, `uir-subtype`, and the `effects` field on `uir-fn`). `uir-enum`/`uir-enum-variant` exist for ADTs. `uir-match`/`uir-case`/`uir-pat-*` exist for pattern matching. What is missing is a representation of effect operations and handlers.

## UIR Gaps

Koka needs two new structs, both proposed in the effects proposal but not yet added to `uir/types.rkt`:

- `uir-effect-op` — `(struct uir-effect-op (name params) #:transparent)`. An effect operation declared inside an `effect` block.
- `uir-effect-handler` — `(struct uir-effect-handler (effect ops body) #:transparent)`. A `with ... handler` expression: the effect being handled, the operation clauses, and the body.

Naming pitfall: do not use the name `uir-effect`. That struct already exists and means "reactive effect" (deps plus body) in the React/Tier 2 IR. The algebraic-effect declaration needs a distinct name, so use `uir-effect-decl` for `(struct uir-effect-decl (name ops) #:transparent)`.

Two further gaps are shared with OCaml and Haskell rather than Koka-specific:

- Constructor patterns. `uir-pat-class` (cls + positional + keyword) is the closest fit for patterns like `Cons(x, xs)`; extend it or add a dedicated `uir-pat-constructor` rather than reusing `uir-pat-value`, which carries no sub-patterns.
- Operator precedence on emission. Koka, like Haskell, treats operators as ordinary functions, so `uir-call` with a `uir-symbol` operator is the right IR, but the emitter needs a precedence table to parenthesize correctly.

## Scope: What We Handle (Pass 1)

- Functions (`fun`), typed parameters, return types
- `let` and `val` bindings
- Expressions: literals, variables, calls, operators, field access
- Control flow: `if`/`then`/`else`, `match` with patterns and guards
- Algebraic data types via `type` (constructors with positional fields)
- Lambdas (`fn(x) ...`)
- `effect` declarations and their operations
- `with ... handler` expressions, including `resume`
- Effect annotations on function types (lowered into `uir-fn`'s `effects` field as `uir-effect-row`)
- Modules and imports

## Scope: What We Skip (Pass 1)

- The Perceus reference-counting annotations and `div`/`ndet`/`st` semantics (they are types only; we do not interpret them)
- Named effect typing subtleties beyond simple rows
- Higher-kinded and heap (`H`) types
- `struct`/`interface`/`extern` interop with C
- Async/`await` sugar (Koka expresses it as handlers; represent the handler, not the sugar)
- Layout-sensitive edge cases beyond the standard block rules
- Compilation targets (C/JS/WASM output is out of scope; we emit Koka source only)

## Module Split Plan

Follow the established pattern: flat entry-point dispatcher plus a same-named subdirectory.

```
racklr/
  lower-koka.rkt              # dispatcher (~60 lines)
  lower-koka/
    helpers.rkt               # shared CST-walking utilities
    expr.rkt                  # expressions, operators, calls
    item.rkt                  # fun, type, effect declarations
    pattern.rkt               # match cases and constructor patterns
    handler.rkt               # with ... handler, resume
  emit-koka.rkt               # dispatcher (~40 lines)
  emit-koka/
    expr.rkt                  # expression emission with precedence
    item.rkt                  # fun/type/effect emission
racklr-test/
  lower-koka-test.rkt
```

## Phased Implementation

### Phase 1: Grammar acquisition

Author `KokaLexer.g4` and `KokaParser.g4` from scratch under `grammars-v4/koka/`, including the layout rule. There is no existing ANTLR4 grammar to adapt; the tree-sitter grammar can serve as a reference for the rule set but must be rewritten in ANTLR4. Verify the generated lexer and parser on a fixture covering functions, `match`, `type`, `effect`, and `with`. This phase is the dominant cost and gates everything else.

### Phase 2: UIR structs

Add `uir-effect-decl`, `uir-effect-op`, and `uir-effect-handler` to `uir/types.rkt`, with tests in `racklr-test/uir-test.rkt`. Decide the constructor-pattern representation.

### Phase 3: Lowering

Implement `lower-koka.rkt` and its submodules. Lower `effect` blocks into `uir-effect-decl`, handler clauses into `uir-effect-handler`, and effect type annotations into `uir-effect-row`. Lower `type` into `uir-enum`.

### Phase 4: Emission

Implement `emit-koka.rkt` and its submodules. Emit `uir-effect-decl` back to `effect`, `uir-effect-handler` back to `with ... handler`, and `uir-enum` back to `type`.

### Phase 5: Round-trip and effect-row tests

Add `lower-koka-test.rkt` with round-trip fixtures and a fixture that exercises the effect layer: a generator effect with `yield`, a `with` handler, and `resume`.

## Files Created/Modified

- `grammars-v4/koka/KokaLexer.g4` — Create (Phase 1)
- `grammars-v4/koka/KokaParser.g4` — Create (Phase 1)
- `racklr/uir/types.rkt` — Modify (add effect-decl/op/handler structs)
- `racklr-test/uir-test.rkt` — Modify (add struct tests)
- `racklr/lower-koka.rkt` — Create
- `racklr/lower-koka/helpers.rkt` — Create
- `racklr/lower-koka/expr.rkt` — Create
- `racklr/lower-koka/item.rkt` — Create
- `racklr/lower-koka/pattern.rkt` — Create
- `racklr/lower-koka/handler.rkt` — Create
- `racklr/emit-koka.rkt` — Create
- `racklr/emit-koka/expr.rkt` — Create
- `racklr/emit-koka/item.rkt` — Create
- `racklr-test/lower-koka-test.rkt` — Create

## Design Decisions

1. **Effect operations are first-class UIR nodes, not `uir-effect-entry`.** `uir-effect-entry` is a contract constraint (name, modality, ops, budget, scope). A Koka operation is a declaration with parameters and a body. The two are different and must not be conflated.
2. **Name `uir-effect-decl`, not `uir-effect`.** `uir-effect` is taken by the React reactive-effect node. Renaming or overloading it would break the Tier 2 IR and every TSX test.
3. **Constructor patterns use `uir-pat-class`.** Positional constructor arguments go in `positional`; record fields go in `keyword`. Do not extend `uir-pat-value`, which is a bare path with no sub-patterns.
4. **Curried functions lower to flat `uir-fn`.** A Koka `fun(x,y)` becomes a single `uir-fn` with two params. The emitter re-curries on output. Nested `uir-fn` would make cross-language emission (to Python, JS) produce spurious nesting.
5. **Operators are `uir-call` with a `uir-symbol` callee.** The emitter owns a precedence table. The lowerer never bakes parentheses into the IR.
6. **Effect annotations ride on `uir-fn`'s existing `effects` field.** Koka's `: console ()` and `: <exn, div> a` both lower to a `uir-effect-row`; `total` lowers to an empty row.
