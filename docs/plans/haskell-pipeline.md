# Plan: Haskell Lowering & Raising Pipeline

## Overview

Add Haskell as a source and target language: `lower-haskell.rkt` (Haskell CST to UIR) and `emit-haskell.rkt` (UIR to Haskell source), with tests. Enables Haskell round-tripping and cross-compilation through the UIR.

Haskell has no effect type system in the language. Effects are handled by monads and, above them, by effect libraries. The "first-class typed effects" motivation does not hold for Haskell in the language sense; what Haskell offers is the pure/effectful split on `IO` and a mature ecosystem of effect libraries that racklr can map onto its effect rows.

## Status (verified 2026-11-01)

**Phase 1 (grammar verification): DONE.** The upstream `antlr/grammars-v4`
pair cannot parse under racklr's generator: its `OCURLY`/`VOCURLY`/`SEMI`/
`CCURLY` layout tokens are produced by the Java `HaskellBaseLexer` preprocessor,
whose lexer actions racklr compiles as no-ops, and its `NEWLINE` rule does not
carry the next line's indentation, so no `INDENT`/`DEDENT` is inserted. A
tracked authored pass-1 grammar now lives at `grammars/haskell/Haskell.g4`;
`grammars/haskell/README.md` records the acquisition evidence, the coverage
table, and the pass-1 limits, and `racklr-test/gen-haskell-test.rkt` exercises
it (14 test cases: tokenizer, layout, 32 Phase-1 parser fixtures, 15 rejects;
observed 14/14, exit 0). Phases 2-5 (lowering, emission,
typeclass-to-effect mapping, round-trip tests) are open. The `Current State`
section below records the pre-work survey, not this post-Phase-1 state.

## Effect-System Status (read this first)

Haskell distinguishes pure functions from effectful ones through a single type, `IO`, not through effect rows.

- There is no effect row in the type of a Haskell function. `IO a` says "may do arbitrary I/O," and nothing finer-grained.
- Finer effect tracking is a library concern: `mtl` typeclasses (`MonadState`, `MonadReader`, `MonadError`), `effectful`'s `Eff` monad, and `polysemy`/`fused-effects`. These are encodings in the type system, not language features.
- The research lineage is explicit about this: Wadler and Thiemann's "Marriage of Effects and Monads" shows effect systems translate into monads, and Orchard and Petricek's "Embedding effect systems in Haskell" treats effect systems as something embedded into, not built into, Haskell.

So Haskell has first-class monadic effects and a strong library ecosystem, but no first-class typed effect system in the language.

Implication for racklr: the interesting mapping is from typeclass constraints and `mtl`-style effects to `uir-effect-row`. A function with `MonadState S m => ...` lowers to a `uir-fn` whose effects row contains a `state` entry, which racklr's existing contract algebra can then check.

## Current State

- **Grammar**: Vendored. `grammars-v4/haskell/` has `HaskellLexer.g4` (1742 lines) and `HaskellParser.g4` (1771 lines), entry rule `module`. This is the only one of the three languages whose grammar is already present.
- **Lowering**: None. `lower-haskell.rkt` does not exist.
- **Emission**: None. `emit-haskell.rkt` does not exist.
- **UIR**: ADTs map to `uir-enum`/`uir-enum-variant`. Typeclasses map to `uir-interface`. Pattern matching maps to `uir-match`/`uir-case`/`uir-pat-*`. Do-notation needs a desugaring decision (below).

## UIR Gaps

- **Constructor patterns with sub-patterns.** `Just x` and `Branch l r` need a constructor pattern with children. Reuse `uir-pat-class`, matching the Koka and OCaml plans.
- **Guard expressions on case alternatives.** `uir-case` already has a `guard` field, which covers `| cond ->`.
- **Type signatures and type variables.** `foo :: Int -> Int` and the `a` in `map :: (a -> b) -> [a] -> [b]` need a type representation. Pass 1 strips type parameters and keeps concrete signatures as opaque type nodes; see design decisions.
- **Do-notation.** `do { x <- m; ... }` has no direct UIR node. It must be desugared to a chain of `uir-call` bind operations or lowered structurally to `uir-block` plus `uir-let`. Decision below.

## Scope: What We Handle (Pass 1)

- Top-level function bindings and type signatures
- `let`/`where` bindings
- Expressions: literals, variables, calls, operators, lambdas (`\x -> ...`)
- Control flow: `if`/`then`/`else`, `case` with patterns and guards
- Algebraic data types (`data`) with positional and named-field constructors
- `newtype` (lowered as `uir-enum` with one variant)
- Typeclasses and instances (lowered as `uir-interface` and a class-like node)
- Do-notation (desugared to bind chains)
- Modules and imports
- `mtl`-style typeclass constraints lowered to `uir-effect-row` entries

## Scope: What We Skip (Pass 1)

- The layout rule's hard cases (we rely on the grammar's layout handling and revisit failures)
- GADTs and existential types
- Higher-kinded types and type families
- Template Haskell and quasiquotes
- Type and data families
- Deriving strategies and `deriving via`
- Pattern synonyms
- `foreign import` and FFI
- Strictness and unpack annotations (`!`, `{-# UNPACK #-}`)

## Module Split Plan

```
racklr/
  lower-haskell.rkt            # dispatcher (~60 lines)
  lower-haskell/
    helpers.rkt
    expr.rkt                   # expressions, operators, lambdas
    item.rkt                   # bindings, data, typeclass, module
    pattern.rkt                # case alternatives and constructor patterns
    do.rkt                     # do-notation desugaring
  emit-haskell.rkt             # dispatcher (~40 lines)
  emit-haskell/
    expr.rkt                   # expression emission with precedence
    item.rkt                   # binding/data/typeclass emission
racklr-test/
  lower-haskell-test.rkt
```

## Phased Implementation

### Phase 1: Grammar verification

Verify the vendored Haskell grammar generates a working lexer and parser under racklr's generator. The grammar is large and layout-sensitive, so expect lexer-conversion quirks of the kind already documented for the Python and TSX lexers. This phase should be reached first, since it is the least-costly unknown.

### Phase 2: Lowering

Implement `lower-haskell.rkt` and submodules. Lower `data` into `uir-enum`, typeclasses into `uir-interface`, `case` into `uir-match`, and do-notation into bind chains.

### Phase 3: Emission

Implement `emit-haskell.rkt` and submodules with an operator-precedence table and the standard function-application style.

### Phase 4: Effect mapping from typeclass constraints

Map `MonadState S m`, `MonadReader R m`, `MonadError E m`, and `MonadIO m` constraints on function signatures to `uir-effect-row` entries (`state`, `read`, `throw`, `io`). Feed these rows into the existing contract-algebra pass.

### Phase 5: Round-trip tests

Add `lower-haskell-test.rkt` with round-trip fixtures and the typeclass-to-effect-row fixture.

## Files Created/Modified

- `racklr/lower-haskell.rkt` — Create
- `racklr/lower-haskell/helpers.rkt` — Create
- `racklr/lower-haskell/expr.rkt` — Create
- `racklr/lower-haskell/item.rkt` — Create
- `racklr/lower-haskell/pattern.rkt` — Create
- `racklr/lower-haskell/do.rkt` — Create
- `racklr/emit-haskell.rkt` — Create
- `racklr/emit-haskell/expr.rkt` — Create
- `racklr/emit-haskell/item.rkt` — Create
- `racklr-test/lower-haskell-test.rkt` — Create

## Design Decisions

1. **No new UIR for Haskell effects.** There is no effect syntax in the language to lower. The effect connection is a mapping from `mtl` typeclass constraints to `uir-effect-row`, which is a lowering rule, not a new struct.
2. **Typeclasses lower to `uir-interface`; instances are folded.** An instance's methods become a `uir-class`-like node carrying the target type. This mirrors the Rust plan's `impl` folding decision.
3. **Do-notation desugars to bind chains.** `do { x <- m; body }` becomes `(uir-call (uir-symbol ">>=") (list m (uir-fn ...)))`. Representing the raw `do` as a block would lose the monadic structure that the effect mapping needs.
4. **Type variables are stripped in Pass 1.** Signatures keep concrete types as opaque type nodes; polymorphism is dropped and re-inferred nowhere. This matches the Rust plan's treatment of generics.
5. **Operators are `uir-call` with a `uir-symbol` callee.** Precedence and fixity live only in the emitter's table; the lowerer stores the operator symbol as-is.
6. **`where` clauses lower to a `uir-block` of `uir-let` bindings.** The order is normalized so the emitter can choose between `let` and `where` on output without changing meaning.
