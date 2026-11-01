# Plan: OCaml Lowering & Raising Pipeline

## Overview

Add OCaml as a source and target language: `lower-ocaml.rkt` (OCaml CST to UIR) and `emit-ocaml.rkt` (UIR to OCaml source), with tests. Enables OCaml round-tripping and cross-compilation through the UIR.

OCaml 5 has first-class effect handlers, but they are untyped. The "first-class typed effects" motivation does not hold for OCaml as it stands. The more interesting angle is the reverse: racklr's effect-inference layer can add static effect tracking on top of OCaml's untyped handlers, which the language itself does not provide.

## Status (verified 2026-11-01)

**Phase 1 (grammar acquisition): DONE.** No upstream ANTLR4 OCaml grammar
exists (`antlr/grammars-v4` has no `ocaml/` directory), so
`grammars/ocaml/OCamlLexer.g4` and `grammars/ocaml/OCamlParser.g4` are authored
from the OCaml 5.2 manual with no third-party grammar text. `grammars/ocaml/README.md`
records the acquisition evidence, the generator constraints worked around, and
the pass-1 limits; `racklr-test/gen-ocaml-test.rkt` exercises the Phase-1
fixture set (24 test cases; observed 24/24, exit 0). Phases 2-5 (lowering,
emission, effect inference over handlers, round-trip tests) are open. The
`Current State` section below records the pre-work survey.

## Effect-System Status (read this first)

OCaml 5 (released December 2022) added algebraic effect handlers to the runtime and standard library, but deliberately not to the type system.

- Effects are declared as GADT constructors: `type _ Effect.t += Xchg : int -> int Effect.t`.
- They are performed with `Effect.perform` and handled with `Effect.Deep.match_with` or shallow handlers.
- The OCaml manual states the key limitation plainly: "Unlike languages such as Eff and Koka, effect handlers in OCaml do not provide effect safety; the compiler does not statically ensure that all the effects performed by the program are handled."
- An unhandled effect raises `Effect.Unhandled` at runtime, not at compile time.

So OCaml has first-class effect handlers as a runtime feature, but typed effects are a research topic (gradual typing for effect handlers), not a shipped language feature. Typed effect support in OCaml is not present today.

Implication for racklr: OCaml effect usage lowers cleanly into ordinary UIR. An effect declaration is just a GADT constructor, `perform` is just a function call, and a handler is just a function with a `match`. No new UIR structs are required for the effect syntax itself. The value racklr adds is static: run the existing effect-inference pass over OCaml `perform` calls to recover the effect safety the type checker omits.

## Current State

- **Grammar**: None vendored. `grammars-v4/` has no OCaml grammar in the checkout, and the upstream grammars-v4 repository does not ship one. Options are to author `OCamlLexer.g4`/`OCamlParser.g4` directly, or to adapt the third-party tree-sitter-ocaml grammar. OCaml is not layout-sensitive, which removes the indentation problem, but its syntax is large: polymorphic variants, objects, modules, functors, and a long list of infix operators.
- **Lowering**: None. `lower-ocaml.rkt` does not exist.
- **Emission**: None. `emit-ocaml.rkt` does not exist.
- **UIR**: ADTs map to the existing `uir-enum`/`uir-enum-variant`. Pattern matching maps to `uir-match`/`uir-case`/`uir-pat-*`. Effect usage needs no new structs. Modules map to `uir-module`.

## UIR Gaps

OCaml is closer to the existing UIR than Koka, but has three real gaps:

- **Variant constructors with both positional and named fields.** `uir-enum-variant` carries `fields` as a list of `uir-typed-param`, which covers both. The gap is inline record variants and record types (`type t = { x : int; y : int }`), which need `uir-record` plus a type-level name, not just an anonymous value-level record.
- **Constructor patterns with sub-patterns.** `Some x` and `A (B y, z)` need a constructor pattern with positional/keyword children. Reuse `uir-pat-class` rather than the childless `uir-pat-value`.
- **Labeled and optional arguments.** `f ~x:1 ?y:2` does not map cleanly onto positional `uir-call` args. A design decision below proposes how to represent them in Pass 1.

## Scope: What We Handle (Pass 1)

- Values: `let ... in`, `let rec ... and ...`, literals, variables
- Functions (`fun`, `function`) and lambdas
- Calls, including labeled arguments (lowered as keyword entries)
- Operators and precedence
- Control flow: `if`/`then`/`else`, `match` with patterns and `when` guards
- Algebraic data types (`type ... = A | B of int`)
- Record types and record expressions
- Modules (`module M = struct ... end`) and `open`
- Effect declarations and handlers: `type _ Effect.t += ...`, `perform`, `Effect.Deep.match_with`

## Scope: What We Skip (Pass 1)

- Functors and first-class modules
- Objects and class types
- Polymorphic variants (`` `Foo ``)
- GADTs
- The module signature language (`sig`/`val`/`include`)
- Type coercions and `@@`/`|>` sugar
- OCaml's object-oriented features
- The full labeled-argument type machinery (we lower the syntax, not the semantics)

## Module Split Plan

```
racklr/
  lower-ocaml.rkt              # dispatcher (~60 lines)
  lower-ocaml/
    helpers.rkt
    expr.rkt                   # expressions, calls, operators, records
    item.rkt                   # let, type, module, effect declarations
    pattern.rkt                # match cases and constructor patterns
  emit-ocaml.rkt               # dispatcher (~40 lines)
  emit-ocaml/
    expr.rkt                   # expression emission with precedence
    item.rkt                   # let/type/module/effect emission
racklr-test/
  lower-ocaml-test.rkt
```

## Phased Implementation

### Phase 1: Grammar acquisition

Author or adapt `OCamlLexer.g4` and `OCamlParser.g4` under `grammars-v4/ocaml/`. Verify on a fixture covering `let`, `match`, `type`, records, and the `Effect` module. This is the dominant cost and gates the rest.

### Phase 2: Lowering

Implement `lower-ocaml.rkt` and submodules. Lower `type` variants into `uir-enum`, records into `uir-record`, `match` into `uir-match`, and `Effect.perform` into `uir-call`.

### Phase 3: Emission

Implement `emit-ocaml.rkt` and submodules with an operator-precedence table.

### Phase 4: Effect inference over OCaml handlers

Run the existing effect-inference pass over OCaml. A fixture with a `perform` that has no matching handler should be reported statically, which is exactly the check OCaml's own compiler skips.

### Phase 5: Round-trip tests

Add `lower-ocaml-test.rkt` with round-trip fixtures and the effect-inference fixture.

## Files Created/Modified

- `grammars-v4/ocaml/OCamlLexer.g4` — Create (Phase 1)
- `grammars-v4/ocaml/OCamlParser.g4` — Create (Phase 1)
- `racklr/lower-ocaml.rkt` — Create
- `racklr/lower-ocaml/helpers.rkt` — Create
- `racklr/lower-ocaml/expr.rkt` — Create
- `racklr/lower-ocaml/item.rkt` — Create
- `racklr/lower-ocaml/pattern.rkt` — Create
- `racklr/emit-ocaml.rkt` — Create
- `racklr/emit-ocaml/expr.rkt` — Create
- `racklr/emit-ocaml/item.rkt` — Create
- `racklr-test/lower-ocaml-test.rkt` — Create

## Design Decisions

1. **No new UIR for OCaml effects.** An effect is a GADT constructor, `perform` is a call, and a handler is a function with a match. Adding `uir-effect-decl`/`uir-effect-handler` for OCaml would invent IR for syntax the language does not actually have. Those structs belong to Koka, where the syntax is real.
2. **Labeled arguments lower to `uir-record` at the call site.** `f ~x:1 ?y:2` becomes a call whose argument is a `uir-record` of keyword entries. The emitter reconstructs the `~x:` syntax from the same record. This keeps `uir-call` positional and avoids a new call node.
3. **Records lower to `uir-record`; the type name rides on `uir-ann-set!` or a type field.** Value-level `{ x = 1 }` is already `uir-record`. The named type annotation is carried on the enclosing binding's type slot.
4. **Constructor patterns use `uir-pat-class`.** Same rule as the Koka plan; `uir-pat-value` stays childless.
5. **Modules lower to `uir-module`; `open` lowers to `uir-import`.** Functors and signatures are deferred because they do not round-trip through the existing struct set.
