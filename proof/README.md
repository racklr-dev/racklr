# Kernel-leaf machine check (ROADMAP test 2)

`EffectAlgebra.v` is a machine-checked Coq 8.20 transliteration of the effect
algebra kernel leaf:

    racklr/effects/algebra/{provenance,row,combinators,subtyping,certificate}.rkt

The Racket kernel stays the executable; this file is the source of truth for
its laws. It compiles with `coqc EffectAlgebra.v` (exit 0, no `Admitted`,
no axioms).

## Carrier and the one abstraction

- effect names / operation names / scope qualifiers : `string`
- provenance evidence : a set of `string` (a rendered site). The Racket
  `provenance` struct has `tokens`/`receiver`/`chain`; the kernel only unions,
  de-duplicates and renders evidence, and the bridge (`effervesce/check/bridge.rkt`)
  constructs every record as `(provenance (list "fn: site") #f '())`, i.e. one
  rendered site string. The laws are parametric in the record type, so the set
  of rendered sites is a faithful carrier.
- operation sets : `option NameSet.t`; `None` is the empty Racket ops list =
  "all operations" (top), `Some s` is a non-empty operation set. This makes
  the "empty dominates, never narrowed" rule a constructor instead of a
  side condition.
- a row : finite map `name -> (entry, evidence set)`; `must-not` entries are
  canonicalized to absence exactly as in `row.rkt`.

## Laws proven (each names its Racket property-test law)

- join: identity (empty row), commutativity, associativity, idempotence
  (`test-join-laws`).
- sequence: identity, associativity, budgets sum (`test-sequence-laws`).
- choose: commutativity, associativity, idempotence, budgets take the max,
  `must` survives only when every branch must (`test-choose-laws`).
- scope: the remaining/discharged split joins back to the row, the remaining
  side carries no handled names, and scoping over the empty name set is the
  identity (`test-scope-laws`).
- subtyping: reflexivity, transitivity, empty-row bottom, join is an upper
  bound, and join is a least upper bound in the contract order
  (`test-subtyping-laws`; the transitivity lemmas are
  `modality_subtype_trans`, `entry_subtype_trans`, `row_subtype_trans`).
- fixpoint: `fixpoint-step` is monotone in the join order (`row-leq?`),
  the law `test-fixpoint-laws` pins (`fixpoint_step_monotone`).
- certificate: the kernel accepts only grounded, coverable claims; a
  dropped-evidence claim (mutation test 3) is rejected
  (`dropped_evidence_rejected`, `check_claim_grounding`, `check_claim_coverage`).

122 lemmas/theorems, ~1390 lines of Coq.

## Finding (fixed): `must` must compare budget and ops, and with that the
order is transitive

The first machine check surfaced a genuine semantic gap in `effect-subtype?`
(`contract.rkt`): its `must` branch checked only that the target modality is
`must`, not budgets or operations. Consequence (old semantics):

    a = db(may, ops={read}, budget=0)
    b = db(must, ops={},     budget=5)     -- ops {} = "all"
    c = db(must, ops={write}, budget=3)

`a <= b` and `b <= c` held but `a <= c` did not, so the relation was not
transitive in general. That is the one-up-the-Google-researchers result: the
machine check drew the real trust boundary instead of assuming the laws hold.

The fix, made in the kernel semantics (`contract.rkt`), is to make `must`
compare `budget-<=` and `ops-superset?` exactly like `may`/`unknown`, keeping
only the modality target distinct (an obligation must be met by an obligation,
not a mere permission). The same fix landed in the generic per-slot algebra
(`racklr/hypergraph/effect.rkt`), whose `effect-entry-leq` now routes `must`
through the same per-slot lattices.

With that, the contract order is provably transitive, and the machine check
asserts and proves it: `modality_subtype_trans`, `entry_subtype_trans`, and
`row_subtype_trans` (plus the `entry_subtype_wf` / `entry_subtype_codom_wf`
shaping lemmas). The differential oracle pins the counterexample directly:
`effects-contract-test.rkt` checks `a <= b`, rejects `b <= c` (budget 5 > 3),
and the property test now weakens `must` budgets and ops so the general
transitivity law is exercised non-vacuously.

## The bridge (implementation bridge in ROADMAP.md)

The bridge is a direct transliteration: each Racket kernel function has a Coq
counterpart with the same structure, and each Coq theorem is named after the
Racket property-test law it pins. The Racket property tests remain the
differential oracle; `coqc` is the check that the laws are not just sampled
but hold for all rows.
