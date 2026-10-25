# AGENTS.md

Project rules for racklr and effervesce. The authoritative contract is
`STYLE_GUIDE.md`. Read it before any change. `README.md` documents the
architecture, `docs/` holds the design documents (`uir-schema.md`,
`effects-proposal.md`), and `LOOP_PLAN.md` / `ROADMAP.md` track the work.

## What this is

racklr lowers Python3, TypeScript, TSX, JavaScript, JSX, Clojure, and Koka
source into one universal intermediate representation (UIR) and emits target
code from it. effervesce infers a typed effect row for every function and
method, grounded in the concrete call chain, and checks that graph against
declared capability contracts.

## Obligations (non-negotiable)

- Every function and method carries an effect row. `unknown` is a tracked
  state with an elimination obligation, never a silent default.
- Effect keys come from the call chain via receiver-type dataflow
  (class to module to domain), never from a method-name table in engine code.
- Library-specific tokens live only in `racklr/effects/library-tokens.rkt` and
  `racklr/effects/imports/library-config.rkt`. Engine code carries none.
- Every effect edge carries provenance you can point at.
- UIR is the single source of truth.

## Modularity

- No `.rkt` file over 400 lines. Split by responsibility, not size.
- Respect layer boundaries: absorb, uir, effect, check, graph, emit.
- A new front end is a grammar plus a lowering hook plus token-lexicon data.
  It must not edit core modules.
- Keep public call shapes stable. Add injectable config as an optional
  argument with a default, not a breaking change.

## Test gate

Run from the repo root unless noted. `raco test` is unreliable here: zero exit
is not success. Grep the output for FAILURE and ERROR and read the real exit
code.

- `raco setup racklr` then `raco setup effervesce`, both exit 0.
- Racklr suites: `cd racklr-test && racket <suite>-test.rkt`.
- Effervesce suites: `cd effervesce && racket <suite>-test.rkt` (must run from
  `effervesce/`).
- Shared baseline:
  `racket effervesce/tools/type-graph.rkt django/django/core` must print
  `112 parsed, 0 failed, db 52 files, network 2, unknown 83, inherits 126`.
  Drift from these numbers is a regression until proven otherwise.

## Working rules

- Confirm the baseline is green before touching anything. Record the numbers
  you observed before and after.
- Report only what you observed. Say plainly when you could not verify
  something. Never manufacture a green result or suppress a failing check.
- Fix the underlying representation, not the symptom.
- Do not commit. Commits are the operator's job, GPG-signed. Leave the working
  tree uncommitted and say what changed. Multi-step work lives on a dedicated
  feature branch; stage specific files, never `git add -A`.
