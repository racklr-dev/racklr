# ROADMAP

The end state, the goal, and the order of work that gets from here to there.

## The goal in one paragraph

One system that reads any source language into a single self-describing UIR, attaches a typed effect row to every function grounded in the concrete call chain with zero unknown leaves, stores the whole thing in one searchable hypergraph where context is a scoped subgraph, checks that graph against declared capability contracts through a small, machine-checked effect-algebra kernel, emits any target language with effects carried, compiles its own compiler, and modifies itself only through its own effect gate. A tower of babel for programming, where the tower is not a translation service but a language whose type system is effects — and where the only code that must be trusted is a kernel leaf small enough to prove correct.

Five sentences hold the whole thing together:

- The effect algebra is the type system.
- The kernel leaf is the trust root.
- The hypergraph is the memory.
- The tower is the bidirectional transform.
- Self-modification is effect-gated.

Everything in this document is those five sentences made concrete.

## What already exists

This roadmap is additive, not a rewrite. The foundation is real and green today:

- Parsers for Python3, TypeScript, TSX, JavaScript, JSX, Clojure, and Koka generated from vendored grammars via `racklr/gend-parser.rkt` and lowered into UIR through the `lower-*` modules.
- The Django corpus as the proving ground: 907 of 907 files parse with zero parse failures.
- A UIR data model in `racklr/uir.rkt` and `racklr/uir/types.rkt`.
- Effect inference in `racklr/effects/`: a monotone fixpoint over the call graph that computes effect rows (db, io, network, process, cache, llm, time, email) with MRO-resolved `self.m()` dispatch and every call site classified.
- Checkers in `effervesce/check/`: type, policy, model, and bridge. The bridge checker already compares inferred host effect rows against declared component `needs`.
- A hypergraph substrate in `racklr/hypergraph.rkt` that already holds the effervesce capability graph.
- Emit modules (`emit-python.rkt`, `emit-javascript.rkt`, `emit-jsx.rkt`, `emit-koka.rkt`, `emit-clojure.rkt`, plus DOM, HTML, and JSON) that are already decoupled from lowering, one per target.

The missing pieces are not parsers or checkers. They are the effect algebra as an explicit type system, the kernel leaf that makes the algebra checkable rather than trusted, the self-describing UIR, the bidirectional emit contract, the single-substrate graph, and the living gated system.

## The end state

The system settles into seven layers:

- Absorb: grammars plus lowering hooks turn source text into UIR. Language knowledge is data, not core code.
- UIR: a schema that describes itself, versioned and migratable.
- Effect: an explicit typed algebra with combinators, polymorphism, and subtyping, grounded in provenance, whose kernel leaf is machine-checked.
- Check: type, policy, model, and bridge checkers over the graph, each reduced to a certificate the kernel accepts or rejects.
- Graph: one hypergraph holding UIR, call graph, type graph, effect graph, capability graph, and module graph with provenance edges.
- Emit: one contract that renders any UIR plus effect row into any target's native effect vocabulary.
- Live: a long-lived process with a durable journal, loop-as-data, and a self-modification gate that is the effect checker, running on the machine-checked kernel so the gate is checkable rather than asserted.

## The invariants

These are the laws. Each is a checkable property, not an aspiration.

- No unknown effects. Every node carries a row. Unknown is a first-class tracked state with an elimination obligation, never a silent default. Finishing with an unknown leaf is as unacceptable as finishing with an unhandled exception.
- Evidence over assertion. Every effect edge carries provenance: the tokens, the receiver type, and the call chain that produced it. Something is `db` because you can point at why, or it is not classified.
- Round-trip fidelity within a declared loss model. Absorb then emit is identity up to a loss model you declare, not one the tool hides. Where two languages genuinely disagree, the disagreement is a first-class artifact.
- The gate is the contract. Any self-modification must satisfy the same effect algebra and capability graph the system enforces on its inputs. The system that checks code is itself checked.
- The kernel leaf is small and checked. The effect algebra has a machine-checked kernel — its laws, its subtype relation, and its certificate checker — small enough to audit, and every check the system makes reduces to a certificate the kernel accepts or rejects. Everything outside the kernel — inference, lowering, emit, and any agent — is untrusted by construction and cannot weaken a guarantee the kernel has accepted.

## The roadmap

Ordered by dependency. Each phase retires a specific risk before the next one builds on it.

### Phase 0: Make the foundation load-bearing

Freeze the substrate so later phases are not built on sand.

- Version the UIR schema and give it a migration path. The known trap of changing a struct's field count and silently breaking serialization becomes a controlled event.
- Land receiver-type dataflow: effect classification becomes a property of the resolved receiver class, resolving `X.objects.Y` and imported-receiver calls to a class and its module domain. Method-name heuristics come out.
- Drive the Django corpus to the acceptance bar: 907 of 907 files parse, every function and method carries a row, zero unknown leaves, provenance on every edge.

Definition of done: the full suite is green and the UIR schema is versioned with a migration path.

Risk retired: later phases built on an unstable or guessed representation.

### Phase 1: Extract the effect algebra and verify its kernel leaf

The creative core. This is what makes the project more than another transpiler.

- New module for the effect algebra, separate from inference: a lattice of effect kinds, effect rows as sets of kind-provenance pairs, and combinators over them.
- Join is the fixpoint operation. Sequence composes rows. Choose handles branches. Scope bounds a handler.
- Effect polymorphism: a row hole that a call site instantiates and a handler closes. Higher-order functions type once and specialize at each call site instead of collapsing to unknown.
- Effect subtyping: row A is a subtype of row B when every effect in A is interpretable by a handler in B. The bridge checker's capability-interpretation table is this relation today, restricted to capability names. Generalize it to the full algebra.
- Split the algebra into a kernel leaf and an untrusted producer. The kernel is the laws, the subtype relation, and a certificate checker that re-verifies a claimed row against a declared contract. The producer is inference — the fixpoint, receiver-type dataflow, and lexicons — which emits a row plus a certificate and never decides. A bug in the producer can emit a wrong row; it cannot make the kernel accept a certificate that is not derivable.
- Machine-check the kernel leaf: join associativity/commutativity/idempotence, fixpoint monotonicity, the subtyping laws, and the soundness of the certificate checker. The property tests stay as a differential oracle; the machine-checked proof is the guarantee.

Definition of done: the algebra is a standalone typed library with property tests (associativity of join, monotonicity of the fixpoint, subtyping laws); its kernel leaf is machine-checked within a declared trusted-line budget (start at 1,000 lines and shrink); and inference results are unchanged because the new algebra is what the old fixpoint was doing implicitly, with the corpus running through the certificate checker unchanged.

Risk retired: the effect system stays a set of labels instead of becoming a type system, and the checker stays trusted-by-convention instead of trusted-by-proof.

### Phase 2: Self-describing UIR and data-driven front ends

Makes "any language" an engineering problem instead of a rewrite.

- Express the UIR schema as data, written in UIR itself. New node kinds, fields, and types are additions to a schema value, not edits to core struct definitions.
- Move all language knowledge out of the core into injectable data: grammars, lowering hooks, token lexicons, receiver-type domain tables.
- The receiver-type dataflow from Phase 0 becomes the type layer that feeds both effect classification and emit decisions.

Definition of done: adding a new front end touches zero core files. Prove it by adding one non-trivial front end (Rust or Go) from the vendored grammars plus hooks and data.

Risk retired: a schema that must describe itself but cannot.

### Phase 3: The bidirectional tower

Universal absorb and emit, and therefore translation.

- Unify the emit modules behind one contract that renders an effect row into the target's native effect vocabulary: typed effects for Koka, an effect type or documented annotation for TypeScript, a structured comment or type for Python, and so on.
- Define the loss model explicitly. Two languages with different concurrency or memory semantics cannot round-trip perfectly. The loss model says what is preserved, what is approximated, and what is refused, per language pair.
- Translation is absorb then emit. Any supported pair A to B goes through UIR once, effects carried.

Definition of done: a source in language A translates to language B with every effect row preserved in B's native form, and the round-trip property holds within the declared loss model, verified by the existing round-trip harness.

Risk retired: emit stays a set of ad-hoc pretty printers.

### Phase 4: The hypergraph as the single substrate

One graph, all questions as traversals.

- Put UIR, call graph, type graph, effect graph, capability graph, and module graph into `hypergraph.rkt` as one substrate with typed edges.
- Contextual scoping: a query anchors at a node and scopes to the subgraph reachable from it along declared edge kinds, by module, component, capability, or call chain.
- Provenance queries become first class: "what does `main()` touch" and "show the exact call chain that makes this function `db`" are the same query shape as effervesce's `ask`.

Definition of done: every question `ask` and `verify` answer today is a typed traversal, and a new question is a new query, not new ad-hoc code.

Risk retired: reasoning scattered across checker modules instead of centralized in one graph.

### Phase 5: The living system, self-modification through the effect gate

The fully effectual self-modifying system. This is the only genuinely new subsystem.

- Live image: the tower runs as a long-lived process. Racket on Chez already gives dynamic redefinition and eval, the same property SBCL gives nodecode and Jolt gives samizdat.
- Durable journal: an append-only log of every modification, each entry recording the proposed definition, its inferred effect row, and the accept, isolate, or rollback decision. The system resumes by replaying the journal.
- Loop as data: the workflow is a UIR program or manifest the agent can inspect and rewrite, behind the same gate as anything else.
- The gate: a proposed change is absorbed, inferred, and checked against its component's capability contract. Inference proposes; the kernel disposes. A covered effect is accepted only when the kernel accepts the certificate. Uncovered effects are isolated or rolled back, and the refusal names the offending effect with its provenance.

Definition of done: the system modifies its own definition set, and refuses a change whose effects its contract does not cover, showing exactly which call made the change illegal — with the accept/refuse decision made by the kernel, not by the inference that produced the row.

Risk retired: the effect algebra stays a report instead of becoming a permission system.

### Phase 6: The reflexive tower, any-to-any including itself

The literal tower of babel.

- The compiler is a UIR program with effect rows.
- Emit the compiler itself into a target language.
- The emitted tower passes the same suite the original passes.

Definition of done: the tower emits itself, and the self-emitted tower translates languages the original does.

Risk retired: the implementation language becomes a permanent dependency instead of an implementation detail.

## The kernel-leaf theory and how to test it

The grand vision borrows the one move that makes AlphaProof Code trustworthy: separate an untrusted producer from a small trusted kernel, and let the kernel be the only thing that decides. Here the producer is effect inference — the fixpoint, receiver-type dataflow, and lexicons — and the kernel is the effect algebra: its laws, its subtype relation, and a certificate checker that re-verifies a claimed row against a declared contract. A bug in inference can produce a wrong row; it cannot make the kernel accept a certificate that is not derivable. That is the theory.

It is a falsifiable claim, and the roadmap tests it rather than assumes it. The tests, in order:

1. **Extract the kernel (Phase 1).** Can the algebra split into a small trusted core plus an untrusted producer? The success metric is a line budget, not a feeling: the kernel — laws, subtype relation, and certificate checker, excluding the proof of itself — must fit in a stated budget, starting at 1,000 lines and shrinking. If the split forces the certificate checker to re-implement inference, the theory is falsified.
2. **Prove the kernel once (Phase 1).** Machine-check the algebra laws (join associativity/commutativity/idempotence, sequence/choose/scope, subtyping reflexivity and transitivity, fixpoint monotonicity) and the soundness of the certificate checker. The property tests become a differential oracle against the machine-checked proof; they must keep agreeing.
3. **Break the untrusted part on purpose (the mutation test).** Inject a known bug into inference — wrong receiver resolution, a dropped provenance edge, a lexicon entry — and show the kernel rejects the resulting certificate or the gate still refuses the change. A kernel that accepts a broken producer's output is not a kernel.
4. **Re-prove the re-injected bug (the regression test).** Take a historical misclassification or a deliberately introduced effect (a pure function that becomes `db`) and show the kernel-certificate path flags it. This mirrors AlphaProof Code re-injecting historical libsignal bugs to test its specification.
5. **Hold the corpus invariant.** The Django baseline and the full corpus must run through the kernel-certificate path unchanged. The kernel is a new checker for the same algebra, not a new algebra; any graph drift is a regression until reviewed.

Falsification criteria, stated up front so the test cannot be rigged:

- The kernel cannot be made small: the certificate checker needs the whole receiver-type dataflow or lexicon machinery to decide, so the trusted core is not a leaf.
- The kernel cannot be proved sound: the algebra's real semantics — grounding in provenance, row holes, budgets, scopes — outrun what can be machine-checked at the leaf.
- The kernel rejects a certificate the existing checker accepts, or accepts one it should reject, and the discrepancy is a genuine semantic gap rather than a bug in either.

Any of those is a publishable finding, not a failure: it draws the real trust boundary. If the theory survives, the payoff is the one thing the current roadmap lacks — a gate that is checkable by construction, not by convention, which is the property Phase 5 needs before it lets the system modify itself.

Implementation bridge: the proof assistant is a proof artifact, not a rewrite of the tower. The kernel stays implemented in Racket; its Lean (or Coq) specification is the source of truth, and the bridge is one of a direct transliteration of the kernel (small enough to review line by line), extraction to Racket, or a differential harness that runs the property tests against the machine-checked laws. The tower does not move languages to host its own proof.

## The language decision, resolved

The roadmap dissolves the question of which language to write the tower in.

The tower is language-neutral at its core. Its implementation language is an implementation detail, and by Phase 6 it stops being even that, because the tower emits itself into whichever language you want. The implementation language's only real job is to be the place where the tower is already most built, and to provide the live-image property the living layer needs.

Racket on Chez is exactly that. The parsers, the effect inference, the checkers, and the hypergraph are already written in it, and its live image is the same property nodecode buys from SBCL and samizdat from Jolt.

So the answer is not Rhombus, Chez, Common Lisp, Clojure, Elixir, or Erlang. The answer is: keep the tower in Racket through Phase 5, and let Phase 6 make the question irrelevant. Switching now strands the effect inference and the front ends, which is the one part neither nodecode nor samizdat has, and the part that makes the whole thing worth building.

## The hard parts

The places this can fail, stated up front.

- Effect subtyping plus handler semantics is the real research. Koka and effect-ts are the references, but the constraint here is stricter: every row must be grounded in tokens, never in a library table. That grounding requirement is what makes this novel, and it is the part that can stall.
- Round-trip fidelity is bounded by language semantics, not effort. Concurrency, memory models, and runtime dispatch genuinely differ. The loss model is the honest answer, and it must be enforced, not papered over.
- Self-description is a bootstrap problem. The schema that describes UIR must itself be in UIR. Resolve it with a fixed-point kernel that stays hand-written and versioned, like a trust root. Everything else builds on it.
- Zero unknown leaves is a moving target as language coverage grows. The honest formulation is an elimination obligation, not a static guarantee. Unknown must be tracked, surfaced, and driven down, never silently defaulted.
- The gate has a trust problem: who gates the gate? Resolve it the way samizdat resolves its kernel: the journal, the gate, and the checker are agent-immutable. The kernel leaf makes that immutability checkable rather than asserted by convention.
- The kernel-leaf bet can fail. The theory is that the effect algebra separates into a small trusted core with everything else untrusted. It is falsified if the grounding requirement — provenance and receiver-type dataflow — cannot be pushed outside the kernel, or if the certificate format must re-embed the whole inference engine to be checkable. Falsification is a result: it tells us the gate must stay asserted-by-convention and the trust boundary is wider than hoped.

## Non-goals

- This is not nodecode and not samizdat. It borrows their live-image and gate properties, but the effect system here is a typed N-way algebra with provenance, not a boolean pure-or-effectful flag.
- This is not a general-purpose concurrency platform. The BEAM's process model is out of scope; the workload is tree walking, graph traversal, and a fixpoint, which do not need it.
- This is not a promise of perfect any-to-any translation. The loss model is the guarantee, and it is explicit and enforced.
- This is not a plan to formally verify the language semantics, the parsers, the lowering passes, or the emitters. The kernel-leaf bet is that verifying the effect algebra alone is enough; the rest stays untrusted by construction, and a wider verification campaign is a separate question.

## The first move

Phase 0 and Phase 1 are where value compounds. The effect algebra is the thing every later phase treats as its type system, and it has to be real — and its kernel leaf proven — before the gate, the reflexive tower, or the self-describing UIR can stand on it.

The next concrete step is a task-by-task plan for Phase 0 and Phase 1 with exact file paths and definitions of done, with kernel-leaf extraction as Phase 1's first task.
