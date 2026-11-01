#lang racket

;; racklr/hypergraph.rkt
;;
;; The graph layer of racklr: ONE typed hypergraph substrate for the six
;; graphs the system reasons over.
;;
;;   uir         the materialized UIR/AST (struct vertices + `slot` edges)
;;   call        function -> callee (`calls` edges)
;;   type        named types, subtyping, and the typed relations
;;               (assign / self-attr / param / return)
;;   effect      function -> inferred effect-row hyperedges (`fn-effect-row`)
;;   capability  the Effervesce needs/provides/interprets graph
;;               (component-needs, component-provides, capability-interprets,
;;                given, capability-effect, action-writes, effect-touches)
;;   module      modules and their imports (`module-imports`, `module-defines`)
;;
;; A single `hypergraph` instance holds all six at once: a `function` vertex is
;; the same atom in the call, effect, and module graphs, so a question crosses
;; graph boundaries without translating between parallel structures. Every edge
;; type declares its graph kind in schema.rkt, which is what makes
;; `hg-links` (by kind), `hg-edges` (by type), `hg-out`/`hg-in` (by direction),
;; and `hg-ill-typed-edges` (schema validation) work over the whole substrate.
;;
;; This module is a thin facade; the implementation is split by responsibility
;; under racklr/hypergraph/ (the 400-line modularity rule):
;;
;;   kernel.rkt    atoms, lattices, per-slot algebra, pattern query,
;;                 generic materialize, flat-atom closure
;;   atomese.rkt   OpenCog-compatible Atomese rendering
;;   effect.rkt    the effect-row instance of the algebra
;;   types.rkt     the type + capability instance (subtyping, needs/provides)
;;   schema.rkt    the six graph kinds and typed-edge schema
;;   instance.rkt  the single hypergraph instance + typed queries
;;   project.rkt   the six projections + vocabulary constructors
;;
;; The UIR/inference-facing adapters live in racklr/hypergraph/uir-project.rkt
;; (effect-row -> hyperedges, structural call graph, module import graph). They
;; are not re-exported here on purpose: the facade stays free of UIR and effect
;; dependencies, and the adapter module carries them where a caller needs them.
;;
;; The public API below is a superset of the pre-P4.1 kernel: every previously
;; provided name keeps its shape, so existing consumers (effervesce's graph and
;; query suites, the type-graph baseline tool, hypergraph-test) load unchanged.
;;
;; History: the kernel started as the AtomSpace-style algebra behind two
;; hand-rolled domains — effect rows (5-slot hyperedges over a per-slot
;; lattice) and types (a base type node, constructor hyperedges, variance-aware
;; subtyping with an embedded effect row) — then grew the Effervesce capability
;; graph and the generic AST materialization. P4.1 extends it into the shared
;; substrate so those projections are one graph instance with typed, queryable
;; edges.

(require "hypergraph/kernel.rkt"
         "hypergraph/atomese.rkt"
         "hypergraph/effect.rkt"
         "hypergraph/types.rkt"
         "hypergraph/schema.rkt"
         "hypergraph/instance.rkt"
         "hypergraph/project.rkt")

(provide
 ;; atoms + algebra + query + materialization
 (all-from-out "hypergraph/kernel.rkt")
 ;; Atomese projection
 (all-from-out "hypergraph/atomese.rkt")
 ;; effect-row instance
 (all-from-out "hypergraph/effect.rkt")
 ;; type + capability instance
 (all-from-out "hypergraph/types.rkt")
 ;; six graph kinds + typed-edge schema
 (all-from-out "hypergraph/schema.rkt")
 ;; the single graph instance + typed queries
 (all-from-out "hypergraph/instance.rkt")
 ;; the six projections + vocabulary constructors
 (all-from-out "hypergraph/project.rkt"))

(module+ main
  (displayln "racklr/hypergraph — typed six-graph hypergraph substrate loaded."))
