#lang racket

;; racklr/hypergraph/uir-project.rkt
;;
;; Production projections from a UIR program (and the effect rows inference
;; computes for it) into the hypergraph substrate.
;;
;; The substrate itself stays dependency-free: racklr/hypergraph.rkt and
;; racklr/hypergraph/project.rkt define atoms, typed edges, the single instance,
;; and data-driven projectors that take plain data. This module is the adapter
;; layer that knows about UIR structure and effect inference, so callers do not
;; have to reverse-engineer the conversions (they used to live only in
;; hypergraph-test.rkt). It is deliberately NOT re-exported by the facade, so
;; requiring racklr/hypergraph does not pull in the UIR or inference layers.
;;
;; The documented path from inference to one instance:
;;
;;   (require racklr/hypergraph
;;            racklr/hypergraph/uir-project)
;;   (define program ...)                      ; a UIR program
;;   (build-hypergraph
;;     #:uir program
;;     #:calls (program-call-edges program)
;;     #:effects (effect-rows->graph-rows program)
;;     #:modules (program-module-deps program 'app)
;;     #:types ...  #:capabilities ...)        ; those two are already plain data
;;
;; The projection is one-way by design: the graph is a derived, queryable view
;; and UIR remains the single source of truth (see docs/plans/dark-factory-library.md).

(require racklr/uir
         racklr/effects/contract
         racklr/effects/infer
         racklr/effects/collect
         racklr/effects/walk
         "effect.rkt")

(provide effect-entry->link
         effect-row->links
         effect-rows->graph-rows
         program-call-edges
         program-import-sources
         program-module-deps)

;; ── effect rows ─────────────────────────────────────────────────────

;; One `uir-effect-entry` as the substrate's 5-slot effect hyperedge
;; (name, modality, ops, budget, scope). `infer-effects` produces UIR structs;
;; `effect-graph-atoms` consumes these links.
(define (effect-entry->link e)
  (effect-entry
   (effect-name-symbol e)
   (uir-effect-entry-modality e)
   (entry-ops e)
   (uir-effect-entry-budget e)
   (and (uir-effect-entry-scope e)
        (string->symbol (uir-symbol-name (uir-effect-entry-scope e))))))

;; The effect hyperedges of one inferred `uir-effect-row`.
(define (effect-row->links row)
  (for/list ([e (in-list (uir-effect-row-entries row))])
    (effect-entry->link e)))

;; The `#:effects` input of `build-hypergraph`: one (function-name entries) pair
;; per named function/method, sorted by name so the projection is deterministic.
;; This is the documented path from `infer-effects` to the effect graph.
(define (effect-rows->graph-rows program #:provenance-table [provenance-table #f])
  (sort (for/list ([(name row) (in-hash (infer-effects program
                                                       #:provenance-table provenance-table))])
          (list name (effect-row->links row)))
        (λ (a b) (symbol<? (first a) (first b)))))

;; ── call graph ──────────────────────────────────────────────────────

(define (callee-symbol c)
  (and (uir-symbol? c) (string->symbol (uir-symbol-name c))))

;; Every callee name invoked in one function body. `walk-body` stops at nested
;; function boundaries, and the collectors enumerate nested named functions as
;; their own call-graph nodes, so this attributes a call to its enclosing
;; function only.
(define (body-callees body)
  (define out '())
  (walk-body body
             (λ (n)
               (when (uir-call? n)
                 (define callee (callee-symbol (uir-call-callee n)))
                 (when callee (set! out (cons callee out))))))
  out)

;; The structural call graph: one (caller callee) pair per named call whose
;; callee is a plain symbol. Receiver-dispatched calls (`obj.m()`) are resolved
;; by the classifier's receiver-type dataflow, not guessed here. Candidates are
;; the same named functions/methods inference builds its call graph over.
(define (program-call-edges program)
  (define edges '())
  (define (add-callees caller body)
    (for ([callee (in-list (body-callees body))])
      (set! edges (cons (list caller callee) edges))))
  (for ([(name body) (in-hash (collect-functions program))])
    (add-callees name body))
  (for ([(name body) (in-hash (collect-methods program))])
    (add-callees name body))
  (sort (remove-duplicates edges)
        (λ (a b)
          (define ka (format "~a.~a" (first a) (second a)))
          (define kb (format "~a.~a" (first b) (second b)))
          (string<? ka kb))))

;; ── module graph ────────────────────────────────────────────────────

;; The import sources named by a program's UIR (`import a.b` / `from x import y`
;; lower to `uir-import` nodes whose source is the dotted name), in first-seen
;; order. This is the module-graph edge source; full module resolution stays in
;; racklr/effects/imports.
(define (program-import-sources program)
  (define sources '())
  (walk-uir program
            (λ (n)
              (when (uir-import? n)
                (define src (uir-import-source n))
                (when (and src (uir-symbol? src))
                  (set! sources (cons (string->symbol (uir-symbol-name src)) sources))))))
  (remove-duplicates (reverse sources)))

;; The `#:modules` input of `build-hypergraph` for a program rooted at
;; `module-name`: that module imports every source the program names.
(define (program-module-deps program module-name)
  (list (append (list module-name) (program-import-sources program))))

(module+ main
  (displayln "racklr/hypergraph/uir-project — UIR/inference projections loaded."))
