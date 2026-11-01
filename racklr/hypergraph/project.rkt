#lang racket

;; racklr/hypergraph/project.rkt
;;
;; The six graph projections into the one substrate, plus the vocabulary
;; constructors that make each projection a typed edge rather than a raw atom
;; soup. Each projector returns atoms (nodes + typed links) and every edge type
;; it emits is declared in schema.rkt:
;;
;;   uir-graph-atoms        struct tree / S-expression  -> uir graph
;;   call-graph-atoms       (caller callee) pairs        -> call graph
;;   type-graph-atoms       (relation subject target)    -> type graph
;;   effect-graph-atoms     function -> effect entries   -> effect graph
;;   capability-graph-atoms .fizz-style declarations     -> capability graph
;;   module-graph-atoms     module -> imports            -> module graph
;;
;; `build-hypergraph` composes all six into ONE `hypergraph` instance; a
;; function node is then the same atom in the call, effect, and module graphs,
;; so a question can cross graph boundaries without translating between
;; separate structures.
;;
;; The projectors are data-driven and dependency-free (no UIR or effect-layer
;; requires): the engine that produces each data shape calls the projector, and
;; this module never decides how a graph is computed.
;;
;; Vocabulary constructors are `hg-`-prefixed so they cannot collide with a
;; consumer that also defines or provides the same edge shape (Effervesce's
;; check/graph.rkt predates the substrate and still owns its local copies until
;; P4.3 re-expresses it over this module).

(require "kernel.rkt"
         "types.rkt"
         "schema.rkt"
         "instance.rkt")

(provide
  ;; shared vertex vocabulary
  hg-function-node
  ;; uir graph
  uir-graph-atoms
  ;; call graph
  hg-calls-link call-graph-atoms
  ;; type graph
  hg-type-slot-node hg-subtype-link hg-type-relation-link type-graph-atoms
  ;; effect graph
  hg-fn-effect-row-link effect-graph-atoms
  ;; capability graph (the Effervesce vocabulary, one shared shape)
  hg-component-node hg-capability-node hg-effect-node
  hg-state-node hg-action-node hg-resource-node
  hg-component-needs-link hg-component-provides-link
  hg-capability-interprets-link hg-capability-effect-link hg-given-link
  hg-action-writes-link hg-fn-effects-link hg-effect-touches-link
  capability-graph-atoms
  ;; module graph
  hg-module-node hg-module-imports-link hg-module-defines-link module-graph-atoms
  ;; the single instance holding all six
  build-hypergraph)

;; ── shared vertex vocabulary ────────────────────────────────────────

;; A named function/method. The same atom is the vertex of the call graph, the
;; effect graph (its row), and the module graph (what a module defines).
(define (hg-function-node name) (node 'function name))

;; ── UIR graph ───────────────────────────────────────────────────────

;; The generic materialized UIR projection: struct vertices typed by their tag,
;; `slot` edges (vertex, field index, child). No UIR-specific require is needed
;; because materialize reads transparent struct trees generically.
(define (uir-graph-atoms program)
  (materialize program))

;; ── call graph ──────────────────────────────────────────────────────

;; Caller -> callee over function nodes.
(define (hg-calls-link caller callee)
  (link 'calls (list (hg-function-node caller) (hg-function-node callee))))

;; `edges` is a list of (caller callee) name pairs. Nodes are emitted with the
;; edges so the projector's output is self-contained.
(define (call-graph-atoms edges)
  (append-map
   (λ (e)
     (define caller (first e))
     (define callee (second e))
     (list (hg-function-node caller)
           (hg-function-node callee)
           (hg-calls-link caller callee)))
   edges))

;; ── type graph ──────────────────────────────────────────────────────

;; A slot of the type relation graph: a local, parameter, self-attribute, or
;; return position, named by its dotted key (for example `fetch.result`).
(define (hg-type-slot-node key) (node 'type-slot key))

;; Named types as vertices (the same `type` nodes the type instance uses), and
;; an explicit subtyping edge `sub <= super`.
(define (hg-subtype-link sub super)
  (link 'subtype (list (t-base sub) (t-base super))))

;; A typed relation edge mirroring racklr/effects/type-relations.rkt:
;; assign / self-attr / param / return map a key slot to a type; subtype maps a
;; type to a type. Unknown relation names are rejected rather than stored as
;; untyped edges.
(define (hg-type-relation-link rel subject target)
  (case rel
    [(subtype) (hg-subtype-link subject target)]
    [(assign self-attr param return)
     (link rel (list (hg-type-slot-node subject) (t-base target)))]
    [else (error 'hg-type-relation-link "unknown type relation: ~a" rel)]))

;; `relations` is a list of (relation subject target) triples. Nodes are
;; emitted with the edges so the projection is self-contained.
(define (type-graph-atoms relations)
  (append-map
   (λ (r)
     (define rel (first r))
     (define subject (second r))
     (define target (third r))
     (case rel
       [(subtype) (list (t-base subject) (t-base target)
                        (hg-subtype-link subject target))]
       [else (list (hg-type-slot-node subject) (t-base target)
                   (hg-type-relation-link rel subject target))]))
   relations))

;; ── effect graph ────────────────────────────────────────────────────

;; A function's inferred row as one hyperedge: (function, effect-entry ...).
;; A pure function is a one-slot row (the function node alone); its row is the
;; empty set of entries. Variadic by design: a row can hold any number of
;; entries, one per effect name.
(define (hg-fn-effect-row-link fn-name entries)
  (link 'fn-effect-row (cons (hg-function-node fn-name) entries)))

;; `rows` is a list of (function-name entries) pairs, where entries are the
;; 5-slot effect-entry hyperedges of the function's inferred row.
(define (effect-graph-atoms rows)
  (append-map
   (λ (r)
     (define fn-name (first r))
     (define entries (second r))
     (append (list (hg-function-node fn-name))
             entries
             (list (hg-fn-effect-row-link fn-name entries))))
   rows))

;; ── capability graph ────────────────────────────────────────────────
;;
;; The Effervesce needs/provides/interprets vocabulary. Capability identity is
;; a ref-key (name . type-args), the same identity the type checker uses, so
;; substrate queries and checker verdicts agree. These constructors match
;; effervesce/check/graph.rkt atom for atom; the substrate is where that graph
;; lives now.

(define (hg-component-node name) (node 'component name))
(define (hg-capability-node key) (node 'capability key))
(define (hg-effect-node name) (node 'effect name))
(define (hg-state-node name) (node 'state name))
(define (hg-action-node name) (node 'action name))
(define (hg-resource-node scope) (node 'resource scope))

(define (hg-component-needs-link cname ref)
  (link 'component-needs (list (hg-component-node cname) (hg-capability-node ref))))

(define (hg-component-provides-link cname ref)
  (link 'component-provides (list (hg-component-node cname) (hg-capability-node ref))))

(define (hg-capability-interprets-link effect ref)
  (link 'capability-interprets (list (hg-capability-node effect) (hg-capability-node ref))))

(define (hg-capability-effect-link cap-name eff-name)
  (link 'capability-effect (list (node 'capability (cons cap-name '()))
                                 (hg-effect-node eff-name))))

(define (hg-given-link ref)
  (link 'given (list (hg-capability-node ref))))

(define (hg-action-writes-link aname sname)
  (link 'action-writes (list (hg-action-node aname) (hg-state-node sname))))

(define (hg-fn-effects-link cname eff-name)
  (link 'fn-effects (list (hg-component-node cname) (hg-effect-node eff-name))))

(define (hg-effect-touches-link eff-name scope)
  (link 'effect-touches (list (hg-effect-node eff-name) (hg-resource-node scope))))

;; One declaration of the capability graph, mirroring the uir-eff-system
;; declarations effervesce/check/graph.rkt projects:
;;   (component name needs provides)   needs/provides are ref-keys
;;   (given refs ...)
;;   (handler effect-ref row-refs ...)
;;   (state name)
;;   (action name written-states ...)
(define (capability-decl-atoms d)
  (case (first d)
    [(component)
     (define name (second d))
     (define needs (third d))
     (define provides (fourth d))
     (append (list (hg-component-node name))
             (map (λ (r) (hg-component-needs-link name r)) needs)
             (map (λ (r) (hg-component-provides-link name r)) provides)
             (map hg-capability-node needs)
             (map hg-capability-node provides))]
    [(given) (map hg-given-link (rest d))]
    [(handler)
     (define e (second d))
     (define row (third d))
     (append (list (hg-capability-node e))
             (map hg-capability-node row)
             (map (λ (r) (hg-capability-interprets-link e r)) row))]
    [(state) (list (hg-state-node (second d)))]
    [(action)
     (define aname (second d))
     (define written (rest (rest d)))
     (cons (hg-action-node aname)
           (map (λ (s) (hg-action-writes-link aname s)) written))]
    [else (error 'capability-graph-atoms
                 "unknown capability declaration: ~a" d)]))

(define (capability-graph-atoms decls)
  (append-map capability-decl-atoms decls))

;; ── module graph ────────────────────────────────────────────────────

(define (hg-module-node name) (node 'module name))

(define (hg-module-imports-link from to)
  (link 'module-imports (list (hg-module-node from) (hg-module-node to))))

(define (hg-module-defines-link module fn-name)
  (link 'module-defines (list (hg-module-node module) (hg-function-node fn-name))))

;; `deps` is a list of (module import ...) entries. Nodes are emitted with the
;; edges; module-defines edges are added by the engine that knows definitions.
(define (module-graph-atoms deps)
  (append-map
   (λ (d)
     (define module (first d))
     (define imports (rest d))
     (append (list (hg-module-node module))
             (map hg-module-node imports)
             (map (λ (i) (hg-module-imports-link module i)) imports)))
   deps))

;; ── the single instance ─────────────────────────────────────────────

;; Build ONE hypergraph instance holding all six graphs. Each keyword takes the
;; data shape its projector expects; #:schema injects a different typed-edge
;; vocabulary when a caller adds a graph kind.
(define (build-hypergraph #:uir [program #f]
                          #:calls [calls '()]
                          #:types [relations '()]
                          #:effects [effect-rows '()]
                          #:capabilities [capability-decls '()]
                          #:modules [module-deps '()]
                          #:schema [schema default-edge-schema])
  (hg-add* (hg-empty schema)
           (append
            (if program (uir-graph-atoms program) '())
            (call-graph-atoms calls)
            (type-graph-atoms relations)
            (effect-graph-atoms effect-rows)
            (capability-graph-atoms capability-decls)
            (module-graph-atoms module-deps))))

(module+ main
  (displayln "racklr/hypergraph/project — six-graph projections loaded."))
