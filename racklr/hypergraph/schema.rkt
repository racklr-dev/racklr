#lang racket

;; racklr/hypergraph/schema.rkt
;;
;; The typed-edge schema of the merged substrate. One hypergraph instance can
;; hold the six graphs the system reasons over — UIR, call, type, effect,
;; capability, and module — because every edge type in those graphs declares
;; which graph it belongs to (`edge-spec-kind`) and what its slots hold
;; (`edge-spec-roles`). A new question about the system is then a query over
;; typed edges, and a new edge kind is an entry in this table, not a new
;; ad-hoc accessor.
;;
;; `edge-spec-arity` is the exact out-list length, or #f for a variadic
;; hyperedge (`ref` has a variable number of type arguments; `fn-effect-row`
;; carries one function plus any number of effect entries). `edge-spec-roles`
;; is a prefix of expected node types; #f means "any atom". Prefix matching is
;; what makes a variadic link still typed at its head slots.

(require "kernel.rkt")

(provide
  graph-kinds
  edge-spec edge-spec? edge-spec-kind edge-spec-arity edge-spec-roles
  default-edge-schema
  edge-kind edge-spec-of edge-types-for-kind
  node-kinds)

;; The six graphs of the single substrate. ROADMAP Phase 4 names them: UIR,
;; call graph, type graph, effect graph, capability graph, module graph.
(define graph-kinds '(uir call type effect capability module))

;; A typed edge declaration: which graph `kind` an edge type belongs to, its
;; `arity` (or #f), and the node types of its leading slots (or #f for any).
(struct edge-spec (kind arity roles) #:transparent)

;; The default vocabulary. Every edge type a projector in project.rkt (or the
;; existing Effervesce/hypergraph vocabulary) emits is declared here with its
;; graph kind, so `edge-kind` can classify it and `hg-ill-typed-edges` can
;; reject a mistyped edge instead of storing it silently.
(define default-edge-schema
  (hash
   ;; ── UIR graph: materialized struct trees ──
   'slot               (edge-spec 'uir 3 (list #f 'index #f))
   ;; ── call graph ──
   'calls              (edge-spec 'call 2 (list 'function 'function))
   ;; ── type graph: constructors (variadic `ref`) + typed relations ──
   'subtype            (edge-spec 'type 2 (list 'type 'type))
   'assign             (edge-spec 'type 2 (list 'type-slot 'type))
   'self-attr          (edge-spec 'type 2 (list 'type-slot 'type))
   'param              (edge-spec 'type 2 (list 'type-slot 'type))
   'return             (edge-spec 'type 2 (list 'type-slot 'type))
   'fn                 (edge-spec 'type 3 (list #f #f #f))
   'listof             (edge-spec 'type 1 (list #f))
   'ref                (edge-spec 'type #f (list #f))
   ;; ── effect graph ──
   'effect             (edge-spec 'effect 5 (list 'name 'modality 'ops 'budget 'scope))
   'fn-effect-row      (edge-spec 'effect #f (list 'function))
   'fn-effects         (edge-spec 'effect 2 (list 'component 'effect))
   'effect-touches     (edge-spec 'effect 2 (list 'effect 'resource))
   ;; ── capability graph ──
   'effect-op          (edge-spec 'capability 3 (list 'domain 'type 'op))
   'inherits           (edge-spec 'capability 2 (list 'type 'type))
   'given              (edge-spec 'capability 1 (list 'capability))
   'component-needs    (edge-spec 'capability 2 (list 'component 'capability))
   'component-provides (edge-spec 'capability 2 (list 'component 'capability))
   'capability-interprets (edge-spec 'capability 2 (list 'capability 'capability))
   'capability-effect  (edge-spec 'capability 2 (list 'capability 'effect))
   'action-writes      (edge-spec 'capability 2 (list 'action 'state))
   ;; ── module graph ──
   'module-imports     (edge-spec 'module 2 (list 'module 'module))
   'module-defines     (edge-spec 'module 2 (list 'module 'function))))

;; Graph kind of an edge type, or #f when the type is not in the schema.
(define (edge-kind type [schema default-edge-schema])
  (define s (hash-ref schema type #f))
  (and s (edge-spec-kind s)))

;; The schema entry for an edge type, or #f.
(define (edge-spec-of type [schema default-edge-schema])
  (hash-ref schema type #f))

;; All edge types declared for one graph kind, sorted for deterministic output.
(define (edge-types-for-kind kind [schema default-edge-schema])
  (sort (for/list ([(t s) (in-hash schema)] #:when (eq? (edge-spec-kind s) kind)) t)
        symbol<?))

;; Which graph(s) a node type participates in. UIR materialization names nodes
;; by their struct tag, so anything not declared here is a UIR node. A node can
;; belong to more than one graph: a `function` node is the shared identity of
;; the call, effect, and module graphs, which is what makes them one substrate
;; instead of three parallel lists.
(define node-kind-table
  (hash
   'component '(capability)
   'capability '(capability)
   'state     '(capability)
   'action    '(capability)
   'resource  '(capability)
   'domain    '(capability)
   'op        '(capability)
   'function  '(call effect module)
   'type      '(type capability)
   'type-slot '(type)
   'module    '(module)
   'name      '(effect)
   'modality  '(effect)
   'ops       '(effect)
   'budget    '(effect)
   'scope     '(effect)
   'effect    '(effect capability)
   'index     '(uir)))

;; Graph kinds a node type participates in (`'(uir)` for an undeclared struct
;; tag, since the UIR graph is the generic materialized tree).
(define (node-kinds type)
  (hash-ref node-kind-table type '(uir)))

(module+ main
  (displayln "racklr/hypergraph/schema — typed six-graph edge schema loaded."))
