#lang racket

;; racklr/hypergraph/instance.rkt
;;
;; The single graph instance. One `hypergraph` value holds the atoms of all six
;; graphs (UIR, call, type, effect, capability, module) and the typed-edge
;; schema that classifies them, so the six graphs are one queryable substrate
;; rather than six parallel structures. Queries are typed by graph kind
;; (hg-links), by edge type (hg-edges), or by node type (hg-nodes); edges are
;; directed with slot 0 as the source and the remaining slots as targets
;; (hg-out / hg-in / hg-incident).
;;
;; Atoms are kept in insertion order and the accessor returns the flat
;; AtomSpace closure (deduplicated by `equal?`), so a node nested inside a
;; hyperedge is still a first-class queryable atom. The graph is immutable;
;; hg-add returns a new instance, which keeps the schema and every query pure.

(require "kernel.rkt"
         "schema.rkt")

(provide
  hypergraph? hypergraph-atoms hypergraph-schema
  hg-empty hg-add hg-add*
  hg-atoms hg-links hg-edges hg-nodes hg-node
  hg-out hg-in hg-incident hg-kind hg-kinds
  hg-ill-typed-edges)

;; A single graph instance: the added atoms plus the typed-edge schema that
;; classifies them.
(struct hypergraph (atoms schema) #:transparent)

;; An empty instance with the default six-graph vocabulary, or with an
;; injected schema (a hash of edge type -> edge-spec) for a new graph kind.
(define (hg-empty [schema default-edge-schema])
  (hypergraph '() schema))

;; Add atoms to an instance, returning the extended instance.
(define (hg-add g . atoms) (hg-add* g atoms))

(define (hg-add* g atoms)
  (hypergraph (append (hypergraph-atoms g) atoms) (hypergraph-schema g)))

;; The full flat atom set: every added atom plus every node/link reachable
;; from a link, deduplicated in first-seen order (AtomSpace set semantics).
(define (hg-atoms g)
  (remove-duplicates (flatten-atoms (hypergraph-atoms g))))

;; ── typed queries ───────────────────────────────────────────────────

;; Every link whose edge type belongs to graph `kind`; all links when #f.
;; Use hg-edges for an exact edge type — a link type may itself be a symbol
;; that also names a graph kind (`effect`), so the two filters are separate.
(define (hg-links g [kind #f])
  (define schema (hypergraph-schema g))
  (for/list ([l (in-list (hg-edges g))] #:when (and l (or (not kind) (eq? (edge-kind (link-type l) schema) kind))))
    l))

;; Every link of exactly `type`; all links when #f.
(define (hg-edges g [type #f])
  (for/list ([a (in-list (hg-atoms g))]
             #:when (and (link? a) (or (not type) (eq? (link-type a) type))))
    a))

;; Every node, optionally filtered to a node type.
(define (hg-nodes g [type #f])
  (for/list ([a (in-list (hg-atoms g))]
             #:when (and (node? a) (or (not type) (eq? (node-type a) type))))
    a))

;; The first node with `type` and `name`, or #f.
(define (hg-node g type name)
  (for/first ([a (in-list (hg-atoms g))]
              #:when (and (node? a) (eq? (node-type a) type) (equal? (node-name a) name)))
    a))

;; Links that a node is the SOURCE of (slot 0), optionally of one edge type.
(define (hg-out g atom [type #f])
  (for/list ([l (in-list (hg-edges g type))]
             #:when (and (pair? (link-out l)) (equal? (first (link-out l)) atom)))
    l))

;; Links that a node is a TARGET of (any slot after the source), optionally of
;; one edge type.
(define (hg-in g atom [type #f])
  (for/list ([l (in-list (hg-edges g type))]
             #:when (member atom (rest (link-out l))))
    l))

;; Links that mention a node in any slot, optionally of one edge type.
(define (hg-incident g atom [type #f])
  (for/list ([l (in-list (hg-edges g type))]
             #:when (member atom (link-out l)))
    l))

;; Graph kind of one link, or #f when its edge type is not in the schema.
(define (hg-kind g l)
  (and (link? l) (edge-kind (link-type l) (hypergraph-schema g))))

;; The graph kinds present among this instance's typed edges, sorted. One
;; instance holding all six graphs means this returns all of `graph-kinds`.
(define (hg-kinds g)
  (sort (remove-duplicates
         (filter values (for/list ([l (in-list (hg-edges g))]) (hg-kind g l))))
        symbol<?))

;; ── typed-edge validation ───────────────────────────────────────────

;; Why an edge violates its schema, or #f when it is well-typed.
(define (edge-problem spec l)
  (define out (link-out l))
  (define arity (edge-spec-arity spec))
  (or
   (and arity
        (not (= arity (length out)))
        (format "arity: expected ~a slot(s), got ~a" arity (length out)))
   (for/or ([role (in-list (edge-spec-roles spec))]
            [a (in-list out)]
            #:when (symbol? role))
     (and (not (and (node? a) (eq? (node-type a) role)))
          (format "slot expected node type ~a, got ~a"
                  role (if (node? a) (node-type a) a))))))

;; Every edge that its schema rejects, as (list reason link). Empty means the
;; whole instance is typed edge by edge.
(define (hg-ill-typed-edges g)
  (define schema (hypergraph-schema g))
  (for/list ([l (in-list (hg-edges g))]
             #:do [(define spec (edge-spec-of (link-type l) schema))
                   (define reason (if spec
                                      (edge-problem spec l)
                                      (format "unknown edge type ~a" (link-type l))))]
             #:when reason)
    (list reason l)))

(module+ main
  (displayln "racklr/hypergraph/instance — single graph instance loaded."))
