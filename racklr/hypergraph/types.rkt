#lang racket

;; racklr/hypergraph/types.rkt
;;
;; The type instance of the typed-hypergraph kernel, plus the Effervesce-style
;; capability instance that sits on the same relation:
;;
;;   - A base type is a node; a type constructor is a hyperedge (fn, listof,
;;     ref). Subtyping is a recursive relation over those hyperedges with
;;     variance, and an effect row (effect.rkt) is embedded in a function type
;;     so effect subtyping composes with type subtyping.
;;   - A capability reference is a `ref` hyperedge; every need is satisfiable
;;     by some provide with the same name and covariant type arguments.
;;   - A typed operation capability (`db<QuerySet>.read`) keeps the receiver as
;;     its own node, so two reads on different receiver classes stay distinct
;;     atoms and can be related by MRO inheritance.
;;
;; The generic capability-graph edges (component-needs, capability-interprets,
;; ...) are in schema/project because they are graph vocabulary, not type
;; algebra.

(require "kernel.rkt"
         "effect.rkt")

(provide
  ;; type instance
  t-base t-fn t-listof t-ref
  type-leq
  ;; capability instance (Effervesce-style needs/provides)
  cap-ref cap-satisfied?
  ;; OpenCog-compatible effect atoms
  effect-op inherits)

;; ── type instance ───────────────────────────────────────────────────

;; Base type node.
(define (t-base name) (node 'type name))

;; Function type: (fn arg ret effects), effects = list of effect hyperedges.
(define (t-fn arg ret effects) (link 'fn (list arg ret effects)))

;; List type: (listof elem).
(define (t-listof elem) (link 'listof (list elem)))

;; Capability/parameterized-type reference: (ref Base args...).
(define (t-ref base . args) (link 'ref (cons (t-base base) args)))

;; Subtyping over typed hyperedges, with variance. Covariant: a ≤ b means
;; "a usable where b is expected".
(define (type-leq a b)
  (cond
    [(and (node? a) (node? b) (eq? (node-type a) 'type) (eq? (node-type b) 'type))
     (equal? (node-name a) (node-name b))]
    [(and (link? a) (link? b) (eq? (link-type a) (link-type b)))
     (case (link-type a)
       [(listof) (type-leq (first (link-out a)) (first (link-out b)))]
       [(ref) (and (type-leq (first (link-out a)) (first (link-out b)))
                   (andmap type-leq (rest (link-out a)) (rest (link-out b))))]
       [(fn) (define ao (link-out a)) (define bo (link-out b))
        (and (type-leq (first bo) (first ao))      ; contravariant arg
             (type-leq (second ao) (second bo))    ; covariant ret
             (effect-row-leq (third ao) (third bo)))] ; covariant effect row
       [else #f])]
    [else #f]))

;; ── capability instance (Effervesce-style needs/provides) ───────────

;; A capability reference: (ref Name args...). Same shape as t-ref; kept as
;; a named alias so capability code reads as capabilities.
(define (cap-ref name . args) (apply t-ref name args))

;; Every need is satisfiable by some provide: same capability name and the
;; need's type arguments are subtypes of the provide's (covariant).
(define (cap-satisfied? needs provides)
  (andmap
   (λ (n)
     (and (link? n) (eq? (link-type n) 'ref)
          (for/or ([p (in-list provides)])
            (and (link? p) (eq? (link-type p) 'ref)
                 (eq? (node-name (first (link-out p)))
                      (node-name (first (link-out n))))
                 (type-leq n p)))))
   needs))

;; ── OpenCog-compatible effect atoms ─────────────────────────────────
;;
;; The projection of a typed capability `db<QuerySet>.read` into atom form.
;; A bare `db.read` conflates every receiver that shares the verb; the OpenCog
;; shape keeps the receiver as its own node and connects it to the operation
;; with a hyperedge, so two reads on different receiver classes stay distinct
;; atoms and can be related by MRO inheritance.

;; A typed operation capability: (effect-op domain receiver verb). The receiver
;; TYPE is a distinct node (not folded into the op name) so operations on
;; different classes remain distinct atoms. Serializes to an EvaluationLink:
;; (EvaluationLink (PredicateNode "db.read") (ConceptNode "QuerySet")).
(define (effect-op domain receiver verb)
  (link 'effect-op (list (node 'domain domain) (node 'type receiver) (node 'op verb))))

;; Subtyping / MRO edge: (inherits child parent). Capabilities flow from parent
;; to child (a subclass holds every capability its superclass holds).
;; Serializes to (InheritanceLink (ConceptNode "Child") (ConceptNode "Parent")).
(define (inherits child parent)
  (link 'inherits (list (node 'type child) (node 'type parent))))

(module+ main
  (displayln "racklr/hypergraph/types — type/capability hypergraph instance loaded."))
