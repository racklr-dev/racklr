#lang racket

;; racklr/hypergraph/effect.rkt
;;
;; The effect-row instance of the typed-hypergraph kernel: an entry
;; (name, modality, ops, budget, scope) is a 5-slot hyperedge; a row is a set
;; of hyperedges; union/intersection/difference/subtyping are the
;; join/meet/difference/≤ of those sets under a per-slot lattice.
;;
;; This is one instance of the generic per-slot algebra (kernel.rkt), kept in
;; its own module so the algebra stays reusable and every file stays small.
;; Type subtyping embeds a row through the function type in types.rkt.

(require "kernel.rkt")

(provide
  ;; effect-row instance
  effect-entry effect-entry? effect-key effect-key?
  effect-entry-join effect-entry-meet effect-entry-leq
  effect-row effect-row-join effect-row-meet effect-row-diff effect-row-leq)

;; ── per-slot lattices ───────────────────────────────────────────────

;; Modality: 'may | 'must | 'must-not | 'unknown.
(define (modality-join a b)
  (cond [(or (eq? a 'must-not) (eq? b 'must-not)) 'must-not]
        [(or (eq? a 'must) (eq? b 'must)) 'must]
        [else 'may]))
(define (modality-meet a b)
  (cond [(or (eq? a 'must-not) (eq? b 'must-not)) 'must-not]
        [(and (eq? a 'must) (eq? b 'must)) 'must]
        [else 'may]))
(define (modality-leq sub super)
  (case sub
    [(may unknown) (memq super '(may must unknown))]
    [(must) (eq? super 'must)]
    [(must-not) #t]
    [else #f]))

;; Budget: #f is unbounded (⊤ of the ≤ order); join sums (unbounded absorbs),
;; meet takes the min.
(define (budget-join a b)
  (cond [(or (not a) (not b)) #f]
        [else (+ a b)]))
(define (budget-meet a b)
  (cond [(and (not a) (not b)) #f]
        [(not a) b]
        [(not b) a]
        [else (min a b)]))
(define (budget-leq sub super)
  (or (not super) (and sub (<= sub super))))

;; Ops: a list of operation symbols; '() means "all operations" (⊤).
(define (ops-join a b)
  (cond [(or (null? a) (null? b)) '()]
        [else (remove-duplicates (append a b))]))
(define (ops-meet a b)
  (cond [(and (null? a) (null? b)) '()]
        [(null? a) b]
        [(null? b) a]
        [else (filter (λ (x) (memq x b)) a)]))
(define (ops-leq sub super)
  (or (null? super)
      (and (not (null? sub))
           (andmap (λ (s) (memq s super)) sub))))

;; Scope: #f is unconstrained (⊥); equal values merge, conflict → #f.
;; NOTE: contract.rkt never checks scope in subtyping; scope-leq is
;; always-true to preserve that (see the report for why this matters).
(define (scope-merge a b)
  (cond [(not a) b]
        [(not b) a]
        [(equal? a b) a]
        [else #f]))
(define (scope-leq sub super) #t)

(define modality-lattice (lattice modality-join modality-meet modality-leq))
(define budget-lattice   (lattice budget-join   budget-meet   budget-leq))
(define ops-lattice      (lattice ops-join      ops-meet      ops-leq))
(define scope-lattice    (lattice scope-merge   scope-merge   scope-leq))

;; ── effect-row instance ─────────────────────────────────────────────

;; Slot order of an effect hyperedge: [name, modality, ops, budget, scope].
;; name is the key; the four remaining slots each carry a lattice, in the
;; same order.
(define effect-lattices (list modality-lattice ops-lattice budget-lattice scope-lattice))

(define (effect-entry name modality ops budget scope)
  (link 'effect (list (node 'name name)
                      (node 'modality modality)
                      (node 'ops ops)
                      (node 'budget budget)
                      (node 'scope scope))))

(define (effect-entry? x)
  (and (link? x) (eq? (link-type x) 'effect)))

(define (effect-key l) (node-name (first (link-out l))))
(define (effect-key? x) (and (link? x) (eq? (link-type x) 'effect)))

(define (entry-modality l) (node-name (second (link-out l))))

(define (effect-entry-join a b)
  (cond [(and (effect-entry? a) (effect-entry? b)
              (equal? (effect-key a) (effect-key b)))
         (link 'effect (cons (first (link-out a))
                             (slots-join effect-lattices
                                         (rest (link-out a))
                                         (rest (link-out b)))))]
        [a a]
        [else b]))

(define (effect-entry-meet a b)
  (and (effect-entry? a) (effect-entry? b)
       (equal? (effect-key a) (effect-key b))
       (link 'effect (cons (first (link-out a))
                           (slots-meet effect-lattices
                                       (rest (link-out a))
                                       (rest (link-out b)))))))

;; Entry ≤ entry. `b` may be #f (absent in the super row). Matches
;; contract.rkt's effect-subtype? exactly, including its two special cases:
;; a `must-not` entry constrains nothing, and a `must` entry is checked only
;; for obligation-compatibility (budget/ops are not compared).
(define (effect-entry-leq a b)
  (case (entry-modality a)
    [(must-not) #t]
    [(must) (and b (eq? (entry-modality b) 'must))]
    [else (and b (slots-leq effect-lattices (rest (link-out a)) (rest (link-out b))))]))

;; A row is a set of effect hyperedges keyed by name (plain list; order is
;; irrelevant to every operation below).
(define (effect-row . entries) entries)

(define (effect-row-lookup row name)
  (for/or ([e (in-list row)])
    (and (equal? (effect-key e) name) e)))

(define (effect-row-join r1 r2)
  (define names (remove-duplicates (append (map effect-key r1) (map effect-key r2))))
  (for/list ([n (in-list names)])
    (define a (effect-row-lookup r1 n))
    (define b (effect-row-lookup r2 n))
    (cond [(and a b) (effect-entry-join a b)]
          [a a]
          [else b])))

(define (effect-row-meet r1 r2)
  (define names (remove-duplicates (map effect-key r1)))
  (for/list ([n (in-list names)]
             #:when (and (effect-row-lookup r1 n) (effect-row-lookup r2 n)))
    (effect-entry-meet (effect-row-lookup r1 n) (effect-row-lookup r2 n))))

(define (effect-row-diff r1 r2)
  (define names2 (map effect-key r2))
  (filter (λ (e) (not (memq (effect-key e) names2))) r1))

(define (effect-row-leq L R)
  (andmap (λ (e) (effect-entry-leq e (effect-row-lookup R (effect-key e)))) L))

(module+ main
  (displayln "racklr/hypergraph/effect — effect-row hypergraph instance loaded."))
