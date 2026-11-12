#lang racket

;; Effect-row contract algebra.
;; Pure functions over uir-effect-row / uir-effect-entry nodes. No I/O.
;; The semantics follow docs/effects-proposal.md §3.2–§3.3 and §7:
;;   - absence of an entry is prohibition (must-not by default)
;;   - `may` is permission, `must` is obligation, `must-not` is prohibition
;;   - union sums budgets, intersection takes the minimum
;;   - subtyping (≤): every `may`/`must` in L is `may`/`must` in R, budgets in
;;     L ≤ budgets in R, ops in L ⊆ ops in R (empty = all)

(require racklr/uir)

(provide
  effect-name-symbol
  entry-ops
  row-lookup
  row-allows?
  row-prohibits?
  effect-entry-union
  effect-entry-intersect
  effect-row-union
  effect-row-intersect
  effect-row-difference
  effect-subtype?
  mutator-ok?)

;; The opaque effect name of an entry, as a plain Racket symbol.
(define (effect-name-symbol e)
  (string->symbol (uir-symbol-name (uir-effect-entry-name e))))

;; Operations as a list of Racket symbols. Empty list means "all operations".
(define (entry-ops e)
  (map (λ (o) (string->symbol (uir-symbol-name o)))
       (uir-effect-entry-ops e)))

;; Rebuild a uir-symbol ops list from Racket symbols.
(define (ops->uir syms)
  (map (λ (s) (uir-symbol (symbol->string s))) syms))

(define (row-lookup row name)
  (for/or ([e (uir-effect-row-entries row)])
    (and (eq? (effect-name-symbol e) name) e)))

;; Does `row` allow (permit or require) `name`?
(define (row-allows? row name)
  (define e (row-lookup row name))
  (and (and e (memq (uir-effect-entry-modality e) '(may must unknown))) #t))

;; Prohibition: absence or explicit must-not.
(define (row-prohibits? row name)
  (not (row-allows? row name)))

;; ---- entry-level algebra (same effect name) -------------------------

;; super covers sub? empty ops = "all".
(define (ops-superset? super sub)
  (and (or (null? super)
           (and (not (null? sub))
                (andmap (λ (s) (memq s super)) sub)))
       #t))

(define (ops-union a b)
  (cond [(or (null? a) (null? b)) '()]
        [else (remove-duplicates (append a b))]))

(define (ops-intersect a b)
  (cond [(and (null? a) (null? b)) '()]
        [(null? a) b]
        [(null? b) a]
        [else (filter (λ (x) (memq x b)) a)]))

(define (budget-sum a b)
  (cond [(or (not a) (not b)) #f]  ; unbounded dominates
        [else (+ a b)]))

(define (budget-min a b)
  (cond [(and (not a) (not b)) #f]
        [(not a) b]
        [(not b) a]
        [else (min a b)]))

(define (scope-merge a b)
  (cond [(not a) b]
        [(not b) a]
        [(string=? (uir-symbol-name a) (uir-symbol-name b)) a]
        [else #f]))

(define (effect-entry-union a b)
  (uir-effect-entry
   (uir-effect-entry-name a)
   (cond [(eq? (uir-effect-entry-modality a) 'must-not) 'must-not]
         [(eq? (uir-effect-entry-modality b) 'must-not) 'must-not]
         [(memq 'must (list (uir-effect-entry-modality a)
                            (uir-effect-entry-modality b))) 'must]
         [else 'may])
   (ops->uir (ops-union (entry-ops a) (entry-ops b)))
   (budget-sum (uir-effect-entry-budget a) (uir-effect-entry-budget b))
   (scope-merge (uir-effect-entry-scope a) (uir-effect-entry-scope b))))

(define (effect-entry-intersect a b)
  (uir-effect-entry
   (uir-effect-entry-name a)
   (cond [(or (eq? (uir-effect-entry-modality a) 'must-not)
              (eq? (uir-effect-entry-modality b) 'must-not)) 'must-not]
         [(and (eq? (uir-effect-entry-modality a) 'must)
               (eq? (uir-effect-entry-modality b) 'must)) 'must]
         [else 'may])
   (ops->uir (ops-intersect (entry-ops a) (entry-ops b)))
   (budget-min (uir-effect-entry-budget a) (uir-effect-entry-budget b))
   (scope-merge (uir-effect-entry-scope a) (uir-effect-entry-scope b))))

;; ---- row-level algebra ----------------------------------------------

(define (effect-row-union r1 r2)
  (define names (remove-duplicates
                 (append (map effect-name-symbol (uir-effect-row-entries r1))
                         (map effect-name-symbol (uir-effect-row-entries r2)))))
  (uir-effect-row
   (for/list ([n (in-list names)])
     (define a (row-lookup r1 n))
     (define b (row-lookup r2 n))
     (cond [(and a b) (effect-entry-union a b)]
           [a a]
           [else b]))))

(define (effect-row-intersect r1 r2)
  (define names (remove-duplicates
                 (map effect-name-symbol (uir-effect-row-entries r1))))
  (uir-effect-row
   (for/list ([n (in-list names)]
              #:when (and (row-lookup r1 n) (row-lookup r2 n)))
     (effect-entry-intersect (row-lookup r1 n) (row-lookup r2 n)))))

(define (effect-row-difference r1 r2)
  (define names2 (map effect-name-symbol (uir-effect-row-entries r2)))
  (uir-effect-row
   (filter (λ (e) (not (memq (effect-name-symbol e) names2)))
           (uir-effect-row-entries r1))))

;; ---- subtyping ------------------------------------------------------

(define (budget-≤ a b)
  (or (not b)          ; b unbounded
      (and a (<= a b))))

(define (effect-subtype? L R)
  (andmap
   (λ (e)
     (define n (effect-name-symbol e))
     (define r (row-lookup R n))
     (case (uir-effect-entry-modality e)
       [(may unknown)
        (and (and r
                  (memq (uir-effect-entry-modality r) '(may must unknown))
                  (budget-≤ (uir-effect-entry-budget e) (uir-effect-entry-budget r))
                  (ops-superset? (entry-ops r) (entry-ops e)))
             #t)]
       [(must)
        (and (and r
                  (eq? (uir-effect-entry-modality r) 'must)
                  (budget-≤ (uir-effect-entry-budget e) (uir-effect-entry-budget r))
                  (ops-superset? (entry-ops r) (entry-ops e)))
             #t)]
       [(must-not) #t]
       [else #f]))
   (uir-effect-row-entries L)))

;; A mutator's inferred/annotated row must be a subtype of its declared row.
(define (mutator-ok? inferred declared)
  (effect-subtype? inferred declared))

(module+ main
  (displayln "racklr/effects/contract — effect-row contract algebra loaded."))
