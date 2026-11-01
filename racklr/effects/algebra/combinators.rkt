#lang racket

;; racklr/effects/algebra/combinators.rkt
;;
;; The effect-algebra combinators over provenance-carrying rows
;; (`racklr/effects/algebra/row`):
;;
;;   join      the fixpoint operation: least upper bound over rows.
;;   sequence  compose two rows in order (both computations run).
;;   choose    merge two mutually exclusive branches.
;;   scope     bound a handled effect region (discharge the handled effects).
;;
;; Semantics follow docs/effects-proposal.md §3.2–§3.3 and ROADMAP Phase 1:
;;   - absence of an entry is prohibition (must-not) by default;
;;   - `may` is permission, `must` an obligation, `unknown` a tracked deferral;
;;   - budgets are invocation upper bounds; empty ops mean "all operations";
;;   - join is the least upper bound in the contract order
;;     (`effects/algebra/subtyping`), which is what makes it the fixpoint
;;     operator: idempotent, commutative, and associative.
;;
;; The combinators differ in how they compose budgets and modalities:
;;   join      budgets take the max (an upper bound); the fixpoint's merge.
;;   sequence  budgets sum (both sides execute); obligations accumulate.
;;   choose    budgets take the max (only one branch executes); `must` survives
;;             only when EVERY branch must.
;;
;; The merge is a bounded lattice per effect name, so a row with the same
;; entries and evidence joins to itself: `join` is idempotent. This is why the
;; algebra's join does not use the contract's budget-summing union for its
;; entries — `contract.rkt`'s `effect-row-union` remains the capability
;; combination operator for declared rows.

(require racklr/uir
         (only-in racklr/effects/contract effect-name-symbol)
         "row.rkt")

(provide
  join
  sequence
  choose
  scope
  row-leq?
  row-equiv?)

;; entry helpers -------------------------------------------------------

(define (entry-name e)
  (effect-name-symbol e))

(define (entry-modality e)
  (uir-effect-entry-modality e))

(define (entry-budget e)
  (uir-effect-entry-budget e))

(define (entry-scope e)
  (uir-effect-entry-scope e))

;; Ops as plain symbols. Empty list means "all operations".
(define (entry-ops e)
  (map (λ (o) (string->symbol (uir-symbol-name o)))
       (uir-effect-entry-ops e)))

(define (ops->uir syms)
  (map (λ (s) (uir-symbol (symbol->string s))) syms))

(define (ops-union a b)
  ;; Empty ops mean "all operations" and dominate (never narrowed). Order is
  ;; the accumulation order: operation order is presentation, and the algebra's
  ;; `same-row?` compares it as a set, so join/choose still commute. Keeping
  ;; accumulation order keeps materialized UIR rows identical to the old
  ;; fixpoint's output (the type-graph baseline does not move).
  (cond [(or (null? a) (null? b)) '()]
        [else (remove-duplicates (append a b))]))

(define (budget-lub a b)
  ;; #f is unbounded (top); otherwise the upper bound covers both sides.
  (cond [(or (not a) (not b)) #f]
        [else (max a b)]))

(define (budget-sum a b)
  ;; A sequential composition executes both sides; #f (unbounded) dominates.
  (cond [(or (not a) (not b)) #f]
        [else (+ a b)]))

;; Scope join for every combinator: #f means unconstrained (top); otherwise
;; only an identical qualifier is a common upper bound. Using the same
;; semilattice for join/sequence/choose keeps scope composition associative
;; (a conflict of two different constraints is "unconstrained", never an
;; identity that a later merge could absorb differently on either side).
(define (scope-lub a b)
  (cond [(or (not a) (not b)) #f]
        [(string=? (uir-symbol-name a) (uir-symbol-name b)) a]
        [else #f]))

;; Modality lattice for `join`: must-not ⊑ may ⊑ unknown ⊑ must. `unknown` is
;; the deferral marker; it survives a join with `may` (the obligation to
;; eliminate it is not silently absorbed) and is covered by `must`.
(define (modality-rank m)
  (case m
    [(must-not) 0]
    [(may) 1]
    [(unknown) 2]
    [(must) 3]
    [else (error 'modality-rank "unknown modality: ~a" m)]))

(define (modality-at-rank r)
  (case r
    [(0) 'must-not]
    [(1) 'may]
    [(2) 'unknown]
    [(3) 'must]
    [else (error 'modality-at-rank "no modality at rank ~a" r)]))

(define (modality-join a b)
  (modality-at-rank (max (modality-rank a) (modality-rank b))))

;; Modality lattice for `sequence`: both computations run. A prohibition on
;; either side is a prohibition overall; an obligation or deferral on either
;; side survives.
(define (modality-sequence a b)
  (cond [(or (eq? a 'must-not) (eq? b 'must-not)) 'must-not]
        [(or (eq? a 'must) (eq? b 'must)) 'must]
        [(or (eq? a 'unknown) (eq? b 'unknown)) 'unknown]
        [else 'may]))

;; Modality merge for `choose`: only one branch executes, so `must` requires
;; every branch to must, while a prohibition requires every branch to prohibit.
;; An absent entry on one side is a prohibited branch (`must-not`).
(define (modality-choose a b)
  (cond [(and (eq? a 'must) (eq? b 'must)) 'must]
        [(and (eq? a 'must-not) (eq? b 'must-not)) 'must-not]
        [(or (eq? a 'unknown) (eq? b 'unknown)) 'unknown]
        [else 'may]))

;; entry-level merges --------------------------------------------------

(define (make-entry name modality ops budget scope)
  (uir-effect-entry (uir-symbol (symbol->string name))
                    modality
                    (ops->uir ops)
                    budget
                    scope))

(define (entry-merge-join a b)
  (make-entry (entry-name a)
              (modality-join (entry-modality a) (entry-modality b))
              (ops-union (entry-ops a) (entry-ops b))
              (budget-lub (entry-budget a) (entry-budget b))
              (scope-lub (entry-scope a) (entry-scope b))))

(define (entry-merge-sequence a b)
  (make-entry (entry-name a)
              (modality-sequence (entry-modality a) (entry-modality b))
              (ops-union (entry-ops a) (entry-ops b))
              (budget-sum (entry-budget a) (entry-budget b))
              (scope-lub (entry-scope a) (entry-scope b))))

(define (entry-merge-choose a b)
  (make-entry (entry-name a)
              (modality-choose (entry-modality a) (entry-modality b))
              (ops-union (entry-ops a) (entry-ops b))
              (budget-lub (entry-budget a) (entry-budget b))
              (scope-lub (entry-scope a) (entry-scope b))))

;; row-level merge -----------------------------------------------------

(define (merge-provenance a b names)
  (for/fold ([table (hash)]) ([n (in-list names)])
    (hash-set table n
              (append (row-provenance a n) (row-provenance b n)))))

;; Combine two rows with a name-wise entry merge. Evidence from both sides is
;; kept: a row is a set of (effect, evidence) pairs.
(define (merge-rows a b merge-entry)
  (define names
    (remove-duplicates
     (append (row-effect-names a) (row-effect-names b))))
  (define entries
    (for/list ([n (in-list names)])
      (define ea (row-lookup a n))
      (define eb (row-lookup b n))
      (cond [(and ea eb) (merge-entry ea eb)]
            [ea ea]
            [else eb])))
  (make-algebra-row (uir-effect-row entries) (merge-provenance a b names)))

;; The fixpoint operation: least upper bound over rows. Idempotent,
;; commutative, associative; the empty row is its identity.
(define (join a b)
  (merge-rows a b entry-merge-join))

;; Sequential composition: both computations run, so budgets sum.
(define (sequence a b)
  (merge-rows a b entry-merge-sequence))

;; Branch merge: only one branch executes, so budgets take the max and a
;; one-sided entry cannot be `must` (the other branch does not perform it).
(define (choose a b)
  (define names
    (remove-duplicates
     (append (row-effect-names a) (row-effect-names b))))
  (define (demote entry)
    (struct-copy uir-effect-entry entry
                 [modality (modality-choose (entry-modality entry) 'must-not)]))
  (define entries
    (for/list ([n (in-list names)])
      (define ea (row-lookup a n))
      (define eb (row-lookup b n))
      (cond [(and ea eb) (entry-merge-choose ea eb)]
            [ea (demote ea)]
            [else (demote eb)])))
  (make-algebra-row (uir-effect-row entries) (merge-provenance a b names)))

;; Discharge the effects a handler covers: return the row the handler body
;; still performs, and the row of effects the handler interprets (with their
;; evidence, so a discharged effect can still be traced).
(define (scope r handled-names)
  (define (handled? name) (and (memq name handled-names) #t))
  (define remaining (filter (λ (n) (not (handled? n))) (row-effect-names r)))
  (define discharged (filter handled? (row-effect-names r)))
  (define (subrow names)
    (make-algebra-row
     (uir-effect-row
      (filter (λ (e) (memq (entry-name e) names))
              (uir-effect-row-entries (algebra-row->uir r))))
     (for/fold ([table (hash)]) ([n (in-list names)])
       (hash-set table n (row-provenance r n)))))
  (values (subrow remaining) (subrow discharged)))

;; The algebra's row order: `a` is at most `b` when joining them adds nothing
;; to `b`. This is the partial order join is the least upper bound of.
(define (row-leq? a b)
  (same-row? (join a b) b))

(define (row-equiv? a b)
  (and (row-leq? a b) (row-leq? b a)))
