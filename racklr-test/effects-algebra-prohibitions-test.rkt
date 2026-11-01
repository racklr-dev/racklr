#lang racket

;; racklr-test/effects-algebra-prohibitions-test.rkt
;;
;; Prohibition and contract-order properties for the effect algebra.
;;
;; Algebra rows canonicalize `must-not` entries to absence (a prohibition is
;; `≤`-equivalent to an absent entry on the inferred side, docs §3.2). These
;; tests pin the two consequences that matter before Pass-4 routes the contract
;; check through the algebra:
;;
;;   1. `row-subtype?` of the stripped algebra rows still agrees with
;;      `effect-subtype?` of the declared UIR rows, for rows carrying `must-not`
;;      on either side of `≤` (randomized, deterministic seed);
;;   2. a declared prohibition is still readable — `declared-prohibited-effects`
;;      reads it from the UIR row, which the contract check must use, because
;;      the algebra row has dropped it.

(require rackunit
         racklr/uir
         (only-in racklr/effects/contract effect-subtype?)
         racklr/effects/algebra/main)

(random-seed 20261102)
(define trial-count 60)
(define effect-names '(db io network process cache llm time email))
(define modalities '(may must must-not unknown))
(define ops-pool '("read" "write" "query" "embed"))
(define scope-pool '(#f "class" "module" "domain"))

(define (pick lst) (list-ref lst (random (length lst))))
(define (subset lst) (filter (λ (x) (zero? (random 2))) lst))

(define (ent name modality #:ops [ops '()] #:budget [budget #f] #:scope [scope #f])
  (uir-effect-entry (uir-symbol (symbol->string name)) modality
                    (map uir-symbol ops) budget
                    (if scope (uir-symbol scope) #f)))

;; A declared row: any modality (including `must-not`) on any effect, with
;; operations, budgets and scopes so the differential covers the slot fields.
(define (random-declared-row)
  (uir-effect-row
   (for/list ([name (in-list (subset effect-names))])
     (ent name (pick modalities)
          #:ops (subset ops-pool)
          #:budget (if (zero? (random 3)) #f (random 4))
          #:scope (pick scope-pool)))))

;; ── agreement: algebra-row ≤ is the declared-row contract order ─────

(define (test-agreement)
  (for ([trial (in-range trial-count)])
    (define l (random-declared-row))
    (define r (random-declared-row))
    ;; `must-not` may appear on either side of both relations.
    (check-equal? (row-subtype? (algebra-row-of l) (algebra-row-of r))
                  (effect-subtype? l r)
                  (format "trial ~a: l=~s r=~s" trial l r))))

;; ── `must-not` on the left and on the right, explicitly ─────────────

(define (test-must-not-both-sides)
  ;; A prohibition on the left is bottom: always a subtype, whatever its budget
  ;; or operations say (subtyping ignores a must-not entry's slots).
  (define prohibited
    (uir-effect-row (list (ent 'db 'must-not #:budget 10 #:ops '("read")))))
  (define permitted
    (uir-effect-row (list (ent 'db 'may #:budget 3))))
  (check-true (effect-subtype? prohibited permitted))
  (check-true (row-subtype? (algebra-row-of prohibited) (algebra-row-of permitted)))
  ;; A prohibition on the right is not a permission: a permitted db is not ≤ it.
  (check-false (effect-subtype? permitted prohibited))
  (check-false (row-subtype? (algebra-row-of permitted) (algebra-row-of prohibited)))
  ;; Empty row against a prohibiting row: agreement both ways.
  (check-equal? (row-subtype? (algebra-row-of (uir-effect-row '()))
                              (algebra-row-of prohibited))
                (effect-subtype? (uir-effect-row '()) prohibited))
  (check-equal? (row-subtype? (algebra-row-of prohibited)
                              (algebra-row-of (uir-effect-row '())))
                (effect-subtype? prohibited (uir-effect-row '())))
  ;; The algebra row itself carries no prohibition: it canonicalizes to absence.
  (check-equal? (row-effect-names (algebra-row-of prohibited)) '())
  (check-true (same-row? (algebra-row-of prohibited) (empty-algebra-row))))

;; ── declared prohibitions are readable from the UIR row ─────────────

(define (test-declared-prohibited-effects)
  (define declared
    (uir-effect-row (list (ent 'db 'must-not)
                          (ent 'llm 'may)
                          (ent 'io 'must-not))))
  (check-equal? (declared-prohibited-effects declared) '(db io))
  (check-equal? (declared-prohibited-effects (uir-effect-row '())) '())
  ;; The algebra row drops them, so a contract check on declared rows must read
  ;; the prohibitions from the UIR row, not infer them from the algebra row.
  (check-equal? (row-effect-names (algebra-row-of declared)) '(llm))
  (check-exn exn:fail:contract?
             (λ () (declared-prohibited-effects (algebra-row-of declared)))))

(module+ main
  (test-agreement)
  (test-must-not-both-sides)
  (test-declared-prohibited-effects)
  (displayln "All algebra prohibition tests passed."))
