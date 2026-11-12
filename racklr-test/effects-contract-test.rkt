#lang racket

(require rackunit
         racklr/uir
         racklr/effects/contract)

(define (ent name modality #:ops [ops '()] #:budget [budget #f] #:scope [scope #f])
  (uir-effect-entry (uir-symbol name) modality
                    (map uir-symbol ops)
                    budget
                    (if scope (uir-symbol scope) #f)))

(define (row . entries)
  (uir-effect-row entries))

(define (test-name-symbol)
  (check-equal? (effect-name-symbol (ent "llm" 'may)) 'llm))

(define (test-union-budget-sums)
  (define u (effect-row-union (row (ent "llm" 'may #:budget 3))
                              (row (ent "llm" 'may #:budget 4))))
  (check-equal? (uir-effect-entry-budget (row-lookup u 'llm)) 7))

(define (test-union-prohibition-wins)
  (define u (effect-row-union (row (ent "db" 'may))
                              (row (ent "db" 'must-not))))
  (check-equal? (uir-effect-entry-modality (row-lookup u 'db)) 'must-not))

(define (test-union-ops)
  (define u (effect-row-union (row (ent "llm" 'may #:ops '("query")))
                              (row (ent "llm" 'may #:ops '("embed")))))
  (check-equal? (entry-ops (row-lookup u 'llm)) '(query embed)))

(define (test-intersect-shared-and-min)
  (define i (effect-row-intersect (row (ent "llm" 'may #:budget 3) (ent "db" 'may))
                                  (row (ent "llm" 'may #:budget 4) (ent "net" 'may))))
  (check-equal? (length (uir-effect-row-entries i)) 1)
  (check-equal? (uir-effect-entry-budget (row-lookup i 'llm)) 3))

(define (test-difference)
  (define d (effect-row-difference (row (ent "llm" 'may) (ent "db" 'may))
                                   (row (ent "db" 'may))))
  (check-equal? (length (uir-effect-row-entries d)) 1)
  (check-not-false (row-lookup d 'llm))
  (check-false (row-lookup d 'db)))

(define (test-subtype-may-within-may)
  (check-true (effect-subtype? (row (ent "llm" 'may #:budget 2))
                               (row (ent "llm" 'may #:budget 5)))))
(define (test-subtype-budget-exceeded)
  (check-false (effect-subtype? (row (ent "llm" 'may #:budget 6))
                                (row (ent "llm" 'may #:budget 5)))))
(define (test-subtype-prohibition)
  (check-false (effect-subtype? (row (ent "db" 'may))
                                (row (ent "db" 'must-not)))))
(define (test-subtype-absent-is-prohibition)
  (check-false (effect-subtype? (row (ent "db" 'may)) (row))))
(define (test-subtype-must)
  (check-true (effect-subtype? (row (ent "llm" 'must)) (row (ent "llm" 'must))))
  (check-false (effect-subtype? (row (ent "llm" 'must)) (row (ent "llm" 'may)))))
(define (test-subtype-must-budget-ops)
  ;; A `must` obligation now compares budget and ops exactly like `may`/
  ;; `unknown`: it is satisfied only by a declared `must` that covers its ops
  ;; and its budget, not merely by any same-modality entry.
  (check-true (effect-subtype? (row (ent "llm" 'must #:ops '("query") #:budget 2))
                               (row (ent "llm" 'must #:ops '("query" "embed") #:budget 5))))
  (check-false (effect-subtype? (row (ent "llm" 'must #:ops '("query") #:budget 6))
                                (row (ent "llm" 'must #:ops '("query" "embed") #:budget 5))))
  (check-false (effect-subtype? (row (ent "llm" 'must #:ops '("write")))
                                (row (ent "llm" 'must #:ops '("read")))))
  ;; The non-transitivity counterexample from proof/README.md: the old
  ;; modality-only `must` check accepted b <= c (budget 5 > 3) and so broke
  ;; transitivity (a <= b and b <= c, yet not a <= c). With budget and ops
  ;; compared, b <= c is rejected.
  (define a (row (ent "db" 'may #:ops '("read") #:budget 0)))
  (define b (row (ent "db" 'must #:ops '() #:budget 5)))
  (define c (row (ent "db" 'must #:ops '("write") #:budget 3)))
  (check-true (effect-subtype? a b))
  (check-false (effect-subtype? b c))
  (check-false (effect-subtype? a c))
  ;; The same chain holds once c covers b's ops and budget; transitivity is
  ;; then the general law the property test pins.
  (define c2 (row (ent "db" 'must #:ops '() #:budget 7)))
  (check-true (effect-subtype? b c2))
  (check-true (effect-subtype? a c2)))
(define (test-subtype-must-not-is-unconstrained)
  (check-true (effect-subtype? (row (ent "db" 'must-not)) (row (ent "db" 'may)))))
(define (test-subtype-unknown-defers)
  (check-true (effect-subtype? (row (ent "llm" 'unknown)) (row (ent "llm" 'may)))))
(define (test-subtype-ops-subset)
  (check-true (effect-subtype? (row (ent "llm" 'may #:ops '("query")))
                               (row (ent "llm" 'may #:ops '("query" "embed")))))
  (check-false (effect-subtype? (row (ent "llm" 'may #:ops '("query" "embed")))
                                (row (ent "llm" 'may #:ops '("query"))))))
(define (test-subtype-ops-all)
  (check-true (effect-subtype? (row (ent "llm" 'may #:ops '()))
                               (row (ent "llm" 'may #:ops '()))))
  (check-false (effect-subtype? (row (ent "llm" 'may #:ops '()))
                                (row (ent "llm" 'may #:ops '("query"))))))
(define (test-mutator-ok)
  (check-true (mutator-ok? (row (ent "llm" 'may #:budget 1))
                           (row (ent "llm" 'may #:budget 5))))
  (check-false (mutator-ok? (row (ent "llm" 'may #:budget 6))
                            (row (ent "llm" 'may #:budget 5))))
  (check-false (mutator-ok? (row (ent "db" 'may))
                            (row (ent "db" 'must-not)))))

(module+ main
  (test-name-symbol)
  (test-union-budget-sums)
  (test-union-prohibition-wins)
  (test-union-ops)
  (test-intersect-shared-and-min)
  (test-difference)
  (test-subtype-may-within-may)
  (test-subtype-budget-exceeded)
  (test-subtype-prohibition)
  (test-subtype-absent-is-prohibition)
  (test-subtype-must)
  (test-subtype-must-budget-ops)
  (test-subtype-must-not-is-unconstrained)
  (test-subtype-unknown-defers)
  (test-subtype-ops-subset)
  (test-subtype-ops-all)
  (test-mutator-ok)
  (displayln "All contract tests passed."))
