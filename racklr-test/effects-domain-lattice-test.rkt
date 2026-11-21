#lang racket

;; effects-domain-lattice-test.rkt — pin the effect-domain lattice: the poset
;; laws (reflexivity, transitivity, antisymmetry), the umbrella edges, and the
;; interpretation helper (`effect-name-subtypes`) the bridge consumes so a fine
;; host effect satisfies a coarse declared `needs`. Uses the text-ui runner so
;; a failure gives a real non-zero exit.
;;
;; Note: the lattice deliberately does NOT thread into the kernel's
;; `effect-subtype?` (name-equality): a leaf matching different umbrellas breaks
;; transitivity and join-lub there (pinned by trans-check.rkt, a<=b, b<=c but
;; a!<=c). Umbrella+subtyping lives in the interpretation layer instead.

(require rackunit
         (rename-in rackunit/text-ui [run-tests run-test-suite])
         racklr/effects/domain-lattice)

(define leaves->umbrella
  '((fs . io) (stdio . io)
    (http . network) (socket . network) (websocket . network)
    (sql . db) (nosql . db) (vector . db)
    (clock . time) (sleep . time) (timer . time)))

(define (all-domains)
  (remove-duplicates
   (append effect-domains
           (apply append (map (lambda (p) (list (car p) (cdr p)))
                              leaves->umbrella)))))

(define suite
  (test-suite
   "effect-domain-lattice"

   (test-case
    "every domain is known and every parent edge is declared"
    (for-each (lambda (p)
                (check-true (effect-domain? (car p)) (format "~a is a domain" (car p)))
                (check-true (effect-domain? (cdr p)) (format "~a is a domain" (cdr p)))
                (check-eq? (effect-domain-parent (car p)) (cdr p)))
              leaves->umbrella))

   (test-case
    "leaf is a subtype of its umbrella, and not the reverse"
    (for-each (lambda (p)
                (check-true (effect-name-subtype? (car p) (cdr p))
                            (format "~a <= ~a" (car p) (cdr p)))
                (check-false (effect-name-subtype? (cdr p) (car p))
                             (format "~a !<= ~a" (cdr p) (car p))))
              leaves->umbrella))

   (test-case
    "reflexivity over every domain"
    (for-each (lambda (d) (check-true (effect-name-subtype? d d)))
              (all-domains)))

   (test-case
    "transitive closure, no cross-tree leakage"
    (check-true (effect-name-subtype? 'http 'network))
    (check-true (effect-name-subtype? 'network 'network))
    (check-false (effect-name-subtype? 'http 'db))
    (check-false (effect-name-subtype? 'fs 'network)))

   (test-case
    "antisymmetry: no distinct domains are mutually subtypes (no cycles)"
    (for* ([a (in-list (all-domains))]
           [b (in-list (all-domains))]
           #:unless (eq? a b))
      (check-false
       (and (effect-name-subtype? a b) (effect-name-subtype? b a))
       (format "~a and ~a are distinct and mutually subtype" a b))))

   (test-case
    "ancestors are nearest-first and complete"
    (check-equal? (effect-name-ancestors 'http) '(network))
    (check-equal? (effect-name-ancestors 'fs) '(io))
    (check-equal? (effect-name-ancestors 'network) '())
    (check-equal? (effect-name-ancestors 'sql) '(db)))

   (test-case
    "lub agrees with the forest"
    (check-eq? (effect-name-lub 'http 'http) 'http)
    (check-eq? (effect-name-lub 'http 'network) 'network)
    (check-eq? (effect-name-lub 'fs 'stdio) #f)     ; siblings, incomparable
    (check-eq? (effect-name-lub 'http 'socket) #f)   ; siblings, incomparable
    (check-eq? (effect-name-lub 'sql 'db) 'db))

   (test-case
    "interpretation helper: an umbrella capability covers its leaves"
    (check-equal? (effect-name-subtypes 'network) '(http socket websocket network))
    (check-equal? (effect-name-subtypes 'db) '(sql nosql vector db))
    (check-equal? (effect-name-subtypes 'io) '(fs stdio io))
    (check-equal? (effect-name-subtypes 'time) '(clock sleep timer time))
    (check-equal? (effect-name-subtypes 'http) '(http))
    (check-equal? (effect-name-subtypes 'cache) '(cache)))))

(exit (run-test-suite suite))
