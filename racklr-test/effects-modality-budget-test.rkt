#lang racket

;; racklr-test/effects-modality-budget-test.rkt
;;
;; Inference-side modality, budget and scope. A direct classified effect is a
;; `must` obligation when its call site is guaranteed to be evaluated, `may`
;; when a branch, short-circuit operand or exception path can skip it; budget is
;; the static invocation upper bound (`#f` = unbounded); scope is the grounded
;; resource qualifier. The tests pin each control-flow composition rule the
;; inference driver uses (`racklr/effects/invocation-row.rkt`).

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/effects/infer
         (prefix-in alg: racklr/effects/algebra/main))

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))
(define (llm-call) (uir-call (dot (v "llm") "query") '()))
(define (db-save) (uir-call (dot (v "db") "save") '()))
(define (db-read) (uir-call (dot (v "db") "read") '()))
(define (row-of p name) (hash-ref (infer-effects p) (string->symbol name)))
(define (entry row name)
  (define n (if (symbol? name) name (string->symbol name)))
  (for/or ([e (in-list (uir-effect-row-entries row))])
    (and (eq? (effect-name-symbol e) n) e)))
(define (modality row name) (and (entry row name) (uir-effect-entry-modality (entry row name))))
(define (budget row name) (and (entry row name) (uir-effect-entry-budget (entry row name))))
(define (scope row name)
  (define e (entry row name))
  (and e (uir-effect-entry-scope e) (uir-symbol-name (uir-effect-entry-scope e))))

;; ── modality: guaranteed vs conditional ─────────────────────────────

(define (test-direct-effect-is-must)
  (define r (row-of (prog (fn "f" (llm-call))) "f"))
  (check-equal? (modality r 'llm) 'must)
  (check-equal? (budget r 'llm) 1))

(define (test-branch-demotes-to-may)
  (define r (row-of (prog (fn "f" (uir-if (v "c") (db-save) (uir-null)))) "f"))
  (check-equal? (modality r 'db) 'may)
  (check-equal? (budget r 'db) 1))

(define (test-both-branches-are-must)
  (define r (row-of (prog (fn "f" (uir-if (v "c") (db-save) (db-read)))) "f"))
  (check-equal? (modality r 'db) 'must)
  (check-equal? (budget r 'db) 1))

(define (test-early-return-makes-rest-may)
  ;; `if c: return` leaves a path that skips the later call.
  (define r (row-of (prog (fn "f" (uir-block
                                  (list (uir-if (v "c") (uir-return (uir-null)) (uir-null))
                                        (db-save)))))
                    "f"))
  (check-equal? (modality r 'db) 'may)
  (check-equal? (budget r 'db) 1))

(define (test-sequential-calls-sum-budgets)
  (define r (row-of (prog (fn "f" (uir-block (list (db-save) (db-save))))) "f"))
  (check-equal? (modality r 'db) 'must)
  (check-equal? (budget r 'db) 2))

(define (test-loop-body-is-may-and-unbounded)
  (define loop
    (uir-for-each (sym "x") (v "xs")
                  (uir-block (list (db-save) (db-save)))
                  (uir-null)))
  (define r (row-of (prog (fn "f" loop)) "f"))
  (check-equal? (modality r 'db) 'may)
  (check-equal? (budget r 'db) #f))

(define (test-loop-test-keeps-must-but-is-unbounded)
  ;; The test is evaluated at least once, so its effect is `must`; it can run
  ;; again on every iteration, so its budget is unbounded.
  (define loop (uir-while (uir-call (dot (v "time") "now") '())
                          (uir-null) (uir-null)))
  (define r (row-of (prog (fn "f" loop)) "f"))
  (check-equal? (modality r 'time) 'must)
  (check-equal? (budget r 'time) #f))

(define (test-short-circuit-right-operand-is-conditional)
  ;; `x and db.save()`: the right operand runs only when the left is truthy.
  (define r (row-of (prog (fn "f" (uir-call (sym "and") (list (v "x") (db-save))))) "f"))
  (check-equal? (modality r 'db) 'may)
  (check-equal? (budget r 'db) 1))

(define (test-try-body-obligation-is-weakened)
  ;; An exception can abandon the try body, so its obligation weakens; the
  ;; handler is a branch; the bound is the max of the two paths, not their sum.
  (define t (uir-try (uir-block (list (db-save)))
                     (list (list (uir-null) (uir-null)
                                 (uir-block (list (db-save)))))
                     #f #f))
  (define r (row-of (prog (fn "f" t)) "f"))
  (check-equal? (modality r 'db) 'may)
  (check-equal? (budget r 'db) 1))

;; ── propagation through the call graph ──────────────────────────────

(define (test-must-propagates-through-unconditional-call)
  (define r (row-of (prog (fn "f" (llm-call))
                          (fn "g" (uir-call (v "f") '())))
                    "g"))
  (check-equal? (modality r 'llm) 'must)
  (check-equal? (budget r 'llm) 1))

(define (test-must-demotes-through-conditional-call)
  (define r (row-of (prog (fn "f" (llm-call))
                          (fn "g" (uir-if (v "c") (uir-call (v "f") '()) (uir-null))))
                    "g"))
  (check-equal? (modality r 'llm) 'may)
  (check-equal? (budget r 'llm) 1))

(define (test-two-calls-to-same-callee-sum-budget)
  (define r (row-of (prog (fn "f" (llm-call))
                          (fn "g" (uir-block (list (uir-call (v "f") '())
                                                   (uir-call (v "f") '())))))
                    "g"))
  (check-equal? (modality r 'llm) 'must)
  (check-equal? (budget r 'llm) 2))

(define (test-recursive-budget-is-widened-to-unbounded)
  ;; A callee reached through a call-graph cycle can run an unbounded number of
  ;; times: its budgets widen to #f, while the local obligation stays `must`.
  (define r (row-of (prog (fn "f" (uir-block (list (db-save)
                                                  (uir-call (v "f") '())))))
                    "f"))
  (check-equal? (modality r 'db) 'must)
  (check-equal? (budget r 'db) #f))

(define (test-mutual-recursion-widens-both-directions)
  (define p (prog (fn "f" (uir-block (list (db-save) (uir-call (v "g") '()))))
                  (fn "g" (uir-block (list (uir-call (v "f") '()))))))
  (define f (row-of p "f"))
  (define g (row-of p "g"))
  (check-equal? (modality f 'db) 'must)
  (check-equal? (budget f 'db) #f)
  (check-equal? (modality g 'db) 'must)
  (check-equal? (budget g 'db) #f))

(define (test-branching-callees-take-max-not-sum)
  (define r (row-of (prog (fn "f" (db-save))
                          (fn "g" (db-save))
                          (fn "h" (uir-if (v "c") (uir-call (v "f") '())
                                          (uir-call (v "g") '()))))
                    "h"))
  (check-equal? (modality r 'db) 'must)
  (check-equal? (budget r 'db) 1))

;; ── unknown stays a tracked deferral ────────────────────────────────

(define (test-unknown-is-never-must-or-bounded)
  (define r (row-of (prog (fn "f" (uir-block (list (uir-call (v "mystery") '()))))) "f"))
  (check-equal? (modality r 'unknown) 'unknown)
  (check-equal? (budget r 'unknown) #f))

(define (test-unknown-propagates-through-unconditional-call)
  (define r (row-of (prog (fn "f" (uir-call (v "mystery") '()))
                          (fn "g" (uir-call (v "f") '())))
                    "g"))
  (check-equal? (modality r 'unknown) 'unknown))

;; ── scope: grounded resource qualifier ──────────────────────────────

(define (test-db-manager-call-grounds-read-scope)
  (define p (prog (uir-class (sym "User") '() '() '())
                  (fn "f" (uir-call (dot (dot (v "User") "objects") "filter") '()))))
  (define r (row-of p "f"))
  (check-equal? (modality r 'db) 'must)
  (check-equal? (scope r 'db) "reads=User"))

(define (test-db-save-grounds-write-scope)
  (define p (prog (uir-class (sym "User") '() '() '())
                  (fn "f" (uir-call (dot (v "User") "save") '()))))
  (define r (row-of p "f"))
  (check-equal? (scope r 'db) "writes=User"))

(define (test-scope-propagates-through-callee)
  (define p (prog (uir-class (sym "User") '() '() '())
                  (fn "f" (uir-call (dot (dot (v "User") "objects") "filter") '()))
                  (fn "g" (uir-call (v "f") '()))))
  (check-equal? (scope (row-of p "g") 'db) "reads=User"))

(define (test-conflicting-resources-lose-scope)
  ;; Two different resources in one row: the single scope slot cannot
  ;; characterize both, so it stays #f (uncharacterized, never guessed).
  (define p (prog (uir-class (sym "User") '() '() '())
                  (uir-class (sym "Post") '() '() '())
                  (fn "f" (uir-block
                           (list (uir-call (dot (dot (v "User") "objects") "filter") '())
                                 (uir-call (dot (dot (v "Post") "objects") "filter") '()))))))
  (check-false (scope (row-of p "f") 'db)))

(define (test-unrooted-resource-loses-scope)
  ;; `self.save()` has no named resource in the call chain: scope stays #f.
  (define p (prog (uir-class (sym "Thing") '() '() '())
                  (fn "f" (uir-call (dot (v "self") "save") '()))))
  (check-false (scope (row-of p "f") 'db)))

;; ── algebra region combinators ──────────────────────────────────────

(define (row-with modality budget)
  (alg:make-algebra-row
   (uir-effect-row (list (uir-effect-entry (sym "db") modality '() budget #f)))
   (hash)))

(define (test-repeatable-demotes-and-unbounds)
  (define r (alg:repeatable (row-with 'must 2)))
  (check-equal? (uir-effect-entry-modality (alg:row-lookup r 'db)) 'may)
  (check-equal? (uir-effect-entry-budget (alg:row-lookup r 'db)) #f))

(define (test-unbounded-budget-keeps-modality)
  (define r (alg:unbounded-budget (row-with 'must 2)))
  (check-equal? (uir-effect-entry-modality (alg:row-lookup r 'db)) 'must)
  (check-equal? (uir-effect-entry-budget (alg:row-lookup r 'db)) #f))

(define (test-weaken-must-keeps-budget)
  (define r (alg:weaken-must (row-with 'must 2)))
  (check-equal? (uir-effect-entry-modality (alg:row-lookup r 'db)) 'may)
  (check-equal? (uir-effect-entry-budget (alg:row-lookup r 'db)) 2))

;; ── runner ──────────────────────────────────────────────────────────

(define tests
  (list test-direct-effect-is-must
        test-branch-demotes-to-may
        test-both-branches-are-must
        test-early-return-makes-rest-may
        test-sequential-calls-sum-budgets
        test-loop-body-is-may-and-unbounded
        test-loop-test-keeps-must-but-is-unbounded
        test-short-circuit-right-operand-is-conditional
        test-try-body-obligation-is-weakened
        test-must-propagates-through-unconditional-call
        test-must-demotes-through-conditional-call
        test-two-calls-to-same-callee-sum-budget
        test-recursive-budget-is-widened-to-unbounded
        test-mutual-recursion-widens-both-directions
        test-branching-callees-take-max-not-sum
        test-unknown-is-never-must-or-bounded
        test-unknown-propagates-through-unconditional-call
        test-db-manager-call-grounds-read-scope
        test-db-save-grounds-write-scope
        test-scope-propagates-through-callee
        test-conflicting-resources-lose-scope
        test-unrooted-resource-loses-scope
        test-repeatable-demotes-and-unbounds
        test-unbounded-budget-keeps-modality
        test-weaken-must-keeps-budget))

(for ([t (in-list tests)]) (t))
(printf "effects-modality-budget-test: ~a tests passed\n" (length tests))
