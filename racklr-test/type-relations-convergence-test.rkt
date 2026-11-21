#lang racket

;; racklr-test/type-relations-convergence-test.rkt
;;
;; Regression tests for the terminating type-relations join (ticket #111, Gap 1).
;;
;; `close-type-relations!` closes assign / self-attr / param / return by
;; recomputing them from each other. The single-class projection is not
;; monotone (a value can move single -> unresolved when a second candidate
;; appears, then back when that candidate collapses), so a feedback loop
;; through the four relations can flip values forever. That is what the old
;; fixed round budget silently truncated on the full Django corpus.
;;
;; `racklr/effects/type-relations-widen.rkt` makes the join terminating:
;; the round map is deterministic, so a repeated full state proves the
;; iteration is periodic; the entries that moved in the detected cycle are
;; widened to unresolved and frozen. Converging inputs never repeat a state,
;; so they never widen.
;;
;; Pinned here:
;;   (a) a synthetic join fixture with a genuine 2-cycle: it hits the explicit
;;       round-budget state without widening, and terminates deterministically
;;       with the moving entry widened when widening is on (no "did not
;;       converge" / budget state is reachable);
;;   (b) a converging UIR fixture whose four closed relations equal the exact
;;       values the fixed-point loop produces, with no widening report and no
;;       diagnostics;
;;   (c) the same converging fixture returns identical relations with widening
;;       disabled and a generous budget (the shared baseline's guarantee).

(require rackunit
         racklr/uir
         racklr/effects/collect
         racklr/effects/imports
         racklr/effects/library-tokens
         racklr/effects/type-relations
         racklr/effects/type-relations-widen)

;; ── UIR constructors for the fixtures ───────────────────────────────

(define (sym s) (uir-symbol s))
(define (var s) (uir-var (uir-symbol s)))
(define (get b f) (uir-get b (uir-symbol f)))
(define (call c args) (uir-call c args))
(define (block . xs) (uir-block xs))
(define (fn name params body) (uir-fn (sym name) params body #f #f))
(define (meth name params body) (uir-method (sym name) params body 'public))

;; The converging fixture in UIR, equivalent to:
;;
;;   class A:
;;       def __init__(self, v): self.v = v
;;   class B: pass
;;   def seed(b): return b
;;   def make():
;;       a = A(B())
;;       x = seed(a)
;;       return x
;;
;; It exercises all four relations: assign (locals `a`, `x`, `b`), self-attr
;; (A.v), param (A.__init__ arg 1, seed arg 0) and return (seed, make).
(define (convergent-program)
  (block
   (uir-class (sym "A") '() '()
              (list (meth "__init__"
                          (list (sym "self") (sym "v"))
                          (block (uir-set! (get (var "self") "v") (var "v"))))))
   (uir-class (sym "B") '() '() '())
   (fn "seed" (list (sym "b")) (block (uir-return (var "b"))))
   (fn "make" '()
       (block
        (uir-set! (var "a") (call (sym "A") (list (call (sym "B") '()))))
        (uir-set! (var "x") (call (var "seed") (list (var "a"))))
        (uir-return (var "x"))))))

;; Mirror the relation inputs whole-program.rkt builds for one component, then
;; materialize + close. Returns the type-relations value and the widening report
;; box (set to #t/#f only if a widening happened).
(define (close-fixture program
                       #:widen? [widen? #t]
                       #:max-rounds [max-rounds #f])
  (define comp "fixture")
  (define comp-programs (list (cons comp program)))
  (define fns (make-hash))
  (define methods (make-hash))
  (define fn-params (make-hash))
  (define method-params (make-hash))
  (define class-defs (make-hash))
  (define fn-defs (make-hash))
  (define comp-supers (make-hash))
  (define fn-parents (make-hash))
  (for ([(n body) (in-hash (collect-functions program))])
    (hash-set! fns (cons comp n) body)
    (hash-update! fn-defs n (λ (l) (cons comp l)) '()))
  (for ([(n ps) (in-hash (collect-function-params program))])
    (hash-set! fn-params (cons comp n) ps))
  (for ([(n parent) (in-hash (collect-fn-parents program))])
    (hash-set! fn-parents (cons comp n) parent))
  (for ([(n body) (in-hash (collect-methods program))])
    (hash-set! methods (cons comp n) body))
  (for ([(n ps) (in-hash (collect-method-params program))])
    (hash-set! method-params (cons comp n) ps))
  (for ([(n _) (in-hash (collect-class-names program))])
    (hash-update! class-defs n (λ (l) (cons comp l)) '()))
  (for ([(n chain) (in-hash (collect-supers program))])
    (hash-set! comp-supers (cons comp n) chain))
  (define env (import-env (hash) (hash) (hash)))
  (define report (box #f))
  (define tr (materialize-type-relations comp-programs class-defs env
                                         #:tokens default-effect-tokens))
  (close-type-relations! tr
                         #:comp-programs comp-programs
                         #:class-defs class-defs
                         #:import-env env
                         #:fns fns
                         #:methods methods
                         #:fn-params fn-params
                         #:method-params method-params
                         #:fn-defs fn-defs
                         #:comp-supers comp-supers
                         #:fn-parents fn-parents
                         #:tokens default-effect-tokens
                         #:max-rounds max-rounds
                         #:widen? widen?
                         #:widening-report report)
  (values tr report))

(define (run-close/captured program #:widen? [widen? #t] #:max-rounds [max-rounds #f])
  (define errs (open-output-string))
  (define-values (tr report)
    (parameterize ([current-error-port errs])
      (close-fixture program #:widen? widen? #:max-rounds max-rounds)))
  (values tr report (get-output-string errs)))

;; Relation tables from the join are mutable hashes (and hold mutable nested
;; hashes); `equal?` on a mutable hash never equals an immutable literal. Compare
;; the canonical association-list shape instead.
(define (hash-shape h)
  (sort (for/list ([(k v) (in-hash h)])
          (cons k (if (hash? v) (hash-shape v) v)))
        string<? #:key (λ (p) (format "~s" (car p)))))

(define (check-hash-equal? actual expected)
  (check-equal? (hash-shape actual) (hash-shape expected)))

;; ── (a) synthetic oscillation: 2-cycle over one assign entry ────────
;;
;; The smallest shape of the non-monotone feedback: a join entry whose value
;; flips A -> B -> A forever. This is exactly what a relation cycle produces;
;; it stands in for the corpus case without needing a 907-file front end.

(define (make-oscillating-step)
  (define n (box 0))
  (λ ()
    (set-box! n (add1 (unbox n)))
    (values (hash (cons "fixture" 'f) (hash 'v (if (even? (unbox n)) 'A 'B)))
            (hash)
            (hash)
            (hash))))

(define (run-join/captured step #:widen? [widen? #t] #:max-rounds [max-rounds #f])
  (define errs (open-output-string))
  (define-values (assign self-attr param return)
    (parameterize ([current-error-port errs])
      (run-join! step #:widen? widen? #:max-rounds max-rounds)))
  (values (list assign self-attr param return) (get-output-string errs)))

(define (test-oscillation-without-widening-hits-explicit-budget)
  (define-values (_state stderr)
    (run-join/captured (make-oscillating-step) #:widen? #f #:max-rounds 12))
  ;; The raw iteration provably cannot converge; it must say so explicitly
  ;; rather than silently return a truncated state.
  (check-regexp-match #rx"round budget 12 exhausted before convergence" stderr)
  (check-false (regexp-match? #rx"did not converge" stderr)))

(define (test-oscillation-terminates-with-widening)
  (define report (box #f))
  (define errs (open-output-string))
  (define state
    (parameterize ([current-error-port errs])
      (define-values (assign self-attr param return)
        (run-join! (make-oscillating-step)
                   #:widen? #t
                   #:widening-report report))
      (list assign self-attr param return)))
  (define stderr (get-output-string errs))
  (define-values (state2 _stderr2)
    (run-join/captured (make-oscillating-step) #:widen? #t))
  ;; Deterministic: a fresh fixture run yields the same closed relations.
  (check-equal? state state2)
  ;; The moving entry was widened to unresolved and frozen: the local `v` is
  ;; gone, the function's assign entry remains as an empty local map.
  (check-hash-equal? (first state) (hash (cons "fixture" 'f) (hash)))
  (check-hash-equal? (second state) (hash))
  (check-hash-equal? (third state) (hash))
  (check-hash-equal? (fourth state) (hash))
  ;; Programmatic + printed widening report: exactly one assign entry.
  (check-equal? (unbox report) (list 1 (hash 'assign 1 'self-attr 0 'param 0 'return 0)))
  (check-regexp-match #rx"widening 1 entry/entries to unresolved" stderr)
  ;; No fixed-cap fallback state is reachable for this fixture.
  (check-false (regexp-match? #rx"did not converge|round budget" stderr)))

;; ── (b) converging fixture: relation values unchanged, no widening ──

(define expected-assign
  (hash (cons "fixture" 'A.__init__) (hash 'v 'B)
        (cons "fixture" 'make) (hash 'a 'A 'x 'A)
        (cons "fixture" 'seed) (hash 'b 'A)))
(define expected-self-attr
  (hash (cons "fixture" 'A) (hash 'v 'B)
        (cons "fixture" 'B) (hash)))
(define expected-param
  (hash (cons (cons "fixture" 'A.__init__) 1) 'B
        (cons (cons "fixture" 'seed) 0) 'A))
(define expected-return
  (hash (cons "fixture" 'A.__init__) #f
        (cons "fixture" 'make) 'A
        (cons "fixture" 'seed) 'A))

(define (test-converging-fixture-relations)
  (define-values (tr report stderr)
    (run-close/captured (convergent-program)))
  (check-hash-equal? (type-relations-assign tr) expected-assign)
  (check-hash-equal? (type-relations-self-attr tr) expected-self-attr)
  (check-hash-equal? (type-relations-param tr) expected-param)
  (check-hash-equal? (type-relations-return tr) expected-return)
  ;; A converging input never repeats a state, so it must never widen and must
  ;; print no diagnostic.
  (check-false (unbox report))
  (check-equal? stderr ""))

(define (test-converging-fixture-same-without-widening)
  ;; The same closed relations with the widening switch off and a generous
  ;; budget: the fix must not have changed what the old fixed-point loop
  ;; produced.
  (define-values (tr report stderr)
    (run-close/captured (convergent-program) #:widen? #f #:max-rounds 500))
  (check-hash-equal? (type-relations-assign tr) expected-assign)
  (check-hash-equal? (type-relations-self-attr tr) expected-self-attr)
  (check-hash-equal? (type-relations-param tr) expected-param)
  (check-hash-equal? (type-relations-return tr) expected-return)
  (check-false (unbox report))
  (check-equal? stderr ""))

;; ── run ─────────────────────────────────────────────────────────────

(test-oscillation-without-widening-hits-explicit-budget)
(test-oscillation-terminates-with-widening)
(test-converging-fixture-relations)
(test-converging-fixture-same-without-widening)
(displayln "All type-relations convergence tests passed.")
