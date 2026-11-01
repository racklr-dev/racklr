#lang racket

;; The emit contract: one dispatch, every supported target, UIR plus an
;; explicit effect row in.
;;
;; These tests cover the contract itself (racklr/emit-contract) and the single
;; dispatch (racklr/emit):
;;
;;   - the target table registers every emitter with the same call shape;
;;   - the same UIR-plus-row input renders every row-capable target with the
;;     row present in that target's native vocabulary;
;;   - a target with no native row vocabulary refuses the row instead of
;;     silently dropping it;
;;   - a row with no function to annotate is an error, not a silent no-op;
;;   - the row-absent call shape is unchanged from before the contract.

(require rackunit
         racklr/uir
         racklr/emit-contract
         racklr/emit
         racklr/emit-python
         racklr/emit-javascript
         racklr/emit-koka
         racklr/emit-clojure
         racklr/emit-jsx
         racklr/emit-dom-javascript
         racklr/emit-html
         racklr/emit-json
         racklr/emit-effervesce)

;; ── Fixtures ────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))

(define (row-of name [budget #f])
  (uir-effect-row
   (list (uir-effect-entry (sym name) 'may '() budget #f))))

(define (llm-row) (row-of "llm" 5))

(define (empty-row) (uir-effect-row '()))

;; The one UIR-plus-row input used for every target: a run with a single named
;; function, so the explicit entry row has a function to annotate everywhere.
(define (program)
  (uir-block (list (uir-fn (sym "summarize") '() (uir-null) #f #f))))

;; Targets whose native vocabulary can carry a row, plus the check that the
;; rendered source actually contains it in that vocabulary.
(define row-capable-targets
  (list (cons 'python
              (λ (s) (and (string-contains? s "@__withEffects(")
                          (string-contains? s "\"llm\""))))
        (cons 'javascript
              (λ (s) (and (string-contains? s "__withEffects(")
                          (string-contains? s "\"llm\""))))
        (cons 'html
              (λ (s) (and (string-contains? s "__withEffects(")
                          (string-contains? s "\"llm\""))))
        (cons 'koka
              (λ (s) (and (string-contains? s "<llm>")
                          (string-contains? s "fun summarize"))))
        (cons 'clojure
              (λ (s) (and (string-contains? s ":__effects__")
                          (string-contains? s ":llm"))))))

(define row-less-targets '(jsx dom-js json effervesce))

;; ── The contract and the target table ───────────────────────────────

(define (test-target-table)
  (check-equal? (supported-targets)
                '(python javascript jsx koka clojure dom-js html json effervesce)
                "the table covers every emitter")
  (for ([t (in-list (supported-targets))])
    (define target (emit-target-for t))
    (check-equal? (emit-target-target target) t "target symbol round-trips")
    (check-true (procedure? (emit-target-render target)) "render is a function")
    (check-true (row-syntax? (emit-target-row-syntax target))
                (format "~a declares a known row syntax" t)))
  (check-exn exn:fail?
             (λ () (emit-target-for 'cobol))
             "unknown targets are a contract error"))

(define (test-contract-requires-the-row-argument)
  ;; A one-argument renderer keeps the pre-contract shape and would drop the
  ;; row; `make-emit-target` must reject it.
  (check-exn exn:fail?
             (λ () (make-emit-target 'bad "bad target" 'none (λ (u) "")))
             "a renderer that does not accept the row is not contract-shaped")
  (check-exn exn:fail?
             (λ () (make-emit-target 'bad "bad target" 'made-up emit-json))
             "unknown row syntax is rejected"))

;; ── The same input renders every row-capable target ─────────────────

(define (test-same-input-every-row-capable-target)
  (for ([entry (in-list row-capable-targets)])
    (define t (car entry))
    (define row-present? (cdr entry))
    (define out (emit t (program) (llm-row)))
    (check-true (row-present? out)
                (format "~a reifies the explicit row natively: ~a" t out))))

(define (test-explicit-row-wins-over-the-uir-row)
  (define program-with-db
    (uir-block (list (uir-fn (sym "summarize") '() (uir-null) #f (row-of "db")))))
  (define out (emit 'python program-with-db (llm-row)))
  (check-true (string-contains? out "\"llm\"")
              "the explicit row is rendered")
  (check-false (string-contains? out "\"db\"")
               "the explicit row is authoritative over the UIR row"))

(define (test-empty-explicit-row-is-not-dropped)
  ;; The empty row is a real declaration (no effects allowed), not "no row".
  (check-true (string-contains? (emit 'python (program) (empty-row))
                                "@__withEffects({})")
              "python renders the empty row as an empty guard")
  (check-true (string-contains? (emit 'koka (program) (empty-row))
                                "total")
              "koka renders the empty row as total")
  (check-true (string-contains? (emit 'clojure (program) (empty-row))
                                ":__effects__ {}")
              "clojure renders the empty row as empty metadata"))

;; ── Row-less targets refuse an explicit row ─────────────────────────

(define (test-row-less-targets-refuse-the-row)
  (for ([t (in-list row-less-targets)])
    (check-exn exn:fail?
               (λ () (emit t (program) (llm-row)))
               (format "~a refuses an explicit row" t)))
  ;; Direct calls refuse too, not only dispatch.
  (check-exn exn:fail? (λ () (emit-jsx (uir-string "x") (llm-row))))
  (check-exn exn:fail? (λ () (emit-dom-js (uir-string "x") (llm-row))))
  (check-exn exn:fail? (λ () (emit-json (uir-null) (llm-row))))
  (check-exn exn:fail? (λ () (emit-effervesce (uir-number "1") (llm-row)))))

;; ── The row always lands on a function ──────────────────────────────

(define (test-entry-function-wrappers-carry-the-row)
  ;; JavaScript declaration: (set! name fn).
  (define js-program
    (uir-block (list (uir-set! (sym "f") (uir-fn #f '() (uir-block '()) #f #f)))))
  (check-true (string-contains? (emit 'javascript js-program (llm-row))
                                "__withEffects(")
              "a set!-bound function is the entry function")
  ;; Python `async def` encoding: (call "async-fn" (list fn)).
  (define py-async
    (uir-block
     (list (uir-call (sym "async-fn")
                     (list (uir-fn (sym "f") '() (uir-null) #f #f))))))
  (define py-out (emit 'python py-async (llm-row)))
  (check-true (string-contains? py-out "async def f")
              "the async wrapper still renders as async def")
  (check-true (string-contains? py-out "@__withEffects(")
              "the async wrapper's row reaches the inner function")
  ;; Decorated definition: the row lands on the decorated inner function.
  (define decorated
    (uir-block
     (list (uir-decorated
            (list (uir-call (sym "cache") '()))
            (uir-fn (sym "f") '() (uir-null) #f #f)))))
  (check-true (string-contains? (emit 'python decorated (llm-row))
                                "@__withEffects(")
              "a decorated function is the entry function"))

(define (test-row-with-no-function-is-an-error)
  (check-exn exn:fail?
             (λ () (emit 'python (uir-block (list (uir-number "1"))) (llm-row)))
             "a row with no function to annotate is refused, not dropped"))

(define (test-row-input-must-be-a-row)
  (check-exn exn:fail?
             (λ () (emit 'python (program) 'llm))
             "a non-row explicit input is rejected"))

;; ── The pre-contract call shape is unchanged ────────────────────────

(define (test-row-absent-keeps-the-old-call-shape)
  ;; The row argument is optional; with no row the dispatch returns exactly
  ;; what the legacy one-argument emitter returned.
  (check-equal? (emit 'python (program)) (emit-python (program)))
  (check-equal? (emit 'javascript (program)) (emit-javascript (program)))
  (check-equal? (emit 'koka (program)) (emit-koka (program)))
  (check-equal? (emit 'clojure (program)) (emit-clojure (program)))
  (check-equal? (emit 'html (program)) (emit-html (program)))
  (check-equal? (emit 'jsx (uir-string "x")) (emit-jsx (uir-string "x")))
  (check-equal? (emit 'dom-js (uir-string "x")) (emit-dom-js (uir-string "x")))
  (check-equal? (emit 'json (uir-null)) (emit-json (uir-null)))
  (check-equal? (emit 'effervesce (uir-number "1"))
                (emit-effervesce (uir-number "1"))))

(module+ main
  (test-target-table)
  (test-contract-requires-the-row-argument)
  (test-same-input-every-row-capable-target)
  (test-explicit-row-wins-over-the-uir-row)
  (test-empty-explicit-row-is-not-dropped)
  (test-row-less-targets-refuse-the-row)
  (test-entry-function-wrappers-carry-the-row)
  (test-row-with-no-function-is-an-error)
  (test-row-input-must-be-a-row)
  (test-row-absent-keeps-the-old-call-shape)
  (displayln "All emit-contract tests passed."))
