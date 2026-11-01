#lang racket

;; TypeScript/JS `withEffects(row)(fn)` annotation extraction (gap 4 extraction
;; half). Mirrors effects-python-annot-test.rkt: the module recognizes the
;; curried wrapper's UIR shape and turns the row literal into a uir-effect-row.

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/effects/ts-annot
         racklr/emit-javascript)

(define (sym s) (uir-symbol s))
(define (num s) (uir-number s))
(define (str s) (uir-string s))

(define (record . pairs)
  (uir-record (map (λ (p) (cons (str (car p)) (cadr p))) pairs)))

;; (call (call (var withEffects) (list ROW)) (list FN))
(define (with-effects-wrapper row fn)
  (uir-call (uir-call (uir-var (sym "withEffects")) (list row))
            (list fn)))

(define (modality row name)
  (define e (row-lookup row name))
  (and e (uir-effect-entry-modality e)))

(define (budget row name)
  (define e (row-lookup row name))
  (and e (uir-effect-entry-budget e)))

(define (scope row name)
  (define e (row-lookup row name))
  (and e (uir-effect-entry-scope e) (uir-symbol-name (uir-effect-entry-scope e))))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-with-effects-call-predicate)
  (check-true (with-effects-call? (with-effects-wrapper (record) (sym "f"))))
  (check-false (with-effects-call? (uir-call (uir-var (sym "other"))
                                             (list (record)))))
  (check-false (with-effects-call? (uir-null))))

(define (test-not-a-with-effects-wrapper)
  (check-false (extract-with-effects-row
                (uir-call (uir-var (sym "effects")) (list (record))))))

(define (test-empty-row)
  (define r (extract-with-effects-row (with-effects-wrapper (record) (sym "f"))))
  (check-true (uir-effect-row? r))
  (check-equal? (length (uir-effect-row-entries r)) 0))

(define (test-budget)
  (define r (extract-with-effects-row
             (with-effects-wrapper
              (record (list "llm" (record (list "max" (num "5")))))
              (sym "f"))))
  (check-equal? (modality r 'llm) 'may)
  (check-equal? (budget r 'llm) 5))

(define (test-scope-shorthand)
  (define r (extract-with-effects-row
             (with-effects-wrapper
              (record (list "db" (record (list "reads" (str "search_index")))))
              (sym "f"))))
  (check-equal? (scope r 'db) "reads=search_index"))

(define (test-explicit-scope)
  (define r (extract-with-effects-row
             (with-effects-wrapper
              (record (list "net" (record (list "scope" (str "tenant_a")))))
              (sym "f"))))
  (check-equal? (scope r 'net) "tenant_a"))

(define (test-ops)
  (define r (extract-with-effects-row
             (with-effects-wrapper
              (record (list "llm" (record (list "ops" (uir-list (list (str "query") (str "embed")))))))
              (sym "f"))))
  (check-equal? (entry-ops (row-lookup r 'llm)) '(query embed)))

(define (test-modality)
  (define r (extract-with-effects-row
             (with-effects-wrapper
              (record (list "net" (record (list "modality" (str "must-not")))))
              (sym "f"))))
  (check-equal? (modality r 'net) 'must-not))

(define (test-unconstrained-entry)
  (define r (extract-with-effects-row
             (with-effects-wrapper
              (record (list "state" (uir-null)))
              (sym "f"))))
  (check-equal? (modality r 'state) 'may)
  (check-equal? (budget r 'state) #f))

(define (test-extracts-wrapped-fn)
  (define fn (uir-fn #f '() (uir-block '()) #f #f))
  (define w (with-effects-wrapper (record) fn))
  (check-equal? (extract-with-effects-fn w) fn))

(define (test-extraction-feeds-emit)
  ;; extraction -> attach row to the fn -> emit renders __withEffects guard.
  (define fn (uir-fn #f '() (uir-block '()) #f #f))
  (define row (extract-with-effects-row
               (with-effects-wrapper
                (record (list "llm" (record (list "max" (num "5")))))
                fn)))
  (define annotated (struct-copy uir-fn fn [effects row]))
  (define out (emit-javascript
               (uir-block (list (uir-set! (sym "summarize") annotated)))))
  (check-true (string-contains? out "__withEffects(")
              "annotated fn emitted through the guard")
  (check-true (string-contains? out "/** @effects {")
              "documented @effects annotation emitted")
  (check-true (string-contains? out "\"max\": 5")
              "budget preserved through extraction"))

(module+ main
  (test-with-effects-call-predicate)
  (test-not-a-with-effects-wrapper)
  (test-empty-row)
  (test-budget)
  (test-scope-shorthand)
  (test-explicit-scope)
  (test-ops)
  (test-modality)
  (test-unconstrained-entry)
  (test-extracts-wrapped-fn)
  (test-extraction-feeds-emit)
  (displayln "All ts-annot tests passed."))
