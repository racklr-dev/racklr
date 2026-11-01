#lang racket

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/effects/python-annot
         racklr/emit-python)

(define (sym s) (uir-symbol s))
(define (num s) (uir-number s))
(define (str s) (uir-string s))

(define (effects-decorator . args)
  (uir-call (uir-var (uir-symbol "effects")) args))

(define (record . pairs)
  (uir-record (map (λ (p) (cons (str (car p)) (cadr p))) pairs)))

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

(define (test-effects-decorator-predicate)
  (check-true (effects-decorator? (effects-decorator (record))))
  (check-false (effects-decorator? (uir-call (uir-var (uir-symbol "app"))
                                             (list (str "/x")))))
  (check-false (effects-decorator? (uir-null))))

(define (test-not-an-effect-name)
  (check-false (extract-effects-row (uir-call (uir-var (uir-symbol "other"))
                                              (list (record))))))

(define (test-empty-row)
  (define r (extract-effects-row (effects-decorator (record))))
  (check-true (uir-effect-row? r))
  (check-equal? (length (uir-effect-row-entries r)) 0))

(define (test-budget)
  (define r (extract-effects-row
             (effects-decorator (record (list "llm" (record (list "max" (num "5"))))))))
  (check-equal? (modality r 'llm) 'may)
  (check-equal? (budget r 'llm) 5))

(define (test-scope-shorthand)
  (define r (extract-effects-row
             (effects-decorator (record (list "db" (record (list "reads" (str "search_index"))))))))
  (check-equal? (scope r 'db) "reads=search_index"))

(define (test-explicit-scope)
  (define r (extract-effects-row
             (effects-decorator (record (list "net" (record (list "scope" (str "tenant_a"))))))))
  (check-equal? (scope r 'net) "tenant_a"))

(define (test-ops)
  (define r (extract-effects-row
             (effects-decorator
              (record (list "llm" (record (list "ops" (uir-list (list (str "query") (str "embed"))))))))))
  (check-equal? (entry-ops (row-lookup r 'llm)) '(query embed)))

(define (test-modality)
  (define r (extract-effects-row
             (effects-decorator
              (record (list "net" (record (list "modality" (str "must-not"))))))))
  (check-equal? (modality r 'net) 'must-not))

(define (test-unconstrained-entry)
  (define r (extract-effects-row
             (effects-decorator (record (list "state" (uir-null))))))
  (check-equal? (modality r 'state) 'may)
  (check-equal? (budget r 'state) #f))

(define (test-multiple-entries)
  (define r (extract-effects-row
             (effects-decorator
              (record (list "llm" (record (list "max" (num "3"))))
                      (list "db"  (record (list "reads" (str "idx"))))))))
  (check-equal? (length (uir-effect-row-entries r)) 2)
  (check-equal? (budget r 'llm) 3)
  (check-equal? (scope r 'db) "reads=idx"))

;; ── extraction feeds the native emission ────────────────────────────

;; A known row extracted from the front-end annotation surface must render in
;; the target's native Python form: the `# @effects {...}` documented comment
;; and the `@__withEffects({...})` guard, both carrying the row.
(define (test-extraction-feeds-emit)
  (define r (extract-effects-row
             (effects-decorator
              (record (list "llm" (record (list "max" (num "5"))))))))
  (define fn (uir-fn (sym "summarize") '()
                     (uir-block (list (uir-null)))
                     #f r))
  (define out (emit-python (uir-block (list fn))))
  (check-true (string-contains? out "# @effects {\"llm\"")
              "documented @effects annotation emitted")
  (check-true (string-contains? out "@__withEffects({\"llm\"")
              "runtime guard emitted with the native row")
  (check-true (string-contains? out "\"max\": 5")
              "budget preserved through extraction -> emit"))

(module+ main
  (test-effects-decorator-predicate)
  (test-not-an-effect-name)
  (test-empty-row)
  (test-budget)
  (test-scope-shorthand)
  (test-explicit-scope)
  (test-ops)
  (test-modality)
  (test-unconstrained-entry)
  (test-multiple-entries)
  (test-extraction-feeds-emit)
  (displayln "All python-annot tests passed."))
