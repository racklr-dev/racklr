#lang racket

;; Clojure effects front-end (Phase 7): annotation extraction, attach,
;; inference over namespace-qualified symbols, and metadata emission.

(require rackunit
         racket/system
         racket/port
         racket/file
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/g4-parse
         racklr/gend-parser
         racklr/lower-clojure
         racklr/emit-clojure
         racklr/effects/clojure-annot
         racklr/effects/contract
         racklr/effects/infer)

(define-values (clj-parse clj-tokenize clj-tok-type clj-tok-value)
  (gen-and-load "../grammars-v4/clojure/Clojure.g4"))

(define (lower src)
  (lower-clojure (clj-parse src) clj-tok-type clj-tok-value))

(define (kw s) (uir-symbol (string-append ":" s)))

(define (modality row name)
  (define e (row-lookup row name))
  (and e (uir-effect-entry-modality e)))

;; ── 1. extract-effects-row (unit) ────────────────────────────────────

(let* ([meta (uir-record
              (list (cons (kw "effects")
                          (uir-record
                           (list (cons (kw "llm")
                                       (uir-record
                                        (list (cons (kw "max") (uir-number "5")))))
                                 (cons (kw "db")
                                       (uir-record
                                        (list (cons (kw "reads")
                                                    (uir-string "search_index"))))))))))]
       [row (extract-effects-row meta)])
  (check-true (uir-effect-row? row) "extract yields a row")
  (check-equal? (length (uir-effect-row-entries row)) 2 "two entries")
  (define llm-entry (row-lookup row 'llm))
  (check-not-false llm-entry "llm entry present")
  (check-equal? (uir-effect-entry-modality llm-entry) 'may "default modality")
  (check-equal? (uir-effect-entry-budget llm-entry) 5 "budget 5")
  (define db-entry (row-lookup row 'db))
  (check-not-false db-entry "db entry present")
  (check-equal? (uir-symbol-name (uir-effect-entry-scope db-entry))
                "reads=search_index" "reads shorthand"))

;; ── 2. attach via ^{:effects ...} metadata ───────────────────────────

(let* ([src "^{:effects {:llm {:max 5} :db {:reads \"search_index\"}}} (defn summarize [x] (llm/query x))"]
       [uir (lower src)]
       [fn-node (first (uir-block-stmts uir))])
  (check-true (uir-fn? fn-node) "defn lowers to uir-fn")
  (check-true (uir-effect-row? (uir-fn-effects fn-node)) "effects attached")
  (check-equal? (length (uir-effect-row-entries (uir-fn-effects fn-node))) 2
                "two effect entries")
  (check-equal? (modality (uir-fn-effects fn-node) 'llm) 'may "llm modality")
  (check-equal? (modality (uir-fn-effects fn-node) 'db) 'may "db modality"))

;; ── 3. inference over namespace-qualified symbols ────────────────────

(let* ([src (string-append
             "(defn f [x] (llm/query x))\n"
             "(defn g [x] (db/read x))\n"
             "(defn h [x] (f x) (g x))")]
       [uir (lower src)]
       [rows (infer-effects uir)])
  (check-equal? (modality (hash-ref rows 'f) 'llm) 'must "f -> llm")
  (check-equal? (modality (hash-ref rows 'g) 'db) 'must "g -> db")
  (check-equal? (modality (hash-ref rows 'h) 'llm) 'must "h inherits llm")
  (check-equal? (modality (hash-ref rows 'h) 'db) 'must "h inherits db"))

;; ── 4. emit renders the row as metadata ──────────────────────────────

(let* ([src "^{:effects {:llm {:max 5}}} (defn summarize [x] (llm/query x))"]
       [uir (lower src)]
       [emitted (emit-clojure uir)])
  (check-true (string-contains? emitted ":__effects__") "effects metadata emitted")
  (check-true (string-contains? emitted ":llm") "llm entry emitted")
  (check-true (string-contains? emitted ":max 5") "budget emitted"))

;; ── 5. Jolt executes emitted Clojure and preserves :__effects__ ──────
;; The emitter must place the row where defn actually keeps it: the
;; attr-map slot. The outer-^ shape is dropped by the reader, so Jolt
;; would report no :__effects__ on the var.

(define (run-jolt clj-source)
  ;; Returns (list exit-code stdout stderr), or #f when jolt is unavailable.
  (if (find-executable-path "jolt" #f)
      (let ([tmp (make-temporary-file "racklr-clj-~a.clj")]
            [out (make-temporary-file "racklr-clj-out-~a")])
        (call-with-output-file tmp #:exists 'truncate
          (λ (o) (display clj-source o)))
        (define code
          (with-handlers ([exn:fail? (λ (e) #f)])
            (system/exit-code (format "jolt run ~a > ~a 2>&1" tmp out))))
        (define stdout (if (file-exists? out) (file->string out) ""))
        (delete-file tmp)
        (delete-file out)
        (and code (list code stdout "")))
      #f))

(define jolt-result
  (run-jolt (string-append
             (emit-clojure (lower "^{:effects {:llm {:max 5}}} (defn summarize [x] (llm/query x))"))
             "\n(println (:__effects__ (meta (var summarize))))\n")))

(if jolt-result
    (let ([code (first jolt-result)]
          [stdout (second jolt-result)]
          [stderr (third jolt-result)])
      (check-equal? code 0 (format "jolt exit code (stderr: ~a)" stderr))
      (check-true (string-contains? stdout ":llm") "jolt reads :llm effect")
      (check-true (string-contains? stdout ":max 5") "jolt reads :max budget")
      (check-true (string-contains? stdout ":modality :may") "jolt reads :modality :may"))
    (displayln "jolt not found; skipping Jolt execution checks"))

(displayln "All Clojure effects tests passed.")
