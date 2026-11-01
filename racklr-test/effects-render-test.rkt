#lang racket

;; The emit contract itself: every supported target renders a row in its native
;; effect vocabulary, fields a target cannot spell are documented rather than
;; dropped, and `unknown` entries never become a concrete effect.
;;
;; Per-target placement is covered by the effects-*-annot-test suites; this
;; suite grades the contract's mapping in one place.

(require rackunit
         racklr/uir
         racklr/emit-effects)

(define (sym s) (uir-symbol s))

(define (entry name #:modality [modality 'may] #:max [budget #f]
               #:ops [ops '()] #:scope [scope #f])
  (uir-effect-entry (sym name) modality (map sym ops) budget
                    (and scope (sym scope))))

(define (row . entries) (uir-effect-row entries))

(define constrained
  (row (entry "llm" #:max 5 #:scope "reads=idx" #:ops '("query"))))

;; ── every supported target renders the row ──────────────────────────

(for ([target (in-list effect-targets)])
  (define out (render-effect-row target constrained))
  (check-true (string? out) (format "~a renders a row" target))
  (check-true (string-contains? out "llm")
              (format "~a row names the effect" target)))

;; The data-only JSON target has no function/effect vocabulary.
(check-false (render-effect-row 'json constrained)
             "an unsupported target renders no row")

;; ── object-literal targets keep every field ─────────────────────────

(for ([target (in-list '(python javascript typescript))])
  (check-equal? (render-effect-row target constrained)
                "{\"llm\": {\"modality\": \"may\", \"max\": 5, \"scope\": \"reads=idx\", \"ops\": [\"query\"]}}"
                (format "~a keeps modality/budget/scope/ops" target))
  (check-true (string-prefix? (render-effect-documented target constrained)
                              (case target
                                [(python) "# @effects "]
                                [else "/** @effects "]))
              (format "~a documents the row in native comment syntax" target)))

;; ── Clojure metadata keeps every field, needs no extra comment ──────

(check-equal? (render-effect-row 'clojure constrained)
              "{:llm {:modality :may :max 5 :scope \"reads=idx\" :ops [\"query\"]}}"
              "Clojure renders a keyword-keyed metadata map")
(check-false (render-effect-annotation 'clojure constrained)
             "Clojure's native map is the whole row")

;; ── Koka typed row: names only, constraints documented ──────────────

(check-equal? (render-effect-row 'koka (row (entry "llm") (entry "db")))
              "<llm, db>"
              "Koka renders a typed effect row")
(check-equal? (render-effect-row 'koka (row)) "total"
              "an empty row is total")
(check-false (render-effect-annotation 'koka (row (entry "llm")))
             "a plain may-row needs no comment")
(check-equal? (render-effect-annotation 'koka constrained)
              "// @effects {\"llm\": {\"modality\": \"may\", \"max\": 5, \"scope\": \"reads=idx\", \"ops\": [\"query\"]}}"
              "Koka documents the fields it cannot type")

;; A prohibition is documented, never granted by the typed row.
(let ([prohibited (row (entry "db" #:modality 'must-not))])
  (check-equal? (render-effect-row 'koka prohibited) "total"
                "must-not is not granted as a typed effect")
  (check-true (string-contains? (render-effect-documented 'koka prohibited)
                                "\"modality\": \"must-not\"")
              "the prohibition is still documented"))

;; ── unknown is not a concrete effect ────────────────────────────────

(let ([r (row (entry "llm") (entry "unknown" #:modality 'unknown))])
  (check-equal? (render-effect-row 'koka r) "<llm>"
                "unknown entries do not reach the typed row")
  (check-false (string-contains? (render-effect-row 'python r) "unknown")
               "unknown entries are not materialized")
  (check-true (string-contains? (render-effect-row 'python r) "llm")
              "the concrete part of the row still renders"))

(displayln "All emit-effects rendering tests passed.")
