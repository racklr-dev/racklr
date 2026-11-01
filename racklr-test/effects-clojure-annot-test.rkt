#lang racket

;; Clojure per-target effect-row mapping (the emit contract's Clojure
;; rendering).
;;
;; Clojure's native effect vocabulary is the `defn` attr-map:
;;
;;   (defn summarize {:__effects__ {:llm {:modality :may :max 5}}} [x] ...)
;;
;; The attr-map is what the reader actually keeps on the var (an outer `^`
;; metadata map is dropped), so the emitter places the row there, and the
;; front end reads that same slot back: emit -> absorb recovers the row.

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-clojure
         racklr/emit-clojure
         racklr/effects/infer)

(define-values (clj-parse clj-tokenize clj-tok-type clj-tok-value)
  (gen-and-load "../grammars-v4/clojure/Clojure.g4"))

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))

(define (lower src)
  (lower-clojure (clj-parse src) clj-tok-type clj-tok-value))

(define (emit uir) (emit-clojure uir))

;; One effect entry: (name modality ops budget scope).
(define (entry name #:modality [modality 'may] #:max [budget #f]
               #:ops [ops '()] #:scope [scope #f])
  (uir-effect-entry (sym name) modality (map sym ops) budget
                    (and scope (sym scope))))

(define (row . entries) (uir-effect-row entries))

(define (named-fn name r)
  (uir-fn (sym name) (list (sym "x")) (uir-number "1") #f r))

(define (row-lookup r name)
  (for/or ([e (in-list (uir-effect-row-entries r))])
    (and (equal? (uir-symbol-name (uir-effect-entry-name e)) name) e)))

;; ── 1. a known row renders as defn metadata ─────────────────────────

(check-equal?
 (emit (uir-block (list (named-fn "summarize" (row (entry "llm" #:max 5))))))
 "(defn summarize {:__effects__ {:llm {:modality :may :max 5}}} [x] 1)"
 "known row is placed in the defn attr-map")

;; Every row field is native in the metadata map: modality, budget, scope, ops.
(let* ([r (row (entry "db" #:modality 'must-not #:max 2
                       #:scope "reads=search_index" #:ops '("read")))]
       [out (emit (uir-block (list (named-fn "guarded" r))))])
  (check-true (string-contains? out ":db {:modality :must-not :max 2")
              "modality and budget are preserved")
  (check-true (string-contains? out ":scope \"reads=search_index\"")
              "scope is preserved")
  (check-true (string-contains? out ":ops [\"read\"]")
              "typed ops are preserved"))

;; ── 2. emit -> absorb recovers the same row ─────────────────────────

(let* ([r (row (entry "llm" #:max 5)
               (entry "db" #:scope "reads=search_index" #:ops '("read")))]
       [out (emit (uir-block (list (named-fn "summarize" r))))]
       [back (first (uir-block-stmts (lower out)))])
  (check-true (uir-fn? back) "the emitted metadata defn re-lowers to a uir-fn")
  (check-equal? (uir-effect-row-entries (uir-fn-effects back))
                (uir-effect-row-entries r)
                "the native metadata round-trips the whole row"))

;; ── 3. an inferred known row is emitted, not discarded ──────────────

(let* ([out (emit (lower "(defn g [x] (llm/query x))"))]
       [back (first (uir-block-stmts (lower out)))])
  (check-true (string-contains? out "(defn g {:__effects__ {:llm")
              "the inferred row is rendered as native metadata")
  (check-true (uir-effect-row? (uir-fn-effects back))
              "the emitted metadata is a row the front end reads back")
  (check-equal? (uir-symbol-name (uir-effect-entry-name
                                  (row-lookup (uir-fn-effects back) "llm")))
                "llm"
                "the re-absorbed row names the inferred effect"))

;; ── 4. a pure function stays unannotated ────────────────────────────

(let ([out (emit (lower "(defn add [a b] (+ a b))"))])
  (check-false (string-contains? out ":__effects__")
               "a pure function carries no effect metadata"))

;; ── 5. a non-effects metadata map round-trips unchanged ─────────────

(check-equal? (emit (lower "^{:doc \"hi\"} (defn f [] 1)"))
              "^{:doc \"hi\"} (defn f [] 1)"
              "ordinary metadata is not confused with the effects slot")

(displayln "All clojure-annot tests passed.")
