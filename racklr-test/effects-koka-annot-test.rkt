#lang racket

;; Koka per-target effect-row mapping (the emit contract's Koka rendering).
;;
;; Koka's native effect vocabulary is the typed effect row in a function's
;; return type, so a function with a known row must emit `: <llm, db> ret`.
;; Budgets, scopes, ops and prohibitions have no typed-system spelling; the
;; contract documents them in a structured `// @effects` comment instead of
;; dropping them, and a `must-not` entry is never granted as a typed effect.

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-koka
         racklr/emit-koka)

(define-values (koka-parse koka-tokenize koka-tok-type koka-tok-value)
  (gen-and-load-layout "../grammars/koka/Koka.g4"))

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))

;; One effect entry: (name modality ops budget scope).
(define (entry name #:modality [modality 'may] #:max [budget #f]
               #:ops [ops '()] #:scope [scope #f])
  (uir-effect-entry (sym name) modality (map sym ops) budget
                    (and scope (sym scope))))

(define (row . entries) (uir-effect-row entries))

(define (named-fn name r)
  (uir-fn (sym name) '() (uir-number "1") (sym "int") r))

(define (prog . stmts) (uir-block stmts))

(define (emitted r)
  (emit-koka (prog (named-fn "summarize" r))))

(define (row-names r)
  (map (λ (e) (uir-symbol-name (uir-effect-entry-name e)))
       (uir-effect-row-entries r)))

;; ── 1. typed effects are Koka's native row ──────────────────────────

(let ([out (emitted (row (entry "llm") (entry "db")))])
  (check-true (string-contains? out "fun summarize() : <llm, db> int")
              "known row renders as the typed effect row"))

;; An empty row is `total` (pure), with no documentation comment.
(let ([out (emitted (row))])
  (check-true (string-contains? out "fun summarize() : total int")
              "empty row renders as total")
  (check-false (string-contains? out "@effects")
               "an empty row needs no annotation"))

;; ── 2. non-typed fields are documented, not dropped ─────────────────

(let ([out (emitted (row (entry "llm" #:max 5 #:scope "reads=idx"
                                 #:ops '("query"))))])
  (check-true (string-contains? out "fun summarize() : <llm> int")
              "the typed row still carries the effect name")
  (check-true (string-contains?
               out
               "// @effects {\"llm\": {\"modality\": \"may\", \"max\": 5, \"scope\": \"reads=idx\", \"ops\": [\"query\"]}}")
              "budget, scope and ops are preserved in the structured comment"))

;; ── 3. a prohibition is documented, never granted ───────────────────

(let ([out (emitted (row (entry "db" #:modality 'must-not)))])
  (check-false (string-contains? out "<db>")
               "must-not must not appear in the typed effect row")
  (check-true (string-contains? out "fun summarize() : total int")
              "a prohibition-only row is total")
  (check-true (string-contains? out "\"db\": {\"modality\": \"must-not\"}")
              "the prohibition is documented in the structured comment"))

;; ── 4. a row on a function with no declared result type ────────────
;;
;; Koka's grammar only accepts an effect row in front of a result type, so the
;; typed position is unavailable; the row must still be emitted, as the
;; documented `// @effects` row, and the output must stay valid Koka.

(let* ([row* (row (entry "llm" #:max 5))]
       [fn-node (uir-fn (sym "g") '() (uir-number "1") #f row*)]
       [out (emit-koka (prog fn-node))])
  (check-true (string-contains? out "// @effects {\"llm\": {\"modality\": \"may\", \"max\": 5}}")
              "the row is documented when no result type follows the row")
  (check-false (string-contains? out "<llm>")
               "no ill-formed effects-only rettype is emitted")
  (check-true (any-tree? (koka-parse out))
              "the emitted function is valid Koka"))

;; ── 5. source typed row -> UIR -> emit is the native form ───────────

(define typed-src "fun f(x: int) : <exn, div> int\n  1")
(define typed-uir (lower-koka (koka-parse typed-src) koka-tok-type koka-tok-value))
(check-true (uir-effect-row? (uir-fn-effects (first (uir-block-stmts typed-uir))))
            "the Koka front end reads a typed row into the UIR")
(define typed-out (emit-koka typed-uir))
(check-true (string-contains? typed-out "fun f(x : int) : <exn, div> int")
            "the typed row round-trips in its native form")
(check-true (any-tree? (koka-parse typed-out))
            "the emitted typed-row function re-parses")
(define typed-back (lower-koka (koka-parse typed-out) koka-tok-type koka-tok-value))
(check-equal? (row-names (uir-fn-effects (first (uir-block-stmts typed-back))))
              '("exn" "div")
              "the emitted typed row re-absorbs to the same row")

;; ── 6. an inferred known row is emitted, not discarded ──────────────

(define inferred-src "fun g(x) : int\n  llm.query(x)")
(define inferred-uir (lower-koka (koka-parse inferred-src) koka-tok-type koka-tok-value))
(define inferred-out (emit-koka inferred-uir))
(check-true (string-contains? inferred-out "fun g(x) : <llm> int")
            "an inferred row is carried into the typed effect row")
(define reabsorbed
  (lower-koka (koka-parse inferred-out) koka-tok-type koka-tok-value))
(check-equal? (row-names (uir-fn-effects (first (uir-block-stmts reabsorbed))))
              '("llm")
              "the emitted typed row re-absorbs to the same row")

;; ── 7. a pure function stays unannotated ────────────────────────────

(define pure-out
  (emit-koka (lower-koka (koka-parse "fun add(x: int, y: int)\n  x + y")
                         koka-tok-type koka-tok-value)))
(check-false (string-contains? pure-out "<")
             "a pure function emits no effect row")

(displayln "All koka-annot tests passed.")
