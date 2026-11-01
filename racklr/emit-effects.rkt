#lang racket

;; racklr/emit-effects.rkt — the emit contract's effect-row rendering.
;;
;; One function owns the spelling of an effect row in each supported target's
;; native vocabulary; the target emitters own where the row is placed:
;;
;;   python      {"llm": {"modality": "may", "max": 5}}   @__withEffects(...)
;;               # @effects {...}                        documented annotation
;;   javascript  {"llm": {"modality": "may", "max": 5}}   __withEffects(row, fn)
;;               /** @effects {...} */                   documented annotation
;;   typescript  same as javascript (TS emits through the JS emitter)
;;   clojure     {:llm {:modality :may :max 5}}          defn attr-map metadata
;;   koka        <llm, db>                               typed effect row
;;               // @effects {...}                       documented constraint
;;
;; A row never disappears silently. Entries that have no native spelling in a
;; target (Koka has typed effects but no budget/scope syntax; JSON has no
;; function or effect vocabulary at all) are recorded by
;; `render-effect-annotation` as a structured comment when the target supports
;; one, and otherwise documented as an explicit loss in README.md.
;;
;; `unknown` entries are not concrete effects: a runtime guard cannot enforce
;; an unclassified effect, so the contract drops them here (the obligation is to
;; eliminate the leaf in inference, not to ship it as an effect).

(require racklr/uir)

(provide
  effect-targets
  effect-row-empty?
  render-effect-row
  render-effect-annotation
  render-effect-documented)

;; Targets with a native effect vocabulary for function rows. A target absent
;; from this list has no function/effect surface at all (the JSON data target)
;; and renders no rows.
(define effect-targets '(python javascript typescript clojure koka))

;; ── Row accessors ───────────────────────────────────────────────────

;; Entries that may be materialized in an emitted guard or annotation:
;; `unknown` is a checker signal, not a concrete effect.
(define (concrete-entries row)
  (if (uir-effect-row? row)
      (filter (λ (e) (not (eq? (effect-entry-modality e) 'unknown)))
              (uir-effect-row-entries row))
      '()))

(define (effect-entry-modality e) (uir-effect-entry-modality e))
(define (effect-entry-name e) (uir-symbol-name (uir-effect-entry-name e)))

(define (effect-row-empty? row)
  (null? (concrete-entries row)))

;; The entries that a target may truthfully type as capabilities it grants.
;; A `must-not` entry is a prohibition: listing it as a Koka effect would grant
;; permission the row forbids, so the typed row omits it (the structured
;; annotation still records it).
(define (grantable-entries row)
  (filter (λ (e) (not (eq? (effect-entry-modality e) 'must-not)))
          (concrete-entries row)))

;; ── Object-literal rows (Python / JavaScript / TypeScript) ──────────

;; Render one entry's constraint object as `{"modality": "may", ...}`; the key
;; order is fixed (modality, max, scope, ops) so emitted text is stable.
(define (object-constraints e)
  (define base (list (format "~s: ~s" "modality"
                             (symbol->string (effect-entry-modality e)))))
  (define with-budget
    (if (uir-effect-entry-budget e)
        (cons (format "~s: ~a" "max" (uir-effect-entry-budget e)) base)
        base))
  (define with-scope
    (if (uir-effect-entry-scope e)
        (cons (format "~s: ~s" "scope"
                      (uir-symbol-name (uir-effect-entry-scope e)))
              with-budget)
        with-budget))
  (define with-ops
    (if (pair? (uir-effect-entry-ops e))
        (cons (format "~s: [~a]" "ops"
                      (string-join
                       (map (λ (o) (format "~s" (uir-symbol-name o)))
                            (uir-effect-entry-ops e))
                       ", "))
              with-scope)
        with-scope))
  (string-append "{" (string-join (reverse with-ops) ", ") "}"))

(define (object-row row)
  (string-append
   "{"
   (string-join
    (map (λ (e) (format "~s: ~a" (effect-entry-name e) (object-constraints e)))
         (concrete-entries row))
    ", ")
   "}"))

;; ── Clojure metadata row ────────────────────────────────────────────

(define (clojure-constraints e)
  (define parts (list (format ":modality :~a"
                              (symbol->string (effect-entry-modality e)))))
  (define with-budget
    (if (uir-effect-entry-budget e)
        (cons (format ":max ~a" (uir-effect-entry-budget e)) parts)
        parts))
  (define with-scope
    (if (uir-effect-entry-scope e)
        (cons (format ":scope ~s" (uir-symbol-name (uir-effect-entry-scope e)))
              with-budget)
        with-budget))
  (define with-ops
    (if (pair? (uir-effect-entry-ops e))
        (cons (format ":ops [~a]"
                      (string-join
                       (map (λ (o) (format "~s" (uir-symbol-name o)))
                            (uir-effect-entry-ops e))
                       " "))
              with-scope)
        with-scope))
  (string-append "{" (string-join (reverse with-ops) " ") "}"))

(define (clojure-row row)
  (string-append
   "{"
   (string-join
    (map (λ (e) (format ":~a ~a" (effect-entry-name e) (clojure-constraints e)))
         (concrete-entries row))
    " ")
   "}"))

;; ── Koka typed effect row ───────────────────────────────────────────

(define (koka-row row)
  (define names (map effect-entry-name (grantable-entries row)))
  (if (null? names)
      "total"
      (string-append "<" (string-join names ", ") ">")))

;; A Koka typed row carries names only. Any other row field (budget, scope,
;; ops, must-not) has no native Koka spelling, so it is rendered as a
;; structured annotation instead of being dropped.
(define (koka-row-lossy? row)
  (for/or ([e (in-list (concrete-entries row))])
    (or (not (eq? (effect-entry-modality e) 'may))
        (uir-effect-entry-budget e)
        (uir-effect-entry-scope e)
        (pair? (uir-effect-entry-ops e)))))

;; ── Public contract ─────────────────────────────────────────────────

;; The row in the target's native effect vocabulary, or #f when the target has
;; no function-effect surface.
(define (render-effect-row target row)
  (case target
    [(python javascript typescript) (object-row row)]
    [(clojure) (clojure-row row)]
    [(koka) (koka-row row)]
    [else #f]))

;; A structured comment that records the whole row in the target's native
;; comment syntax: `# @effects {...}` (Python), `/** @effects {...} */`
;; (JavaScript/TypeScript), `// @effects {...}` (Koka). #f for targets whose
;; native annotation is the row itself (Clojure metadata).
(define (render-effect-documented target row)
  (and (not (effect-row-empty? row))
       (case target
         [(python) (format "# @effects ~a" (object-row row))]
         [(javascript typescript) (format "/** @effects ~a */" (object-row row))]
         [(koka) (format "// @effects ~a" (object-row row))]
         [else #f])))

;; The documented annotation a target needs on top of its native row: for
;; Python/JS/TS that is always (the row is otherwise only enforced at runtime);
;; for Koka it is needed only when a typed row cannot spell a field of the row.
;; #f when the target needs none (Clojure's metadata row carries every field).
(define (render-effect-annotation target row)
  (and (not (effect-row-empty? row))
       (case target
         [(python javascript typescript) (render-effect-documented target row)]
         [(koka)
          (and (koka-row-lossy? row)
               (render-effect-documented target row))]
         [else #f])))

(module+ main
  (displayln "racklr/emit-effects — effect-row emit contract loaded."))
