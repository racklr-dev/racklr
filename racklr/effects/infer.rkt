#lang racket

;; Effect inference (Pass 2.4).
;;
;; Computes effect rows for functions lacking annotations via a monotone
;; fixpoint over the call graph. Semantics follow docs/effects-proposal.md §6:
;;   - build the call graph over named uir-fn nodes
;;   - initialize every function's row to ⊥ (empty)
;;   - iterate to fixpoint: row = union(direct effects, callee rows)
;;   - direct effects: llm/openai → llm, db/database/sql/Model.objects → db,
;;     fetch/axios → network, revalidatePath/invalidate → cache
;;   - unresolvable (higher-order) calls → an entry named 'unknown with
;;     modality 'unknown, deferred to runtime
;;
;; This module is the public facade and the per-file fixpoint + write-back.
;; The supporting machinery lives in sibling modules:
;;   walk.rkt          — structural traversal
;;   shape.rkt         — callee / name / function shape predicates
;;   effect-roots.rkt  — effect name tables + direct-effect-of-callee
;;   pure-roots.rkt    — pure name tables
;;   collect.rkt       — program-surface collection + MRO
;;   rows.rkt          — effect-row algebra
;;   classify.rkt      — classify-call / classify-nonedge
;;   whole-program.rkt — cross-file fixpoint (infer-effects-all)

(require racklr/uir
         racklr/effects/contract
         racklr/effects/ts-annot
         racklr/effects/walk
         racklr/effects/shape
         racklr/effects/effect-roots
         racklr/effects/collect
         racklr/effects/rows
         racklr/effects/classify
         racklr/effects/whole-program)

(provide
  infer-effects
  infer-effects-all
  annotate-effects
  collect-functions
  collect-methods
  collect-function-params
  collect-method-params
  classify-call
  direct-effect-of-callee
  effect-op-of-callee
  empty-row)

;; Compute effect rows: name-symbol → uir-effect-row, via a monotone fixpoint
;; over the call graph. Every callee named by an `edge` classification must be
;; present in `known` (the intra-program name table); a name missing from
;; `known` falls back to an empty row.
(define (compute-effect-rows fns methods class-names supers known)
  (define direct-rows (make-hash))  ;; name → uir-effect-row (direct only)
  (define edges (make-hash))        ;; name → (listof name-symbol)

  (define all-names (append (hash-keys fns) (hash-keys methods)))
  (for ([n (in-list all-names)])
    (define body (if (hash-has-key? fns n)
                     (hash-ref fns n)
                     (hash-ref methods n)))
    (define owner (owner-of-name methods n))
    (define effects '())
    (define callees '())
    (define (visit! node)
      (if (uir-call? node)
          (let ([cls (classify-call (uir-call-callee node) known owner class-names supers)])
            (case (car cls)
              [(edge) (set! callees (cons (cdr cls) callees))]
              [(effect) (set! effects (cons (cdr cls) effects))]
              [(pure) (void)]
              [(unknown) (set! effects (cons 'unknown effects))]))
          (void)))
    (walk-body body visit!)
    (hash-set! direct-rows n (effects->row effects))
    (hash-set! edges n (sort (remove-duplicates callees) symbol<?)))

  (define rows (make-hash))
  (for ([n (in-list all-names)])
    (hash-set! rows n (hash-ref direct-rows n)))

  (let loop ()
    (define changed? #f)
    (for ([n (in-list all-names)])
      (define current (hash-ref rows n))
      (define callee-rows
        (map (λ (g) (hash-ref rows g (empty-row))) (hash-ref edges n)))
      (define new-row
        (foldl infer-row-union (hash-ref direct-rows n) callee-rows))
      (if (equal? current new-row)
          (void)
          (begin
            (set! changed? #t)
            (hash-set! rows n new-row))))
    (if changed? (loop) (void)))

  rows)

;; Compute the fixpoint effect rows for a single program: name-symbol →
;; uir-effect-row. Only calls into functions/methods defined in `program` (or a
;; program passed together with it) resolve as edges; anything else falls to
;; `unknown`.
(define (infer-effects program)
  (define fns (collect-functions program))
  (define methods (collect-methods program))
  (define class-names (collect-class-names program))
  (define supers (collect-supers program))
  (define known (make-hash))
  (for ([(n _) (in-hash fns)]) (hash-set! known n #t))
  (for ([(n _) (in-hash methods)]) (hash-set! known n #t))
  (compute-effect-rows fns methods class-names supers known))

;; ── write-back ──────────────────────────────────────────────────────

;; Rewrite a program so every named function carries its inferred effect row
;; in the `effects` field: named uir-fn nodes, plus the JS/TS `set!`-bound
;; function declarations (writing back into the wrapped uir-fn).
(define (annotate-effects program)
  (define rows (infer-effects program))
  ;; Materialize an inferred row for emission: 'unknown is a checker signal
  ;; ("could not verify"), not a runtime effect, and the runtime guards reject
  ;; the 'unknown modality. So it is dropped here; an all-unknown row becomes
  ;; empty and the function stays unannotated.
  (define (concrete-row r)
    (uir-effect-row
     (filter (λ (e) (not (eq? (effect-name-symbol e) 'unknown)))
             (uir-effect-row-entries r))))
  ;; The concrete (runtime-guardable) inferred row for a named function, or #f
  ;; when there is nothing to emit (no inference, an empty row, or an
  ;; all-'unknown row).
  (define (inferred-row nm)
    (define inferred (hash-ref rows nm #f))
    (and inferred
         (pair? (uir-effect-row-entries inferred))
         (let ([c (concrete-row inferred)])
           (and (pair? (uir-effect-row-entries c)) c))))
  (define (with-effects fn nm)
    (define existing (uir-fn-effects fn))
    (cond
      ;; A hand-written row wins: it carries budgets/scopes inference cannot
      ;; recover, so re-inference must not overwrite it.
      [(uir-effect-row? existing) fn]
      [(inferred-row nm) => (λ (c) (struct-copy uir-fn fn [effects c]))]
      [else fn]))
  ;; Arrow and function expressions have no effects slot. A guardable row is
  ;; represented by wrapping the value in the __withEffects(row, fn) call the
  ;; JS emitter renders.
  (define (with-effects-wrap val nm)
    (define c (inferred-row nm))
    (if c
        (uir-call (uir-symbol "__withEffects") (list c val))
        val))
  ;; A hand-written `withEffects(row)(fn)` annotation declares its row
  ;; explicitly. Rewrite it into the internal __withEffects(row, fn) call the
  ;; JS emitter renders, so it is guarded like any other effect row and
  ;; program-has-effects? sees the embedded uir-effect-row.
  (define (rewrite-with-effects val)
    (define row (extract-with-effects-row val))
    (define fn (extract-with-effects-fn val))
    (if row
        (uir-call (uir-symbol "__withEffects") (list row fn))
        val))
  (define (rewrite-value val nm)
    (cond
      [(with-effects-call? val) (rewrite-with-effects val)]
      [(uir-fn? val) (with-effects val nm)]
      [(and (uir-call? val)
            (uir-symbol? (uir-call-callee val))
            (member (uir-symbol-name (uir-call-callee val)) '("async-fn" "gen-fn"))
            (= (length (uir-call-args val)) 1)
            (uir-fn? (first (uir-call-args val))))
       (struct-copy uir-call val
                    [args (list (with-effects (first (uir-call-args val)) nm))])]
      [(or (arrow-call? val) (function-expr-call? val))
       (with-effects-wrap val nm)]
      [else val]))
  (define (rewrite n)
    (cond
      [(uir-fn? n)
       (let ([nm (uir-fn-name n)])
         (if (and nm (uir-symbol? nm))
             (with-effects n (string->symbol (uir-symbol-name nm)))
             n))]
      [(and (uir-set!? n) (uir-symbol? (uir-set!-name n)))
       (struct-copy uir-set! n
                    [value (rewrite-value (uir-set!-value n)
                                          (string->symbol (uir-symbol-name (uir-set!-name n))))])]
      ;; const/let/var declarations wrap their binding one level deep:
      ;; (call "const"/"let"/"var" (list (set! NAME <fn/arrow>))). Descend to
      ;; reach the named binding so its inferred row is written back.
      [(and (uir-call? n) (uir-symbol? (uir-call-callee n))
            (member (uir-symbol-name (uir-call-callee n)) '("var" "let" "const"))
            (= (length (uir-call-args n)) 1))
       (struct-copy uir-call n
                    [args (list (rewrite (first (uir-call-args n))))])]
       [(with-effects-call? n) (rewrite-with-effects n)]
       ;; A bare async/generator function statement ((call "async-fn" (list fn)))
       ;; is a named function (the Python `async def` encoding); write its
       ;; inferred row back into the wrapped uir-fn.
       [(wrapped-fn n)
        => (λ (fn)
             (define nm (and (uir-fn-name fn) (uir-symbol? (uir-fn-name fn))
                             (string->symbol (uir-symbol-name (uir-fn-name fn)))))
             (if nm
                 (struct-copy uir-call n [args (list (with-effects fn nm))])
                 n))]
       [else n]))
  (cond
    [(list? program) (map rewrite program)]
    [(uir-block? program)
     (struct-copy uir-block program
                  [stmts (map rewrite (uir-block-stmts program))])]
    [(uir-module? program)
     (struct-copy uir-module program
                  [body (map rewrite (uir-module-body program))])]
    [else (rewrite program)]))

(module+ main
  (displayln "racklr/effects/infer — effect inference loaded."))
