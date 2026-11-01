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
;;   rows.rkt          — effect-row construction + algebra adapter
;;   algebra/          — the effect algebra: rows, join, fixpoint, subtyping
;;   classify.rkt      — classify-call / classify-nonedge
;;   provenance.rkt    — structured effect-edge provenance (grounds + call graph)
;;   whole-program.rkt — cross-file fixpoint (infer-effects-all)

(require racklr/uir
         racklr/effects/contract
         (prefix-in alg: racklr/effects/algebra/main)
         racklr/effects/annotation-hooks
         racklr/effects/walk
         racklr/effects/shape
         racklr/effects/effect-roots
         racklr/effects/collect
         racklr/effects/rows
         racklr/effects/classify
         racklr/effects/labels
         racklr/effects/provenance
         racklr/effects/whole-program
         racklr/effects/lexicon)

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
;;
;; Two provenance views are available, both filled on request:
;;
;;   #:provenance-table  — the compact legacy view consumed by Effervesce's
;;                         bridge checker: name → (hash effect → list of
;;                         classifier strings), a propagated entry prefixed with
;;                         the callee it came through ("g::site").
;;   #:provenance-record — the first-class record (racklr/effects/provenance.rkt)
;;                         projected into the graph substrate: every classifying
;;                         call site is a ground, and every call-graph edge is a
;;                         `prov-call` carrying the callee expression. The record
;;                         is filled even when no effect was detected, because the
;;                         call graph (edges without direct effects) is what a
;;                         provenance path walks. Each ground/call carries a
;;                         stable site origin (positions are ordinals over the
;;                         body's call nodes).
;;   #:source-file       — the source file of `program`, when the caller knows
;;                         it; it fills the `file` component of every site
;;                         origin, so per-file inference and whole-program
;;                         inference agree on a site identity.
(define (compute-effect-rows fns methods class-names supers known
                             #:provenance-table [prov-box #f]
                             #:provenance-record [rec-box #f]
                             #:lexicon [lex default-lexicon]
                             #:source-file [source-file #f])
  (define tokens (lexicon-tokens lex))
  (define direct-rows (make-hash))  ;; name → algebra-row (direct entries only)
  (define edges (make-hash))        ;; name → (listof name-symbol)
  (define direct-prov (and prov-box (make-hash)))  ;; name → hash effect → sites
  (define prov (and rec-box (make-provenance)))

  ;; Merge a classifying call-site string into an effect→sites hash, dropping
  ;; duplicates and preserving first-seen order (legacy table view).
  (define (sites-add! h eff site)
    (hash-update! h eff (λ (l) (if (member site l) l (cons site l))) '()))

  (define all-names (append (hash-keys fns) (hash-keys methods)))
  (for ([n (in-list all-names)])
    (define body (if (hash-has-key? fns n)
                     (hash-ref fns n)
                     (hash-ref methods n)))
    (define owner (owner-of-name methods n))
    (define effects '())
    (define callees '())
    ;; The stable positional discriminator of a site: the ordinal of the call
    ;; node in this body's pre-order walk. It resets per function, is derived
    ;; from source structure only, and is therefore identical across runs.
    (define call-position 0)
    (define (visit! node)
      (cond
        [(uir-call? node)
         (define position call-position)
         (set! call-position (add1 call-position))
         (let* ([callee (uir-call-callee node)]
                [cls (classify-call callee known owner class-names supers
                                    #:lexicon lex)]
                [site (callee-site-string callee)])
           (case (car cls)
             [(edge)
              (set! callees (cons (cdr cls) callees))
              (when prov
                (provenance-add-call! prov
                                      (prov-call (function-fq n)
                                                 (function-fq (cdr cls))
                                                 site
                                                 (make-call-origin (function-fq n) callee
                                                                   #:file source-file
                                                                   #:position position))))]
             [(effect)
              ;; (cdr cls) is the effect-name symbol (effects->row builds the
              ;; entry), so it keys both provenance views directly.
              (define e (cdr cls))
              (set! effects (cons e effects))
              (when direct-prov
                (sites-add! (hash-ref! direct-prov n make-hash) e site))
              (when prov
                (provenance-add-ground! prov
                                        (make-callee-site (function-fq n) e callee
                                                          #:file source-file
                                                          #:position position)))]
             [(pure) (void)]
             [(unknown)
              (set! effects (cons 'unknown effects))
              (when direct-prov
                (sites-add! (hash-ref! direct-prov n make-hash) 'unknown site))
              (when prov
                ;; Per-file inference has no whole-program receiver
                ;; environment, so an unresolved site's evidence is the local
                ;; call expression: a worked instance (local/literal). The
                ;; whole-program pass refines a parameter-rooted site to
                ;; 'census-param.
                (provenance-add-ground! prov
                                        (make-callee-site (function-fq n) 'unknown callee
                                                          #:kind 'unresolved
                                                          #:coverage
                                                          (unresolved-coverage callee #f tokens)
                                                          #:file source-file
                                                          #:position position)))]))]
        [else (void)]))
    (walk-body body visit!)
    (hash-set! direct-rows n (effects->algebra-row effects))
    (hash-set! edges n (sort (remove-duplicates callees) symbol<?)))

  ;; The fixpoint is the algebra's: iterate `join` (the least upper bound over
  ;; rows) from the direct rows until the table stops changing. This is exactly
  ;; the implicit union the pass used to fold by hand, now the algebra's
  ;; monotone operation, so the result is the least fixed point over the call
  ;; graph and inference results are unchanged.
  (define rows (alg:least-fixpoint direct-rows edges))

  ;; Legacy table: a second monotone fixpoint over the rendered call-site
  ;; strings, independent of the rows. A propagated entry keeps the callee's
  ;; classifying sites, prefixed with the callee it came through (the rendered
  ;; call chain).
  (define prov-strings (and direct-prov (make-hash)))
  (when prov-strings
    (for ([n (in-list all-names)])
      (hash-set! prov-strings n (hash-copy (hash-ref direct-prov n (make-hash)))))
    (let loop ()
      (define changed? #f)
      (for ([n (in-list all-names)])
        (define np (hash-copy (hash-ref direct-prov n (make-hash))))
        (for ([g (in-list (hash-ref edges n))])
          (for ([(e sites) (in-hash (hash-ref prov-strings g (make-hash)))])
            (for ([s (in-list sites)])
              (sites-add! np e (format "~a::~a" g s)))))
        (unless (equal? np (hash-ref prov-strings n))
          (set! changed? #t)
          (hash-set! prov-strings n np)))
      (if changed? (loop) (void))))

  (when prov-box (set-box! prov-box prov-strings))
  (when rec-box (set-box! rec-box prov))

  ;; Materialize the algebra rows back to the UIR row contract: entries
  ;; unchanged, provenance side table dropped (the UIR row has no slot for it).
  (for/hash ([(n r) (in-hash rows)])
    (values n (alg:algebra-row->uir r))))


;; Compute the fixpoint effect rows for a single program: name-symbol →
;; uir-effect-row. Only calls into functions/methods defined in `program` (or a
;; program passed together with it) resolve as edges; anything else falls to
;; `unknown`.
(define (infer-effects program
                       #:provenance-table [prov-box #f]
                       #:provenance-record [rec-box #f]
                       #:lexicon [lex default-lexicon]
                       #:source-file [source-file #f])
  (define fns (collect-functions program))
  (define methods (collect-methods program))
  (define class-names (collect-class-names program))
  (define supers (collect-supers program))
  (define known (make-hash))
  (for ([(n _) (in-hash fns)]) (hash-set! known n #t))
  (for ([(n _) (in-hash methods)]) (hash-set! known n #t))
  (compute-effect-rows fns methods class-names supers known
                       #:provenance-table prov-box
                       #:provenance-record rec-box
                       #:lexicon lex
                       #:source-file source-file))

;; ── write-back ──────────────────────────────────────────────────────

;; Rewrite a program so every named function carries its inferred effect row
;; in the `effects` field: named uir-fn nodes, plus the JS/TS `set!`-bound
;; function declarations (writing back into the wrapped uir-fn).
;;
;; `#:wrap-anonymous?` controls the JS/Python runtime-guard convention: when
;; true (the default) an arrow/function expression with no effects slot gets
;; wrapped in the internal `__withEffects(row, fn)` call those emitters render.
;; Targets with their own row placement (Koka's typed effect row, Clojure's
;; defn metadata) pass #f so their emitters see the row on the uir-fn instead.
(define (annotate-effects program
                          #:wrap-anonymous? [wrap? #t]
                          #:lexicon [lex default-lexicon]
                          #:annotation-hooks [hooks default-annotation-hooks])
  (define tokens (lexicon-tokens lex))
  (define with-effects-call? (annotation-hooks-with-effects-call? hooks))
  (define extract-with-effects-row (annotation-hooks-extract-row hooks))
  (define extract-with-effects-fn (annotation-hooks-extract-fn hooks))
  (define rows (infer-effects program #:lexicon lex))
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
    (define annotation-callee (tokens-annotation-callee tokens))
    (if (and c annotation-callee)
        (uir-call (uir-symbol annotation-callee) (list c val))
        val))
  ;; A hand-written `withEffects(row)(fn)` annotation declares its row
  ;; explicitly. Rewrite it into the internal __withEffects(row, fn) call the
  ;; JS emitter renders, so it is guarded like any other effect row and
  ;; program-has-effects? sees the embedded uir-effect-row.
  (define (rewrite-with-effects val)
    (define row (extract-with-effects-row val))
    (define fn (extract-with-effects-fn val))
    (define annotation-callee (tokens-annotation-callee tokens))
    (if (and row annotation-callee)
        (uir-call (uir-symbol annotation-callee) (list row fn))
        val))
  (define (rewrite-value val nm)
    (cond
      [(with-effects-call? val) (rewrite-with-effects val)]
      [(uir-fn? val) (with-effects val nm)]
      [(and (uir-call? val)
            (uir-symbol? (uir-call-callee val))
            (set-member? (tokens-wrapper-callees tokens)
                         (uir-symbol-name (uir-call-callee val)))
            (= (length (uir-call-args val)) 1)
            (uir-fn? (first (uir-call-args val))))
       (struct-copy uir-call val
                    [args (list (with-effects (first (uir-call-args val)) nm))])]
      [(or (arrow-call? val) (function-expr-call? val))
       (if wrap? (with-effects-wrap val nm) val)]
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
            (set-member? (tokens-binding-callees tokens)
                         (uir-symbol-name (uir-call-callee n)))
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
