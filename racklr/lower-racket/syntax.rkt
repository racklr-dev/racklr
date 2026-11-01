#lang racket

;; racklr/lower-racket/syntax.rkt
;;
;; Shared building blocks of the Racket lowering hook: the lowering context,
;; the lexical environment, literal/data lowering, parameter and binding
;; shapes, and call-callee resolution.
;;
;; Environment design: an environment maps a lexically bound name to the UIR
;; symbol that represents it. Parameters and let-bound values map to themselves
;; (a call through such a name is a higher-order call and stays bare → tracked
;; unknown). Internal definitions and anonymous functions get a unique local
;; symbol (name@N) so two helpers with the same name in one module do not merge
;; into one call-graph node. Top-level definitions and resolved imports map to
;; their mangled "<module id>::<name>" call-graph identity.

(require racket/list
         racket/string
         racklr/uir
         "scan.rkt"
         "resolve.rkt")

(provide ctx? ctx-module-id ctx-info ctx-imports
         make-ctx
         next-local-name
         extend-env
         extended-env
         env-ref
         callee-uir
         definition-symbol
         bare-symbol
         lower-datum
         lower-params
         binding-ids
         binding-uir)

;; Lowering context: the current module's identity, scanned metadata, resolved
;; import table (local name → (defining-module-id . local-name)) and a
;; deterministic counter for unique local function symbols.
(struct ctx (module-id info imports counter) #:transparent)

(define (make-ctx module-id info imports)
  (ctx module-id info imports (box 0)))

;; A deterministic unique symbol for a nested function / internal definition.
(define (next-local-name ctx base)
  (define n (unbox (ctx-counter ctx)))
  (set-box! (ctx-counter ctx) (add1 n))
  (string->symbol (format "~a@~a" base n)))

;; ── environment ────────────────────────────────────────────────────

;; Extend an environment with names bound to themselves.
(define (extend-env env names)
  (for/fold ([env env]) ([n (in-list names)])
    (cond
      [(symbol? n) (hash-set env n n)]
      [else env])))

;; Extend with explicit name → emitted-symbol pairs.
(define (extended-env env pairs)
  (for/fold ([env env]) ([pr (in-list pairs)])
    (hash-set env (car pr) (cdr pr))))

;; The UIR symbol a lexically bound name denotes, or #f.
(define (env-ref env name) (hash-ref env name #f))

;; ── names ──────────────────────────────────────────────────────────

;; Top-level definitions and resolved imports are mangled to their defining
;; module so the call graph is unambiguous across modules that reuse names.
(define (definition-symbol ctx name)
  (uir-symbol (symbol->string (mangle-name (ctx-module-id ctx) name))))

(define (bare-symbol name) (uir-symbol (symbol->string name)))

;; A callee reference: lexical bindings first; then the module's own
;; definitions; then resolved imports; otherwise the bare name, which the
;; effect classifier may resolve through the stdlib token lexicon or else
;; report as tracked unknown.
(define (callee-uir ctx env name)
  (define bound (env-ref env name))
  (cond
    [bound (uir-var (bare-symbol bound))]
    [(hash-has-key? (module-info-defines (ctx-info ctx)) name)
     (uir-var (definition-symbol ctx name))]
    [else
     (define target (hash-ref (ctx-imports ctx) name #f))
     (if target (uir-var (uir-symbol (symbol->string (mangle-target target))))
         (uir-var (bare-symbol name)))]))

;; ── literals ───────────────────────────────────────────────────────

(define (lower-datum d)
  (cond
    [(symbol? d) (uir-symbol (symbol->string d))]
    [(number? d) (uir-number (number->string d))]
    [(string? d) (uir-string d)]
    [(boolean? d) (uir-bool d)]
    [(char? d) (uir-string (string d))]
    [(keyword? d) (uir-symbol (format "~a" d))]
    [(null? d) (uir-list '())]
    [(pair? d) (uir-list (list (lower-datum (car d)) (lower-datum (cdr d))))]
    [(vector? d) (uir-list (map lower-datum (vector->list d)))]
    [else (uir-null)]))

;; ── parameter / binding shapes ─────────────────────────────────────

;; Parameter list → (listof uir-typed-param), unwrapping optional [id default]
;; entries, keyword formals and a dotted rest argument.
(define (lower-params ps)
  (cond
    [(symbol? ps) (list (uir-symbol (symbol->string ps)))]
    [(null? ps) '()]
    [(pair? ps)
     (define head (let ([p (car ps)])
                    (cond
                      [(symbol? p) p]
                      [(and (pair? p) (symbol? (car p))) (car p)]
                      [else #f])))
     (append (if head (list (uir-symbol (symbol->string head))) '())
             (lower-params (cdr ps)))]
    [else (list (uir-symbol (symbol->string ps)))]))

;; Name symbols bound by a binding target (`x`, `(a b)`, `(a . rest)`, `()`).
(define (binding-ids b)
  (cond
    [(symbol? b) (list b)]
    [(pair? b) (append (binding-ids (car b)) (binding-ids (cdr b)))]
    [else '()]))

;; A binding form `(target value)` → uir nodes evaluating the value and binding
;; each name (names bind to themselves: a value binding is never a call-graph
;; node).
(define (binding-uir ctx env b expr-fn)
  (cond
    [(and (pair? b) (symbol? (car b)))
     (values (list (uir-set! (bare-symbol (car b))
                             (if (pair? (cdr b)) (expr-fn ctx env (cadr b)) (uir-null))))
             (binding-ids (car b)))]
    [(and (pair? b) (pair? (car b)))
     (values (list (expr-fn ctx env (if (pair? (cdr b)) (cadr b) (uir-null))))
             (binding-ids (car b)))]
    [(symbol? b) (values '() (list b))]
    [else (values '() '())]))
