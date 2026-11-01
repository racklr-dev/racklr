#lang racket

;; racklr/lower-racket/expr.rkt
;;
;; Expression / body lowering for the Racket front end: literals, calls,
;; function bodies with internal definitions, and definition forms. Special
;; control forms live in special.rkt; lexical naming and callee resolution live
;; in syntax.rkt.
;;
;; Effect-honest rules:
;;   - a symbol-headed form that is not a core special form lowers to
;;     `uir-call`, with the callee resolved through the module's definitions and
;;     imports or left bare for the effect-token lexicon;
;;   - internal definitions and anonymous functions get unique local symbols,
;;     so a recursive helper contributes its own row instead of merging with a
;;     same-named helper elsewhere;
;;   - quoted data, patterns and syntax templates never become calls.

(require racket/list
         racklr/uir
         "scan.rkt"
         "resolve.rkt"
         "syntax.rkt"
         "special.rkt")

(provide lower-expr
         lower-body
         lower-define-form
         lower-struct-form)

;; A definition form at module scope keeps its mangled module name; inside a
;; function body the definition is local and gets a unique local symbol.
(define (definition-name ctx name mangle? [allocated (hash)])
  (if mangle?
      (mangle-name (ctx-module-id ctx) name)
      (hash-ref allocated name (λ () (next-local-name ctx name)))))

;; ── expression dispatch ────────────────────────────────────────────

(define (lower-expr ctx env e)
  (cond
    [(symbol? e)
     (define bound (env-ref env e))
     (cond
       [bound (uir-var (bare-symbol bound))]
       [(hash-has-key? (module-info-defines (ctx-info ctx)) e)
        (uir-var (definition-symbol ctx e))]
       [else
        (define target (hash-ref (ctx-imports ctx) e #f))
        (if target
            (uir-var (uir-symbol (symbol->string (mangle-target target))))
            (uir-var (bare-symbol e)))])]
    [(number? e) (uir-number (number->string e))]
    [(string? e) (uir-string e)]
    [(boolean? e) (uir-bool e)]
    [(char? e) (uir-string (string e))]
    [(keyword? e) (uir-symbol (format "~a" e))]
    [(vector? e) (uir-list (map (λ (x) (lower-expr ctx env x)) (vector->list e)))]
    [(null? e) (uir-list '())]
    [(pair? e)
     (cond
       [(not (list? e)) (lower-datum e)]
       [(special-form? (car e)) (lower-special-form ctx env (car e) (cdr e) lower-expr lower-body)]
       [(symbol? (car e))
        (define lowered-args (map (λ (a) (lower-expr ctx env a)) (cdr e)))
        (cond
          ;; Racket's raising primitives are language constructs, not library
          ;; calls: they abort with an exception and perform no domain effect.
          ;; Lowering them as the UIR raise construct keeps that effect-free
          ;; fact structural instead of a library token in the shared lexicon.
          [(raise-primitive? ctx env (car e)) (uir-call (uir-symbol "raise") lowered-args)]
          [else (uir-call (callee-uir ctx env (car e)) lowered-args)])]
       [else
        (uir-call (lower-expr ctx env (car e))
                  (map (λ (a) (lower-expr ctx env a)) (cdr e)))])]
    [else (uir-null)]))

;; Racket raising primitives, when they are not locally bound or shadowed by a
;; module definition (a bare `error` name would otherwise be indistinguishable
;; from an arbitrary host-code callable in the shared effect lexicon).
(define raise-primitives
  '(error raise raise-user-error raise-arguments-error raise-result-error
    raise-arity-error raise-type-error raise-range-error raise-mismatch-error
    raise-unknown-error))

(define (raise-primitive? ctx env name)
  (and (memq name raise-primitives)
       (not (hash-has-key? env name))
       (not (hash-has-key? (module-info-defines (ctx-info ctx)) name))
       (not (hash-ref (ctx-imports ctx) name #f))))

;; ── bodies ─────────────────────────────────────────────────────────

(define (internal-define? f)
  (and (pair? f) (symbol? (car f))
       (memq (car f) '(define define-values define/match define-syntax
                        define-syntax-rule struct require provide))))

;; Internal definitions are letrec-scoped: every definition's name is allocated
;; and bound before any form is lowered, so a forward reference or mutual
;; recursion between local helpers resolves to the local call-graph node
;; instead of falling to a bare (unknown) name.
(define (lower-body ctx env forms)
  (define allocated (allocate-body-names ctx forms))
  (define env* (extended-env env (hash->list allocated)))
  (let loop ([forms forms] [acc '()])
    (cond
      [(null? forms) (uir-block (reverse acc))]
      [else
       (define f (car forms))
       (cond
         [(internal-define? f)
          (define-values (nodes _env) (lower-define-form ctx env* f #f allocated))
          (loop (cdr forms) (append (reverse nodes) acc))]
         [else (loop (cdr forms) (cons (lower-expr ctx env* f) acc))])])))

;; Pre-allocate the emitted symbol of every internal definition in a body.
(define (allocate-body-names ctx forms)
  (for/fold ([h (hash)]) ([f (in-list forms)])
    (if (internal-define? f) (allocate-define-names ctx f h) h)))

(define (allocate-define-names ctx form h)
  (case (car form)
    [(define)
     (define target (second form))
     (define name (cond
                    [(pair? target) (if (pair? (car target)) (car (car target)) (car target))]
                    [(symbol? target) target]
                    [else #f]))
     (if (symbol? name) (hash-set h name (next-local-name ctx name)) h)]
    [(define/match)
     (define target (second form))
     (define name (cond [(pair? target) (car target)] [(symbol? target) target] [else #f]))
     (if (symbol? name) (hash-set h name (next-local-name ctx name)) h)]
    [(define-values)
     (for/fold ([h h]) ([n (in-list (binding-ids (second form)))])
       (hash-set h n (next-local-name ctx n)))]
    [(struct)
     (define name (second form))
     (cond
       [(not (symbol? name)) h]
       [else
        (define has-fields? (and (pair? (cddr form)) (list? (third form))))
        (define fields (if has-fields? (third form) '()))
        (define options (if has-fields? (cdddr form) (cddr form)))
        (define gens (hash-keys (struct-generated-names name fields options)))
        (for/fold ([h h]) ([n (in-list (cons name gens))])
          (hash-set h n (next-local-name ctx n)))])]
    [else h]))

;; ── definitions ────────────────────────────────────────────────────

;; Returns (values nodes env): the UIR defining the name, and the environment
;; binding it for the following forms.
(define (lower-define-form ctx env form mangle? [allocated (hash)])
  (case (car form)
    [(define) (lower-plain-define ctx env form mangle? allocated)]
    [(define-values) (lower-define-values ctx env form mangle? allocated)]
    [(define/match) (lower-define-match ctx env form mangle? allocated)]
    [(define-syntax define-syntax-rule) (values '() env)]
    [(struct) (lower-struct-form ctx env form mangle? allocated)]
    [(require) (values (list (uir-import (uir-string "require") '())) env)]
    [(provide) (values (list (uir-export '())) env)]
    [else (values '() env)]))

(define (lower-plain-define ctx env form mangle? allocated)
  (define target (second form))
  (cond
    [(pair? target)
     ;; (define (f args ...) body ...) / curried (define ((f a) b) ...)
     (define name (if (pair? (car target)) (car (car target)) (car target)))
     (define ps (if (pair? (car target))
                    (append (cdr (car target)) (cdr target))
                    (cdr target)))
     (lower-fn-define ctx env name ps (cddr form) mangle? allocated)]
    [(symbol? target)
     (define value (if (pair? (cddr form)) (third form) (uir-null)))
     (if (and (pair? value) (memq (car value) '(lambda λ)))
         (lower-fn-define ctx env target (second value) (cddr value) mangle? allocated)
         (lower-value-define ctx env target value mangle? allocated))]
    [else (values '() env)]))

(define (lower-fn-define ctx env name ps body mangle? allocated)
  (define emitted (definition-name ctx name mangle? allocated))
  (define param-names (binding-ids ps))
  ;; A local function can refer to itself (letrec-style): bind its own name in
  ;; the body environment. A module-level name resolves through the module's
  ;; definition table instead.
  (define env*
    (if mangle?
        (extend-env env param-names)
        (extended-env (extend-env env param-names)
                      (if (symbol? name) (list (cons name emitted)) '()))))
  (values (list (uir-fn (bare-symbol emitted) (lower-params ps) (lower-body ctx env* body) #f #f))
          (if mangle?
              env
              (extended-env env (if (symbol? name) (list (cons name emitted)) '())))))

(define (lower-value-define ctx env name value mangle? allocated)
  (define emitted (definition-name ctx name mangle? allocated))
  (define env* (if mangle? env (extended-env env (list (cons name emitted)))))
  (values (list (uir-set! (bare-symbol emitted) (lower-expr ctx env* value)))
          (if mangle? env env*)))

(define (lower-define-values ctx env form mangle? allocated)
  (define names (binding-ids (second form)))
  (define value (if (pair? (cddr form)) (lower-expr ctx env (third form)) (uir-null)))
  (define emitted (for/list ([n (in-list names)]) (definition-name ctx n mangle? allocated)))
  (values (cons value (for/list ([sym (in-list emitted)]) (uir-set! (bare-symbol sym) (uir-null))))
          (if mangle?
              env
              (for/fold ([env env]) ([n names] [sym emitted])
                (extended-env env (list (cons n sym)))))))

(define (lower-match-param-env ctx env name emitted ps mangle?)
  (define params (binding-ids ps))
  (if mangle?
      (extend-env env params)
      (extended-env (extend-env env params) (list (cons name emitted)))))

(define (lower-define-match ctx env form mangle? allocated)
  (define target (second form))
  (define name (cond [(pair? target) (car target)] [(symbol? target) target] [else #f]))
  (cond
    [(not (symbol? name)) (values '() env)]
    [else
     (define emitted (definition-name ctx name mangle? allocated))
     (define ps (if (pair? target) (cdr target) '()))
     (define env* (lower-match-param-env ctx env name emitted ps mangle?))
     (define clause-bodies
       (map (λ (cl) `(begin ,@(if (pair? cl) (cdr cl) '()))) (cddr form)))
     (values (list (uir-fn (bare-symbol emitted) (lower-params ps)
                           (lower-body ctx env* clause-bodies) #f #f))
             (if mangle? env (extended-env env (list (cons name emitted)))))] ))

;; ── struct definitions ─────────────────────────────────────────────

;; A struct definition becomes a uir-class (the record type / constructor) plus
;; a pure stub function for every generated name (predicate, accessors,
;; setters). Calls to the constructor resolve to the class and classify as
;; pure instantiation; calls to the stubs are call-graph edges into an empty
;; function, i.e. pure by construction.
(define (lower-struct-form ctx env form mangle? [allocated (hash)])
  (define name (second form))
  (cond
    [(not (symbol? name)) (values '() env)]
    [else
     (define has-fields? (and (pair? (cddr form)) (list? (third form))))
     (define fields (if has-fields? (third form) '()))
     (define options (if has-fields? (cdddr form) (cddr form)))
     (define class-name (definition-name ctx name mangle? allocated))
     (define field-uirs (for/list ([f (in-list fields)] #:when (symbol? f))
                          (uir-field (bare-symbol f) #f #f)))
     (define class (uir-class (bare-symbol class-name) '() field-uirs '()))
     (define gens (struct-generated-names name fields options))
     (define emitted-gens (make-hash))
     (for ([(gen kind) (in-hash gens)])
       (hash-set! emitted-gens gen (definition-name ctx gen mangle? allocated)))
     (define stubs
       (for/list ([(gen kind) (in-hash gens)]
                  #:when (memq kind '(predicate accessor setter)))
         (uir-fn (bare-symbol (hash-ref emitted-gens gen)) '() (uir-block '()) #f #f)))
     (define env*
       (if mangle?
           env
           (for/fold ([e env]) ([(gen emitted) (in-hash emitted-gens)])
             (extended-env e (list (cons gen emitted))))))
     (values (cons class stubs) env*)]))
