#lang racket

;; racklr/lower-koka/params.rkt — Koka parameter lists → UIR typed parameters.
;;
;; Koka has three parameter shapes:
;;   parameters  : `(name : type = default, ...)`        (data constructors,
;;                                                       extern declarations)
;;   pparameters : `(pattern [: type] [= default], ...)`  (functions; the
;;                                                       pattern may be a
;;                                                       constructor/tuple
;;                                                       pattern as well as a
;;                                                       name)
;;   opparam     : `(name [: type], ...)`                 (handler operations)
;;
;; All three lower to `uir-typed-param`, whose `name` field admits a pattern
;; node for pattern parameters. A `^` borrow marker has no UIR field, so it
;; travels on an `uir-decorated` wrapper around the parameter.

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "pattern.rkt"
         "hooks.rkt")

(provide lower-parameters lower-parameter lower-pparameters lower-opparams lower-opparam lower-paramid-name)

;; ── Helpers ─────────────────────────────────────────────────────────

(define (borrow? node)
  (and node (for/or ([t (in-list (tokens node))]) (tok? t 'OP) (string=? (tok-value t) "^"))))

(define (wrap-borrow node) (wrap-borrow* node))

(define (wrap-borrow* node)
  ;; helper kept separate so emit can rely on the decorator shape
  (uir-decorated (list (uir-symbol "^")) node))

(define (with-borrow borrow-node param)
  (if (borrow? borrow-node) (wrap-borrow* param) param))

;; A parameter name: a bare identifier lowers to a symbol, anything else (a
;; constructor/tuple pattern, a wildcard) lowers to a pattern node.
(define (lower-paramid-name node)
  (cond
    [(not node) (uir-symbol "_")]
    [(eq? (node-tag node) 'identifier) (uir-symbol (identifier-text node))]
    [(eq? (node-tag node) 'wildcard) (uir-symbol (identifier-text node))]
    [else (uir-symbol (identifier-text node))]))

(define (lower-pattern-name pat-node)
  (cond
    [(not pat-node) (uir-symbol "_")]
    [else
     (define ident (find-node pat-node 'identifier))
     (define tag (node-tag pat-node))
     (cond
       ;; bare identifier / wildcard: a name, not a pattern node
       [(and ident (not (find-node pat-node 'pattern)))
        (uir-symbol (identifier-text ident))]
       [else (lower-pattern pat-node)])]))

;; ── parameters (typed, e.g. constructors and externs) ───────────────

(define (rule-items node item-tag)
  (define first (find-node node item-tag))
  (define rest
    (for/list ([g (in-list (all-list-items node))]
               #:when (and (node? g) (eq? (node-tag g) 'group)))
      (find-node g item-tag)))
  (filter values (cons first rest)))

(define (lower-parameters node)
  (for/list ([p (in-list (rule-items node 'parameter))])
    (lower-parameter p)))

(define (lower-parameter node)
  (define borrow (find-node node 'borrow))
  (define name-node (find-node node 'paramid))
  (define implicit-node (find-node node 'qimplicit))
  (define kt (find-node node 'ktype))
  (define default (find-default node))
  ;; `parameter : borrow paramid? COLON QUESTION? ktype (EQ expr)?
  ;;            | borrow qimplicit COLON QUESTION? ktype (EQ expr)?`
  ;; An unnamed parameter and an implicit (`?x`) parameter both survive.
  (define explicit-implicit? (has-token? node 'QUESTION))
  (define base-name (lower-paramid-name name-node))
  (define name
    (cond
      [implicit-node (uir-implicit-ref (uir-symbol (qimplicit-name implicit-node)))]
      [(and explicit-implicit? (uir-symbol? base-name))
       (uir-implicit-ref (uir-symbol (uir-symbol-name base-name)))]
      [else base-name]))
  (with-borrow borrow
               (uir-typed-param name
                                (if kt (uir-symbol (lower-type-text kt)) #f)
                                default
                                #f)))

(define (find-default node)
  ;; A trailing `(EQ expr)` group holds the default value.
  (for/or ([c (in-list (children node))] #:when (node? c))
    (and (eq? (node-tag c) 'group)
         (find-token c 'EQ)
         ((lower-expr-fn) (find-node c 'expr)))))

;; ── pparameters (function parameters, patterns allowed) ─────────────

(define (lower-pparameters node)
  (for/list ([p (in-list (rule-items node 'pparameter))])
    (lower-pparameter p)))

(define (lower-pparameter node)
  (define borrow (find-node node 'borrow))
  (define pat (find-node node 'pattern))
  (define implicit (find-node node 'qimplicit))
  (define group (for/first ([c (in-list (children node))]
                            #:when (and (node? c) (eq? (node-tag c) 'group)))
                  c))
  (define type-text
    (and group (find-node group 'ktype) (lower-type-text (find-node group 'ktype))))
  (define default-group
    (and group
         (for/first ([c (in-list (children group))]
                     #:when (and (node? c) (eq? (node-tag c) 'group)
                                 (find-token c 'EQ)))
           c)))
  (define default
    (and default-group ((lower-expr-fn) (find-node default-group 'expr))))
  (define name
    (cond
      [pat (lower-pattern-name pat)]
      [implicit (uir-implicit-ref (uir-symbol (qimplicit-name implicit)))]
      [else (uir-symbol "_")]))
  (with-borrow borrow
               (uir-typed-param name (and type-text (uir-symbol type-text))
                                default #f)))

;; ── opparams (handler operation parameters) ─────────────────────────

(define (lower-opparams node)
  ;; opparams: OPEN_PAREN opparamlist CLOSE_PAREN; the parameters live in the
  ;; opparamlist child, not directly in the opparams node.
  (define list-node (and node (find-node node 'opparamlist)))
  (if (not list-node)
      '()
      (for/list ([p (in-list (rule-items list-node 'opparam))])
        (lower-opparam p))))

(define (lower-opparam node)
  (define name-node (find-node node 'paramid))
  (define kt (find-node node 'ktype))
  (uir-typed-param (lower-paramid-name name-node)
                   (if kt (uir-symbol (lower-type-text kt)) #f)
                   #f #f))
