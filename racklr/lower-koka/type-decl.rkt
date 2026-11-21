#lang racket

;; racklr/lower-koka/type-decl.rkt — Koka type, struct and effect declarations.
;;
;;   type t<a> { C(...); ... }            → uir-enum / uir-enum-variant
;;   type t :: K                          → uir-enum with no variants + `:: K`
;;                                          decorator
;;   struct s<a>(field : t = default)     → uir-struct (fields are typed
;;                                          parameters: name + type + default)
;;   alias a<b> :: K = t                  → uir-type-alias (type params keep
;;                                          their kind annotations as text)
;;   effect e<a> { fun op(x : t) : r }    → uir-effect-decl / uir-effect-op
;;   named/linear/div effect …            → declaration decorators
;;
;; Modifiers are data (`uir-decorated`): `pub` is also the one visibility flag
;; the struct node itself carries. A type-parameter binder with a kind
;; annotation is stored as one binder text (`a :: E`), the same normalized-text
;; convention the type surface uses.

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "params.rkt"
         "hooks.rkt")

(provide lower-typedecl
         lower-typedef
         lower-structdecl
         lower-effectdecl
         lower-aliasdecl)

;; ── Dispatch ────────────────────────────────────────────────────────

(define (lower-typedecl node decorators)
  (define kid (for/first ([c (in-list (nodes node))])
                c))
  (case (and kid (node-tag kid))
    [(typedef) (lower-typedef kid decorators)]
    [(structdecl) (lower-structdecl kid decorators)]
    [(effectdecl) (lower-effectdecl kid decorators)]
    [else (decorate (uir-symbol "?typedecl") decorators)]))

;; ── type declarations ───────────────────────────────────────────────

(define (lower-typedef node decorators)
  (define typemod (find-node node 'typemod))
  (define typeid (find-node node 'typeid))
  (define typeparams (find-node node 'typeparams))
  (define kannot (find-node node 'kannot))
  (define typebody (find-node node 'typebody))
  (define mods (append decorators (typemod-modifiers typemod) (kind-modifiers kannot)))
  (define binders (lower-type-params typeparams))
  (define name (lower-type-text typeid))
  (define variants (if typebody (lower-constructors typebody) '()))
  (define enum (uir-enum (uir-symbol name) variants))
  (define binds (if (pair? binders)
                    (cons (uir-type-params (map binder-node binders)) mods)
                    mods))
  (decorate enum binds))

(define (binder-node b) (if (uir-symbol? b) b (uir-symbol (format "~a" b))))

(define (typemod-modifiers typemod)
  (cond
    [(not typemod) '()]
    [(pair? (tokens typemod)) (map tok-value (tokens typemod))]
    [else
     (define sm (find-node typemod 'structmod))
     (if sm (map tok-value (tokens sm)) '())]))

(define (kind-modifiers kannot)
  (cond
    [(not kannot) '()]
    [(pair? (text-of-tokens kannot)) (list (lower-type-text kannot))]
    [else '()]))

(define (lower-constructors typebody)
  (define cs (find-node typebody 'constructors))
  (if (not cs)
      '()
      (filter values
              (for/list ([g (in-list (all-list-items cs))]
                         #:when (and (node? g) (eq? (node-tag g) 'group)))
                (define c (find-node g 'constructor))
                (and c (lower-constructor c))))))

(define (lower-constructor node)
  (define pub? (pubmod-public? (find-node node 'pubmod)))
  (define conmod (find-node node 'conmod))
  (define lazy? (and conmod
                     (for/or ([t (in-list (tokens conmod))])
                       (token-value-is? t "lazy"))))
  (define con? (has-token? (find-node node 'con) 'CON))
  (define conid (find-node node 'conid))
  (define typeparams (find-node node 'typeparams))
  (define conparams (find-node node 'conparams))
  (define fields (lower-conparams conparams))
  (define body-group
    (for/first ([c (in-list (children node))] #:when (and (node? c) (eq? (node-tag c) 'group))) c))
  ;; Consume the optional `-> blockexpr` lazy-constructor body: it has no UIR
  ;; field on a variant, so it is represented as the variant's discriminant
  ;; (emitted back as `-> expr`), keeping the constructor value faithful.
  (define lazy-body
    (and body-group
         (find-token body-group 'ARROW)
         ((lower-blockexpr-fn) (find-node body-group 'blockexpr))))
  (define variant (uir-enum-variant (uir-symbol (identifier-text conid))
                                    fields
                                    (or lazy-body #f)))
  (define mods (append (if pub? (list "pub") '())
                       (if lazy? (list "lazy") '())
                       (if con? (list "con") '())))
  (define variant* (decorate variant mods))
  (define binders (lower-type-params typeparams))
  (if (pair? binders)
      (decorate variant* (list (uir-type-params binders)))
      variant*))

(define (lower-conparams node)
  (cond
    [(not node) '()]
    [(find-node node 'parameters) (lower-parameters (find-node node 'parameters))]
    [(find-node node 'sconparams)
     (for/list ([p (in-list (rule-items (find-node node 'sconparams) 'parameter))])
       (lower-parameter p))]
    [else '()]))

(define (rule-items node item-tag)
  (define first (find-node node item-tag))
  (define rest
    (for/list ([g (in-list (all-list-items node))]
               #:when (and (node? g) (eq? (node-tag g) 'group)))
      (find-node g item-tag)))
  (filter values (cons first rest)))

;; ── struct declarations ─────────────────────────────────────────────

(define (lower-structdecl node decorators)
  (define structmod (find-node node 'structmod))
  (define typeid (find-node node 'typeid))
  (define typeparams (find-node node 'typeparams))
  (define kannot (find-node node 'kannot))
  (define conparams (find-node node 'conparams))
  (define mods (append decorators
                       (if structmod (map tok-value (tokens structmod)) '())
                       (kind-modifiers kannot)))
  (define visibility (if (member "pub" mods) 'public 'private))
  (define struct
    (uir-struct (uir-symbol (lower-type-text typeid))
                (lower-type-params typeparams)
                (lower-conparams conparams)
                visibility))
  (decorate struct (filter (λ (m) (not (equal? m "pub"))) mods)))

;; ── effect declarations ─────────────────────────────────────────────

(define (lower-effectdecl node decorators)
  (define head (find-node node 'effecthead))
  ;; `effectdecl : effecthead (effectbody | operation)`: the grouped
  ;; alternative wraps the body in a `group` node, so search descendants.
  (define body (find-descendant node 'effectbody))
  (define single (and (not body) (find-descendant node 'operation)))
  (define head-tokens (tokens head))
  (define named? (for/or ([t (in-list head-tokens)]) (tok? t 'NAMED)))
  (define effectmod (find-node head 'effectmod))
  (define varid (find-node head 'varid))
  (define typeparams (find-node head 'typeparams))
  (define kannot (find-node head 'kannot))
  (define in-clause
    (for/first ([g (in-list (nodes head))]
                #:when (and (eq? (node-tag g) 'group)
                            (for/or ([t (in-list (tokens g))])
                              (and (tok? t 'IDENT) (string=? (tok-value t) "in")))))
      g))
  (define mods
    (append decorators
            (if named? (list "named") '())
            (if effectmod (map tok-value (tokens effectmod)) '())
            (kind-modifiers kannot)
            (if in-clause (list (lower-type-text in-clause)) '())))
  (define name
    (uir-symbol (if varid (identifier-text varid) "")))
  (define ops
    (cond
      [body
       (define operations (find-node body 'operations))
       (if (not operations)
           '()
           (filter values
                   (for/list ([g (in-list (all-list-items operations))]
                              #:when (and (node? g) (eq? (node-tag g) 'group)))
                     (define op (find-node g 'operation))
                     (and op (lower-operation op)))))]
      [single (list (lower-operation single))]
      [else '()]))
  (define decl (uir-effect-decl name (filter values ops)))
  (define binders (lower-type-params typeparams))
  ;; `effect fun op() : t` / `effect val op : t` (no braces) is a legal
  ;; one-line effect declaration; remember the spelling.
  (define mods* (if single (cons "single" mods) mods))
  (decorate decl (if (pair? binders)
                     (cons (uir-type-params binders) mods*)
                     mods*)))

(define (lower-operation node)
  (define pubmod (find-node node 'pubmod))
  (define pub? (pubmod-public? pubmod))
  (define name-node (find-node node 'identifier))
  (define typeparams (find-node node 'typeparams))
  (define pparams (find-node node 'pparameters))
  (define tatomic (find-node node 'tatomic))
  (define ctl? (has-token? node 'CTL))
  (define controlmod (find-node node 'controlmod))
  (define val? (has-token? node 'VAL))
  (define kind
    (cond [(and ctl? (has-token? controlmod 'FINAL)) "final"]
          [(and ctl? (has-token? controlmod 'RAW)) "raw"]
          [ctl? "ctl"]
          [val? "val"]
          [else #f]))
  (define op
    (uir-effect-op (uir-symbol (identifier-text name-node))
                   (if pparams (lower-pparameters pparams) '())
                   (if tatomic (uir-symbol (lower-type-text tatomic)) #f)))
  (define binders (lower-type-params typeparams))
  (decorate op (append (if pub? (list "pub") '())
                       (if kind (list kind) '())
                       (if (pair? binders) (list (uir-type-params binders)) '()))))

;; ── alias declarations ──────────────────────────────────────────────

(define (lower-aliasdecl node decorators)
  (define typeid (find-node node 'typeid))
  (define typeparams (find-node node 'typeparams))
  (define kannot (find-node node 'kannot))
  (define ktype (find-node node 'ktype))
  (define binders (lower-type-params typeparams))
  (define name (lower-type-text typeid))
  (define mods (append decorators (kind-modifiers kannot)))
  ;; `alias foo :: K` has no UIR field for the binder kind: it is folded into
  ;; the alias name text, which emits back as the same declaration.
  (define name* (if (and kannot (pair? (text-of-tokens kannot)))
                    (format "~a ~a" name (lower-type-text kannot))
                    name))
  (decorate (uir-type-alias (uir-symbol name*) binders
                            (uir-symbol (lower-type-text ktype)))
            mods))
