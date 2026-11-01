#lang racket

;; racklr/lower-koka/decl.rkt — Koka program structure: module, imports and
;; top-level declarations → UIR.
;;
;;   file        → uir-block of [module header?] + imports + declarations
;;   module path → uir-module with an empty body (a header marker: Koka's
;;                 definitions are the enclosing block's siblings)
;;   import      → uir-import / uir-import-alias (decorated by `pub`)
;;   extern imp. → uir-extern-import (one node per target group)
;;   fixity      → uir-fixity
;;   fun/val     → uir-fn / uir-let or uir-ann-set!
;;   type/struct/effect/alias/extern declarations are delegated to
;;   type-decl.rkt and extern-decl.rkt.
;;
;; Modifiers (`pub`, `inline`, `fip`, `tail`, …) are `uir-decorated` symbol
;; decorators; emission re-orders them into Koka's canonical declaration order.

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "pattern.rkt"
         "hooks.rkt"
         "type-decl.rkt"
         "extern-decl.rkt"
         "fixity.rkt")

(provide lower-file)

;; ── Program ─────────────────────────────────────────────────────────

(define (lower-file cst)
  ;; Operator fixity declarations are file-scoped: start from the Koka prelude
  ;; table for every file.
  (reset-fixities!)
  (define moduleheader (find-node cst 'moduleheader))
  (define body (find-node cst 'body))
  (define imports (and body (find-node body 'imports)))
  (define plan-lists (if imports (lists imports) '()))
  (define (plan i)
    (if (> (length plan-lists) i) (list-ref plan-lists i) '()))
  (define import-nodes
    (append (map lower-impdecl (group-items (plan 0) 'impdecl))
            (append* (map lower-extimpdecl (group-items (plan 1) 'extimpdecl)))))
  (define declaration-nodes
    (append* (map lower-declaration (group-items (plan 2) 'declaration))))
  (define header
    (if moduleheader
        (list (uir-module (uir-symbol (module-name moduleheader)) '() '() (uir-null)))
        '()))
  (uir-block (append header import-nodes (filter values declaration-nodes))))

(define (module-name moduleheader)
  (define mp (find-node moduleheader 'modulepath))
  (if mp (identifier-text mp) ""))

;; The element nodes of a star plan list declared as `(element semis)*`.
(define (group-items plan-list element-tag)
  (filter values
          (for/list ([g (in-list plan-list)]
                     #:when (and (node? g) (eq? (node-tag g) 'group)))
            (find-node g element-tag))))

;; ── Imports ─────────────────────────────────────────────────────────

(define (lower-impdecl node)
  (define pub? (pubmod-public? (find-node node 'pubmod)))
  ;; `impdecl : pubmod IMPORT modulepath ((EQ modulepath) | importconds)?`
  ;;   alias form: import alias = source
  ;;   conditional: import source { [guard] path; ... }
  (define all-paths (find-descendant-nodes node 'modulepath))
  (define cond-node (find-descendant node 'importconds))
  ;; `import <alias> = <modulepath>`: the LEFT modulepath is the alias, the
  ;; RIGHT one is the module being imported.
  (define alias (and (not cond-node) (= (length all-paths) 2) (first all-paths)))
  (define source (if alias (second all-paths) (first all-paths)))
  (define import-node
    (cond
      [alias (uir-import-alias (uir-symbol (identifier-text alias))
                               (uir-symbol (identifier-text source))
                               '())]
      [else (uir-import (uir-symbol (identifier-text source)) '())]))
  (define cond-decorators
    (if cond-node
        (for/list ([g (in-list (all-list-items cond-node))]
                   #:when (eq? (node-tag g) 'group))
          (define ic (find-node g 'importcond))
          (and ic (string-append "cond:" (condition-text ic))))
        '()))
  (decorate import-node
            (append (if pub? (list "pub") '())
                    (filter values cond-decorators))))

;; `[host=js,os=linux] path`: the guard list then the guarded module path.
(define (condition-text node)
  (define guard (find-node node 'targetguard))
  (define path (find-node node 'modulepath))
  (format "~a ~a"
          (if guard (render-tokens (text-of-tokens guard)) "[]")
          (if path (identifier-text path) "")))

;; extern-decl.rkt's `lower-extimpdecl` returns one node per target group; each
;; becomes a sibling declaration.

;; ── Declarations ────────────────────────────────────────────────────

(define (lower-declaration node)
  (define kid (for/first ([c (in-list (nodes node))]) c))
  (cond
    [(not kid) '()]
    [(eq? (node-tag kid) 'fixitydecl) (list (lower-fixitydecl kid))]
    [(eq? (node-tag kid) 'extimpdecl) (lower-extimpdecl kid)]
    [(eq? (node-tag kid) 'topdecl) (list (lower-topdecl kid))]
    [else (list (uir-symbol (format "?declaration/~a" (node-tag kid))))]))

(define (lower-fixitydecl node)
  (define pub? (pubmod-public? (find-node node 'pubmod)))
  (define fixity (find-node node 'fixity))
  (define kind
    (cond [(has-token? fixity 'INFIXL) 'infixl]
          [(has-token? fixity 'INFIXR) 'infixr]
          [else 'infix]))
  (define prec-token (and fixity (find-token fixity 'INT)))
  (define oplist (find-node node 'oplist1))
  (define names
    (if (not oplist)
        '()
        (let ([first (find-node oplist 'identifier)]
              [rest (for/list ([g (in-list (all-list-items oplist))]
                               #:when (and (node? g) (eq? (node-tag g) 'group)))
                      (find-node g 'identifier))])
          (map (λ (id) (uir-symbol (identifier-text id)))
               (filter values (cons first rest))))))
  (define precedence (if prec-token (string->number (tok-value prec-token)) 50))
  (register-fixity! kind precedence (map uir-symbol-name names))
  (decorate (uir-fixity kind precedence names)
            (if pub? (list "pub") '())))

(define (lower-topdecl node)
  (define pub? (pubmod-public? (find-node node 'pubmod)))
  (define abstract? (has-token? (find-node node 'abstractmod) 'ABSTRACT))
  (define target
    (for/last ([c (in-list (nodes node))]
               #:when (not (memq (node-tag c) '(pubmod abstractmod))))
      c))
  (define private? (pubmod-private? (find-node node 'pubmod)))
  (define decorators
    (append (if pub? (list "pub") '())
            (if private? (list "private") '())
            (if abstract? (list "abstract") '())))
  (cond
    [(not target) #f]
    [else
     (case (node-tag target)
       [(puredecl) (lower-puredecl target decorators)]
       [(typedecl) (lower-typedecl target decorators)]
       [(aliasdecl) (lower-aliasdecl target decorators)]
       [(externdecl) (lower-externdecl target decorators)]
       [else (decorate (uir-symbol (format "?topdecl/~a" (node-tag target))) decorators)])]))

;; ── fun / val declarations ──────────────────────────────────────────

(define (lower-puredecl node decorators)
  (define inlinemod (find-node node 'inlinemod))
  (define fipmod (find-node node 'fipmod))
  (define inline-mods
    (if (and inlinemod (pair? (tokens inlinemod))) (map tok-value (tokens inlinemod)) '()))
  (define fip-mods (fip-modifiers fipmod))
  (define mods (append decorators inline-mods fip-mods))
  (cond
    [(has-token? node 'VAL)
     (define binder (find-node node 'binder))
     (define id (binder-name-node binder))
     (define ktype (find-descendant binder 'ktype))
     (define value ((lower-blockexpr-fn) (find-node node 'blockexpr)))
     (define decl
       (if ktype
           (uir-ann-set! (uir-symbol (identifier-text id))
                         (uir-symbol (lower-type-text ktype))
                         value)
           (uir-let (uir-symbol (identifier-text id)) value (uir-null))))
     (decorate decl mods)]
    [(find-node node 'funkw)
     (define name-node (find-node node 'qidentifier))
     (define funbody (find-node node 'funbody))
     (decorate ((lower-funbody-fn) funbody (uir-symbol (identifier-text name-node)))
               mods)]
    [else (uir-symbol "?puredecl")]))

;; `binder : qidentifier (COLON ktype)?`
(define (binder-name-node binder)
  (and binder
       (or (find-descendant binder 'qidentifier)
           (find-descendant binder 'identifier)
           (find-descendant binder 'wildcard))))

(define (fip-modifiers fipmod)
  (cond
    [(not fipmod) '()]
    [else
     (define tailmod (find-node fipmod 'tailmod))
     (define fip-token
       (for/first ([c (in-list (children fipmod))]
                   #:when (and (token-struct? c)
                               (member (tok-value c) '("fip" "fbip"))))
         c))
     (define fiplimit (find-node fipmod 'fiplimit))
     (define limit-text
       (and fiplimit
            (string-append "(" (string-join (map tok-value (tokens fiplimit)) "") ")")))
     (append (if (and tailmod (pair? (tokens tailmod))) (list "tail") '())
             (if fip-token
                 (list (string-append (tok-value fip-token) (or limit-text "")))
                 '()))]))
