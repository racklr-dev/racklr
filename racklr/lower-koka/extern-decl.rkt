#lang racket

;; racklr/lower-koka/extern-decl.rkt — Koka `extern` declarations and
;; external-import groups.
;;
;; Two extern declaration syntaxes exist, distinguished by the type slot:
;;   extern name : <typescheme> { <target> [attrs] [inline] "<signature>" ... }
;;   extern name<tpars>(params) : <result> { ... }
;; so they lower to `uir-extern-value` / `uir-extern-fun`. `inline`/`fip`
;; modifiers may appear before and/or after the extern keyword
;; (`externdecl : inlinemod fipmod externkw inlinemod fipmod ...`) and both
;; groups are collected.
;;
;; An extern body entry is (target [attrs], inline?, string); the target guard
;; (`c[host=libc,os=windows]`) and the inline flag have no pair field, so they
;; are folded into the entry key as `[inline:]target[attrs]` and split again on
;; emission.
;;
;; An external-import block has one target group per entry; each target group
;; becomes one `uir-extern-import` node (they emit as sibling declarations,
;; which is the same program).

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "params.rkt"
         "hooks.rkt")

(provide lower-externdecl lower-extimpdecl)

;; ── extern declarations ─────────────────────────────────────────────

(define (lower-externdecl node decorators)
  (define inlinemods (nodes-with-tag node 'inlinemod))
  (define fipmods (nodes-with-tag node 'fipmod))
  (define qidentifier (find-node node 'qidentifier))
  (define externtype (find-node node 'externtype))
  (define externbody (find-node node 'externbody))
  (define name (identifier-text qidentifier))
  (define visibility
    (cond [(member "pub" decorators) 'public]
          [(member "private" decorators) 'private]
          [else 'private]))
  (define modifiers
    (append (filter (λ (m) (not (member m '("pub" "private")))) decorators)
            (append* (for/list ([im (in-list inlinemods)]) (map tok-value (tokens im))))
            (append* (for/list ([fm (in-list fipmods)]) (fip-modifiers fm)))))
  (define entries (extern-entries externbody))
  (define colon? (and externtype (find-token externtype 'COLON)))
  (cond
    [colon?
     (define scheme (find-node externtype 'typescheme))
     (uir-extern-value (uir-symbol name)
                       (uir-symbol (lower-type-text scheme))
                       entries visibility modifiers)]
    [else
     (define typeparams (find-node externtype 'typeparams))
     (define params (find-node externtype 'parameters))
     (define annotres (find-node externtype 'annotres))
     (define tresult (and annotres (find-node annotres 'tresult)))
     (uir-extern-fun (uir-symbol name)
                     (lower-type-params typeparams)
                     (if params (lower-parameters params) '())
                     (and tresult (uir-symbol (lower-type-text tresult)))
                     entries visibility modifiers)]))

(define (nodes-with-tag node tag)
  (for/list ([c (in-list (nodes node))] #:when (eq? (node-tag c) tag)) c))

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
            (pair? (tokens fiplimit))
            (string-append "(" (string-join (map tok-value (tokens fiplimit)) "") ")")))
     (append (if (and tailmod (pair? (tokens tailmod))) (list "tail") '())
             (if fip-token
                 (list (string-append (tok-value fip-token) (or limit-text "")))
                 '()))]))

;; externbody → list of (target-key-symbol . uir-string)
(define (extern-entries node)
  (cond
    [(not node) '()]
    [else
     (define stats (find-node node 'externstats))
     (if (not stats)
         '()
         (filter values
                 (for/list ([g (in-list (all-list-items stats))]
                            #:when (and (node? g) (eq? (node-tag g) 'group)))
                   (define stat (find-node g 'externstat))
                   (and stat (lower-externstat stat)))))] ))

(define (lower-externstat node)
  (define target (find-node node 'externtarget))
  (define inline-node (find-node node 'externinline))
  (define inline?
    (and inline-node
         (for/or ([t (in-list (tokens inline-node))]) (token-value-is? t "inline"))))
  ;; The signature alternative is grouped (`(STRING | RAWSTRING)`), so collect
  ;; descendant tokens.
  (define string-token
    (for/first ([t (in-list (text-of-tokens node))]
                #:when (memq (tok-type t) '(STRING RAWSTRING)))
      t))
  (cond
    [(not string-token) #f]
    [else
     (define target-text
       (string-append (if target (identifier-text target) "")
                      (extattrs-text node)))
     (define key (string-append (if inline? "inline:" "") target-text))
     (cons (uir-symbol key) (uir-string (strip-quotes (tok-value string-token))))]))

;; Target guards (`c[host=libc,os=windows]`) are part of the target text: they
;; select the code generator, so they must survive into the emitted extern.
(define (extattrs-text node)
  (define attrs (nodes-with-tag node 'extattrs))
  (if (null? attrs)
      ""
      (render-tokens (append* (for/list ([a (in-list attrs)]) (text-of-tokens a))))))

;; ── external import ─────────────────────────────────────────────────

(define (lower-extimpdecl node)
  ;; `extern import ...` / `external include ...`: the keyword words are syntactic.
  (define body (find-node node 'extimpbody))
  (cond
    [(not body) '()]
    [(find-node body 'extimppair)
     ;; extern import = <key> "<file>"
     (list (uir-extern-import #f (list (lower-extimppair (find-node body 'extimppair)))))]
    [else
     (filter values
             (for/list ([g (in-list (all-list-items body))]
                        #:when (and (node? g) (eq? (node-tag g) 'group)))
               (define entry (find-node g 'extimpentry))
               (and entry (lower-extimpentry entry))))]))

(define (lower-extimppair node)
  ;; `extimppair : IDENT EQ (STRING | RAWSTRING)`: the value is a grouped
  ;; alternative, so look at all descendant tokens.
  (define ts (text-of-tokens node))
  (define id-token (for/first ([t (in-list ts)] #:when (tok? t 'IDENT)) t))
  (define string-token
    (for/first ([t (in-list ts)] #:when (memq (tok-type t) '(STRING RAWSTRING))) t))
  (cons (uir-symbol (if id-token (tok-value id-token) "file"))
        (uir-string (if string-token (strip-quotes (tok-value string-token)) ""))))

(define (lower-extimpentry node)
  (define target (find-node node 'externtarget))
  (define target-text (string-append (if target (identifier-text target) "")
                                     (extattrs-text node)))
  ;; The key/value alternative is a parenthesized group containing the brace
  ;; list, so the pairs are found as descendants.
  (define pairs (find-descendant-nodes node 'extimppair))
  (define entries
    (cond
      [(pair? pairs) (map lower-extimppair pairs)]
      [else
       ;; `js file "<f>"` / bare `js "<f>"` (the latter means the file key).
       ;; The alternative is a parenthesized group, so its tokens are inside
       ;; the group child.
       (define alt-group
         (for/first ([c (in-list (nodes node))]
                     #:when (and (eq? (node-tag c) 'group)
                                 (for/or ([t (in-list (text-of-tokens c))])
                                   (tok? t 'STRING))
                                 (not (find-node c 'extimppair))))
           c))
       (define ts (if alt-group (text-of-tokens alt-group) (tokens node)))
       (define string-token (for/first ([t (in-list ts)] #:when (tok? t 'STRING)) t))
       (define key-token (for/first ([t (in-list ts)] #:when (tok? t 'IDENT)) t))
       (cond
         [(not string-token) '()]
         [else (list (cons (uir-symbol (if key-token (tok-value key-token) "file"))
                           (uir-string (strip-quotes (tok-value string-token)))))])]))
  (and (pair? entries)
       (uir-extern-import (if (equal? target-text "") #f (uir-symbol target-text)) entries)))
