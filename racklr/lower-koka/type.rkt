#lang racket

;; racklr/lower-koka/type.rkt — Koka type syntax → UIR type text.
;;
;; UIR has no type-expression node: every front end carries source types as a
;; normalized `uir-symbol` text (the Rust front end precedent). Koka types are
;; rendered here from the CST token stream in a canonical, re-parseable shape:
;; `list<a>`, `(int, string)`, `a -> e b`, `<exn, div>`, `forall<a> a`, `a :: E`.
;;
;; Function declarations are the one place where the effect row is split out of
;; the declaration's result type: `: <exn, div> int` lowers to
;; `return-type "int"` plus a `uir-effect-row`, so the shared effect layer and
;; the Koka emitter can treat the declared row natively. An extensible row
;; `<exn|e>` keeps its tail as the last row entry, spelled `|e`.

(require racklr/uir
         "helpers.rkt")

(provide lower-type-text
         lower-type-params
         lower-kind-text
         lower-tresult
         lower-effect-entry
         lower-type-args
         type-text-of)

;; Normalized text for any type-bearing CST node (ktype/tatomic/tbasic/
;; typescheme/typeid/anntype/kind).
(define (lower-type-text node)
  (if node (render-tokens (text-of-tokens node)) ""))

(define (type-text-of node) (lower-type-text node))

;; The binder texts of a `typeparams` node, e.g. ("a" "e :: E"); an absent
;; `<...>` yields the empty list.
(define (lower-type-params node)
  (cond
    [(not node) '()]
    [else
     (define tp1 (find-node node 'typeparams1))
     (cond
       [(not tp1) '()]
       [else
        (define binders (find-node tp1 'tbinders))
        (cond
          [(not binders) '()]
          [else
           (define first (find-node binders 'tbinder))
           (define rest
             (for/list ([g (in-list (all-list-items binders))]
                        #:when (eq? (node-tag g) 'group))
               (find-node g 'tbinder)))
           (for/list ([b (in-list (filter values (cons first rest)))])
             (uir-symbol (lower-type-text b)))])])]))

(define (lower-kind-text node) (lower-type-text node))

;; One effect-row entry whose name is the normalized effect text.
(define (lower-effect-entry text)
  (uir-effect-entry (uir-symbol text) 'may '() #f #f))

;; Split a declaration result type (`tresult`) into its result-type text and
;; its declared effect row (or #f when no effect was written).
;;
;;   tresult tatomic         → result type only, effects #f
;;   tresult tatomic tbasic  → first tatomic is the effect row, tbasic the result
(define (lower-tresult node)
  (define kids (nodes node))
  (case (length kids)
    [(1) (values (uir-symbol (lower-type-text (first kids))) #f)]
    [(2)
     (define effect (first kids))
     (define result (second kids))
     (values (uir-symbol (lower-type-text result))
             (lower-effect-tatomic effect))]
    [else (values (uir-symbol (lower-type-text node)) #f)]))

;; The effect slot of a `tresult`.
(define (lower-effect-tatomic effect)
  (define kids (nodes effect))
  (define text (lower-type-text effect))
  (cond
    [(equal? text "<>") (uir-effect-row '())]
    [(and (> (length kids) 0) (eq? (node-tag (first kids)) 'tbasic))
     ;; A bare effect name / effect type application: `total` is the empty row,
     ;; everything else is an atom in the row.
     (if (member text '("total" "<>"))
         (uir-effect-row '())
         (uir-effect-row (list (lower-effect-entry text))))]
    [else
     ;; An angle row. Two spellings:
     ;;   < a, b >            fixed row            (children LT targuments GT)
     ;;   < a, b | e >        extensible row       (children LT targuments1 PIPE tatomic GT)
     (define extensible? (for/or ([k (in-list kids)]) (eq? (node-tag k) 'PIPE)))
     ;; child nodes after LT: the argument list (targuments or targuments1)
     (define args-node
       (for/or ([k (in-list kids)])
         (and (memq (node-tag k) '(targuments targuments1)) k)))
     (define names (and args-node (lower-type-args args-node)))
     (cond
       [(not extensible?)
        (uir-effect-row
         (for/list ([n (in-list (or names '()))]) (lower-effect-entry n)))]
       [else
        ;; The last node child after the argument list is the row tail.
        (define tail
          (for/last ([k (in-list kids)]
                     #:when (eq? (node-tag k) 'tatomic))
            k))
        (uir-effect-row
         (append (for/list ([n (in-list (or names '()))]) (lower-effect-entry n))
                 (if tail
                     (list (lower-effect-entry
                            (string-append "|" (lower-type-text tail))))
                     '())))])]))

;; `targuments` / `targuments1`: the anntype texts, comma-separated in source.
(define (lower-type-args args-node)
  (define first (find-node args-node 'anntype))
  (define rest
    (for/list ([g (in-list (all-list-items args-node))]
               #:when (eq? (node-tag g) 'group))
      (find-node g 'anntype)))
  (for/list ([a (in-list (filter values (cons first rest)))])
    (lower-type-text a)))
