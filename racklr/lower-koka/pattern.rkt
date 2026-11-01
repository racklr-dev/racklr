#lang racket

;; racklr/lower-koka/pattern.rkt — Koka patterns and literals → UIR.
;;
;; Mapping:
;;   wildcard              → uir-pat-wildcard
;;   name / `name as pat`  → uir-pat-capture / uir-pat-as
;;   Constructor(args)     → uir-pat-class (positional + named arguments)
;;   (p) / (p, q) / ()     → uir-pat-group / tuple class / unit class
;;   [p, q]                → uir-pat-sequence
;;   literal               → uir-pat-literal
;;
;; Tuple patterns use the class name `(,)` / `(,,)` … (Koka's own tuple binder
;; names) so the emitter can render them back as tuple syntax; pattern
;; annotations (`p : t`) are carried by the surrounding parameter/declaration
;; type slot where Koka allows them.

(require racklr/uir
         "helpers.rkt"
         "type.rkt")

(provide lower-pattern lower-apattern lower-literal)

(define (lower-literal node)
  (define t (or (find-token-in node '(INT FLOAT CHAR STRING RAWSTRING))
                (for/first ([c (in-list (children node))]
                            #:when (and (node? c)
                                        (find-token-in c '(INT FLOAT CHAR STRING RAWSTRING))))
                  (find-token-in c '(INT FLOAT CHAR STRING RAWSTRING)))))
  (cond
    [(not t) (uir-symbol "?literal")]
    [else
     (define ty (tok-type t))
     (define txt (tok-value t))
     (case ty
       [(INT FLOAT) (uir-number txt)]
       [(CHAR) (uir-char txt)]
       [(STRING RAWSTRING) (uir-string (unescape-literal (strip-quotes txt)))]
       [else (uir-symbol "?literal")])]))

;; String literals carry their source escapes; the UIR string is the logical
;; value so the emitter can re-escape it canonically (raw strings lose their
;; raw-ness: an explicit, documented loss, the value is identical).
(define (unescape-literal s)
  (list->string
   (let loop ([cs (string->list s)] [acc '()])
     (cond
       [(null? cs) (reverse acc)]
       [(and (pair? (cdr cs)) (char=? (car cs) #\\))
        (define c (cadr cs))
        (loop (cddr cs)
              (cons (case c
                      [(#\n) #\newline]
                      [(#\t) #\tab]
                      [(#\r) #\return]
                      [(#\\) #\\]
                      [(#\") #\"]
                      [(#\') #\']
                      [else c])
                    acc))]
       [else (loop (cdr cs) (cons (car cs) acc))]))))

;; ── Patterns ────────────────────────────────────────────────────────

;; An `apattern` = pattern + annot; the annotation is returned separately.
(define (lower-apattern node)
  (define pat (find-node node 'pattern))
  (define annot (find-node node 'annot))
  (define type-text
    (and annot
         (find-node annot 'typescheme)
         (lower-type-text (find-node annot 'typescheme))))
  (values (lower-pattern (or pat node)) type-text))

(define (lower-pattern node)
  (cond
    [(not (node? node)) (uir-pat-wildcard)]
    [else
     (define kids (nodes node))
     (define ts (tokens node))
     (cond
       ;; identifier AS pattern
       [(and (>= (length kids) 2)
             (eq? (node-tag (first kids)) 'identifier)
             (for/or ([t (in-list ts)]) (tok? t 'AS)))
        (uir-pat-as (lower-pattern (second kids))
                    (uir-symbol (identifier-text (first kids))))]
       ;; bare identifier capture
       [(and (= (length kids) 1) (memq (node-tag (first kids)) '(identifier)))
        (uir-pat-capture (uir-symbol (identifier-text (first kids))))]
       ;; constructor pattern with arguments
       [(and (>= (length kids) 2) (eq? (node-tag (first kids)) 'conid))
        (lower-constructor-pattern (first kids)
                                   (find-node node 'patargs))]
       ;; nullary constructor
       [(and (= (length kids) 1) (memq (node-tag (first kids)) '(conid qconid)))
        (uir-pat-class (uir-symbol (identifier-text (first kids))) '() '())]
       ;; parenthesized / tuple / unit pattern
       [(and (pair? (tokens node)) (tok? (first (tokens node)) 'OPEN_PAREN))
        (lower-parens-pattern node)]
       ;; list pattern
       [(and (pair? (tokens node)) (tok? (first (tokens node)) 'OPEN_BRACK))
        (uir-pat-sequence (lower-pattern-list (find-node node 'apatterns)))]
       ;; wildcard: `_` or `_name`
       [(and (= (length kids) 1) (eq? (node-tag (first kids)) 'wildcard))
        (uir-pat-wildcard)]
       ;; literal
       [(and (= (length kids) 1) (eq? (node-tag (first kids)) 'literal))
        (uir-pat-literal (lower-literal (first kids)))]
       [else (uir-pat-wildcard)])]))

(define (lower-constructor-pattern conid-node patargs-node)
  (define args (lower-patargs patargs-node))
  (uir-pat-class (uir-symbol (identifier-text conid-node))
                 (for/list ([a (in-list args)] #:unless (car a)) (cdr a))
                 (for/list ([a (in-list args)] #:when (car a))
                   (cons (uir-symbol (car a)) (cdr a)))))

;; patargs → list of (name . pattern): name is a string for `name = pat`.
(define (lower-patargs node)
  (cond
    [(not node) '()]
    [else
     (define first (find-node node 'patarg))
     (define rest
       (for/list ([g (in-list (all-list-items node))] #:when (eq? (node-tag g) 'group))
         (find-node g 'patarg)))
     (for/list ([p (in-list (filter values (cons first rest)))])
       (lower-patarg p))]))

(define (lower-patarg node)
  (define id (find-node node 'identifier))
  (define ap (find-node node 'apattern))
  (cond
    [(and id ap) (cons (identifier-text id) (apattern-pattern ap))]
    [(find-node node 'apattern) => (λ (ap) (cons #f (apattern-pattern ap)))]
    [else (cons #f (lower-pattern node))]))

(define (apattern-pattern node)
  (define-values (p _) (lower-apattern node))
  p)

(define (lower-parens-pattern node)
  (define aps (find-node node 'apatterns))
  (define pats (if aps (lower-pattern-list aps) '()))
  (case (length pats)
    [(0) (uir-pat-class (uir-symbol "()") '() '())]
    [(1) (uir-pat-group (first pats))]
    [else (uir-pat-class (uir-symbol (tuple-class-name (length pats))) pats '())]))

(define (lower-pattern-list aps)
  (define first (find-node aps 'apattern))
  (define rest
    (for/list ([g (in-list (all-list-items aps))] #:when (eq? (node-tag g) 'group))
      (find-node g 'apattern)))
  (for/list ([a (in-list (filter values (cons first rest)))])
    (apattern-pattern a)))

(define (tuple-class-name n)
  (string-append "(" (make-string (sub1 n) #\,) ")"))
