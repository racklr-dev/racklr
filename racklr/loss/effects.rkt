#lang racket

;; racklr/loss/effects.rkt — effect rows in the target's native form.
;;
;; The loss model says whether a pair preserves effect rows. "Preserved" here
;; means the row survives translation in the target's own vocabulary: a Python
;; `@__withEffects({...})` decorator, a JavaScript `__withEffects({...}, fn)`
;; wrapper, a Clojure `{:__effects__ {...}}` attr-map, or a Koka return-type
;; row. This module extracts those native forms from the emitted-then-absorbed
;; UIR (and from the annotated source UIR) and compares them, so a dropped row
;; is a first-class violation instead of an invisible fact.

(require racket/list
         racket/string
         racklr/loss/sexp
         racklr/loss-model
         racklr/loss/report)

(provide extract-effect-rows
         observe-effect-rows
         decode-effect-row
         canonical-row
         effect-row-evidence)

(define (symbol-of x) (and (string? x) (string->symbol x)))

;; A record serializes as (record (KEY VALUE) ...): each entry is a two-element
;; list whose first element is the serialized key.
(define (record-value rec key)
  (and (list-tag? rec 'record)
       (for/or ([e (in-list (cdr rec))])
         (and (pair? e)
              (= 2 (length e))
              (list-tag? (car e) 'string)
              (equal? (cadr (car e)) key)
              (cadr e)))))

;; Decode the emitter's rendered row record back into the canonical
;; (effect-row (effect-entry ...)) form. The renderers (python/js/clojure) all
;; emit the same string-keyed constraint convention.
(define (decode-effect-row rec)
  (define entries
    (and (list-tag? rec 'record)
         (for/list ([e (in-list (cdr rec))]
                    #:when (and (pair? e) (= 2 (length e))
                                (list-tag? (car e) 'string)))
           (define c (cadr e))
           (define modality-node (record-value c "modality"))
           (define budget-node (record-value c "max"))
           (define scope-node (record-value c "scope"))
           (define ops-node (record-value c "ops"))
           (list (cadr (car e))
                 (or (and (list-tag? modality-node 'string)
                          (symbol-of (cadr modality-node)))
                     'may)
                 (cond [(list-tag? ops-node 'list)
                        (for/list ([x (in-list (cdr ops-node))]
                                   #:when (list-tag? x 'string))
                          (list 'symbol (cadr x)))]
                       [else '()])
                 (and (list-tag? budget-node 'number)
                      (string->number (cadr budget-node)))
                 (and (list-tag? scope-node 'string)
                      (symbol-of (cadr scope-node)))))))
  (and entries
       (make-canonical-row
        (for/list ([e (in-list entries)])
          (list 'effect-entry (list 'symbol (car e)) (cadr e)
                (caddr e) (cadddr e) (list-ref e 4))))))

(define (make-canonical-row entries)
  (sort entries
        (λ (a b) (string<? (symbol->string (ref-name (cadr a)))
                           (symbol->string (ref-name (cadr b)))))))

(define (canonical-row row)
  (make-canonical-row
   (cond [(list-tag? row 'effect-row) (cdr row)]
         [else '()])))

(define (row-value x)
  (cond [(list-tag? x 'effect-row) (canonical-row x)]
        [(list-tag? x 'record) (decode-effect-row x)]
        [else #f]))

;; Extract name → canonical effect row from every native representation:
;;   * the function's own UIR effects slot (declared rows, Koka rows)
;;   * a Python `@__withEffects({...})` decorator on a named function
;;   * a JavaScript `__withEffects({...}, fn)` expression bound to a name
;;   * a Clojure `(defn name {:__effects__ {...}} ...)` attr-map
(define (extract-effect-rows sexp)
  (define acc (make-hash))
  (define (record! name row)
    (when (and name row) (hash-set! acc name row)))
  (define (walk x)
    (cond
      [(and (list-tag? x 'fn) (sym-name (cadr x)))
       (define slot (list-ref x 5))
       (when (list-tag? slot 'effect-row)
         (record! (sym-name (cadr x)) (canonical-row slot)))
       (for ([y (in-list x)]) (walk y))]
      [(list-tag? x 'decorated)
       (define decos (cadr x))
       (define inner (caddr x))
       (define row (for/or ([d (in-list decos)]
                            #:when (and (list-tag? d 'call)
                                        (equal? (ref-name (cadr d)) "__withEffects")))
                     (row-value (car (caddr d)))))
       (when (and row (list-tag? inner 'fn) (sym-name (cadr inner)))
         (record! (sym-name (cadr inner)) row))
       (for ([y (in-list x)]) (walk y))]
      [(and (member (tag x) '(set! let)) (ref-name (cadr x)))
       (define name (ref-name (cadr x)))
       (define v (caddr x))
       (cond [(and (list-tag? v 'fn) (list-tag? (list-ref v 5) 'effect-row))
              (record! name (canonical-row (list-ref v 5)))]
             [(and (list-tag? v 'call) (equal? (ref-name (cadr v)) "__withEffects"))
              (record! name (row-value (car (caddr v))))])
       (for ([y (in-list x)]) (walk y))]
      [(and (list-tag? x 'call) (equal? (ref-name (cadr x)) "defn"))
       (define args (caddr x))
       (define nm (and (pair? args) (ref-name (car args))))
       (define row (and nm (pair? args) (row-value (cadr args))))
       (when row (record! nm row))
       (for ([y (in-list x)]) (walk y))]
      [(pair? x) (for ([y (in-list x)]) (walk y))]
      [else (void)]))
  (walk sexp)
  (sort (for/list ([(k v) (in-hash acc)]) (cons k v))
        (λ (a b) (string<? (car a) (car b)))))

(define (effect-row-evidence expected observed)
  (append
   (for/list ([p (in-list expected)] #:when (not (assoc (car p) observed)))
     (format "missing row for ~a: ~s" (car p) (cdr p)))
   (for/list ([p (in-list observed)] #:when (not (assoc (car p) expected)))
     (format "extra row for ~a: ~s" (car p) (cdr p)))
   (for/list ([p (in-list expected)]
              #:when (let ([q (assoc (car p) observed)]) (and q (not (equal? (cdr p) (cdr q))))))
     (format "row for ~a changed: declared ~s observed ~s"
             (car p) (cdr p) (cdr (assoc (car p) observed))))))

(define (observe-effect-rows rule u1a-sexp u2-sexp)
  (define declared (loss-rule-disposition rule))
  (define e (extract-effect-rows u1a-sexp))
  (define o (extract-effect-rows u2-sexp))
  (define names-e (map car e))
  (define names-o (map car o))
  (define missing (filter (λ (n) (not (member n names-o))) names-e))
  (define extra (filter (λ (n) (not (member n names-e))) names-o))
  (define mismatch
    (filter identity
            (for/list ([p (in-list e)] #:when (assoc (car p) o))
              (define q (assoc (car p) o))
              (and (not (equal? (cdr p) (cdr q))) (car p)))))
  (cond
    [(and (null? e) (null? o))
     (loss-observation 'effect-rows declared 'untested
                       "no function in this file carries an effect row")]
    [(eq? declared 'preserved)
     (define ok? (and (null? missing) (null? extra) (null? mismatch)))
     (loss-observation 'effect-rows declared (if ok? 'preserved 'changed)
                       (if ok?
                           (format "~a row(s) carried in the target's native form" (length e))
                           (format "row(s) lost: missing=~s extra=~s constraints-changed=~s"
                                   missing extra mismatch)))]
    [(eq? declared 'approximated)
     (if (equal? (sort names-e string<?) (sort names-o string<?))
         (loss-observation 'effect-rows declared 'approximated
                           (format "~a row name(s) carried; ~a row(s) drop constraints"
                                   (length e) (length mismatch)))
         (loss-observation 'effect-rows declared 'changed
                           (format "carried effect names differ: missing=~s extra=~s"
                                   missing extra)))]
    [(eq? declared 'refused)
     (if (null? o)
         (loss-observation 'effect-rows declared 'refused
                           (format "~a row(s) not carried into the target" (length e)))
         (loss-observation 'effect-rows declared 'present
                           (format "target carried row(s) the model refuses: ~s" names-o)))]
    [else
     (loss-observation 'effect-rows declared 'untested "unknown disposition")]))
