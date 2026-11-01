#lang racket

;; racklr/loss/features.rkt — the projections the loss model's rules name.
;;
;; A feature is a canonical value derived from a normalized UIR serialization.
;; Two features are "preserved" when their values are equal. The projections
;; are deliberately coarse: the exact mode already requires the whole
;; normalized serialization to be equal, while the projected mode (TSX → JS)
;; leans on these named, reviewable projections instead.

(require racket/list
         racket/string
         racklr/loss/sexp
         racklr/loss-model
         racklr/loss/report)

(provide feature-projection
         compare-observed
         observe-rule
         control-tags
         guard-density)

;; Declarations at block scope only: module-level bindings, named functions,
;; and classes. Assignment expressions inside function bodies are deliberately
;; excluded — cross-front-end pairs (TS → JS) represent assignments differently
;; and the loss model must not mistake that representation difference for a
;; lost definition.
(define (definition-inventory sexp)
  (define acc '())
  (define (add n) (when n (set! acc (cons n acc))))
  (define (top-level x)
    (cond [(list-tag? x 'block) (for ([s (in-list (cadr x))]) (top-level s))]
          [(member (tag x) '(fn class method)) (add (ref-name (cadr x)))]
          [(member (tag x) '(let set!))
           (add (ref-name (cadr x)))
           (top-level (caddr x))]
          [(and (list-tag? x 'call)
                (member (ref-name (cadr x)) '("const" "let" "var" "export" "decorated")))
           (for ([a (in-list (caddr x))]) (top-level a))]
          [else (void)]))
  (top-level sexp)
  (sort (remove-duplicates acc) string<?))

(define (param-name p)
  (cond [(sym-name p) (sym-name p)]
        [(list-tag? p 'typed-param) (ref-name (cadr p))]
        [(list-tag? p 'var) (ref-name p)]
        [else #f]))

;; (name arity param-names) for declared functions and declaration-valued
;; function bindings (arrows, function expressions, __withEffects-wrapped).
(define (signature-inventory sexp)
  (define acc '())
  (define (add n params)
    (when (and n params)
      (set! acc (cons (list n (length params) (filter identity (map param-name params))) acc))))
  (define (fn-params fn)
    (call-with-values (λ () (fn-node-fields fn))
      (λ (_name params _body _ret _effects) params)))
  (define (fn-name-of fn)
    (call-with-values (λ () (fn-node-fields fn))
      (λ (name _params _body _ret _effects) name)))
  (define (params-of v)
    (cond [(fn-node? v) (fn-params v)]
          [(and (list-tag? v 'call)
                (member (ref-name (cadr v)) '("=>" "function")))
           (define args (caddr v))
           (define pnode (and (pair? args) (car args)))
           (and (list-tag? pnode 'list) (cdr pnode))]
          [(and (list-tag? v 'call) (equal? (ref-name (cadr v)) "__withEffects"))
           (params-of (last (caddr v)))]
          [else #f]))
  (define (top-level x)
    (cond [(list-tag? x 'block) (for ([s (in-list (cadr x))]) (top-level s))]
          [(fn-node? x) (add (sym-name (fn-name-of x)) (fn-params x))]
          [(member (tag x) '(let set!))
           (add (ref-name (cadr x)) (params-of (caddr x)))
           (top-level (caddr x))]
          [(and (list-tag? x 'call)
                (member (ref-name (cadr x)) '("const" "let" "var" "export" "decorated")))
           (for ([a (in-list (caddr x))]) (top-level a))]
          [else (void)]))
  (top-level sexp)
  (sort acc (λ (a b) (string<? (car a) (car b)))))

(define (class-inventory sexp)
  (for/list ([n (in-list (all-nodes sexp))] #:when (list-tag? n 'class)
             #:when (sym-name (cadr n)))
    (define methods (for/list ([m (in-list (list-ref n 4))]
                               #:when (list-tag? m 'method)
                               #:when (sym-name (cadr m)))
                      (sym-name (cadr m))))
    (list (sym-name (cadr n)) (sort methods string<?))))

(define (literal-inventory sexp)
  (sort (filter identity
                (for/list ([n (in-list (all-nodes sexp))])
                  (cond [(list-tag? n 'string) (format "s:~a" (cadr n))]
                        [(list-tag? n 'number) (format "n:~a" (cadr n))]
                        [else #f])))
        string<?))

(define control-tags
  '(if for-each while try with do match case await yield sub-block
       effect-handler eff-handler))

(define (control-census sexp)
  (sort (for/list ([t (in-list control-tags)])
          (cons t (for/sum ([n (in-list (all-nodes sexp))] #:when (list-tag? n t)) 1)))
        (λ (a b) (symbol<? (car a) (car b)))))

(define (import-inventory sexp)
  (sort (for/list ([n (in-list (all-nodes sexp))] #:when (list-tag? n 'import))
          (format "~s" (ref-name (cadr n))))
        string<?))

(define (type-decl-inventory sexp)
  (sort (filter identity
                (for/list ([n (in-list (all-nodes sexp))]
                           #:when (member (tag n) '(interface enum type-alias)))
                  (ref-name (cadr n))))
        string<?))

(define (jsx-density sexp)
  (for/sum ([n (in-list (all-nodes sexp))] #:when (list-tag? n 'element)) 1))

;; The guard support an emitter injects: generated declarations the source does
;; not bind plus calls to the guard primitives.
(define (guard-density user-names sexp)
  (define generated
    (for/sum ([n (in-list (all-nodes sexp))]
              #:when (and (pair? n) (decl-name n))
              #:when (let ([nm (decl-name n)])
                       (and (generated-name? nm) (not (member nm user-names)))))
      1))
  (define guarded
    (for/sum ([n (in-list (all-nodes sexp))]
              #:when (and (list-tag? n 'call)
                          (member (ref-name (cadr n))
                                  '("__perform" "__withEffects" "__handle"))))
      1))
  (+ generated guarded))

(define (feature-projection feature sexp)
  (case feature
    [(definitions) (definition-inventory sexp)]
    [(signatures) (signature-inventory sexp)]
    [(classes) (class-inventory sexp)]
    [(literals) (literal-inventory sexp)]
    [(control-flow) (control-census sexp)]
    [(types) (type-decl-inventory sexp)]
    [(imports) (import-inventory sexp)]
    [else '()]))

;; An "empty" projection: for the control-flow census every tag count is zero
;; rather than the list being null.
(define (feature-empty? feature v)
  (case feature
    [(control-flow) (and (list? v) (for/and ([p (in-list v)]) (zero? (cdr p))))]
    [else (null? v)]))

(define (compare-observed a b)
  (cond [(equal? a b) 'preserved]
        [(and (null? a) (null? b)) 'untested]
        [else 'changed]))

(define (observe-rule rule n1 n2 raw2 user-names)
  (define feature (loss-rule-feature rule))
  (define declared (loss-rule-disposition rule))
  (define (make observed detail)
    (loss-observation feature declared observed detail))
  (cond
    [(eq? feature 'effect-rows) 'deferred]
    [(eq? feature 'jsx)
     ;; The element tree is measured on the emitted-then-absorbed side, which is
     ;; where JSX lowering has already changed it.
     (define n (jsx-density raw2))
     (define observed
       (cond [(positive? n) (if (eq? declared 'refused) 'present 'approximated)]
             [(eq? declared 'refused) 'refused]
             [else 'untested]))
     (make observed
           (if (positive? n)
               (format "~a JSX element(s) reached the target UIR after lowering" n)
               "no JSX elements in the re-absorbed target UIR"))]
    [(eq? feature 'runtime-guards)
     (define n (guard-density user-names raw2))
     (define observed
       (cond [(positive? n) (if (eq? declared 'refused) 'present 'approximated)]
             [(eq? declared 'refused) 'refused]
             [else 'untested]))
     (make observed
           (if (positive? n)
               (format "~a injected guard declaration(s)/call(s) stripped" n)
               "no effect-guard runtime in the re-absorbed target"))]
    [(eq? feature 'formatting)
     (make 'untested "layout is not representable in UIR")]
    [(eq? feature 'statement-shape)
     (make 'untested "statement regrouping is only checked by the exact mode's structural comparison")]
    [(eq? feature 'comments)
     (make 'untested "UIR carries no comments")]
    [else
     (define p1 (feature-projection feature n1))
     (define p2 (feature-projection feature n2))
     (define same? (equal? p1 p2))
     (define detail
       (cond [(eq? declared 'preserved)
              (if same?
                  (format "~a ~a item(s) preserved" feature (length p1))
                  (format "~a changed: ~a missing, ~a added"
                          feature
                          (length (filter (λ (x) (not (member x p2))) p1))
                          (length (filter (λ (x) (not (member x p1))) p2))))]
             [(eq? declared 'approximated)
              (if same?
                  (format "~a items match despite the declared approximation" feature)
                  (format "~a differs as declared (~a item(s) re-shaped)"
                          feature
                          (+ (length (filter (λ (x) (not (member x p2))) p1))
                             (length (filter (λ (x) (not (member x p1))) p2)))))]
             [else
              (if (feature-empty? feature p2)
                  (format "~a dropped as declared" feature)
                  (format "~a present in the target although the model refuses it" feature))]))
     (make (cond [(eq? declared 'preserved) (if same? 'preserved 'changed)]
                 [(eq? declared 'approximated) (if same? 'preserved 'approximated)]
                 [(eq? declared 'refused) (if (feature-empty? feature p2) 'refused 'present)]
                 [else 'untested])
           detail)]))
