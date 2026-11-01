#lang racket

;; racklr/loss/normalize.rkt — turn an absorbed UIR serialization into the
;; normal form the loss model declares before comparison.
;;
;; Each transform corresponds to a normalization symbol in
;; racklr/loss-model.rkt. Declaring the normalization is what makes a loss
;; "declared": the verifier only strips what the pair's model names, so any
;; other difference surfaces as a violation.

(require racket/list
         racket/string
         racklr/loss-model
         racklr/loss/sexp)

(provide normalize
         bound-names
         strip-generated
         strip-effect-guards
         strip-parens
         erase-js-types)

;; Every name the source itself binds. Generated names are only dropped when
;; the source does not use the name, so user code called `__helper` is never
;; silently removed from the comparison.
(define (bound-names sexp)
  (define acc (make-hash))
  (define (walk x)
    (cond [(pair? x)
           (define n (decl-name x))
           (when n (hash-set! acc n #t))
           (when (list-tag? x 'typed-param)
             (define p (ref-name (cadr x)))
             (when p (hash-set! acc p #t)))
           (for ([y (in-list x)]) (walk y))]
          [else (void)]))
  (walk sexp)
  (hash-keys acc))

(define (normalize model user-names sexp)
  (define norms (loss-model-normalizations model))
  (define x1 (if (memq 'drop-generated norms)
                 (strip-generated (loss-model-injected model) user-names sexp)
                 sexp))
  (define x2 (if (memq 'erase-js-types norms) (erase-js-types x1) x1))
  (define x3 (if (memq 'drop-effect-guards norms) (strip-effect-guards x2) x2))
  (define x4 (if (memq 'drop-parens norms) (strip-parens x3) x3))
  x4)

;; Drop declarations/imports the emitter injects (its effect-guard runtime).
(define (strip-generated injected user-names x)
  (define (drop? stmt)
    (define n (and (pair? stmt) (decl-name stmt)))
    (and n
         (or (and (generated-name? n) (not (member n user-names)))
             (and (list-tag? stmt 'import) (member n injected)))))
  (cond [(list-tag? x 'block)
         (list 'block (for/list ([s (in-list (cadr x))] #:unless (drop? s))
                        (strip-generated injected user-names s)))]
        [(pair? x) (map (λ (y) (strip-generated injected user-names y)) x)]
        [else x]))

;; Effect guards annotate calls rather than changing what the program does.
;; They are dropped from the structural comparison; the effect-row feature
;; verifies separately that the row itself was carried.
(define (strip-effect-guards x)
  (define (guard-callee? c)
    (and (member (ref-name c) '("__perform" "__withEffects" "__handle")) #t))
  (define (unwrap-thunk th)
    ;; A guarded call lowers as a thunk: Python lambda / JS function
    ;; expression. Recover the original call.
    (define body
      (cond [(list-tag? th 'fn) (list-ref th 3)]
            [else #f]))
    (define last-stmt
      (cond [(and (list-tag? body 'block))
             (let ([stmts (cadr body)]) (and (pair? stmts) (last stmts)))]
            [else body]))
    (strip-effect-guards
     (cond [(list-tag? last-stmt 'return) (cadr last-stmt)]
           [else last-stmt])))
  (cond
    [(list-tag? x 'fn)
     (list 'fn (cadr x) (map strip-effect-guards (caddr x))
           (strip-effect-guards (cadddr x))
           (strip-effect-guards (list-ref x 4))
           #f)]
    [(list-tag? x 'decorated)
     (define decos (cadr x))
     (define keep (filter (λ (d) (not (and (list-tag? d 'call) (guard-callee? (cadr d)))))
                          decos))
     (define inner (strip-effect-guards (caddr x)))
     (cond [(null? keep) inner]
           [else (list 'decorated keep inner)])]
    [(and (list-tag? x 'call) (guard-callee? (cadr x)))
     (define name (ref-name (cadr x)))
     (cond [(string=? name "__perform") (unwrap-thunk (last (caddr x)))]
           [else (strip-effect-guards (last (caddr x)))])]
    [(pair? x) (map strip-effect-guards x)]
    [else x]))

(define (strip-parens x)
  (cond [(list-tag? x 'paren) (strip-parens (cadr x))]
        [(pair? x) (map strip-parens x)]
        [else x]))

;; TypeScript type syntax has no JavaScript runtime form. Erase it so a TS→JS
;; round trip is compared on runtime structure; the model's `types` rule
;; records that the erasure is intentional.
(define (erase-js-types x)
  (define (erase-param p)
    (cond [(list-tag? p 'typed-param)
           (define nm (cadr p))
           (define default-arg (let ([d (cadddr p)]) (and d (erase-js-types d))))
           (if default-arg
               (list 'typed-param nm #f default-arg)
               nm)]
          [(list-tag? p 'symbol) p]
          [(list-tag? p 'var) p]
          [(list-tag? p 'list) (map erase-param (cadr p))]  ; rest/pattern markers
          [else (erase-js-types p)]))
  (cond
    [(list-tag? x 'fn)
     (list 'fn (cadr x) (map erase-param (caddr x))
           (erase-js-types (cadddr x))
           #f
           (erase-js-types (list-ref x 5)))]
    [(list-tag? x 'interface) '(null)]
    [(list-tag? x 'enum) '(null)]
    [(list-tag? x 'type-alias) '(null)]
    [(list-tag? x 'ann-set!)
     (define v (cadddr x))
     (list 'set! (cadr x) (if v (erase-js-types v) '(null)))]
    [(pair? x) (map erase-js-types x)]
    [else x]))
