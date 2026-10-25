#lang racket

;; racklr/effects/collect.rkt
;;
;; Program-surface collection: harvest named functions, methods, class names and
;; superclass maps from a UIR program, plus the MRO helper used to resolve
;; inherited methods.

(require racklr/uir
         racklr/effects/walk
         racklr/effects/shape)

(provide
  collect-functions
  collect-methods
  collect-function-params
  collect-method-params
  collect-class-names
  collect-supers
  mro-chain
  class-name-of-name
  owner-of-name)

;; name-symbol → function body, for every named function reachable from
;; `program`: named uir-fn nodes plus the JS/TS `set!`-bound encodings
;; (function declarations, async/generator declarations, and bound arrows).
(define (collect-functions program)
  (define h (make-hash))
  (define (visit! n)
    (cond
      [(uir-fn? n)
       (let ([nm (uir-fn-name n)])
         (if (and nm (uir-symbol? nm))
             (hash-set! h (string->symbol (uir-symbol-name nm)) (uir-fn-body n))
             (void)))]
      [(named-function n)
       => (λ (p) (hash-set! h (car p) (cdr p)))]
      [else (void)]))
  (walk-uir program visit!)
  h)

;; name-symbol → (listof uir-param), for every named function reachable from
;; `program`. Mirrors collect-functions but keeps the parameter list (typed or
;; untyped) so receiver-type inference can seed locals from type annotations.
(define (collect-function-params program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-fn? n)
      (let ([nm (uir-fn-name n)])
        (if (and nm (uir-symbol? nm))
            (hash-set! h (string->symbol (uir-symbol-name nm)) (uir-fn-params n))
            (void)))))
  (walk-uir program visit!)
  h)

;; Class name symbol (or #f) for a `uir-class` node.
(define (class-name-symbol n)
  (and (uir-class? n)
       (let ([nm (uir-class-name n)])
         (and (uir-symbol? nm)
              (string->symbol (uir-symbol-name nm))))))

;; Super references for a `uir-class` node, in left-to-right base order. A bare
;; name (`BaseView`) is a symbol. A dotted name (`models.Model`,
;; `base.Serializer`) is a (first-name-symbol . full-dotted-name-symbol) pair so
;; the whole-program resolver can follow the module alias through the import
;; graph instead of collapsing `base.Serializer` to `Serializer` and self-looping.
;; A subscripted generic base (`Generic[T]`) or other non-symbol base yields no
;; reference for that position.
(define (class-super-names n)
  (and (uir-class? n)
       (filter values
               (map super-ref-of
                    (uir-class-super n)))))

;; Dotted-name components of a uir-get chain as strings, from the root name
;; through each field, or #f when the chain is rooted at something other than a
;; name (a call, subscript, or literal).
(define (get-chain-parts expr)
  (let loop ([e expr] [acc '()])
    (cond
      [(uir-get? e)
       (define f (get-field-symbol (uir-get-field e)))
       (and f (loop (uir-get-base e) (cons (symbol->string f) acc)))]
      [(uir-var? e) (cons (uir-symbol-name (uir-var-name e)) acc)]
      [else #f])))

;; One positional base expression → super reference (symbol for a bare name, a
;; (first . full) pair for a dotted name, #f for anything else).
(define (super-ref-of sup)
  (cond
    [(uir-var? sup) (string->symbol (uir-symbol-name (uir-var-name sup)))]
    [(uir-get? sup)
     (define parts (get-chain-parts sup))
     (cond
       [(and parts (pair? parts))
        (define full (string->symbol (string-join parts ".")))
        (if (null? (cdr parts))
            full
            (cons (string->symbol (car parts)) full))]
       [else #f])]
    [else #f]))

;; A class method-list node to its uir-method, unwrapping the two wrappers the
;; Python lowerer may put around it: (uir-decorated decos inner) for decorated
;; methods, and (call "async-fn"/"gen-fn" (list method)) for async def methods.
(define (unwrap-method m)
  (cond
    [(uir-method? m) m]
    [(uir-decorated? m) (unwrap-method (uir-decorated-inner m))]
    [(and (uir-call? m)
          (uir-symbol? (uir-call-callee m))
          (member (uir-symbol-name (uir-call-callee m)) '("async-fn" "gen-fn"))
          (= (length (uir-call-args m)) 1))
     (unwrap-method (first (uir-call-args m)))]
    [else #f]))

;; Class-qualified name symbol → method body, for every method of every
;; `uir-class` node reachable from `program`. Methods are keyed as `Class.method`
;; so a `self.m()` inside that class (or `Other.m()` elsewhere) resolves to the
;; same call-graph node as a bare function.
(define (collect-methods program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (when cname
        (for ([m (in-list (uir-class-methods n))])
          (define mm (unwrap-method m))
          (when (uir-method? mm)
            (define mname (and (uir-method-name mm)
                               (uir-symbol? (uir-method-name mm))
                               (string->symbol (uir-symbol-name (uir-method-name mm)))))
            (when mname
              (hash-set! h (qualified-name-symbol cname mname) (uir-method-body mm))))))))
  (walk-uir program visit!)
  h)

;; Class-qualified name symbol → (listof uir-param), for every method of every
;; `uir-class` node reachable from `program`. Mirrors collect-methods.
(define (collect-method-params program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (when cname
        (for ([m (in-list (uir-class-methods n))])
          (define mm (unwrap-method m))
          (when (uir-method? mm)
            (define mname (and (uir-method-name mm)
                               (uir-symbol? (uir-method-name mm))
                               (string->symbol (uir-symbol-name (uir-method-name mm)))))
            (when mname
              (hash-set! h (qualified-name-symbol cname mname) (uir-method-params mm))))))))
  (walk-uir program visit!)
  h)

;; Set of all class name symbols reachable from `program`.
(define (collect-class-names program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (when cname (hash-set! h cname #t))))
  (walk-uir program visit!)
  h)

;; Class name symbol → (listof super-name-symbol), for every `uir-class` node
;; reachable from `program`. Each list preserves left-to-right superclass order
;; (Python's C3 MRO); classes with no superclass are absent.
(define (collect-supers program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (define sups (class-super-names n))
      (when (and cname (pair? sups))
        (hash-set! h cname sups))))
  (walk-uir program visit!)
  h)

;; Linearized superclass chain for `class-name` (most derived first, then each
;; super), computed as a depth-first walk over the superclass map with a cycle
;; guard. Includes `class-name` itself. Used to resolve inherited methods:
;; `self.m()` searches the chain for the first class declaring `m`.
(define (mro-chain class-name supers)
  ;; Pre-order DFS over the superclass graph (leftmost base first), visiting each
  ;; class once. This approximates Python's C3 linearization for effect
  ;; resolution: `self.m()` searches the chain for the first class declaring `m`,
  ;; which is exactly the leftmost-depth-first order mixins follow. A qualified
  ;; super reference (`base.Serializer`) is skipped here: single-file inference
  ;; has no import graph to resolve it, so it ends the chain exactly like an
  ;; unknown/external superclass.
  (define seen (make-hash))
  (define out '())
  (let loop ([cls class-name])
    (when (and cls (not (hash-has-key? seen cls)))
      (hash-set! seen cls #t)
      (set! out (cons cls out))
      (for ([s (in-list (hash-ref supers cls '()))])
        (when (symbol? s) (loop s)))))
  (reverse out))

;; Class name symbol before the first dot in a `Class.method` node name, or #f
;; for a bare function name.
(define (class-name-of-name n)
  (define s (symbol->string n))
  (define owner-str (regexp-replace #rx"[.].*$" s ""))
  (and (not (string=? owner-str s)) (string->symbol owner-str)))

;; Enclosing class name symbol (or #f) for a call-graph node keyed `Class.method`.
(define (owner-of-name methods n)
  (and (hash-has-key? methods n) (class-name-of-name n)))
