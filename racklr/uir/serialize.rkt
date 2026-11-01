#lang racket

(require "types.rkt")

(provide
 uir->sexp sexp->uir)

;; ── S-expression serialization ─────────────────────────────────────

(define (uir->sexp v)
  (cond [(uir-null? v) '(null)]
        [(uir-bool? v) `(bool ,(uir-bool-value v))]
        [(uir-number? v) `(number ,(uir-number-value v))]
        [(uir-string? v) `(string ,(uir-string-value v))]
        [(uir-fstring? v) `(fstring ,(uir-fstring-value v))]
        [(uir-list? v) `(list ,@(map uir->sexp (uir-list-items v)))]
        [(uir-record? v)
         `(record ,@(map (lambda (e)
                          (list (uir->sexp (car e))
                                (uir->sexp (cdr e))))
                        (uir-record-entries v)))]
        [(uir-symbol? v) `(symbol ,(uir-symbol-name v))]
        [(uir-fn? v) `(fn ,(let ([n (uir-fn-name v)]) (if n (uir->sexp n) #f))
                          ,(map uir->sexp (uir-fn-params v))
                          ,(uir->sexp (uir-fn-body v))
                          ,(let ([rt (uir-fn-return-type v)]) (if rt (uir->sexp rt) #f))
                          ,(let ([e (uir-fn-effects v)]) (if e (uir->sexp e) #f)))]
        [(uir-typed-param? v) `(typed-param ,(uir->sexp (uir-typed-param-name v))
                                            ,(let ([t (uir-typed-param-type v)]) (if t (uir->sexp t) #f))
                                            ,(let ([d (uir-typed-param-default v)]) (if d (uir->sexp d) #f)))]
        [(uir-call? v) `(call ,(uir->sexp (uir-call-callee v))
                              ,(map uir->sexp (uir-call-args v)))]
        [(uir-let? v) `(let ,(uir->sexp (uir-let-name v))
                            ,(uir->sexp (uir-let-value v))
                            ,(uir->sexp (uir-let-body v)))]
        [(uir-var? v) `(var ,(uir->sexp (uir-var-name v)))]
        [(uir-set!? v) `(set! ,(uir->sexp (uir-set!-name v))
                              ,(uir->sexp (uir-set!-value v)))]
        [(uir-ann-set!? v) `(ann-set! ,(uir->sexp (uir-ann-set!-lhs v))
                                      ,(uir->sexp (uir-ann-set!-type v))
                                      ,(uir->sexp (uir-ann-set!-value v)))]
        [(uir-walrus? v) `(walrus ,(uir->sexp (uir-walrus-name v))
                                   ,(uir->sexp (uir-walrus-value v)))]
        [(uir-type-alias? v) `(type-alias ,(uir->sexp (uir-type-alias-name v))
                                         ,(map uir->sexp (uir-type-alias-type-params v))
                                         ,(uir->sexp (uir-type-alias-value v)))]
        [(uir-if? v) `(if ,(uir->sexp (uir-if-test v))
                          ,(uir->sexp (uir-if-then v))
                          ,(uir->sexp (uir-if-else v)))]
        [(uir-block? v) `(block ,(map uir->sexp (uir-block-stmts v)))]
        [(uir-return? v) `(return ,(uir->sexp (uir-return-value v)))]
        [(uir-for-each? v) `(for-each ,(uir->sexp (uir-for-each-var v))
                                      ,(uir->sexp (uir-for-each-iterable v))
                                      ,(uir->sexp (uir-for-each-body v))
                                      ,(uir->sexp (uir-for-each-else-body v)))]
        [(uir-while? v) `(while ,(uir->sexp (uir-while-test v))
                                ,(uir->sexp (uir-while-body v))
                                ,(uir->sexp (uir-while-else-body v)))]
        [(uir-try? v) `(try ,(uir->sexp (uir-try-body v))
                            ,(map (lambda (cat)
                                    (list (uir->sexp (car cat))
                                          (uir->sexp (cadr cat))
                                          (uir->sexp (caddr cat))))
                                  (uir-try-catches v))
                            ,(let ([eb (uir-try-else-body v)])
                               (if eb (uir->sexp eb) 'unspecified))
                            ,(let ([fb (uir-try-finally-body v)])
                               (if fb (uir->sexp fb) 'unspecified)))]
        [(uir-with? v) `(with ,(map (lambda (item)
                                      (list (uir->sexp (car item))
                                            (if (cadr item)
                                                (uir->sexp (cadr item))
                                                'unspecified)))
                                    (uir-with-items v))
                              ,(uir->sexp (uir-with-body v)))]
        [(uir-await? v) `(await ,(uir->sexp (uir-await-expr v)))]
        [(uir-yield? v) `(yield ,(uir->sexp (uir-yield-value v))
                                ,(uir-yield-from? v))]
        [(uir-decorated? v) `(decorated ,(map uir->sexp (uir-decorated-decorators v))
                                       ,(uir->sexp (uir-decorated-inner v)))]
        [(uir-get? v) `(get ,(uir->sexp (uir-get-base v))
                            ,(uir->sexp (uir-get-field v)))]
        [(uir-paren? v) `(paren ,(uir->sexp (uir-paren-inner v)))]
        [(uir-class? v) `(class ,(uir->sexp (uir-class-name v))
                                ,(map uir->sexp (uir-class-super v))
                                ,(map uir->sexp (uir-class-fields v))
                                ,(map uir->sexp (uir-class-methods v)))]
        [(uir-method? v) `(method ,(uir->sexp (uir-method-name v))
                                  ,(map uir->sexp (uir-method-params v))
                                  ,(uir->sexp (uir-method-body v))
                                  ,(uir-method-visibility v))]
        [(uir-field? v) `(field ,(uir->sexp (uir-field-name v))
                                ,(uir->sexp (uir-field-type v))
                                ,(uir->sexp (uir-field-init v)))]
        [(uir-new? v) `(new ,(uir->sexp (uir-new-class v))
                            ,(map uir->sexp (uir-new-args v)))]
        [(uir-interface? v) `(interface ,(uir->sexp (uir-interface-name v))
                                        ,(map uir->sexp (uir-interface-methods v)))]
        [(uir-module? v) `(module ,(uir->sexp (uir-module-name v))
                                  ,(map uir->sexp (uir-module-imports v))
                                  ,(map uir->sexp (uir-module-exports v))
                                  ,(uir->sexp (uir-module-body v)))]
        [(uir-import? v) `(import ,(uir->sexp (uir-import-source v))
                                  ,(map uir->sexp (uir-import-names v)))]
        [(uir-export? v) `(export ,(map uir->sexp (uir-export-names v)))]
        [(uir-enum? v) `(enum ,(uir->sexp (uir-enum-name v))
                              ,(map uir->sexp (uir-enum-variants v)))]
        [(uir-enum-variant? v) `(enum-variant ,(uir->sexp (uir-enum-variant-name v))
                                              ,(map uir->sexp (uir-enum-variant-fields v))
                                              ,(let ([d (uir-enum-variant-discriminant v)])
                                                 (if d (uir->sexp d) #f)))]
        [(uir-component? v) `(component ,(uir->sexp (uir-component-name v))
                                        ,(map uir->sexp (uir-component-props v))
                                        ,(map uir->sexp (uir-component-states v))
                                        ,(map uir->sexp (uir-component-effects v))
                                        ,(uir->sexp (uir-component-template v)))]
        [(uir-element? v) `(element ,(uir->sexp (uir-element-tag v))
                                    ,(map uir->sexp (uir-element-attrs v))
                                    ,(map uir->sexp (uir-element-children v))
                                    ,(map uir->sexp (uir-element-events v)))]
        [(uir-attribute? v) `(attribute ,(uir->sexp (uir-attribute-name v))
                                        ,(uir->sexp (uir-attribute-value v)))]
        [(uir-event? v) `(event ,(uir->sexp (uir-event-name v))
                                ,(uir->sexp (uir-event-handler v)))]
        [(uir-slot? v) `(slot ,(uir->sexp (uir-slot-name v))
                              ,(uir->sexp (uir-slot-fallback v)))]
        [(uir-text-node? v) `(text-node ,(uir-string-value (uir-text-node-content v)))]
        [(uir-style? v) `(style ,@(map (lambda (p)
                                        (list (uir->sexp (car p)) (uir->sexp (cdr p))))
                                      (uir-style-styles v)))]
        [(uir-effect? v) `(effect ,(map uir->sexp (uir-effect-deps v))
                                  ,(uir->sexp (uir-effect-body v)))]
        [(uir-state? v) `(state ,(uir->sexp (uir-state-name v))
                                ,(uir->sexp (uir-state-init v)))]
        [(uir-jsx-expr? v) `(jsx-expr ,(uir-jsx-expr-content v))]
        [(uir-spread? v) `(spread ,(uir->sexp (uir-spread-expr v)))]
        [(uir-effect-row? v) `(effect-row ,@(map uir->sexp (uir-effect-row-entries v)))]
        [(uir-effect-entry? v)
         `(effect-entry ,(uir->sexp (uir-effect-entry-name v))
                        ,(uir-effect-entry-modality v)
                        ,(map uir->sexp (uir-effect-entry-ops v))
                        ,(let ([b (uir-effect-entry-budget v)]) (if b b #f))
                        ,(let ([s (uir-effect-entry-scope v)]) (if s (uir->sexp s) #f)))]
        [(uir-subtype? v) `(subtype ,(uir->sexp (uir-subtype-left v))
                                    ,(uir->sexp (uir-subtype-right v)))]
        [(uir-effect-decl? v) `(effect-decl ,(uir->sexp (uir-effect-decl-name v))
                                            ,(map uir->sexp (uir-effect-decl-ops v)))]
        [(uir-effect-op? v) `(effect-op ,(uir->sexp (uir-effect-op-name v))
                                        ,(map uir->sexp (uir-effect-op-params v))
                                        ,(let ([rt (uir-effect-op-result-type v)])
                                           (if rt (uir->sexp rt) #f)))]
        [(uir-effect-handler? v)
         `(effect-handler ,(let ([e (uir-effect-handler-effect v)])
                             (if e (uir->sexp e) #f))
                          ,(map (lambda (op)
                                  (list (uir->sexp (car op))
                                        (map uir->sexp (cadr op))
                                        (uir->sexp (caddr op))))
                                (uir-effect-handler-ops v))
                           ,(let ([rc (uir-effect-handler-return-clause v)])
                              (if rc (uir->sexp rc) #f))
                           ,(uir->sexp (uir-effect-handler-body v)))]
        [(uir-eff-system? v) `(eff-system ,(uir->sexp (uir-eff-system-name v))
                                          ,(map uir->sexp (uir-eff-system-decls v)))]
        [(uir-eff-capability? v) `(eff-capability ,(uir->sexp (uir-eff-capability-name v))
                                                  ,(map uir->sexp (uir-eff-capability-type-params v))
                                                  ,(map uir->sexp (uir-eff-capability-ops v)))]
        [(uir-eff-capability-op? v) `(eff-capability-op ,(uir->sexp (uir-eff-capability-op-name v))
                                                        ,(map uir->sexp (uir-eff-capability-op-params v))
                                                        ,(let ([rt (uir-eff-capability-op-result-type v)])
                                                           (if rt (uir->sexp rt) #f)))]
        [(uir-eff-ref? v) `(eff-ref ,(uir->sexp (uir-eff-ref-name v))
                                    ,(map uir->sexp (uir-eff-ref-type-args v)))]
        [(uir-eff-given? v) `(eff-given ,(map uir->sexp (uir-eff-given-row v)))]
        [(uir-eff-handler? v) `(eff-handler ,(uir->sexp (uir-eff-handler-effect v))
                                            ,(map uir->sexp (uir-eff-handler-row v))
                                            ,(map (lambda (op)
                                                    (list (uir->sexp (car op))
                                                          (map uir->sexp (cadr op))
                                                          (let ([rt (caddr op)]) (if rt (uir->sexp rt) #f))
                                                          (uir->sexp (cadddr op))))
                                                  (uir-eff-handler-ops v)))]
        [(uir-eff-component? v) `(eff-component ,(uir->sexp (uir-eff-component-name v))
                                                ,(map uir->sexp (uir-eff-component-needs v))
                                                ,(map uir->sexp (uir-eff-component-provides v))
                                                ,(map (lambda (op)
                                                        (list (uir->sexp (car op))
                                                              (map uir->sexp (cadr op))
                                                              (let ([rt (caddr op)]) (if rt (uir->sexp rt) #f))
                                                              (uir->sexp (cadddr op))))
                                                      (uir-eff-component-ops v)))]
        [(uir-eff-state? v) `(eff-state ,(uir->sexp (uir-eff-state-name v))
                                        ,(uir->sexp (uir-eff-state-type v))
                                        ,(uir->sexp (uir-eff-state-init v)))]
        [(uir-eff-action? v) `(eff-action ,(uir->sexp (uir-eff-action-name v))
                                          ,(map uir->sexp (uir-eff-action-steps v)))]
        [(uir-eff-invariant? v) `(eff-invariant ,(uir->sexp (uir-eff-invariant-expr v)))]
        [(uir-eff-policy? v) `(eff-policy ,(uir-eff-policy-kind v)
                                          ,(uir->sexp (uir-eff-policy-predicate v)))]
        [else (error 'uir->sexp "not a UIR node: ~e" v)]))

(define (sexp->uir s)
  (match s
    ['(null) (uir-null)]
    [`(bool ,v) (uir-bool v)]
    [`(number ,v) (uir-number v)]
    [`(string ,v) (uir-string v)]
    [`(fstring ,v) (uir-fstring v)]
    [`(list ,xs ...) (uir-list (map sexp->uir xs))]
    [`(record ,es ...)
     (uir-record (map (lambda (e)
                       (match e
                         [`(,k ,v) (cons (sexp->uir k) (sexp->uir v))]
                         [_ (error 'sexp->uir "invalid record entry: ~e" e)]))
                     es))]
    [`(symbol ,n) (uir-symbol n)]
    [`(fn ,name ,ps ,body ,rt ,efx) (uir-fn (if name (sexp->uir name) #f) (map sexp->uir ps) (sexp->uir body) (if rt (sexp->uir rt) #f) (if efx (sexp->uir efx) #f))]
    [`(typed-param ,name ,type ,default) (uir-typed-param (sexp->uir name) (if (eq? type #f) #f (sexp->uir type)) (if (eq? default #f) #f (sexp->uir default)))]
    [`(call ,c ,as) (uir-call (sexp->uir c) (map sexp->uir as))]
    [`(let ,n ,v ,body) (uir-let (sexp->uir n) (sexp->uir v) (sexp->uir body))]
    [`(var ,n) (uir-var (sexp->uir n))]
    [`(set! ,n ,v) (uir-set! (sexp->uir n) (sexp->uir v))]
    [`(ann-set! ,lhs ,type ,value) (uir-ann-set! (sexp->uir lhs) (sexp->uir type) (sexp->uir value))]
    [`(walrus ,n ,v) (uir-walrus (sexp->uir n) (sexp->uir v))]
    [`(type-alias ,n ,tps ,v) (uir-type-alias (sexp->uir n) (map sexp->uir tps) (sexp->uir v))]
    [`(if ,tst ,thn ,els) (uir-if (sexp->uir tst) (sexp->uir thn) (sexp->uir els))]
    [`(block ,ss) (uir-block (map sexp->uir ss))]
    [`(return ,v) (uir-return (sexp->uir v))]
    [`(for-each ,var ,iter ,body ,else)
     (uir-for-each (sexp->uir var) (sexp->uir iter) (sexp->uir body) (sexp->uir else))]
    [`(try ,body ,catches ,else-body ,finally-body)
     (uir-try (sexp->uir body)
              (map (lambda (cat)
                     (match cat
                       [`(,et ,en ,b) (list (sexp->uir et) (sexp->uir en) (sexp->uir b))]
                       [_ (error 'sexp->uir "invalid catch: ~e" cat)]))
                   catches)
              (if (eq? else-body 'unspecified) #f (sexp->uir else-body))
              (if (eq? finally-body 'unspecified) #f (sexp->uir finally-body)))]
     [`(with ,items ,body)
      (uir-with (map (lambda (item)
                      (match item
                        [`(,ctx ,as-name)
                         (list (sexp->uir ctx)
                               (if (eq? as-name 'unspecified) #f (sexp->uir as-name)))]
                        [_ (error 'sexp->uir "invalid with-item: ~e" item)]))
                    items)
                (sexp->uir body))]
    [`(await ,expr) (uir-await (sexp->uir expr))]
    [`(yield ,value ,from?) (uir-yield (sexp->uir value) from?)]
    [`(decorated ,decos ,inner) (uir-decorated (map sexp->uir decos) (sexp->uir inner))]
    [`(get ,base ,field) (uir-get (sexp->uir base) (sexp->uir field))]
    [`(paren ,inner) (uir-paren (sexp->uir inner))]
    [`(class ,name ,supers ,fields ,methods)
     (uir-class (sexp->uir name) (map sexp->uir supers)
                (map sexp->uir fields) (map sexp->uir methods))]
    [`(method ,name ,params ,body ,vis)
     (uir-method (sexp->uir name) (map sexp->uir params) (sexp->uir body) vis)]
    [`(field ,name ,type ,init)
     (uir-field (sexp->uir name) (sexp->uir type) (sexp->uir init))]
    [`(new ,cls ,args)
     (uir-new (sexp->uir cls) (map sexp->uir args))]
    [`(interface ,name ,methods)
     (uir-interface (sexp->uir name) (map sexp->uir methods))]
    [`(module ,name ,imports ,exports ,body)
     (uir-module (sexp->uir name) (map sexp->uir imports)
                 (map sexp->uir exports) (sexp->uir body))]
    [`(import ,source ,names)
     (uir-import (sexp->uir source) (map sexp->uir names))]
    [`(export ,names)
     (uir-export (map sexp->uir names))]
    [`(enum ,name ,variants)
     (uir-enum (sexp->uir name) (map sexp->uir variants))]
    [`(enum-variant ,name ,fields ,discriminant)
     (uir-enum-variant (sexp->uir name) (map sexp->uir fields)
                       (if discriminant (sexp->uir discriminant) #f))]
    [`(component ,name ,props ,states ,effects ,tmpl)
     (uir-component (sexp->uir name) (map sexp->uir props)
                    (map sexp->uir states) (map sexp->uir effects)
                    (sexp->uir tmpl))]
    [`(element ,tag ,attrs ,children ,events)
     (uir-element (sexp->uir tag) (map sexp->uir attrs)
                  (map sexp->uir children) (map sexp->uir events))]
    [`(attribute ,n ,v) (uir-attribute (sexp->uir n) (sexp->uir v))]
    [`(event ,n ,h) (uir-event (sexp->uir n) (sexp->uir h))]
    [`(slot ,n ,fb) (uir-slot (sexp->uir n) (sexp->uir fb))]
    [`(text-node ,txt) (uir-text-node (uir-string txt))]
    [`(style ,es ...)
     (uir-style (map (lambda (e)
                      (match e
                        [`(,k ,v) (cons (sexp->uir k) (sexp->uir v))]
                        [_ (error 'sexp->uir "invalid style entry: ~e" e)]))
                    es))]
    [`(effect ,ds ,body) (uir-effect (map sexp->uir ds) (sexp->uir body))]
    [`(state ,n ,init) (uir-state (sexp->uir n) (sexp->uir init))]
     [`(jsx-expr ,content) (uir-jsx-expr content)]
    [`(spread ,expr) (uir-spread (sexp->uir expr))]
    [`(effect-row ,es ...) (uir-effect-row (map sexp->uir es))]
    [`(effect-entry ,name ,modality ,ops ,budget ,scope)
     (uir-effect-entry (sexp->uir name) modality (map sexp->uir ops)
                       (if (eq? budget #f) #f budget)
                       (if (eq? scope #f) #f (sexp->uir scope)))]
    [`(subtype ,l ,r) (uir-subtype (sexp->uir l) (sexp->uir r))]
    [`(effect-decl ,name ,ops)
     (uir-effect-decl (sexp->uir name) (map sexp->uir ops))]
    [`(effect-op ,name ,params ,result-type)
     (uir-effect-op (sexp->uir name) (map sexp->uir params)
                    (if (eq? result-type #f) #f (sexp->uir result-type)))]
    [`(effect-handler ,effect ,ops ,return-clause ,body)
     (uir-effect-handler (if (eq? effect #f) #f (sexp->uir effect))
                         (map (lambda (op)
                                (match op
                                  [`(,n ,ps ,b) (list (sexp->uir n) (map sexp->uir ps) (sexp->uir b))]
                                  [_ (error 'sexp->uir "invalid handler op: ~e" op)]))
                              ops)
                          (if (eq? return-clause #f) #f (sexp->uir return-clause))
                          (sexp->uir body))]
    [`(eff-system ,name ,decls)
     (uir-eff-system (sexp->uir name) (map sexp->uir decls))]
    [`(eff-capability ,name ,type-params ,ops)
     (uir-eff-capability (sexp->uir name) (map sexp->uir type-params) (map sexp->uir ops))]
    [`(eff-capability-op ,name ,params ,result-type)
     (uir-eff-capability-op (sexp->uir name) (map sexp->uir params)
                            (if (eq? result-type #f) #f (sexp->uir result-type)))]
    [`(eff-ref ,name ,type-args)
     (uir-eff-ref (sexp->uir name) (map sexp->uir type-args))]
    [`(eff-given ,row)
     (uir-eff-given (map sexp->uir row))]
    [`(eff-handler ,effect ,row ,ops)
     (uir-eff-handler (sexp->uir effect) (map sexp->uir row)
                      (map (lambda (op)
                             (match op
                               [`(,n ,ps ,rt ,b)
                                (list (sexp->uir n) (map sexp->uir ps)
                                      (if (eq? rt #f) #f (sexp->uir rt))
                                      (sexp->uir b))]
                               [_ (error 'sexp->uir "invalid eff op: ~e" op)]))
                           ops))]
    [`(eff-component ,name ,needs ,provides ,ops)
     (uir-eff-component (sexp->uir name) (map sexp->uir needs) (map sexp->uir provides)
                        (map (lambda (op)
                               (match op
                                 [`(,n ,ps ,rt ,b)
                                  (list (sexp->uir n) (map sexp->uir ps)
                                        (if (eq? rt #f) #f (sexp->uir rt))
                                        (sexp->uir b))]
                                 [_ (error 'sexp->uir "invalid eff op: ~e" op)]))
                             ops))]
    [`(eff-state ,name ,type ,init)
     (uir-eff-state (sexp->uir name) (sexp->uir type) (sexp->uir init))]
    [`(eff-action ,name ,steps)
     (uir-eff-action (sexp->uir name) (map sexp->uir steps))]
    [`(eff-invariant ,expr)
     (uir-eff-invariant (sexp->uir expr))]
    [`(eff-policy ,kind ,predicate)
     (uir-eff-policy kind (sexp->uir predicate))]
    [_ (error 'sexp->uir "invalid uir sexp: ~e" s)]))
