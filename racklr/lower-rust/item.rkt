#lang racket

;; racklr/lower-rust/item.rkt — Rust items (fn/struct/enum/impl/trait/mod/use)
;; → UIR.
;;
;; `impl` blocks are folded into the target `uir-class` in a second pass, so an
;; impl may appear before or after its struct. A trait impl also records the
;; trait in the class's super list (UIR has no "implements" node: documented
;; approximation). Effects are #f on every fn/method: Rust has no effect
;; annotation surface in pass 1, and a pre-attached row would suppress the
;; downstream inference pass (infer.rkt treats any existing row as
;; hand-written).

(require racket/list
         racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "expr.rkt"
         "pattern.rkt"
         "stmt.rkt")

(provide lower-rust-item
         lower-rust-items)

;; Tags a `visItem`/`macroItem` payload can have, ordered for a shallow BFS.
(define item-tags
  '(module externCrate useDeclaration function_ typeAlias struct_ enumeration
    union_ constantItem staticItem trait_ implementation externBlock
    macroInvocationSemi macroRulesDefinition))

;; First node under an `item` whose tag is an item tag, breadth-first so an
;; outermost item wins over a function nested inside an impl/body.
(define (item-payload item)
  (let bfs ([level (list item)])
    (cond
      [(null? level) #f]
      [else
       (define hit (for/or ([n (in-list level)])
                     (and (memq (node-tag n) item-tags) n)))
       (or hit (bfs (append* (for/list ([n (in-list level)])
                               (node-children n)))))])))

;; Lower a list of crate/module `item` nodes, folding impl blocks into classes.
(define (lower-rust-items items tk-type tk-value)
  (define payloads (filter values (map item-payload items)))
  (define impls (filter (λ (p) (eq? (node-tag p) 'implementation)) payloads))
  (define others (filter (λ (p) (not (eq? (node-tag p) 'implementation))) payloads))
  (define lowered (map (λ (p) (lower-rust-item p tk-type tk-value)) others))
  (for/fold ([acc lowered]) ([impl (in-list impls)])
    (attach-impl impl tk-type tk-value acc)))

(define (lower-rust-item payload tk-type tk-value)
  (case (node-tag payload)
    [(function_) (lower-function payload tk-type tk-value #f)]
    [(struct_) (lower-struct payload tk-type tk-value)]
    [(enumeration) (lower-enum payload tk-type tk-value)]
    [(useDeclaration) (lower-use payload tk-type tk-value)]
    [(module) (lower-module payload tk-type tk-value)]
    [(trait_) (lower-trait payload tk-type tk-value)]
    [(typeAlias) (lower-type-alias payload tk-type tk-value)]
    [(constantItem staticItem) (lower-const payload tk-type tk-value)]
    [(externCrate) (uir-import (uir-symbol (or (name-after-keyword payload 'KW_CRATE tk-type tk-value) "?")) '())]
    [(macroInvocationSemi) (lower-expr payload tk-type tk-value)]
    [else (uir-null)]))

;; ── Names ───────────────────────────────────────────────────────────

;; The identifier right after a keyword token (fn/struct/enum/trait/mod/...).
(define (name-after-keyword node keyword tk-type tk-value)
  (define toks (deep-tokens node tk-type))
  (let loop ([ts toks] [seen? #f])
    (cond
      [(null? ts) #f]
      [(and seen? (memq (tk-type (first ts)) '(NON_KEYWORD_IDENTIFIER IDENTIFIER KW_SELFTYPE)))
       (tk-value (first ts))]
      [(eq? (tk-type (first ts)) keyword) (loop (rest ts) #t)]
      [else (loop (rest ts) seen?)])))

(define (offset node)
  (source-pos-offset (car (any-tree-range node))))

;; ── Functions and methods ───────────────────────────────────────────

(define (lower-function fn tk-type tk-value method?)
  (define name (or (name-after-keyword fn 'KW_FN tk-type tk-value) "?fn"))
  (define params (lower-params (find-descendant fn 'functionParameters) tk-type tk-value))
  (define ret-node (find-descendant fn 'functionReturnType))
  (define ret (and ret-node (find-descendant ret-node 'type_)
                   (lower-type (find-descendant ret-node 'type_) tk-type tk-value)))
  (define body-node (find-descendant fn 'blockExpression))
  (define body (if body-node (lower-fn-body body-node tk-type tk-value) (uir-null)))
  (if method?
      (uir-method (uir-symbol name) params body 'public)
      (uir-fn (uir-symbol name) params body ret #f)))

(define (lower-params params-node tk-type tk-value)
  (cond
    [(not params-node) '()]
    [else
     (define nodes
       (sort (append (find-descendants params-node 'selfParam)
                     (find-descendants params-node 'functionParam))
             (λ (a b) (< (offset a) (offset b)))))
     (for/list ([p (in-list nodes)] [i (in-naturals)])
       (lower-param p i tk-type tk-value))]))

(define (lower-param p i tk-type tk-value)
  (cond
    [(eq? (node-tag p) 'selfParam) (uir-typed-param (uir-symbol "self") #f #f #f)]
    [else
     (define fpp (find-descendant p 'functionParamPattern))
     (define type-node (and fpp (find-descendant fpp 'type_)))
     (cond
       [fpp
        (define pat (find-descendant fpp 'pattern))
        (define id (if pat (first-identifier pat tk-type tk-value) (format "_p~a" i)))
        (uir-typed-param (uir-symbol id)
                         (and type-node (lower-type type-node tk-type tk-value)) #f #f)]
       [else
        ;; Bare type parameter (`fn f(i32)`); needs a synthesized name.
        (define tn (find-descendant p 'type_))
        (uir-typed-param (uir-symbol (format "_p~a" i))
                         (and tn (lower-type tn tk-type tk-value)) #f #f)])]))

;; ── Structs, enums, aliases, constants ──────────────────────────────

(define (lower-struct struct tk-type tk-value)
  (define payload (single-node-child struct))
  (define name (or (name-after-keyword struct 'KW_STRUCT tk-type tk-value) "?struct"))
  (define fields
    (cond
      [(eq? (node-tag payload) 'structStruct)
       (for/list ([f (find-descendants payload 'structField)])
         (define id (first-identifier (or (find-descendant f 'identifier) f) tk-type tk-value))
         (define tn (find-descendant f 'type_))
         (uir-field (uir-symbol id) (if tn (lower-type tn tk-type tk-value) (uir-null))
                    (uir-null)))]
      [(eq? (node-tag payload) 'tupleStruct)
       (for/list ([f (find-descendants payload 'tupleField)] [i (in-naturals)])
         (define tn (find-descendant f 'type_))
         (uir-field (uir-symbol (format "_~a" i))
                    (if tn (lower-type tn tk-type tk-value) (uir-null))
                    (uir-null)))]
      [else '()]))
  (uir-class (uir-symbol name) '() fields '()))

(define (lower-enum node tk-type tk-value)
  (define name (or (name-after-keyword node 'KW_ENUM tk-type tk-value) "?enum"))
  (define variants
    (for/list ([item (find-descendants node 'enumItem)])
      (define vname (or (first-identifier (or (find-descendant item 'identifier) item) tk-type tk-value) "?"))
      (define tuple-node (find-descendant item 'enumItemTuple))
      (define struct-node (find-descendant item 'enumItemStruct))
      (define disc-node (find-descendant item 'enumItemDiscriminant))
      (define fields
        (cond
          [tuple-node
           (for/list ([f (find-descendants tuple-node 'tupleField)] [i (in-naturals)])
             (define tn (find-descendant f 'type_))
             (uir-typed-param (uir-symbol (format "_~a" i))
                              (if tn (lower-type tn tk-type tk-value) #f) #f #f))]
          [struct-node
           (for/list ([f (find-descendants struct-node 'structField)])
             (define id (first-identifier (or (find-descendant f 'identifier) f) tk-type tk-value))
             (define tn (find-descendant f 'type_))
             (uir-typed-param (uir-symbol id)
                              (if tn (lower-type tn tk-type tk-value) #f) #f #f))]
          [else '()]))
      (define disc (and disc-node (find-descendant disc-node 'expression)
                        (lower-expr (find-descendant disc-node 'expression) tk-type tk-value)))
      (uir-enum-variant (uir-symbol vname) fields disc)))
  (uir-enum (uir-symbol name) variants))

(define (lower-type-alias node tk-type tk-value)
  (define name (or (name-after-keyword node 'KW_TYPE tk-type tk-value) "?alias"))
  (define types (find-descendants node 'type_))
  (define value (if (pair? types)
                    (lower-type (last types) tk-type tk-value)
                    (uir-null)))
  (uir-type-alias (uir-symbol name) '() value))

(define (lower-const node tk-type tk-value)
  (define name (or (name-after-keyword node 'KW_CONST tk-type tk-value)
                   (name-after-keyword node 'KW_STATIC tk-type tk-value) "?const"))
  (define tn (find-descendant node 'type_))
  (define expr-node (for/or ([c (node-children node)])
                      (and (eq? (node-tag c) 'expression) c)))
  (uir-ann-set! (uir-var (uir-symbol name))
                (if tn (lower-type tn tk-type tk-value) (uir-null))
                (if expr-node (lower-expr expr-node tk-type tk-value) (uir-null))))

;; ── Modules, imports ────────────────────────────────────────────────

(define (lower-module node tk-type tk-value)
  (define name (or (name-after-keyword node 'KW_MOD tk-type tk-value) "?mod"))
  ;; `mod m { ... }` wraps its item list in a group; `mod m;` has no body.
  (define body
    (for*/fold ([items '()])
               ([group (node-children node)]
                #:when (eq? (node-tag group) 'group)
                [list-child (any-tree-children group)]
                #:when (pair? list-child)
                [item list-child]
                #:when (and (cst-node? item) (eq? (node-tag item) 'item)))
      (append items (list item))))
  (uir-module (uir-symbol name) '() '()
              (if (pair? body) (uir-block (lower-rust-items body tk-type tk-value)) (uir-null))))

(define (lower-use node tk-type tk-value)
  (define tree (find-descendant node 'useTree))
  (define simple-paths (find-descendants tree 'simplePath))
  (define nested (and (pair? simple-paths) (rest simple-paths)))
  (define star? (for/or ([t (deep-tokens tree tk-type)]) (eq? (tk-type t) 'STAR)))
  (cond
    [(pair? nested)
     (define source (path-name (first simple-paths) tk-type tk-value))
     (uir-import (uir-symbol source)
                 (for/list ([p (in-list nested)])
                   (uir-symbol (last (string-split (path-name p tk-type tk-value) "::")))))]
    [star? (uir-import (uir-symbol (path-name (first simple-paths) tk-type tk-value))
                       (list (uir-symbol "*")))]
    [else (uir-import (uir-symbol (path-name (or (first simple-paths) tree) tk-type tk-value))
                      '())]))

;; ── Traits and impl blocks ──────────────────────────────────────────

(define (lower-trait node tk-type tk-value)
  (define name (or (name-after-keyword node 'KW_TRAIT tk-type tk-value) "?trait"))
  (define methods
    (for/list ([ai (in-list (find-descendants node 'associatedItem))]
               #:do [(define fn (find-descendant ai 'function_))]
               #:when fn)
      (lower-function fn tk-type tk-value #t)))
  (uir-interface (uir-symbol name) methods))

(define (impl-parts impl tk-type tk-value)
  (define inherent (find-descendant impl 'inherentImpl))
  (define trait-impl (find-descendant impl 'traitImpl))
  (define target
    (cond
      [inherent
       (define t (for/first ([c (node-children inherent)]
                             #:when (eq? (node-tag c) 'type_)) c))
       (and t (lower-type t tk-type tk-value))]
      [trait-impl
       (define direct (for/list ([c (node-children trait-impl)]
                                 #:when (eq? (node-tag c) 'type_)) c))
       (and (pair? direct) (lower-type (last direct) tk-type tk-value))]
      [else (uir-symbol "?impl")]))
  (define trait-name
    (and trait-impl
         (let ([tp (find-descendant trait-impl 'typePath)])
           (and tp (uir-symbol (path-name tp tk-type tk-value))))))
  (define methods
    (for/list ([ai (in-list (find-descendants impl 'associatedItem))]
               #:do [(define fn (find-descendant ai 'function_))]
               #:when fn)
      (lower-function fn tk-type tk-value #t)))
  (list target trait-name methods))

(define (attach-impl impl tk-type tk-value items)
  (define parts (impl-parts impl tk-type tk-value))
  (define target (first parts))
  (define trait-name (second parts))
  (define methods (third parts))
  (define target-name (uir-symbol-name target))
  (define existing
    (for/first ([i (in-list items)]
                #:when (and (uir-class? i)
                            (string=? (uir-symbol-name (uir-class-name i)) target-name)))
      i))
  (cond
    [existing
     (map (λ (i)
            (if (eq? i existing)
                (struct-copy uir-class i
                             [super (if (and trait-name
                                             (not (member trait-name (uir-class-super i))))
                                        (append (uir-class-super i) (list trait-name))
                                        (uir-class-super i))]
                             [methods (append (uir-class-methods i) methods)])
                i))
          items)]
    [else
     (append items
             (list (uir-class (uir-symbol target-name)
                              (if trait-name (list trait-name) '())
                              '() methods)))]))
