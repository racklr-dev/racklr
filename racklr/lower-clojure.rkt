#lang racket

;; Clojure source-language lowering (Phase 7).
;;
;; Clojure.g4 → CST → UIR. Lisp forms map onto the existing core IR:
;;   - list_ whose head is a symbol → uir-call (or a special form)
;;   - list_ whose head is not a symbol → uir-list (literal list, under quote)
;;   - vector → (uir-call "vector" items)   [emitted back as [...]]
;;   - map_   → uir-record                  [emitted back as {...}]
;;   - set_   → (uir-call "hash-set" items) [emitted back as #{...}]
;;   - reader macros → canonical function calls (quote/with-meta/deref/...)
;;   - keyword :foo → uir-symbol ":foo" (the ':' is part of the name)
;;
;; Special forms with real UIR structure: defn, fn, def, let, let*, if,
;; if-let, do. Everything else lowers as a generic call and round-trips.

(require racklr/tree
         racklr/uir
         "effects/clojure-annot.rkt")

(provide lower-clojure)

;; ── CST-walking helpers ─────────────────────────────────────────────

;; Only the cst-node children (skip tokens, lists, 'none).
(define (node-children cst)
  (filter cst-node? (any-tree-children cst)))

;; The first `list` child of a node: the wrapper holding repeated elements.
(define (list-wrapper node)
  (for/or ([c (any-tree-children node)])
    (and (list? c) (pair? c) c)))

;; The `forms` node child of list_/vector/set_.
(define (forms-node node)
  (for/or ([c (any-tree-children node)])
    (and (cst-node? c) (eq? (any-tree-tag c) 'forms) c)))

;; Immediate non-node token value of `node` (the lexeme), or #f.
(define (token-value-of node tk-value)
  (for/or ([c (any-tree-children node)])
    (and (not (cst-node? c))
         (not (null? c))
         (not (list? c))
         (with-handlers ([exn:fail? (λ (_) #f)])
           (tk-value c)))))

;; First token value found anywhere in `node`'s subtree.
(define (deep-token-value node tk-value)
  (or (token-value-of node tk-value)
      (for/or ([c (node-children node)])
        (deep-token-value c tk-value))))

;; The single cst-node child of a wrapper node (form/literal/...).
(define (single-node-child node)
  (for/or ([c (any-tree-children node)])
    (and (cst-node? c) c)))

;; The form nodes held by a `forms` node (form* → a list wrapper).
(define (forms-items forms)
  (let ([w (list-wrapper forms)])
    (filter cst-node? (or w '()))))

;; The (form form)* pairs of a map_: a list wrapper of `group` nodes.
(define (map-groups map-node)
  (let ([w (list-wrapper map-node)])
    (filter cst-node? (or w '()))))

;; A map `group` node has two form children: key and value.
(define (group-key-value group)
  (define fs (node-children group))
  (cons (first fs) (second fs)))

;; ── Lowering entry point ─────────────────────────────────────────────

(define (lower-clojure cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (cond
    [(eq? tag 'file_)
     (define forms (let ([w (list-wrapper cst)]) (filter cst-node? (or w '()))))
     (uir-block (map (λ (f) (lower-form f tk-type tk-value)) forms))]
    [(eq? tag 'form) (lower-form cst tk-type tk-value)]
    [else (uir-symbol (format "?top-~a" tag))]))

;; ── Forms ────────────────────────────────────────────────────────────

(define (lower-form cst tk-type tk-value)
  (define kid (single-node-child cst))
  (if kid
      (lower-form-kind kid tk-type tk-value)
      (uir-null)))

(define (lower-form-kind cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (cond
    [(eq? tag 'literal) (lower-literal cst tk-type tk-value)]
    [(eq? tag 'list_) (lower-list cst tk-type tk-value)]
    [(eq? tag 'vector) (lower-vector cst tk-type tk-value)]
    [(eq? tag 'map_) (lower-map cst tk-type tk-value)]
    [(eq? tag 'reader_macro) (lower-reader-macro cst tk-type tk-value)]
    [else (uir-symbol (format "?form-~a" tag))]))

;; ── Literals ─────────────────────────────────────────────────────────

(define (lower-literal cst tk-type tk-value)
  (define kid (single-node-child cst))
  (if kid
      (lower-literal-kind kid tk-type tk-value)
      ;; only non-node alternative is the BOOLEAN token
      (uir-bool (string=? (token-value-of cst tk-value) "true"))))

(define (lower-literal-kind cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (define raw (deep-token-value cst tk-value))
  (cond
    [(eq? tag 'string_) (uir-string (unquote-clj-string (or raw "")))]
    [(eq? tag 'number) (uir-number (or raw ""))]
    [(eq? tag 'nil_) (uir-null)]
    ;; keyword wraps simple_keyword / macro_keyword
    [(eq? tag 'keyword) (lower-literal-kind (single-node-child cst) tk-type tk-value)]
    ;; keywords keep their ':'/'::' prefix in the symbol name
    [(eq? tag 'simple_keyword) (uir-symbol (format ":~a" (keyword-name cst tk-value)))]
    [(eq? tag 'macro_keyword) (uir-symbol (format "::~a" (keyword-name cst tk-value)))]
    [(eq? tag 'symbol) (lower-symbol cst tk-type tk-value)]
    [(eq? tag 'param_name) (uir-symbol (or raw ""))]
    ;; characters: \a, \newline, \u1234 — keep the raw lexeme verbatim
    [(member tag '(character named_char any_char u_hex_quad))
     (uir-symbol (or raw ""))]
    [else (uir-symbol (format "?lit-~a" tag))]))

(define (lower-symbol cst tk-type tk-value)
  ;; symbol → ns_symbol | simple_sym  (both wrap a single token)
  (define raw (deep-token-value cst tk-value))
  (uir-symbol (or raw "")))

;; Text of the `symbol` child of a keyword node (:foo / ::foo).
(define (keyword-name cst tk-value)
  (for/or ([c (node-children cst)])
    (and (eq? (any-tree-tag c) 'symbol)
         (deep-token-value c tk-value))))

;; Strip one pair of surrounding double quotes.
(define (unquote-clj-string s)
  (if (and (>= (string-length s) 2)
           (eq? (string-ref s 0) #\")
           (eq? (string-ref s (sub1 (string-length s))) #\"))
      (substring s 1 (sub1 (string-length s)))
      s))

;; ── Collections ──────────────────────────────────────────────────────

(define (lower-vector cst tk-type tk-value)
  (define fs (forms-node cst))
  (uir-call (uir-symbol "vector")
            (if fs (map (λ (f) (lower-form f tk-type tk-value)) (forms-items fs))
                '())))

(define (lower-map cst tk-type tk-value)
  (uir-record
   (for/list ([g (map-groups cst)])
     (define kv (group-key-value g))
     (cons (lower-form (car kv) tk-type tk-value)
           (lower-form (cdr kv) tk-type tk-value)))))

;; ── Lists: special forms vs calls vs literal lists ───────────────────

(define (lower-list cst tk-type tk-value)
  (define fs (forms-node cst))
  (define items (if fs (forms-items fs) '()))
  (cond
    [(null? items) (uir-list '())]
    [else
     (define head (first items))
     (define head-sym
       (and (eq? (any-tree-tag head) 'form)
            (let ([kid (single-node-child head)])
              (and kid
                   (eq? (any-tree-tag kid) 'literal)
                   (let ([lk (single-node-child kid)])
                     (and lk
                          (eq? (any-tree-tag lk) 'symbol)
                          (deep-token-value lk tk-value)))))))
     (cond
       [head-sym
        (lower-special-form head-sym (cdr items) tk-type tk-value)]
       [else
        ;; literal list (only reachable under quote)
        (uir-list (map (λ (f) (lower-form f tk-type tk-value)) items))])]))

;; Special forms that get structured lowering. `name` is the head symbol
;; string; `args` is the list of remaining `form` nodes.
(define (lower-special-form name args tk-type tk-value)
  (case (string->symbol name)
    [(defn defn-) (lower-defn (eq? name "defn-") args tk-type tk-value)]
    [(fn fn*) (lower-fn args tk-type tk-value)]
    [(def) (lower-def args tk-type tk-value)]
    [(let let*) (lower-let args tk-type tk-value)]
    [(if) (lower-if args tk-type tk-value)]
    [(if-let) (lower-if-let args tk-type tk-value)]
    [(do) (uir-block (map (λ (f) (lower-form f tk-type tk-value)) args))]
    [else (lower-call name args tk-type tk-value)]))

(define (lower-call name args tk-type tk-value)
  (uir-call (uir-symbol name)
            (map (λ (f) (lower-form f tk-type tk-value)) args)))

;; (defn name doc? attr-map? [params] body...)
;;
;; The attr-map slot is where the emitter places the effect row
;; (`{:__effects__ {:llm {...}}}`), so reading it back makes the native
;; annotation survive an emit -> absorb round trip.
(define (lower-defn private? args tk-type tk-value)
  (cond
    [(null? args) (uir-symbol (if private? "?defn-" "?defn"))]
    [else
     (define name-fn (car args))
     (define name (lower-form name-fn tk-type tk-value))
     (define rest (cdr args))
     ;; skip an optional docstring string before the params vector
     (define rest* (if (and (pair? rest) (form-is-string? (car rest) tk-type tk-value))
                       (cdr rest)
                       rest))
     ;; an attr-map carrying effects metadata is the emitted row shape
     (define attr-row
       (and (pair? rest*)
            (let ([m (lower-form (car rest*) tk-type tk-value)])
              (and (uir-record? m) (extract-effects-row m)))))
     (define rest** (if attr-row (cdr rest*) rest*))
     (if (and (pair? rest**) (form-is-vector? (car rest**) tk-type tk-value))
         (let* ([params (lower-vector-items (car rest**) tk-type tk-value)]
                [body (map (λ (f) (lower-form f tk-type tk-value)) (cdr rest**))]
                [body-node (if (null? body)
                               (uir-null)
                               (if (= (length body) 1)
                                   (car body)
                                   (uir-block body)))])
           (uir-fn (and (uir-symbol? name) name)
                   params
                   body-node
                   #f
                   attr-row))
         ;; non-standard defn (multi-arity etc.): round-trip as a call
         (lower-call (if private? "defn-" "defn") args tk-type tk-value))]))

;; (fn name? [params] body...) — name may be absent (anonymous).
(define (lower-fn args tk-type tk-value)
  (define rest (if (and (pair? args) (form-is-symbol? (car args) tk-type tk-value))
                   (cdr args)
                   args))
  (define name (and (pair? args) (form-is-symbol? (car args) tk-type tk-value)
                    (lower-form (car args) tk-type tk-value)))
  (cond
    [(and (pair? rest) (form-is-vector? (car rest) tk-type tk-value))
     (define params (lower-vector-items (car rest) tk-type tk-value))
     (define body (map (λ (f) (lower-form f tk-type tk-value)) (cdr rest)))
     (uir-fn (and (uir-symbol? name) name)
             params
             (if (= (length body) 1) (car body) (uir-block body))
             #f
             #f)]
    [else (lower-call "fn" args tk-type tk-value)]))

;; (def name value)
(define (lower-def args tk-type tk-value)
  (if (>= (length args) 2)
      (uir-set! (lower-form (car args) tk-type tk-value)
                (lower-form (cadr args) tk-type tk-value))
      (lower-call "def" args tk-type tk-value)))

;; (let [bindings...] body...) → nested uir-let
(define (lower-let args tk-type tk-value)
  (cond
    [(< (length args) 2) (lower-call "let" args tk-type tk-value)]
    [else
     (define bindings (lower-vector-items (car args) tk-type tk-value))
     (define body (map (λ (f) (lower-form f tk-type tk-value)) (cdr args)))
     (define body-node (if (= (length body) 1) (car body) (uir-block body)))
     (foldr (λ (b acc)
              (uir-let (car b) (cdr b) acc))
            body-node
            (pair-bindings bindings))]))

;; [a 1 b 2] → ((a . 1) (b . 2)); odd trailing element dropped.
(define (pair-bindings items)
  (cond [(or (null? items) (null? (cdr items))) '()]
        [else (cons (cons (car items) (cadr items))
                    (pair-bindings (cddr items)))]))

;; (if test then [else])
(define (lower-if args tk-type tk-value)
  (cond
    [(= (length args) 2)
     (uir-if (lower-form (car args) tk-type tk-value)
             (lower-form (cadr args) tk-type tk-value)
             (uir-null))]
    [(>= (length args) 3)
     (uir-if (lower-form (car args) tk-type tk-value)
             (lower-form (cadr args) tk-type tk-value)
             (lower-form (caddr args) tk-type tk-value))]
    [else (lower-call "if" args tk-type tk-value)]))

;; (if-let [x test] then else) → (let x test (if x then else))
(define (lower-if-let args tk-type tk-value)
  (cond
    [(< (length args) 3) (lower-call "if-let" args tk-type tk-value)]
    [else
     (define bindings (lower-vector-items (car args) tk-type tk-value))
     (define then-b (lower-form (cadr args) tk-type tk-value))
     (define else-b (lower-form (caddr args) tk-type tk-value))
     (if (and (= (length bindings) 2))
         (uir-let (car bindings) (cadr bindings)
                  (uir-if (car bindings) then-b else-b))
         (lower-call "if-let" args tk-type tk-value))]))

;; ── Reader macros ────────────────────────────────────────────────────

(define (lower-reader-macro cst tk-type tk-value)
  (define kid (single-node-child cst))
  (if (not kid)
      (uir-null)
      (lower-reader-macro-kind kid tk-type tk-value)))

(define (lower-reader-macro-kind cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (define kids (node-children cst))
  (define (form k) (lower-form k tk-type tk-value))
  (case tag
    [(quote) (uir-call (uir-symbol "quote") (map form kids))]
    [(backtick) (uir-call (uir-symbol "syntax-quote") (map form kids))]
    [(unquote) (uir-call (uir-symbol "unquote") (map form kids))]
    [(unquote_splicing) (uir-call (uir-symbol "unquote-splicing") (map form kids))]
    [(deref) (uir-call (uir-symbol "deref") (map form kids))]
    [(var_quote)
     (define sym (for/or ([k kids]) (and (eq? (any-tree-tag k) 'symbol) k)))
     (uir-call (uir-symbol "var")
               (list (if sym (lower-symbol sym tk-type tk-value) (uir-symbol "?"))))]
    [(regex)
     (define str-node (for/or ([k kids]) (and (eq? (any-tree-tag k) 'string_) k)))
     (uir-call (uir-symbol "regex")
               (list (if str-node (lower-literal-kind str-node tk-type tk-value) (uir-string ""))))]
    [(gensym)
     ;; SYMBOL '#' → an auto-gensym; keep the trailing '#' in the name
     (define raw (token-value-of cst tk-value))
     (uir-symbol (if raw (format "~a#" raw) "#"))]
    [(lambda_) (lower-lambda cst tk-type tk-value)]
    [(tag) (lower-tag cst tk-type tk-value)]
    [(meta_data) (lower-meta-data cst tk-type tk-value)]
    [(host_expr) (uir-call (uir-symbol "host-expr") (map form kids))]
    [(discard) (uir-call (uir-symbol "#_") (map form kids))]
    [(dispatch)
     (define sym (for/or ([k kids]) (and (eq? (any-tree-tag k) 'symbol) k)))
     (define sym-name (if sym (deep-token-value sym tk-value) "?"))
     (uir-call (uir-symbol (format "#~a" sym-name))
               (for/list ([k kids] #:when (eq? (any-tree-tag k) 'form)) (form k)))]
    [(set_) (lower-set cst tk-type tk-value)]
    [else (uir-symbol (format "?macro-~a" tag))]))

;; #(...) anonymous fn sugar → (fn* body...)
(define (lower-lambda cst tk-type tk-value)
  (define forms (forms-node cst))
  (uir-call (uir-symbol "fn*")
            (if forms
                (map (λ (f) (lower-form f tk-type tk-value)) (forms-items forms))
                '())))

;; #{...} set literal → (hash-set items)
(define (lower-set cst tk-type tk-value)
  (define fs (forms-node cst))
  (uir-call (uir-symbol "hash-set")
            (if fs (map (λ (f) (lower-form f tk-type tk-value)) (forms-items fs))
                '())))

;; ^meta target  →  attach :effects row to a fn, else (with-meta target meta)
(define (lower-tag cst tk-type tk-value)
  (define fs (node-children cst))
  (if (= (length fs) 2)
      (let* ([meta (lower-form (first fs) tk-type tk-value)]
             [target (lower-form (second fs) tk-type tk-value)]
             [row (extract-effects-row meta)])
        (if (and row (uir-fn? target))
            (struct-copy uir-fn target [effects row])
            (uir-call (uir-symbol "with-meta") (list target meta))))
      (uir-symbol "?tag")))

;; #^meta target  →  attach :effects row to a fn, else (with-meta target meta)
(define (lower-meta-data cst tk-type tk-value)
  (define fs (node-children cst))
  (if (= (length fs) 2)
      (let* ([meta (lower-form (first fs) tk-type tk-value)]
             [target (lower-form (second fs) tk-type tk-value)]
             [row (extract-effects-row meta)])
        (if (and row (uir-fn? target))
            (struct-copy uir-fn target [effects row])
            (uir-call (uir-symbol "with-meta") (list target meta))))
      (uir-symbol "?meta-data")))

;; ── form predicates ──────────────────────────────────────────────────

(define (form-is-string? f tk-type tk-value)
  (let ([kid (single-node-child f)])
    (and kid
         (eq? (any-tree-tag kid) 'literal)
         (let ([lk (single-node-child kid)])
           (and lk (eq? (any-tree-tag lk) 'string_))))))

(define (form-is-symbol? f tk-type tk-value)
  (let ([kid (single-node-child f)])
    (and kid
         (eq? (any-tree-tag kid) 'literal)
         (let ([lk (single-node-child kid)])
           (and lk (eq? (any-tree-tag lk) 'symbol))))))

(define (form-is-vector? f tk-type tk-value)
  (let ([kid (single-node-child f)])
    (and kid (eq? (any-tree-tag kid) 'vector))))

;; Extract the form items of a vector `form` node.
(define (lower-vector-items vector-form tk-type tk-value)
  (define v (single-node-child vector-form))
  (if (and v (eq? (any-tree-tag v) 'vector))
      (let ([fs (forms-node v)])
        (if fs
            (map (λ (f) (lower-form f tk-type tk-value)) (forms-items fs))
            '()))
      '()))

(module+ main
  (displayln "racklr/lower-clojure — Clojure CST → UIR lowering loaded."))
