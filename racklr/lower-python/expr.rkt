#lang racket

(require racklr/tree racklr/uir "helpers.rkt")

(provide lower-expr lower-dotted-name lower-not-test lower-comparison lower-binop
         lower-atom-expr lower-trailer find-node lower-subscriptlist find-subscripts
         lower-arglist lower-argument lower-group-expr lower-atom lower-name
         unquote-string lower-string-token decode-python-escapes literal-prefix-and-body
         lower-string-tokens plain-string-token?
         walk-collection lower-collection-items
         lower-dictorsetmaker lower-tuple-items lower-display-items has-comp-for? lower-comp-for
         lower-comprehension lower-exprlist lower-lambdef lower-varargslist
         lower-get-path lower-assignment-target normalize-target
         sequence-item-nodes target-sequence?)

;; Lower a sequence expression into UIR
(define (lower-expr cst tk-type tk-value)
  (define u (unwrap-expr cst))
  (define tag (any-tree-tag u))
  (define kids (node-children u))
  (cond
    [(eq? tag 'test)
     ;; Could be ternary: test 'if' test 'else' test
     (if (= (length kids) 2)
         (let* ([then-expr (lower-expr (first kids) tk-type tk-value)]
                [if-group (second kids)]
                [gk (node-children if-group)]
                [cond-expr (if (>= (length gk) 1) (lower-expr (first gk) tk-type tk-value) (uir-null))]
                [else-expr (if (>= (length gk) 2) (lower-expr (second gk) tk-type tk-value) (uir-null))])
           (uir-if cond-expr then-expr else-expr))
         ;; Non-ternary test — unwrap
         (if (and (= (length kids) 1) (member (any-tree-tag (first kids)) '(test or_test and_test not_test comparison expr atom_expr)))
             (lower-expr (first kids) tk-type tk-value)
             (uir-symbol "?test")))]
    [(eq? tag 'namedexpr_test)
     ;; Walrus assignment expression: test ':=' test
     (cond
       [(= (length kids) 2)
        (let* ([lhs (lower-expr (first kids) tk-type tk-value)]
               [second (second kids)]
               [rhs (cond [(and (cst-node? second) (eq? (any-tree-tag second) 'group))
                           (let ([gk (node-children second)])
                             (if (pair? gk) (lower-expr (first gk) tk-type tk-value) (uir-null)))]
                          [else (lower-expr second tk-type tk-value)])])
          (uir-walrus lhs rhs))]
       [(= (length kids) 1) (lower-expr (first kids) tk-type tk-value)]
       [else (uir-symbol "?namedexpr_test")])]
    [(eq? tag 'comparison) (lower-comparison u tk-type tk-value)]
    [(eq? tag 'expr) (lower-binop u tk-type tk-value)]
    [(eq? tag 'or_test) (lower-binop u tk-type tk-value)]    ;; 'or' operator
    [(eq? tag 'and_test) (lower-binop u tk-type tk-value)]   ;; 'and' operator
    [(eq? tag 'not_test) (lower-not-test u tk-type tk-value)] ;; 'not' operator
    [(eq? tag 'atom_expr) (lower-atom-expr u tk-type tk-value)]
    [(eq? tag 'atom) (lower-atom u tk-type tk-value)]
    [(eq? tag 'name) (lower-name u tk-type tk-value)]
    [(eq? tag 'star_expr)
     ;; star_expr: '*' expr — unpacking target or splat argument
     (if (null? kids)
         (uir-symbol "?star_expr")
         (uir-spread (lower-expr (first kids) tk-type tk-value)))]
    [(eq? tag 'number) (uir-number (first (cst-tokens u tk-type tk-value)))]
    [(eq? tag 'lambdef) (lower-lambdef u tk-type tk-value)]
    [(eq? tag 'group) (if (null? kids) (uir-null) (lower-expr (first kids) tk-type tk-value))]
    [(eq? tag 'dotted_name) (lower-dotted-name u tk-type tk-value)]
    [else (uir-symbol (format "?~a" tag))]))

;; Lower a dotted_name (a.b.c) into a full attribute chain. The generated
;; parser wraps every dotted name (including a single name) in a dotted_name
;; node: the first NAME is a `name` child, the rest are list-wrapped
;; group(DOT, name) nodes, so the token walk below is the reliable source of
;; the name sequence.
(define (lower-dotted-name cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (define names (for/list ([t toks] #:when (eq? (first t) 'NAME)) (second t)))
  (cond
    [(null? names) (uir-symbol "?dotted")]
    [else
     (foldl (lambda (nm acc) (uir-get acc (uir-string nm)))
            (uir-var (uir-symbol (car names)))
            (cdr names))]))

;; Handle 'not' operator: not_test → 'not' not_test | comparison
(define (lower-not-test cst tk-type tk-value)
  ;; Check for NOT token in raw children
  (define has-not?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c)) (not (null? c)) (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'NOT)))))
  (define kids (node-children cst))
  (if (or (not has-not?) (= (length kids) 0))
      ;; No 'not' prefix — pass through
      (if (= (length kids) 1)
          (lower-expr (first kids) tk-type tk-value)
          (uir-symbol "?not_test"))
      ;; Has 'not' prefix: lower the inner expression and wrap
      (uir-call (uir-symbol "not")
                (list (if (pair? kids)
                         (lower-expr (first kids) tk-type tk-value)
                         (uir-null))))))

;; Operator symbol for an op node: a raw token (`|`, `**` from the left-recursive
;; `expr` alternatives), a comp_op node (possibly two tokens: "not in", "is not"),
;; or a group wrapping the operator token.
(define (op-symbol op-node tk-type tk-value)
  (cond
    [(token-like? op-node tk-type) (tk-value op-node)]
    [(cst-node? op-node)
     (let ([toks (cst-tokens op-node tk-type tk-value)])
       (if (pair? toks)
           (string-join (map second toks) " ")
           "?op"))]
    [else "?op"]))

;; Collect (op . right) pairs from the children after the first operand of an
;; operator chain. Three generated-parser shapes occur:
;;   1. a direct group node (op expr) followed by the next expr   [a + b * c]
;;   2. a list of group nodes, each (op expr)                     [a < b < c]
;;   3. a raw operator token between two expr nodes               [1 | 2, 2 ** 3]
(define (collect-op-pairs remaining tk-type)
  (define (list-item-pair item)
    (and (cst-node? item)
         (let ([gk (node-children item)])
           (cond [(>= (length gk) 2) (cons (first gk) (second gk))]
                 [(= (length gk) 1) (cons item (first gk))]
                 [else #f]))))
  (cond
    [(null? remaining) '()]
    [else
     (let ([c (car remaining)])
       (cond
         [(and (token-like? c tk-type) (pair? (cdr remaining)) (cst-node? (cadr remaining)))
          (cons (cons c (cadr remaining)) (collect-op-pairs (cddr remaining) tk-type))]
         [(and (list? c) (pair? c))
          (append (filter pair? (map list-item-pair c))
                  (collect-op-pairs (cdr remaining) tk-type))]
         [(and (cst-node? c) (pair? (cdr remaining)) (cst-node? (cadr remaining)))
          (cons (cons c (cadr remaining)) (collect-op-pairs (cddr remaining) tk-type))]
         [else (collect-op-pairs (cdr remaining) tk-type)]))]))

;; Handle comparison: expr (comp_op expr)*
;;
;; Python's chained comparison (`a < b < c`) means `a < b and b < c` and
;; evaluates `b` once; lowering it as nested `<` calls is semantically wrong
;; (and re-emits `a < b < c` only by accident when the nesting is uniform).
;; A single comparison keeps the binary `<` call encoding; a chain lowers to
;; the dedicated operand/operator node the emitter renders back exactly:
;;   (uir-call (uir-symbol "cmp-chain")
;;            (list a (uir-symbol "<") b (uir-symbol "<") c))
;; Operator entries use the same names lower-comparison already produces for
;; single comparisons, including the two-token "is not" / "not in".
(define (lower-comparison cst tk-type tk-value)
  (define kids (node-children cst))
  (define pairs (collect-op-pairs (cdr (any-tree-children cst)) tk-type))
  (cond
    [(null? pairs)
     (if (= (length kids) 1)
         (lower-expr (first kids) tk-type tk-value)
         (uir-symbol "?comparison"))]
    [(null? (cdr pairs))
     (define p (car pairs))
     (uir-call (uir-symbol (op-symbol (car p) tk-type tk-value))
               (list (if (pair? kids) (lower-expr (first kids) tk-type tk-value) (uir-null))
                     (lower-expr (cdr p) tk-type tk-value)))]
    [else
     (uir-call
      (uir-symbol "cmp-chain")
      (append
       (list (if (pair? kids) (lower-expr (first kids) tk-type tk-value) (uir-null)))
       (append-map
        (lambda (p)
          (list (uir-symbol (op-symbol (car p) tk-type tk-value))
                (lower-expr (cdr p) tk-type tk-value)))
        pairs)))]))

;; Handle binary expressions: expr (binop expr)*
(define (lower-binop cst tk-type tk-value)
  (define kids (node-children cst))
  (define pairs (collect-op-pairs (cdr (any-tree-children cst)) tk-type))
  (if (null? pairs)
      (if (= (length kids) 1)
          (lower-expr (first kids) tk-type tk-value)
          (uir-symbol "?expr"))
      (let fold ([pairs pairs]
                 [acc (if (pair? kids) (lower-expr (first kids) tk-type tk-value) (uir-null))])
        (if (null? pairs)
            acc
            (let ([p (car pairs)])
              (fold (cdr pairs)
                    (uir-call (uir-symbol (op-symbol (car p) tk-type tk-value))
                              (list acc (lower-expr (cdr p) tk-type tk-value)))))))))

;; Handle atom_expr: trailer (attr access, call, subscript)
(define (lower-atom-expr cst tk-type tk-value)
  ;; Check for AWAIT prefix in raw children (non-CST-node token)
  (define has-await?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c))
           (not (null? c))
           (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'AWAIT)))))
  (define kids (node-children cst))
  ;; Also extract list-wrapped trailers from raw children
  (define list-trailers
    (apply append
           (for/list ([c (any-tree-children cst)]
                      #:when (and (list? c) (pair? c)))
             (for/list ([item c]
                        #:when (and (cst-node? item)
                                    (eq? (any-tree-tag item) 'trailer)))
               item))))
  (define all-trailers (append (if (>= (length kids) 2) (cdr kids) '()) list-trailers))
  (define result
    (if (and (= (length kids) 1) (null? list-trailers))
        (lower-expr (first kids) tk-type tk-value)
        ;; base then trailers
        (let loop ([remaining all-trailers]
                   [acc (if (pair? kids) (lower-expr (first kids) tk-type tk-value) (uir-null))])
          (if (null? remaining)
              acc
              (let* ([trailer (first remaining)]
                     [next (lower-trailer trailer acc tk-type tk-value)])
                (loop (cdr remaining) next))))))
  (if has-await? (uir-await result) result))

(define (lower-trailer cst base tk-type tk-value)
  ;; trailer can be: '(' arglist? ')' call, '[' subscriptlist ']' index, '.' NAME attr
  (define kids (node-children cst))
  ;; Check for DOT token
  (define has-dot?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c))
           (not (null? c))
           (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'DOT)))))
  ;; Check for OPEN_BRACK token
  (define has-brack?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c))
           (not (null? c))
           (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'OPEN_BRACK)))))
  ;; Check for OPEN_PAREN token (call)
  (define has-open-paren?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c))
           (not (null? c))
           (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'OPEN_PAREN)))))
  (cond
    [has-dot?
     ;; Attribute access: base.attr
     (let ([name-node (find-node kids 'name)])
       (if name-node
           (let ([name-str (first-token name-node tk-type tk-value)])
             (uir-get base (uir-string (if name-str (cdr name-str) "?name"))))
           (uir-get base (uir-string "?attr"))))]
    [has-brack?
     ;; Subscript: base[key]
     (let ([sub-node (find-node kids 'subscriptlist)])
       (if sub-node
           (uir-get base (lower-subscriptlist sub-node tk-type tk-value))
           (uir-get base (uir-null))))]
    [(null? kids)
     (if has-open-paren?
         (uir-call base '())
         base)]
    [(eq? (any-tree-tag (first kids)) 'arglist)
     (uir-call base (lower-arglist (first kids) tk-type tk-value))]
    [else base]))

;; Find a node with given tag in a list
(define (find-node kids tag)
  (for/or ([k kids]
           #:when (eq? (any-tree-tag k) tag))
    k))

;; Lower subscriptlist into a UIR expression
;; For single subscript: return the expression
;; For multiple subscripts: return as uir-list (tuple)

;; Wrap a single string subscript index so it is distinct from an attribute
;; name (see lower-subscriptlist).
(define (uir-get-field-wrap idx) (uir-list (list idx)))

(define (lower-subscriptlist cst tk-type tk-value)
  (define sub-exprs
    (map (lambda (sub) (lower-subscript sub tk-type tk-value))
         (find-subscripts cst)))
  (cond
    [(null? sub-exprs) (uir-null)]
    [(= (length sub-exprs) 1)
     (define idx (first sub-exprs))
     ;; A single string index (`d["a"]`) must not be confusable with attribute
     ;; access (`d.a`): both would otherwise be (uir-get d (uir-string "a")).
     ;; The subscript keeps a one-element index uir-list in the field slot; the
     ;; emitter renders a uir-list field as `[...]` and a bare uir-string field
     ;; as `.attr`, so both spellings round-trip to distinct nodes.
     (if (uir-string? idx)
         (uir-get-field-wrap idx)
         idx)]
    [else (uir-list sub-exprs)]))

;; Lower one subscript_ node: a plain index or a slice.
;; subscript_ : test | test? ':' test? sliceop?
;; A slice lowers to (call "slice" (list start stop step)) with uir-null for
;; any missing bound, so `a[::-1]` and `a[1:2:3]` keep their full shape instead
;; of collapsing to the first child (or an unknown ?sliceop symbol).
(define (lower-subscript sub tk-type tk-value)
  (define raw (any-tree-children sub))
  (define (colon? x) (and (token-like? x tk-type) (eq? (tk-type x) 'COLON)))
  (define sliceop-node
    (for/or ([x raw]) (and (cst-node? x) (eq? (any-tree-tag x) 'sliceop) x)))
  (define slice? (or sliceop-node (for/or ([x raw]) (colon? x))))
  (if (not slice?)
      (let ([kids (node-children sub)])
        (if (null? kids)
            (uir-null)
            (lower-expr (first kids) tk-type tk-value)))
      (let loop ([rest raw] [seg 0] [start #f] [stop #f] [step #f])
        (cond
          [(null? rest)
           (uir-call (uir-symbol "slice")
                     (list (or start (uir-null)) (or stop (uir-null)) (or step (uir-null))))]
          [(colon? (car rest))
           (loop (cdr rest) (add1 seg) start stop step)]
          [(eq? (car rest) sliceop-node)
           (let ([sk (node-children sliceop-node)])
             (loop (cdr rest) (add1 seg) start stop
                   (and (pair? sk) (lower-expr (first sk) tk-type tk-value))))]
          [(cst-node? (car rest))
           (define v (lower-expr (car rest) tk-type tk-value))
           (cond [(= seg 0) (loop (cdr rest) seg v stop step)]
                 [(= seg 1) (loop (cdr rest) seg start v step)]
                 [(= seg 2) (loop (cdr rest) seg start stop v)]
                 [else (loop (cdr rest) seg start stop step)])]
          [else (loop (cdr rest) seg start stop step)]))))

;; Recursively find all subscript_ nodes (handles list-wrapping from commas)
(define (find-subscripts node)
  (cond [(and (cst-node? node) (eq? (any-tree-tag node) 'subscript_))
         (list node)]
        [(cst-node? node)
         (append-map find-subscripts (any-tree-children node))]
        [(and (list? node) (pair? node))
         (append-map find-subscripts node)]
        [else '()]))

(define (lower-arglist cst tk-type tk-value)
  ;; arglist : argument (',' argument)* ','?
  ;; The generated parser emits the first argument directly and collects the
  ;; (',' argument)* repetition into a LIST of group(COMMA, argument) nodes,
  ;; so a plain node-children filter drops every argument after the first.
  (define result '())
  (let loop ([remaining (any-tree-children cst)])
    (cond
      [(null? remaining) (reverse result)]
      [(and (list? (car remaining)) (pair? (car remaining)))
       (loop (append (car remaining) (cdr remaining)))]
      [(cst-node? (car remaining))
       (define t (any-tree-tag (car remaining)))
       (cond
         [(eq? t 'argument)
          (set! result (cons (lower-argument (car remaining) tk-type tk-value) result))
          (loop (cdr remaining))]
         [(eq? t 'group)
          ;; comma wrapper group(COMMA, argument): lower its argument child
          (define arg
            (for/or ([k (any-tree-children (car remaining))])
              (and (cst-node? k) (eq? (any-tree-tag k) 'argument) k)))
          (set! result
                (cons (if arg
                          (lower-argument arg tk-type tk-value)
                          (lower-group-expr (car remaining) tk-type tk-value))
                      result))
          (loop (cdr remaining))]
         [else (loop (cdr remaining))])]
      [else (loop (cdr remaining))])))

(define (lower-argument cst tk-type tk-value)
  ;; argument : test (comp_for)? | keyword '=' test | '**' test | '*' test
  ;;
  ;; `*` / `**` unpacking appears as a raw STAR / POWER token before the
  ;; argument's test child (node-children only keeps CST nodes, so the marker
  ;; used to be dropped and `f(*xs)` lowered exactly like `f(xs)`). Encode
  ;; them with the conventions the emitter renders back:
  ;;   f(*xs)  → (uir-call f (list (uir-spread (uir-var xs))))
  ;;   f(**kw) → (uir-call f (list (uir-call (uir-symbol "kw-spread")
  ;;                                         (list (uir-var kw)))))
  ;; A keyword argument (`a=1`) is the first kid being a `keyword` node and the
  ;; second kid the value `test` (the `=` token is filtered out by
  ;; node-children). Emit it as (call "kwarg" (list name value)) so the value
  ;; is preserved for the type-graph search and emit round-trips.
  ;; A generator expression (`f(x for x in items)`) has a comp_for as the
  ;; second kid: lower it to (call "gen-exp" (list result loop-var iter
  ;; filter)) so the loop variable is preserved for receiver-type dataflow
  ;; instead of being dropped.
  (define kids (node-children cst))
  (define raw (any-tree-children cst))
  (define (raw-marker? ty)
    (for/or ([c raw] #:when (token-like? c tk-type)) (eq? (tk-type c) ty)))
  (define (test-child)
    (or (for/or ([k kids] #:when (eq? (any-tree-tag k) 'test)) k)
        (and (pair? kids) (first kids))))
  (cond
    [ (raw-marker? 'STAR)
     (uir-spread (if (test-child) (lower-expr (test-child) tk-type tk-value) (uir-null)))]
    [ (raw-marker? 'POWER)
     (uir-call (uir-symbol "kw-spread")
               (list (if (test-child) (lower-expr (test-child) tk-type tk-value) (uir-null))))]
    [(and (= (length kids) 2)
          (eq? (any-tree-tag (first kids)) 'keyword)
          (eq? (any-tree-tag (second kids)) 'test))
     (uir-call (uir-symbol "kwarg")
               (list (lower-name (first kids) tk-type tk-value)
                     (lower-expr (second kids) tk-type tk-value)))]
    [(null? kids)
     (uir-null)]
    [(and (pair? (cdr kids))
          (has-comp-for? (second kids)))
     (let* ([result (lower-expr (first kids) tk-type tk-value)]
            [comp-for-node
             (let find ([n (second kids)])
               (cond [(any-tree? n)
                      (if (eq? (any-tree-tag n) 'comp_for)
                          n
                          (for/or ([c (any-tree-children n)])
                            (cond [(any-tree? c) (find c)]
                                  [(and (list? c) (pair? c))
                                   (for/or ([cc c]) (find cc))]
                                  [else #f])))]
                     [else #f]))]
            [parts (lower-comp-for comp-for-node tk-type tk-value)])
       (uir-call (uir-symbol "gen-exp")
                 (list result (car parts) (cadr parts) (caddr parts))))]
    [else
     (lower-expr (first kids) tk-type tk-value)]))

(define (lower-group-expr cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (lower-expr (first kids) tk-type tk-value)))

;; Convert a Python integer-literal token value ("0xFF", "0o17", "0b1010",
;; underscores allowed) into its decimal string form.
(define (python-int->decimal s radix)
  (define digits (regexp-replace* #rx"_" (substring s 2) ""))
  (define n (string->number digits radix))
  (if n (number->string n) s))

;; True for a plain or u-prefixed string token (no prefix, or u/U): the family
;; whose value the UIR stores decoded.
(define (plain-string-token? s)
  (define-values (prefix _delim _body) (literal-prefix-and-body s))
  (member (string-downcase prefix) '("" "u") equal?))

;; Concatenate an implicit string-literal sequence (`"a" "b"` is one literal in
;; Python). The generated parser wraps the whole STRING+ run in one atom, and
;; only when every token is in the decoded family can the pieces be merged into
;; the single uir-string the UIR models; a mixed run (an f-string or a
;; raw/bytes prefix next to another token) keeps the first token and is a
;; recorded residual gap (the corpus uses neither).
(define (lower-string-tokens toks)
  (cond
    [(null? toks) (uir-null)]
    [(null? (cdr toks)) (lower-string-token (car toks))]
    [(for/and ([t toks]) (plain-string-token? t))
     (uir-string
      (apply string-append
             (for/list ([t toks])
               (define-values (_prefix _delim body) (literal-prefix-and-body t))
               (decode-python-escapes body))))]
    [else (lower-string-token (car toks))]))

;; Handle atom: NAME | NUMBER | STRING+ | NONE | TRUE | FALSE | [] | {} | ()
(define (lower-atom cst tk-type tk-value)
  (define toks (cst-tokens cst tk-type tk-value))
  ;; Also check for tokens wrapped in single-element lists (e.g., STRING+ produces (list STRING))
  (define list-toks
    (for/list ([c (any-tree-children cst)]
               #:when (and (list? c) (pair? c) (null? (cdr c))
                           (not (cst-node? (car c)))
                           (with-handlers ([exn:fail? (lambda (_) #f)])
                             (tk-type (car c)) #t)))
      (list (tk-type (car c)) (tk-value (car c)))))
  (define all-toks (append toks list-toks))
  ;; STRING+ runs (implicit concatenation) are list-wrapped, so collect every
  ;; STRING token in source order rather than only direct/single-list children.
  (define (collect-string-tokens node)
    (cond [(token-like? node tk-type)
           (if (eq? (tk-type node) 'STRING) (list (tk-value node)) '())]
          [(list? node) (append-map collect-string-tokens node)]
          [(cst-node? node) (append-map collect-string-tokens (any-tree-children node))]
          [else '()]))
  (define string-tokens (collect-string-tokens cst))
  (define kids (node-children cst))
  ;; Detect bracket-delimited collections via raw children
  (define bracket-type
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c)) (not (null? c)) (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (match (tk-type c)
               ['OPEN_BRACK 'list]
               ['OPEN_BRACE 'brace]
               ['OPEN_PAREN 'tuple]
               [_ #f])))))
  (cond
    ;; List literal: [expr, ...] — check for comprehension first
    [(and (eq? bracket-type 'list) (pair? kids)
          (eq? (any-tree-tag (first kids)) 'testlist_comp))
     (if (has-comp-for? (first kids))
         (lower-comprehension (first kids) tk-type tk-value "list-comp")
         (uir-list (lower-display-items (first kids) tk-type tk-value)))]
    [(eq? bracket-type 'list) (uir-list '())]
    ;; Dict or set: {expr, ...} — check for comprehension first
    [(and (eq? bracket-type 'brace) (pair? kids)
          (eq? (any-tree-tag (first kids)) 'dictorsetmaker))
     (if (has-comp-for? (first kids))
         (let* ([dsm (first kids)]
                [has-colon? (for/or ([t (walk-collection dsm tk-type)] #:when (eq? (first t) 'colon)) #t)])
           (lower-comprehension dsm tk-type tk-value
                               (if has-colon? "dict-comp" "set-comp")))
         (lower-dictorsetmaker (first kids) tk-type tk-value))]
    [(eq? bracket-type 'brace) (uir-record '())]
    ;; Tuple or parenthesized expression: (expr, ...) or (expr)
     [(eq? bracket-type 'tuple)
      ;; Check if this is a parenthesized expression (no comma) or a tuple (has comma)
      (define has-comma?
        (for/or ([t (cst-tokens-deep cst tk-type tk-value)])
          (eq? (first t) 'COMMA)))
      (if has-comma?
          (uir-call (uir-symbol "tuple") (lower-display-items cst tk-type tk-value))
          ;; Parenthesized expression: lower the inner expression and wrap in uir-paren
          (let* ([items (lower-display-items cst tk-type tk-value)]
                 [inner (if (pair? items) (car items) (uir-null))])
            (uir-paren inner)))]
    [(pair? string-tokens) (lower-string-tokens string-tokens)]
    [(and (null? all-toks) (null? kids)) (uir-null)]
    [(pair? kids)
     (let ([inner (first kids)])
       (match (any-tree-tag inner)
         ['name (lower-name inner tk-type tk-value)]
         ['NONE (uir-null)]
         ['TRUE (uir-bool #t)]
         ['FALSE (uir-bool #f)]
         [_ (uir-symbol (format "?atom-~a" (any-tree-tag inner)))]))]
    [(pair? all-toks)
     (match (first (car all-toks))
       ['NUMBER (uir-number (second (car all-toks)))]
       ['FLOAT_NUMBER (uir-number (second (car all-toks)))]
       ['HEX_INTEGER (uir-number (python-int->decimal (second (car all-toks)) 16))]
       ['OCT_INTEGER (uir-number (python-int->decimal (second (car all-toks)) 8))]
       ['BIN_INTEGER (uir-number (python-int->decimal (second (car all-toks)) 2))]
       ['IMAG_NUMBER (uir-number (second (car all-toks)))]
       ['STRING (lower-string-token (second (car all-toks)))]
        ['TRUE (uir-bool #t)]
        ['FALSE (uir-bool #f)]
        ['NONE (uir-null)]
        ['ELLIPSIS (uir-symbol "...")]
        [_ (uir-symbol (format "?atom-~a" (first (car all-toks))))])]
    [else (uir-null)]))

(define (lower-name cst tk-type tk-value)
  (define toks (cst-tokens cst tk-type tk-value))
  (if (pair? toks)
      (uir-var (uir-symbol (second (car toks))))
      (uir-symbol "?name")))

;; ── String literals ────────────────────────────────────────────────
;;
;; A STRING token's value is the exact source text, including any prefix and
;; delimiter, so its meaning depends on which of Python's four literal families
;; it belongs to. The UIR stores each family in the form its emitter can render
;; back exactly:
;;
;;   no prefix / u / U (single or triple quoted)
;;       → uir-string holding the DECODED content (Python escape sequences
;;         processed). `'a"b'` stores `a"b`, `'\n'` stores a real newline,
;;         `'''a\nb'''` stores a real newline.
;;   r / R / b / B / br / rb (any case)
;;       → uir-string holding the FULL token text verbatim, delimiter
;;         included, because the escaping rules are prefix-dependent and the
;;         emitter writes prefixed literals back unchanged.
;;   f / F / fr / rf (any case)
;;       → uir-fstring holding the RAW body between the delimiters (escape
;;         sequences NOT decoded; the emitter re-adds the prefix and a
;;         delimiter it picks from the body).

;; Split a Python STRING token into (values prefix delimiter body). The lexer
;; guarantees at most a two-letter prefix followed by one of the four
;; delimiters.
(define string-literal-shape-rx
  #px"^([A-Za-z]{0,2})(\"\"\"|'''|\"|')")

(define (literal-prefix-and-body s)
  (define m (regexp-match string-literal-shape-rx s))
  (cond
    [(not m)
     ;; Not shaped like a Python string literal (should be impossible for a
     ;; STRING token); treat the whole value as bodyless content.
     (values "" "" s)]
    [else
     (define prefix (second m))
     (define delim (third m))
     (define start (+ (string-length prefix) (string-length delim)))
     (define end (- (string-length s) (string-length delim)))
     (values prefix delim (if (>= end start) (substring s start end) ""))]))

;; A code point -> character, or #f for a value Python would reject (out of
;; range or a lone surrogate). Invalid escapes are preserved verbatim.
(define (codepoint->char cp)
  (and (integer? cp)
       (<= 0 cp #x10FFFF)
       (not (<= #xD800 cp #xDFFF))
       (integer->char cp)))

;; n hex digits at START in S -> integer or #f
(define (hex-number s start n)
  (and (<= (+ start n) (string-length s))
       (let ([sub (substring s start (+ start n))])
         (and (regexp-match? #px"^[0-9A-Fa-f]+$" sub)
              (string->number sub 16)))))

;; n octal digits at START in S -> integer or #f
(define (octal-number s start n)
  (and (<= (+ start n) (string-length s))
       (let ([sub (substring s start (+ start n))])
         (and (regexp-match? #px"^[0-7]+$" sub)
              (string->number sub 8)))))

;; Decode Python string-literal escape sequences. Handles \\ \' \" \a \b \f
;; \n \r \t \v, octal \0-\777, \xXX, \uXXXX, \UXXXXXXXX, and the backslash
;; line continuation. Any other escape keeps its backslash (so an unsupported
;; escape such as \N{...} round-trips textually: the emitter escapes the
;; backslash and the decoder restores it — the UIR is stable, though the
;; character is not materialized; recorded as a residual gap).
(define (decode-python-escapes s)
  (define n (string-length s))
  (let loop ([i 0] [out '()])
    (cond
      [(>= i n) (list->string (reverse out))]
      [(char=? (string-ref s i) #\\) 
       (cond
         [(>= (add1 i) n) (loop (add1 i) (cons #\\ out))]  ; trailing backslash
         [else
          (define c (string-ref s (add1 i)))
          (cond
            [(char=? c #\\) (loop (+ i 2) (cons #\\ out))]
            [(char=? c #\') (loop (+ i 2) (cons #\' out))]
            [(char=? c #\") (loop (+ i 2) (cons #\" out))]
            [(char=? c #\a) (loop (+ i 2) (cons #\u0007 out))]
            [(char=? c #\b) (loop (+ i 2) (cons #\backspace out))]
            [(char=? c #\f) (loop (+ i 2) (cons #\u000C out))]
            [(char=? c #\n) (loop (+ i 2) (cons #\newline out))]
            [(char=? c #\r) (loop (+ i 2) (cons #\return out))]
            [(char=? c #\t) (loop (+ i 2) (cons #\tab out))]
            [(char=? c #\v) (loop (+ i 2) (cons #\u000B out))]
            ;; Backslash line continuation: remove both (and any \r\n pair).
            [(char=? c #\newline) (loop (+ i 2) out)]
            [(and (char=? c #\return)
                  (< (+ i 2) n)
                  (char=? (string-ref s (+ i 2)) #\newline))
             (loop (+ i 3) out)]
            ;; Octal: 1 to 3 digits.
            [(and (char-numeric? c) (char<=? c #\7))
             (define (digits k)
               (cond [(and (>= (- n (+ i 1)) k) (octal-number s (add1 i) k)) k]
                     [else #f]))
             (define len (or (digits 3) (digits 2) (digits 1)))
             (if len
                 (loop (+ i 1 len)
                       (cons (integer->char (octal-number s (add1 i) len)) out))
                 (loop (+ i 2) (cons c (cons #\\ out))))]
            [(char=? c #\x)
             (define v (hex-number s (+ i 2) 2))
             (define cp (and v (integer->char v)))
             (if cp (loop (+ i 4) (cons cp out))
                 (loop (+ i 2) (cons c (cons #\\ out))))]
            [(char=? c #\u)
             (define v (hex-number s (+ i 2) 4))
             (define cp (and v (codepoint->char v)))
             (if cp (loop (+ i 6) (cons cp out))
                 (loop (+ i 2) (cons c (cons #\\ out))))]
            [(char=? c #\U)
             (define v (hex-number s (+ i 2) 8))
             (define cp (and v (codepoint->char v)))
             (if cp (loop (+ i 10) (cons cp out))
                 (loop (+ i 2) (cons c (cons #\\ out))))]
            ;; Unknown escape (incl. \N{...}): keep the backslash and the
            ;; escaped character verbatim.
            [else (loop (+ i 2) (cons c (cons #\\ out)))])])]
      [else (loop (add1 i) (cons (string-ref s i) out))])))

;; Whether a string token's value should be stored verbatim (raw/bytes family).
(define (verbatim-string-prefix? prefix)
  (member (string-downcase prefix) '("r" "b" "br" "rb") equal?))

;; Whether a string token's value is an f-string.
(define (fstring-prefix? prefix)
  (member (string-downcase prefix) '("f" "fr" "rf") equal?))

;; Lower a STRING token value per the encoding table above.
(define (lower-string-token s)
  (define-values (prefix _delim body) (literal-prefix-and-body s))
  (cond
    [(fstring-prefix? prefix) (uir-fstring body)]
    [(verbatim-string-prefix? prefix) (uir-string s)]
    [else (uir-string (decode-python-escapes body))]))

;; Strip the prefix and full delimiter from a STRING token value without
;; decoding escapes (kept for callers that only need the literal body).
(define (unquote-string s)
  (define-values (_prefix _delim body) (literal-prefix-and-body s))
  body)

;; Unified walker: recursively walk a CST node, collecting all nodes of interest
;; Returns list of '(test node) and '(colon) markers in tree order
(define (walk-collection node tk-type)
  (if (any-tree? node)
      (if (or (eq? (any-tree-tag node) 'test)
              (eq? (any-tree-tag node) 'namedexpr_test))
          (list (list 'test node))
          (append-map (lambda (c)
                        (cond [(cst-node? c) (walk-collection c tk-type)]
                              [(and (list? c) (pair? c))
                               (append-map (lambda (cc) (walk-collection cc tk-type)) c)]
                              [(and (not (null? c)) (not (eq? c 'none))
                                    (with-handlers ([exn:fail? (lambda (_) #f)])
                                      (eq? (tk-type c) 'COLON)))
                               (list (list 'colon))]
                              [else '()]))
                      (any-tree-children node)))
      (if (and (not (null? node)) (not (eq? node 'none))
               (with-handlers ([exn:fail? (lambda (_) #f)])
                 (eq? (tk-type node) 'COLON)))
          (list (list 'colon))
          '())))

;; Recursively find all test nodes and lower them (for list/tuple items)
(define (lower-collection-items cst tk-type tk-value)
  (for/list ([t (walk-collection cst tk-type)] #:when (eq? (first t) 'test))
    (lower-expr (second t) tk-type tk-value)))

;; Lower the items of a list/tuple/set display. Unlike walk-collection (which
;; collects only `test` nodes for dict/comprehension analysis), this keeps
;; iterable-unpacking entries: `[*xs, 1]`, `(1, *xs)`, `{*a, *b}`. A star_expr
;; lowers to uir-spread, which the emitter renders as `*expr`.
(define (lower-display-items cst tk-type tk-value)
  (define out '())
  (define (walk x)
    (cond [(cst-node? x)
           (cond [(memq (any-tree-tag x) '(test namedexpr_test))
                  (set! out (cons (lower-expr x tk-type tk-value) out))]
                 [(eq? (any-tree-tag x) 'star_expr)
                  (define kids (node-children x))
                  (define inner
                    (if (pair? kids) (lower-expr (first kids) tk-type tk-value) (uir-null)))
                  (set! out (cons (uir-spread inner) out))]
                 [else (for-each walk (any-tree-children x))])]
          [(and (list? x) (pair? x)) (for-each walk x)]
          [else (void)]))
  (walk cst)
  (reverse out))

;; Is a dict-display entry node a `**expr` unpack? The generated parser wraps
;; each entry in a group whose raw children carry the entry marker: either a
;; COLON token (key: value) or a POWER token (** expr).
(define (dict-entry-unpack? node tk-type)
  (and (cst-node? node)
       (for/or ([c (any-tree-children node)] #:when (token-like? c tk-type))
         (eq? (tk-type c) 'POWER))))

(define (dict-entry-pair? node tk-type)
  (and (cst-node? node)
       (for/or ([c (any-tree-children node)] #:when (token-like? c tk-type))
         (eq? (tk-type c) 'COLON))))

;; Collect dict-display entries in source order as
;;   ('pair key-uir . value-uir)          for `key: value`
;;   ('unpack . expr-uir)                 for `**expr`
;; The generated parser encodes the comma repetition as a list child of
;; group(COMMA, entry) nodes; entries themselves are group nodes.
(define (collect-dict-display-entries cst tk-type tk-value)
  (define out '())
  (define (expr-child node)
    (for/or ([k (node-children node)]
             #:when (memq (any-tree-tag k) '(test namedexpr_test expr)))
      k))
  (define (walk x)
    (cond
      [(cst-node? x)
       (cond
         [(dict-entry-unpack? x tk-type)
          (define e (expr-child x))
          (set! out (cons (cons 'unpack
                                (if e (lower-expr e tk-type tk-value) (uir-null)))
                          out))]
         [(dict-entry-pair? x tk-type)
          (define tests (for/list ([k (node-children x)]
                                   #:when (memq (any-tree-tag k) '(test namedexpr_test)))
                          k))
          (define key (if (pair? tests) (lower-expr (first tests) tk-type tk-value) (uir-null)))
          (define val (if (>= (length tests) 2)
                          (lower-expr (second tests) tk-type tk-value)
                          (uir-null)))
          (set! out (cons (list 'pair key val) out))]
         [else (for-each walk (any-tree-children x))])]
      [(and (list? x) (pair? x)) (for-each walk x)]
      [else (void)]))
  (walk cst)
  (reverse out))

;; Lower dictorsetmaker: dict if any entry is `key: value` or `**expr`, else a
;; set display. Dict-display entries keep their lowered key expression (numbers
;; stay numbers, strings stay decoded strings) and `**` unpacks keep their
;; value via the kw-spread call encoding the emitter renders back as `**expr`.
(define (lower-dictorsetmaker cst tk-type tk-value)
  (define entries (collect-dict-display-entries cst tk-type tk-value))
  (if (for/or ([e entries]) (memq (car e) '(pair unpack)))
      (uir-record
       (for/list ([e entries])
         (cond
           [(eq? (car e) 'unpack)
            (cons (uir-call (uir-symbol "kw-spread") (list (cdr e))) (uir-null))]
           [else (cons (cadr e) (caddr e))])))
      ;; Set: collect the display items (iterable unpacking `{*a, *b}` lowers
      ;; to uir-spread entries, which the emitter renders as `*expr`).
      (uir-call (uir-symbol "set")
                (lower-display-items cst tk-type tk-value))))

;; Lower tuple items: extract test/yield_expr nodes from a tuple atom
(define (lower-tuple-items cst tk-type tk-value)
  ;; The atom contains a group wrapping the contents
  ;; Walk the tree and collect all test nodes
  (lower-collection-items cst tk-type tk-value))

;; ── Comprehensions ──────────────────────────────────────────────────

;; Check if a CST node contains a comp_for
(define (has-comp-for? node)
  (cond [(any-tree? node)
         (or (eq? (any-tree-tag node) 'comp_for)
             (for/or ([c (any-tree-children node)])
               (cond [(cst-node? c) (has-comp-for? c)]
                     [(and (list? c) (pair? c))
                      (for/or ([cc c]) (has-comp-for? cc))]
                     [else #f])))]
        [else #f]))

;; Lower a comp_for into (list loop-var iterable filter-expr)
(define (lower-comp-for cst tk-type tk-value)
  (define raw-kids (any-tree-children cst))
  (define var-names
    (for*/list ([c raw-kids] #:when (cst-node? c)
                #:when (eq? (any-tree-tag c) 'exprlist)
                [t (cst-tokens-deep c tk-type tk-value)]
                #:when (member (first t) '(NAME UNDERSCORE MATCH CASE)))
      (second t)))
  ;; One loop name is a plain symbol; a destructuring loop (`for a, b in it`)
  ;; carries every bound name as a uir-list so the census can type each element
  ;; projection instead of dropping the second name and reporting it unbound.
  (define loop-var
    (cond
      [(null? var-names) (uir-symbol "?var")]
      [(= (length var-names) 1) (uir-symbol (car var-names))]
      [else (uir-list (map (λ (nm) (uir-symbol nm)) var-names))]))
  (define iterable
    (for/or ([c raw-kids])
      (and (cst-node? c) (eq? (any-tree-tag c) 'or_test)
           (lower-expr c tk-type tk-value))))
  (define comp-iter
    (for/or ([c raw-kids])
      (and (cst-node? c) (eq? (any-tree-tag c) 'comp_iter) c)))
  (define filter-expr
    (if comp-iter
        (let ([ci-kids (node-children comp-iter)])
          (if (and (pair? ci-kids) (eq? (any-tree-tag (first ci-kids)) 'comp_if))
              (let* ([comp-if (first ci-kids)]
                     [cif-kids (node-children comp-if)]
                     [test-node (if (pair? cif-kids) (first cif-kids) #f)])
                (if test-node (lower-expr test-node tk-type tk-value) (uir-null)))
              (uir-null)))
        (uir-null)))
  (list loop-var (or iterable (uir-null)) filter-expr))

;; Lower a comprehension to UIR: (uir-call (uir-symbol "TYPE-comp") (list result loop-var iterable filter))
(define (lower-comprehension cst tk-type tk-value constructor)
  (define comp-for-node
    (let find ([node cst])
      (cond [(any-tree? node)
             (if (eq? (any-tree-tag node) 'comp_for)
                 node
                 (for/or ([c (any-tree-children node)])
                   (cond [(cst-node? c) (find c)]
                         [(and (list? c) (pair? c))
                          (for/or ([cc c]) (find cc))]
                         [else #f])))]
            [else #f])))
  ;; Extract the result expression from the first test sibling
  (define result-expr
    (let* ([ts (walk-collection cst tk-type)]
           [first-test (for/or ([t ts] #:when (eq? (first t) 'test)) (second t))])
      (if first-test (lower-expr first-test tk-type tk-value) (uir-null))))
  (define comp-parts (lower-comp-for comp-for-node tk-type tk-value))
  (uir-call (uir-symbol constructor)
            (list result-expr (car comp-parts) (cadr comp-parts) (caddr comp-parts))))

(define (lower-exprlist cst tk-type tk-value)
  (define exprs '())
  (let walk ([node cst])
    (cond [(and (cst-node? node) (eq? (any-tree-tag node) 'expr))
           (set! exprs (cons (lower-expr node tk-type tk-value) exprs))]
          [(cst-node? node)
           (for-each walk (any-tree-children node))]
          [(and (list? node) (pair? node))
           (for-each walk node)]))
  (reverse exprs))

;; ── Top-level entry ────────────────────────────────────────────────


(define (lower-lambdef cst tk-type tk-value)
  (define kids (node-children cst))
  ;; lambdef: [varargslist?, test]
  ;; varargslist is not a direct kid — it's in a list wrapper
  (define raw-kids (any-tree-children cst))
  ;; Find varargslist: first CST node (could be direct or in a list)
  (define params
    (for/or ([c raw-kids])
      (cond [(cst-node? c)
             (if (eq? (any-tree-tag c) 'varargslist)
                 (lower-varargslist c tk-type tk-value)
                 #f)]
            [(and (list? c) (pair? c) (cst-node? (car c)))
             (if (eq? (any-tree-tag (car c)) 'varargslist)
                 (lower-varargslist (car c) tk-type tk-value)
                 #f)]
            [else #f])))
  ;; Find test: the last CST node (may be wrapped in list)
  (define body-expr
    (for/last ([c raw-kids])
      (cond [(cst-node? c)
             (if (eq? (any-tree-tag c) 'test)
                 (lower-expr c tk-type tk-value)
                 #f)]
            [(and (list? c) (pair? c) (cst-node? (car c)))
             (if (eq? (any-tree-tag (car c)) 'test)
                 (lower-expr (car c) tk-type tk-value)
                 #f)]
            [else #f])))
  (uir-fn #f (or params '()) (or body-expr (uir-null)) #f #f))

;; Lower varargslist for lambda parameters (group of vfpdef nodes)
(define (lower-varargslist cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (for/list ([t toks] #:when (eq? (first t) 'NAME))
    (uir-symbol (second t))))

;; ── Assignment / binding targets ───────────────────────────────────
;;
;; A destructuring target (`a, b = x`, `[a, b] = x`, `a, *rest = x`) used to
;; collapse to its first element because `testlist_star_expr` is not an
;; expression rule. The UIR encoding is one `uir-list` in the set! name slot,
;; whose elements are the lowered targets (uir-var / uir-get / nested uir-list
;; / uir-spread). A single target keeps its plain expression node, so
;; `x = 1` is unchanged.
;;
;; The generated parser encodes a comma-separated sequence as: the first item
;; is a direct CST child, the rest are list-wrapped group(COMMA, item) nodes.
;; Walk only through group/list wrappers and stop at sequence items, so a
;; nested tuple (`(a, b)`) stays one item.
(define sequence-item-tags '(test star_expr namedexpr_test))

(define (sequence-item-nodes cst)
  (define out '())
  (define (walk x)
    (cond [(cst-node? x)
           (cond [(memq (any-tree-tag x) sequence-item-tags)
                  (set! out (cons x out))]
                 [else (for-each walk (any-tree-children x))])]
          [(and (list? x) (pair? x)) (for-each walk x)]
          [else (void)]))
  (walk cst)
  (reverse out))

;; True when CST is a comma-separated target list (`a, b`, `a,`, `*a, b`)
;; rather than a single target. The generated parser puts the repetition in a
;; list child (even an empty one for a trailing comma) or leaves the COMMA
;; token as a direct child. A single target (`a`, `a.b`, `(a, b)`) has neither.
(define (target-sequence? cst tk-type)
  (or (for/or ([c (any-tree-children cst)]) (and (list? c) (pair? c)))
      (for/or ([c (any-tree-children cst)])
        (and (token-like? c tk-type) (eq? (tk-type c) 'COMMA)))))

;; Normalize a lowered target expression into the target encoding: a tuple
;; call (`(a, b)` lowers to (call "tuple" ...)) becomes a uir-list, and lists /
;; spreads are normalized recursively.
(define (normalize-target u)
  (cond
    [(uir-spread? u) (uir-spread (normalize-target (uir-spread-expr u)))]
    [(and (uir-call? u)
          (uir-symbol? (uir-call-callee u))
          (equal? (uir-symbol-name (uir-call-callee u)) "tuple"))
     (uir-list (map normalize-target (uir-call-args u)))]
    [(uir-list? u) (uir-list (map normalize-target (uir-list-items u)))]
    [else u]))

;; Lower one assignment / with-as target subtree into its UIR target node. A
;; comma-separated target list becomes one uir-list; a lone target (including
;; a parenthesized tuple target, which normalizes to a uir-list element) keeps
;; its expression node.
(define (lower-assignment-target cst tk-type tk-value)
  (cond
    [(target-sequence? cst tk-type)
     (define items (sequence-item-nodes cst))
     (if (pair? items)
         (uir-list (map (lambda (n) (normalize-target (lower-expr n tk-type tk-value)))
                        items))
         (normalize-target (lower-expr cst tk-type tk-value)))]
    [else (normalize-target (lower-expr cst tk-type tk-value))]))

;; Helper: lower a dotted name (name_or_attr / dotted_name) to a full uir-get
;; chain. Works off the NAME tokens for the same reason lower-dotted-name does:
;; the generated parser wraps the dot repetition in list children.
(define (lower-get-path cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (define names (for/list ([t toks] #:when (eq? (first t) 'NAME)) (second t)))
  (cond
    [(null? names) (uir-symbol "?")]
    [else
     (foldl (lambda (nm acc) (uir-get acc (uir-string nm)))
            (uir-var (uir-symbol (car names)))
            (cdr names))]))


