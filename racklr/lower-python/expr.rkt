#lang racket

(require racklr/tree racklr/uir "helpers.rkt")

(provide lower-expr lower-dotted-name lower-not-test lower-comparison lower-binop
         lower-atom-expr lower-trailer find-node lower-subscriptlist find-subscripts
         lower-arglist lower-argument lower-group-expr lower-atom lower-name
         unquote-string lower-string-token walk-collection lower-collection-items
         lower-dictorsetmaker lower-tuple-items has-comp-for? lower-comp-for
         lower-comprehension lower-exprlist lower-lambdef lower-varargslist
         lower-get-path)

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
          (uir-set! lhs rhs))]
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

;; Lower a dotted_name (a.b.c) into a symbol or attribute chain
(define (lower-dotted-name cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (define names (for/list ([t toks] #:when (eq? (first t) 'NAME)) (second t)))
  (if (null? names)
      (uir-symbol "?dotted")
      (uir-var (uir-symbol (car names)))))

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
(define (lower-comparison cst tk-type tk-value)
  (define kids (node-children cst))
  (define pairs (collect-op-pairs (cdr (any-tree-children cst)) tk-type))
  (if (null? pairs)
      (if (= (length kids) 1)
          (lower-expr (first kids) tk-type tk-value)
          (uir-symbol "?comparison"))
      (let fold ([pairs pairs]
                 [acc (if (pair? kids) (lower-expr (first kids) tk-type tk-value) (uir-null))])
        (if (null? pairs)
            acc
            (let ([p (car pairs)])
              (fold (cdr pairs)
                    (uir-call (uir-symbol (op-symbol (car p) tk-type tk-value))
                              (list acc (lower-expr (cdr p) tk-type tk-value)))))))))

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
(define (lower-subscriptlist cst tk-type tk-value)
  (define sub-exprs
    (map (lambda (sub) (lower-subscript sub tk-type tk-value))
         (find-subscripts cst)))
  (if (null? sub-exprs)
      (uir-null)
      (if (= (length sub-exprs) 1)
          (first sub-exprs)
          (uir-list sub-exprs))))

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
  ;; A keyword argument (`a=1`) is the first kid being a `keyword` node and the
  ;; second kid the value `test` (the `=` token is filtered out by
  ;; node-children). Emit it as (call "kwarg" (list name value)) so the value
  ;; is preserved for the type-graph search and emit round-trips.
  ;; A generator expression (`f(x for x in items)`) has a comp_for as the
  ;; second kid: lower it to (call "gen-exp" (list result loop-var iter
  ;; filter)) so the loop variable is preserved for receiver-type dataflow
  ;; instead of being dropped.
  (define kids (node-children cst))
  (cond
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
         (uir-list (lower-collection-items (first kids) tk-type tk-value)))]
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
          (uir-call (uir-symbol "tuple") (lower-tuple-items cst tk-type tk-value))
          ;; Parenthesized expression: lower the inner expression and wrap in uir-paren
          (let* ([items (lower-tuple-items cst tk-type tk-value)]
                 [inner (if (pair? items) (car items) (uir-null))])
            (uir-paren inner)))]
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

;; Strip surrounding quotes from a STRING token value
(define (unquote-string s)
  (if (and (>= (string-length s) 2)
           (or (eq? (string-ref s 0) #\") (eq? (string-ref s 0) #\'))
           (eq? (string-ref s 0) (string-ref s (sub1 (string-length s)))))
      (substring s 1 (sub1 (string-length s)))
      s))

;; Lower a STRING token value, detecting f-strings
(define (lower-string-token s)
  (cond [(and (>= (string-length s) 3)
              (or (eq? (string-ref s 0) #\f) (eq? (string-ref s 0) #\F))
              (or (eq? (string-ref s 1) #\") (eq? (string-ref s 1) #\'))
              (eq? (string-ref s 1) (string-ref s (sub1 (string-length s)))))
         ;; f-string: strip 'f' prefix and quotes
         (uir-fstring (substring s 2 (sub1 (string-length s))))]
        [else
         (uir-string (unquote-string s))]))

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

;; Lower dictorsetmaker: detect dict vs set via COLON, extract pairs or items
(define (lower-dictorsetmaker cst tk-type tk-value)
  (define tests-and-colons (walk-collection cst tk-type))
  (define has-colon? (for/or ([t tests-and-colons] #:when (eq? (first t) 'colon)) #t))
  (if has-colon?
      (let* ([tests (filter (lambda (t) (eq? (first t) 'test)) tests-and-colons)]
             [entries
              (let loop ([ts tests])
                (if (or (null? ts) (null? (cdr ts)))
                    '()
                    (let* ([key-expr (lower-expr (second (car ts)) tk-type tk-value)]
                           [val-expr (lower-expr (second (cadr ts)) tk-type tk-value)]
                           [key-str
                            (cond [(uir-string? key-expr) (uir-string-value key-expr)]
                                  [(uir-number? key-expr) (uir-number-value key-expr)]
                                  [(uir-symbol? key-expr) (uir-symbol-name key-expr)]
                                  [(uir-var? key-expr) (uir-symbol-name (uir-var-name key-expr))]
                                  [else (format "~a" (uir-tag key-expr))])])
                      (cons (cons (uir-string key-str) val-expr)
                            (loop (cddr ts))))))])
        (uir-record entries))
      ;; Set: just collect test items
      (uir-call (uir-symbol "set")
       (for/list ([t tests-and-colons] #:when (eq? (first t) 'test))
         (lower-expr (second t) tk-type tk-value)))))

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

;; Helper: lower a dotted_name to uir-symbol or uir-get chain
(define (lower-get-path cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-symbol "?")
      (let* ([first-name (first kids)]
             [nm (lower-name first-name tk-type tk-value)])
        (if (= (length kids) 1)
            nm
            (uir-get nm (lower-name (second kids) tk-type tk-value))))))


