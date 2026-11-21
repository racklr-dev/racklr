#lang racket

(require racklr/tree racklr/uir "helpers.rkt" "expr.rkt")

(provide lower-patterns lower-pattern lower-or-pattern lower-closed-pattern
         lower-literal-pattern catch-literal-token lower-capture-pattern
         lower-value-pattern lower-sequence-pattern lower-maybe-sequence-pattern
         lower-maybe-star-pattern lower-star-pattern lower-mapping-pattern
         lower-items-pattern lower-class-pattern lower-keyword-patterns
         lower-keyword-pattern lower-group-pattern lower-as-pattern
         lower-open-sequence flatten-sequence-patterns)

(define (lower-patterns cst tk-type tk-value)
  ;; cst is a `patterns` node: patterns: open_sequence_pattern | pattern.
  ;; Dispatch on the FIRST CHILD's tag — the node itself is always `patterns`,
  ;; so checking the node tag here never matched open_sequence_pattern.
  (define kids (node-children cst))
  (if (and (pair? kids) (eq? (any-tree-tag (first kids)) 'open_sequence_pattern))
      (lower-open-sequence (first kids) tk-type tk-value)
      (lower-pattern cst tk-type tk-value)))

(define (lower-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (let ([inner (first kids)])
        ;; Unwrap intermediate wrappers: 'pattern → 'or_pattern/'as_pattern
        (match (any-tree-tag inner)
          ['as_pattern (lower-as-pattern inner tk-type tk-value)]
          ['or_pattern (lower-or-pattern inner tk-type tk-value)]
          ['pattern (lower-pattern inner tk-type tk-value)] ;; recurse through nested 'pattern
          [_ (uir-symbol (format "?pat-~a" (any-tree-tag inner)))]))))

(define (lower-or-pattern cst tk-type tk-value)
  (define (collect-pats xs)
    (apply append
           (for/list ([x xs])
             (cond [(and (cst-node? x) (eq? (any-tree-tag x) 'closed_pattern)) (list x)]
                   [(cst-node? x) (collect-pats (any-tree-children x))]
                   [(list? x) (collect-pats x)]
                   [else '()]))))
  (define closed-pats (collect-pats (any-tree-children cst)))
  (if (= (length closed-pats) 1)
      (lower-closed-pattern (first closed-pats) tk-type tk-value)
      (uir-pat-or (map (λ (p) (lower-closed-pattern p tk-type tk-value)) closed-pats))))

(define (lower-closed-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (let ([inner (first kids)])
        (match (any-tree-tag inner)
          ['literal_pattern (lower-literal-pattern inner tk-type tk-value)]
          ['capture_pattern (lower-capture-pattern inner tk-type tk-value)]
          ['wildcard_pattern (uir-pat-wildcard)]
          ['value_pattern (lower-value-pattern inner tk-type tk-value)]
          ['group_pattern (lower-group-pattern inner tk-type tk-value)]
          ['sequence_pattern (lower-sequence-pattern inner tk-type tk-value)]
          ['mapping_pattern (lower-mapping-pattern inner tk-type tk-value)]
          ['class_pattern (lower-class-pattern inner tk-type tk-value)]
          [_ (uir-symbol (format "?closed-~a" (any-tree-tag inner)))]))))

;; literal_pattern: signed_number | complex_number | strings | 'None' | 'True' | 'False'
(define (lower-literal-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (catch-literal-token cst tk-type tk-value)
      (let* ([inner (first kids)]
             [tag (any-tree-tag inner)])
        (cond [(eq? tag 'signed_number)
               ;; signed_number: NUMBER or '-' NUMBER — as raw token children
               (define raw (any-tree-children inner))
               (define num-tok (for/or ([c raw] #:when (and (not (cst-node? c)) (not (null? c)) (not (symbol? c)))) c))
               (if num-tok
                   (uir-pat-literal (uir-number (tk-value num-tok)))
                   (uir-pat-literal (uir-symbol "?num")))]
              [(eq? tag 'complex_number)
               (uir-pat-literal (lower-expr inner tk-type tk-value))]
               [(eq? tag 'strings)
                ;; Store the same normalized literal the expression path
                ;; stores: plain/u literals decoded, raw/bytes verbatim,
                ;; f-strings raw. An implicit concatenation (`"a" "b"`) is one
                ;; literal.
                (define str-toks
                  (let walk ([n inner])
                    (cond [(and (not (cst-node? n)) (not (null? n))
                                (not (symbol? n))
                                (with-handlers ([exn:fail? (λ (_) #f)]) (tk-type n) #t))
                           (if (eq? (tk-type n) 'STRING) (list (tk-value n)) '())]
                          [(cst-node? n) (append-map walk (any-tree-children n))]
                          [(and (list? n) (pair? n)) (append-map walk n)]
                          [else '()])))
                (if (pair? str-toks)
                    (uir-pat-literal (lower-string-tokens str-toks))
                    (uir-pat-literal (uir-symbol "?")))]
              [else (catch-literal-token cst tk-type tk-value)]))))

(define (catch-literal-token cst tk-type tk-value)
  (define raw (any-tree-children cst))
  (define first-tok
    (for/or ([c raw]
             #:when (and (not (cst-node? c)) (not (null? c)) (not (symbol? c))))
      (cons (tk-type c) (tk-value c))))
  (if first-tok
      (match (car first-tok)
        ['NONE (uir-pat-literal (uir-symbol "None"))]
        ['TRUE (uir-pat-literal (uir-bool #t))]
        ['FALSE (uir-pat-literal (uir-bool #f))]
        [_ (uir-symbol (format "?lit-tok-~a" (car first-tok)))])
      (uir-symbol "?lit-unknown")))

;; capture_pattern: pattern_capture_target
;; NOTE: wildcard_pattern '_' is lexed as NAME, so _ always enters as capture_pattern
(define (lower-capture-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (define target (if (pair? kids) (first kids) #f))
  (define target-kids (if target (node-children target) '()))
  (define name-node (if (pair? target-kids) (first target-kids) #f))
  (define name-uir (if name-node (lower-name name-node tk-type tk-value) (uir-symbol "?")))
  (define sym-name (if (uir-var? name-uir) (uir-symbol-name (uir-var-name name-uir)) (uir-symbol-name name-uir)))
  (if (equal? sym-name "_")
      (uir-pat-wildcard)
      (uir-pat-capture (if (uir-var? name-uir) (uir-var-name name-uir) name-uir))))

;; value_pattern: dotted lookup like SomeClass.ATTR
(define (lower-value-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-pat-value (uir-symbol "?"))
      (uir-pat-value (lower-get-path (first kids) tk-type tk-value))))

;; sequence_pattern: '[' maybe_sequence_pattern? ']'
(define (lower-sequence-pattern cst tk-type tk-value)
  (define maybe-seq (for/or ([c (node-children cst)]
                              #:when (eq? (any-tree-tag c) 'maybe_sequence_pattern))
                       c))
  (if maybe-seq
      (lower-maybe-sequence-pattern maybe-seq tk-type tk-value)
      (uir-pat-sequence '())))

(define (lower-maybe-sequence-pattern cst tk-type tk-value)
  (define (collect-elems xs)
    (apply append
           (for/list ([x xs])
             (cond [(and (cst-node? x) (eq? (any-tree-tag x) 'maybe_star_pattern))
                    (list (lower-maybe-star-pattern x tk-type tk-value))]
                   [(and (cst-node? x) (eq? (any-tree-tag x) 'maybe_sequence_pattern))
                    (uir-pat-sequence-elements (lower-maybe-sequence-pattern x tk-type tk-value))]
                   [(cst-node? x) (collect-elems (any-tree-children x))]
                   [(list? x) (collect-elems x)]
                   [else '()]))))
  (uir-pat-sequence (collect-elems (any-tree-children cst))))

(define (lower-maybe-star-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (let ([inner (first kids)])
        (if (eq? (any-tree-tag inner) 'star_pattern)
            (lower-star-pattern inner tk-type tk-value)
            (lower-pattern inner tk-type tk-value)))))

;; star_pattern: '*' pattern_capture_target | '*' wildcard_pattern
(define (lower-star-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-pat-star #f)
      (let ([inner (first kids)])
        (if (eq? (any-tree-tag inner) 'wildcard_pattern)
            (uir-pat-star #f)
            ;; pattern_capture_target
            (let* ([tn (node-children inner)]
                   [name-node (if (pair? tn) (first tn) #f)])
              (uir-pat-star (if name-node (lower-name name-node tk-type tk-value) #f)))))))

;; mapping_pattern: '{' [items_pattern] '}'
(define (lower-mapping-pattern cst tk-type tk-value)
  (define items (for/or ([c (node-children cst)]
                          #:when (eq? (any-tree-tag c) 'items_pattern))
                   c))
  (if items
      (lower-items-pattern items tk-type tk-value)
      (uir-pat-mapping '() #f)))

(define (lower-items-pattern cst tk-type tk-value)
  (define raw-kids (any-tree-children cst))
  (define entries '())
  (define rest #f)
  (define current-key #f)
  (for ([c raw-kids])
    (cond [(and current-key (cst-node? c) (eq? (any-tree-tag c) 'pattern))
           (set! entries (cons (cons current-key (lower-pattern c tk-type tk-value)) entries))
           (set! current-key #f)]
          [(cst-node? c) (set! current-key (lower-pattern c tk-type tk-value))]
          [(and (not (null? c)) (not (symbol? c)))
           (define tt (tk-type c))
           (when (eq? tt 'DOUBLESTAR)
             (set! current-key 'double-star))]))
  ;; If we ended with a key but no value, it's a double-star pattern
  (when (eq? current-key 'double-star)
    (set! current-key #f)
    (set! rest #t))
  (uir-pat-mapping (reverse entries) (if rest (uir-pat-double-star (uir-symbol "?")) #f)))

;; class_pattern: name_or_attr '(' [patterns] [keyword_patterns] ')'
(define (lower-class-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (define cls-path
    (if (pair? kids)
        (lower-get-path (first kids) tk-type tk-value)
        (uir-symbol "?")))
  (define positional '())
  (define keyword '())
  (for ([c (rest kids)])
    (match (any-tree-tag c)
      ['patterns (set! positional (map (λ (p) (lower-pattern p tk-type tk-value)) (node-children c)))]
      ['keyword_patterns (set! keyword (lower-keyword-patterns c tk-type tk-value))]
      [else (void)]))
  (uir-pat-class cls-path positional keyword))

(define (lower-keyword-patterns cst tk-type tk-value)
  (map lower-keyword-pattern (node-children cst)))

(define (lower-keyword-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (define key (if (pair? kids) (lower-name (first kids) tk-type tk-value) (uir-symbol "?")))
  (define val (if (>= (length kids) 2) (lower-pattern (second kids) tk-type tk-value) (uir-null)))
  (cons key val))

;; group_pattern: '(' as_pattern ')'
(define (lower-group-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-pat-group (uir-null))
      (let ([inner (first kids)])
        (uir-pat-group (if (eq? (any-tree-tag inner) 'as_pattern)
                           (lower-as-pattern inner tk-type tk-value)
                           (lower-pattern inner tk-type tk-value))))))

;; as_pattern: or_pattern 'as' pattern_capture_target
(define (lower-as-pattern cst tk-type tk-value)
  (define kids (node-children cst))
  (define or-pat (if (pair? kids) (first kids) #f))
  (define target
    (for/or ([c (node-children cst)]
             #:when (eq? (any-tree-tag c) 'pattern_capture_target))
      c))
  (define name
    (if target
        (let* ([tn (node-children target)]
               [name-node (if (pair? tn) (first tn) #f)])
          (if name-node (lower-name name-node tk-type tk-value) (uir-symbol "?")))
        (uir-symbol "?")))
  (uir-pat-as (if or-pat (lower-or-pattern or-pat tk-type tk-value) (uir-null)) name))

;; open_sequence_pattern: maybe_star_pattern ',' maybe_sequence_pattern?
(define (lower-open-sequence cst tk-type tk-value)
  (define (collect-elems xs)
    (apply append
           (for/list ([x xs])
             (cond [(and (cst-node? x) (eq? (any-tree-tag x) 'maybe_star_pattern))
                    (list (lower-maybe-star-pattern x tk-type tk-value))]
                   [(and (cst-node? x) (eq? (any-tree-tag x) 'maybe_sequence_pattern))
                    (uir-pat-sequence-elements (lower-maybe-sequence-pattern x tk-type tk-value))]
                   [(cst-node? x) (collect-elems (any-tree-children x))]
                   [(list? x) (collect-elems x)]
                   [else '()]))))
  (uir-pat-sequence (collect-elems (any-tree-children cst))))

(define (flatten-sequence-patterns elems)
  (apply append
         (for/list ([e elems])
           (if (uir-pat-sequence? e)
               (uir-pat-sequence-elements e)
               (list e)))))

;; Helper: lower a dotted_name to uir-symbol or uir-get chain

