#lang racket

;; racklr/lower-rust/helpers.rkt — CST-walking helpers for the Rust front end.
;;
;; The ANTLR4 Rust grammar nests every rule in wrapper nodes (`item` wraps a
;; `visItem` wraps a `group` wraps `function_`), and repetitions become list
;; children holding `group` nodes. These helpers let the lowering pass look at
;; structure by tag instead of brittle child positions.

(require racklr/tree)

(provide
 token-like?
 single-node-child
 node-children
 list-wrapper
 node-tag
 group-items
 collect-items
 find-descendant
 find-descendants
 first-descendant-token
 deep-tokens
 immediate-tokens
 token-named?
 has-token?
 text-of-tokens
 strip-quotes
 ordered-nodes
 ordered-items
 path-name
 path-token-types
 first-identifier)

;; ── Generic tree walking ────────────────────────────────────────────

(define (node-tag node)
  (any-tree-tag node))

;; Only the cst-node children (skip tokens, lists, 'none, '()).
(define (node-children node)
  (filter cst-node? (any-tree-children node)))

;; The single deeper cst-node child of a wrapper node (`item` → `group`), or #f.
(define (single-node-child node)
  (for/or ([c (any-tree-children node)])
    (and (cst-node? c) c)))

;; The first non-empty list child: the wrapper holding repeated elements.
(define (list-wrapper node)
  (for/or ([c (any-tree-children node)])
    (and (pair? c) c)))

;; A token is a leaf produced by the generated lexer. Generated tokens respond
;; to tk-type; the grammar's placeholder values ('none, '()) do not.
(define (token-like? x tk-type)
  (and (not (cst-node? x))
       (not (null? x))
       (not (eq? x 'none))
       (not (pair? x))
       (with-handlers ([exn:fail? (λ (_) #f)])
         (tk-type x)
         #t)))

;; ── Repetition wrappers ─────────────────────────────────────────────

;; Unpack one item of a repetition list: a bare node, or a `group` node that
;; wraps a comma and/or a node (e.g. `(group COMMA (functionParam ...))`).
(define (group-items item)
  (cond
    [(not (cst-node? item)) '()]
    [(eq? (node-tag item) 'group) (filter cst-node? (any-tree-children item))]
    [else (list item)]))

;; All cst-node items of a rule's repetition list, with `group` wrappers
;; flattened: elements appear in source order.
(define (collect-items node)
  (for/fold ([acc '()]) ([item (in-list (or (list-wrapper node) '()))])
    (append acc (group-items item))))

;; ── Queries ─────────────────────────────────────────────────────────

;; First descendant cst-node with `tag`, depth-first in document order.
(define (find-descendant node tag)
  (define found #f)
  (let walk ([n node])
    (cond
      [found (void)]
      [(not (cst-node? n))
       (when (and (list? n) (pair? n)) (for ([c n]) (walk c)))]
      [(eq? (node-tag n) tag) (set! found n)]
      [else (for ([c (any-tree-children n)]) (walk c))]))
  found)

;; All descendant cst-nodes with `tag`, depth-first in document order.
(define (find-descendants node tag)
  (define acc '())
  (let walk ([n node])
    (cond
      [(not (cst-node? n))
       (when (and (list? n) (pair? n)) (for ([c n]) (walk c)))]
      [else
       (when (eq? (node-tag n) tag) (set! acc (cons n acc)))
       (for ([c (any-tree-children n)]) (walk c))]))
  (reverse acc))

;; Token children of a node, in order.
(define (immediate-tokens node tk-type)
  (for/list ([c (any-tree-children node)] #:when (token-like? c tk-type)) c))

;; All tokens under a node (through nodes and lists), in source order.
(define (deep-tokens node tk-type [tk-value #f])
  (define acc '())
  (let walk ([n node])
    (cond
      [(token-like? n tk-type) (set! acc (cons n acc))]
      [(cst-node? n) (for ([c (any-tree-children n)]) (walk c))]
      [(and (list? n) (pair? n)) (for ([c n]) (walk c))]
      [else (void)]))
  (reverse acc))

;; First token (immediate only) whose type is `want`, or #f.
(define (token-named? node tk-type want)
  (for/or ([t (immediate-tokens node tk-type)]) (eq? (tk-type t) want)))

;; Does this node have an immediate token of type `want`?
(define (has-token? node tk-type want)
  (and (token-named? node tk-type want) #t))

(define (first-descendant-token node tk-type want)
  (for/or ([t (deep-tokens node tk-type)]) (and (eq? (tk-type t) want) t)))

;; Join token values into a flat string (used to rebuild a Rust path/type).
(define (text-of-tokens tokens tk-value)
  (string-append* (for/list ([t (in-list tokens)]) (tk-value t))))

;; Strip one pair of matching quotes from a lexeme; leave anything else alone.
(define (strip-quotes s)
  (if (and (string? s)
           (>= (string-length s) 2)
           (or (and (char=? (string-ref s 0) #\") (char=? (string-ref s (sub1 (string-length s))) #\"))
               (and (char=? (string-ref s 0) #\') (char=? (string-ref s (sub1 (string-length s))) #\'))))
      (substring s 1 (sub1 (string-length s)))
      s))

;; ── Ordered child collection ────────────────────────────────────────

;; Nodes whose tag is `tag` (or one of `tags`) that are direct children of
;; `node`, of a `group` child, or of a repetition list of either — in source
;; order, without descending into nested nodes of the same kind.
(define (ordered-nodes node tags)
  (define wanted (if (list? tags) tags (list tags)))
  (define out '())
  (define (visit c)
    (cond
      [(and (cst-node? c) (memq (node-tag c) wanted)) (set! out (cons c out))]
      [(and (cst-node? c) (eq? (node-tag c) 'group))
       (for ([cc (any-tree-children c)]) (visit cc))]
      [(and (list? c) (pair? c)) (for ([cc c]) (visit cc))]
      [else (void)]))
  (for ([c (any-tree-children node)]) (visit c))
  (reverse out))

;; `ordered-nodes` on one tag, for the common single-tag case.
(define (ordered-items node tag) (ordered-nodes node (list tag)))

;; ── Rust paths ──────────────────────────────────────────────────────

(define path-token-types
  '(NON_KEYWORD_IDENTIFIER IDENTIFIER KW_SELFVALUE KW_SELFTYPE KW_SUPER KW_CRATE
    KW_MACRORULES LIFETIME_OR_LABEL))

;; "std::fs::read_to_string" from a pathInExpression / simplePath / typePath.
;; Qualified paths are kept as ONE symbol with `::` preserved: the injected
;; effect lexicon matches the whole path, and shape.rkt's dotted-root does not
;; split on "::".
(define (path-name node tk-type tk-value)
  (string-append*
   (for/list ([t (in-list (deep-tokens node tk-type tk-value))]
              #:when (or (memq (tk-type t) path-token-types)
                         (eq? (tk-type t) 'PATHSEP)))
     (if (eq? (tk-type t) 'PATHSEP) "::" (tk-value t)))))

;; First identifier-like lexeme in a subtree, or "?".
(define (first-identifier node tk-type tk-value)
  (or (for/or ([t (in-list (deep-tokens node tk-type))]
               #:when (memq (tk-type t) '(NON_KEYWORD_IDENTIFIER IDENTIFIER
                                          KW_SELFVALUE KW_SELFTYPE KW_SUPER KW_CRATE)))
        (tk-value t))
      "?"))
