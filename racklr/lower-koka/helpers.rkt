#lang racket

;; racklr/lower-koka/helpers.rkt — CST-walking helpers for the Koka 3.x front
;; end (generated parser CST → UIR).
;;
;; The generated parser emits, for every grammar rule, one `cst-node` whose
;; children are, in grammar order: nested `cst-node`s (child rules), token
;; structs (terminals), `'none` (an absent optional), and plain Racket lists
;; (an EBNF star/plus result; a star element that is a parenthesized sequence
;; arrives as a `group` node). These helpers keep the lowering passes free of
;; brittle position arithmetic.
;;
;; Token accessors are injectable: the generated parser module owns its token
;; struct, so the public lowering entry point installs the `tk-type` /
;; `tk-value` accessors once (`set-token-accessors!`); every module in this
;; directory reads tokens through `tok-type` / `tok-value`.

(require racklr/tree
         racklr/uir)

(provide
 ;; token accessors (installed by the public entry point)
 set-token-accessors! tok-type tok-value
 ;; structural predicates
 token-struct? tok? node? none? list-child?
 ;; navigation
 children nodes lists tokens find-node find-node-in find-descendant
 find-descendant-nodes find-token find-token-in list-items all-list-items
 group-of node-tag node-tag?
 ;; reusable child shapes
 star-groups star-list repeated-first
 ;; text
 token-text text-of-tokens identifier-text
 ;; declaration decoration
 decorate has-token? node-has-text? pubmod-public? pubmod-private? token-value-is?
 qimplicit-name implicit-ref?
 ;; rendering token streams (types, kinds, binder text)
 render-tokens strip-quotes)

;; ── Token accessors (set by lower-koka.rkt) ──────────────────────────

(define current-tk-type (make-parameter #f))
(define current-tk-value (make-parameter #f))

(define (set-token-accessors! tk-type tk-value)
  (current-tk-type tk-type)
  (current-tk-value tk-value))

(define (tok-type t) ((current-tk-type) t))
(define (tok-value t) ((current-tk-value) t))

;; A generated token is a transparent Racket struct that is neither a CST
;; node nor a CST leaf; that is enough to recognise it without importing the
;; generated parser's private struct descriptor.
(define (token-struct? x)
  (and (struct? x) (not (cst-node? x)) (not (cst-leaf? x))))

(define (tok? x type) (and (token-struct? x) (eq? (tok-type x) type)))
(define (node? x) (cst-node? x))
(define (none? x) (eq? x 'none))
(define (list-child? x) (and (list? x) #t))

;; ── Navigation ──────────────────────────────────────────────────────

(define (children n) (any-tree-children n))
(define (nodes n) (filter node? (children n)))
(define (lists n) (filter list-child? (children n)))
(define (tokens n) (filter token-struct? (children n)))

;; First immediate child node with the given rule tag.
(define (find-node n tag)
  (for/first ([c (in-list (children n))] #:when (and (node? c) (eq? (node-tag c) tag))) c))

(define (find-node-in n tags)
  (for/first ([c (in-list (children n))]
              #:when (and (node? c) (memq (node-tag c) tags)))
    c))

;; First descendant (pre-order) node with the given tag.
(define (find-descendant n tag)
  (cond [(and (node? n) (eq? (node-tag n) tag)) n]
        [(node? n) (for/or ([c (in-list (children n))]) (find-descendant c tag))]
        [(list? n) (for/or ([c (in-list n)]) (find-descendant c tag))]
        [else #f]))

(define (find-descendant-nodes n tag)
  (append*
   (cond [(and (node? n) (eq? (node-tag n) tag)) (list (list n))]
         [(node? n) (for/list ([c (in-list (children n))]) (find-descendant-nodes c tag))]
         [(list? n) (for/list ([c (in-list n)]) (find-descendant-nodes c tag))]
         [else '()])))

;; First immediate token of the given lexer type.
(define (find-token n type)
  (for/first ([t (in-list (children n))] #:when (tok? t type)) t))

(define (find-token-in n types)
  (for/first ([t (in-list (children n))] #:when (and (token-struct? t) (memq (tok-type t) types))) t))

;; First immediate token whose type is any of a list of types, plus its text.
(define (token-text t) (and (token-struct? t) (tok-value t)))

(define (node-tag n) (any-tree-tag n))
(define (node-tag? n tag) (and (node? n) (eq? (any-tree-tag n) tag)))

;; Every leaf/token under a node (in source order), skipping 'none markers
;; and nested plain lists' grouping structure.
(define (text-of-tokens n)
  (cond [(token-struct? n) (list n)]
        [(cst-leaf? n) (list n)]
        [(node? n) (append* (map text-of-tokens (children n)))]
        [(list? n) (append* (map text-of-tokens n))]
        [else '()]))

;; ── Repeated-child shapes ───────────────────────────────────────────

;; The plan lists that hold repetition elements, e.g. `statements`, `semis`
;; and `imports` children.
(define (list-items l) (if (list? l) (filter node? l) '()))

;; All plain-list children flattened, in source order.
(define (all-list-items n)
  (append* (map list-items (lists n))))

;; The first list child of a node (the repetition plan used most rules).
(define (star-list n)
  (for/first ([l (in-list (children n))] #:when (list-child? l)) l))

;; The `group` nodes of a star list (`(declaration semis)*` etc.).
(define (star-groups n)
  (filter (λ (x) (and (node? x) (eq? (node-tag x) 'group))) (all-list-items n)))

;; A rule written `first (SEP first)*`: the first child item and the rest.
(define (repeated-first n item-tag)
  (define first (find-node n item-tag))
  (define rest
    (for/list ([g (in-list (star-groups n))]
               #:when (eq? (node-tag g) 'group))
      (find-node g item-tag)))
  (cons first rest))

(define (group-of n) (and (node? n) (eq? (node-tag n) 'group) n))

;; ── Identifier / text rendering ─────────────────────────────────────

(define (identifier-text n)
  (cond [(token-struct? n) (tok-value n)]
        [(cst-leaf? n) (cst-leaf-text n)]
        [(node? n)
         (define items (text-of-tokens n))
         (string-append* (map token-text items))]
        [else (format "~a" n)]))

(define closing-tokens '(CLOSE_PAREN CLOSE_BRACK GT))
(define opening-tokens '(OPEN_PAREN OPEN_BRACK LT))
(define word-types '(IDENT CONID QID QCONID IDOP QIDOP WILDCARD INT FLOAT
                     CHAR STRING RAWSTRING))

;; Render an ordered token list as source-like text. The output must re-parse
;; (types, kinds and binder texts are carried as normalized text in the UIR),
;; not reproduce the original spacing.
(define (render-tokens ts)
  (define parts '())
  (define prev #f)
  (for ([t (in-list ts)])
    (define ty (tok-type t))
    (define txt (tok-value t))
    (when prev
      (unless (no-space-between? (car prev) (tok-type (car prev)) ty)
        (set! parts (cons " " parts))))
    (set! parts (cons txt parts))
    (set! prev (cons t ty)))
  (apply string-append (reverse parts)))

(define (no-space-between? prev-tok prev-ty cur-ty)
  (or (memq cur-ty closing-tokens)
      (memq cur-ty '(COMMA SEMI))
      (memq prev-ty opening-tokens)
      (and (eq? prev-ty 'QUESTION) #t)
      (and (eq? prev-ty 'DOT) #t)
      (and (eq? cur-ty 'DOT) #t)
      (and (eq? cur-ty 'OPEN_PAREN) (memq prev-ty '(IDENT CONID QID QCONID IDOP QIDOP)))
      (and (eq? cur-ty 'OPEN_BRACK) (memq prev-ty '(IDENT CONID QID QCONID)))
      (and (eq? cur-ty 'LT)
           (memq prev-ty '(IDENT CONID QID QCONID CTX CLOSE_PAREN CLOSE_BRACK)))
      ;; `<|e>`: the `|` directly follows `<`
      (and (eq? cur-ty 'PIPE) (eq? prev-ty 'LT))
      (and (eq? prev-ty 'PIPE) (eq? cur-ty 'GT))))

;; ── Declaration decoration ──────────────────────────────────────────
;;
;; Koka declaration modifiers (`pub`, `inline`, `fip`, `value`, `open`,
;; `linear`, `abstract`, a `:: kind` annotation, …) and explicit type-parameter
;; binders have no fields on the existing UIR declaration kinds, so they ride on
;; the generic `uir-decorated` wrapper: one symbol per modifier and one
;; `uir-type-params` node for binders. Emission re-orders them into Koka's
;; canonical declaration order.

(define (decorate node modifiers)
  (define mods
    (for/list ([m (in-list modifiers)] #:when m)
      (cond [(symbol? m) (uir-symbol (symbol->string m))]
            [(string? m) (uir-symbol m)]
            [else m])))
  (if (null? mods) node (uir-decorated mods node)))

(define (has-token? node type)
  (and node (for/or ([t (in-list (tokens node))]) (tok? t type))))

;; `pubmod : PUB | 'public' | 'private' | ;` — `public`/`private` are legacy
;; IDENT spellings of the same visibility slot.
(define (node-has-text? node text)
  (and node
       (for/or ([t (in-list (tokens node))])
         (and (token-struct? t) (equal? (tok-value t) text)))))

(define (pubmod-public? node)
  (and node (or (has-token? node 'PUB) (node-has-text? node "public"))))

(define (pubmod-private? node)
  (and node (node-has-text? node "private")))

(define (token-value-is? token text)
  (and (token-struct? token) (equal? (tok-value token) text)))

;; `qimplicit : QUESTION qidentifier` — the reference name without its `?`.
(define (qimplicit-name node)
  (define kid (find-node node 'qidentifier))
  (if kid (identifier-text kid) (identifier-text node)))

(define (implicit-ref? x) (uir-implicit-ref? x))

(define (strip-quotes s)
  (cond
    [(regexp-match? #rx"^r(#*)\"" s)
     ;; raw string: r"..." / r#"..."#
     (define hashes (or (regexp-match #rx"^r(#*)\"" s) '("" "")))
     (define h (cadr hashes))
     (define prefix-len (+ 2 (string-length h)))
     (define suffix-len (+ 1 (string-length h)))
     (if (> (string-length s) (+ prefix-len suffix-len -1))
         (substring s prefix-len (- (string-length s) suffix-len))
         s)]
    [(and (> (string-length s) 1)
          (char=? (string-ref s 0) #\")
          (char=? (string-ref s (sub1 (string-length s))) #\"))
     (substring s 1 (sub1 (string-length s)))]
    [else s]))
