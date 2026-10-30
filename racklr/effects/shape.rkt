#lang racket

;; racklr/effects/shape.rkt
;;
;; Structural predicates over UIR callee / name / function encodings. These are
;; the leaf helpers the effect classifier builds on: how each front end encodes
;; a member access, a bare name, an arrow / function expression, and how a
;; `Class.method` name symbol is formed.

(require racklr/uir)

(provide
  symbol-root
  dotted-root
  dot-call?
  arrow-call?
  function-expr-call?
  get-field-symbol
  callee-name
  effect-op-of-callee
  member-access-callee?
  callee-site-string
  member-access-base
  super-call?
  qualified-name-symbol
  wrapped-fn
  named-function)

;; Root of a (possibly namespace-qualified) symbol name string.
(define (symbol-root name)
  (let loop ([i 0])
    (cond [(>= i (string-length name)) (string->symbol name)]
          [(char=? (string-ref name i) #\/) (string->symbol (substring name 0 i))]
          [else (loop (add1 i))])))

;; Leftmost root name of a (possibly dotted/namespaced) callee expression,
;; as a symbol. Namespace-qualified names split on '/': "llm/query" → llm.
(define (dotted-root c)
  (let loop ([c c])
    (cond
      [(uir-get? c) (loop (uir-get-base c))]
      [(dot-call? c) (loop (first (uir-call-args c)))]
      [(uir-var? c) (symbol-root (uir-symbol-name (uir-var-name c)))]
      [(uir-symbol? c) (symbol-root (uir-symbol-name c))]
      [else #f])))

;; A JS/TS/JSX member access: `a.b` lowers to (call (symbol "dot") (list a b)).
(define (dot-call? c)
  (and (uir-call? c)
       (uir-symbol? (uir-call-callee c))
       (string=? (uir-symbol-name (uir-call-callee c)) "dot")
       (= (length (uir-call-args c)) 2)))

;; A JS/TS/JSX arrow function expression: (call "=>" (list params body)).
(define (arrow-call? c)
  (and (uir-call? c)
       (uir-symbol? (uir-call-callee c))
       (string=? (uir-symbol-name (uir-call-callee c)) "=>")
       (= (length (uir-call-args c)) 2)))

;; A JS/TS anonymous function expression: (call "function" (list params body)),
;; a sibling of `=>`.
(define (function-expr-call? c)
  (and (uir-call? c)
       (uir-symbol? (uir-call-callee c))
       (string=? (uir-symbol-name (uir-call-callee c)) "function")
       (= (length (uir-call-args c)) 2)))

;; Field name of a `uir-get` as a symbol. Front ends differ on whether an
;; attribute name is a uir-symbol or a uir-string, so accept both.
(define (get-field-symbol f)
  (cond
    [(uir-symbol? f) (string->symbol (uir-symbol-name f))]
    [(uir-string? f) (string->symbol (uir-string-value f))]
    [else #f]))

;; Simple (non-dotted) callee name: a uir-var or uir-symbol.
(define (callee-name c)
  (cond
    [(uir-var? c) (string->symbol (uir-symbol-name (uir-var-name c)))]
    [(uir-symbol? c) (string->symbol (uir-symbol-name c))]
    [else #f]))

;; Operation performed by a callee: the last field of a dotted chain, or the
;; bare callee name. Used by the emitters to instrument runtime guards with the
;; specific operation being performed. Returns a symbol or #f.
(define (effect-op-of-callee c)
  (let loop ([c c])
    (cond
      [(uir-get? c) (get-field-symbol (uir-get-field c))]
      [(dot-call? c) (get-field-symbol (second (uir-call-args c)))]
      [(uir-var? c) (string->symbol (uir-symbol-name (uir-var-name c)))]
      [(uir-symbol? c) (string->symbol (uir-symbol-name c))]
      [else #f])))

;; Compact human-readable rendering of a callee expression, for provenance
;; strings: `obj.filter`, `self._lib.get`, `super().__init__`, `fetch`. This is
;; the classifying call site — the expression the classifier saw — not a source
;; location (UIR carries none). Best-effort: unknown bases render as `?`.
(define (callee-site-string c)
  (let loop ([c c] [fields '()])
    (cond
      [(uir-get? c)
       (define f (get-field-symbol (uir-get-field c)))
       (loop (uir-get-base c) (cons (and f (symbol->string f)) fields))]
      [(dot-call? c)
       (define f (get-field-symbol (second (uir-call-args c))))
       (loop (first (uir-call-args c)) (cons (and f (symbol->string f)) fields))]
      [(uir-var? c)
       (string-join (cons (uir-symbol-name (uir-var-name c)) (filter values fields)) ".")]
      [(uir-symbol? c)
       (string-join (cons (uir-symbol-name c) (filter values fields)) ".")]
      [(uir-call? c)
       (string-join (cons (format "(~a)" (callee-site-string (uir-call-callee c)))
                          (filter values fields)) ".")]
      [else (string-join (filter values (cons "?" fields)) ".")])))

;; A Python member access (uir-get) or JS/TS/JSX (call "dot" ...) callee: a
;; dotted-through-object chain, as opposed to a bare var/symbol callee. Used to
;; distinguish `obj.filter(...)` (ORM) from a bare `filter(...)` (builtin).
(define (member-access-callee? c)
  (or (uir-get? c) (dot-call? c)))

;; Base of a member-access callee (the receiver the final field is read from),
;; in either the Python uir-get or JS/TS dot-call encoding. Returns #f for a
;; non-member callee.
(define (member-access-base c)
  (cond
    [(uir-get? c) (uir-get-base c)]
    [(dot-call? c) (first (uir-call-args c))]
    [else #f]))

;; Is `base` a `super()` invocation? `super().__init__(...)` lowers to a
;; uir-get whose base is (uir-call (uir-var "super") ...). Such a call targets
;; the ENCLOSING class's parent chain (most-derived first, C3), not the class
;; itself — resolving it structurally (no name heuristic) turns the dominant
;; Django `super().__init__` pattern into a call-graph edge instead of
;; `unknown`.
(define (super-call? base)
  (and (uir-call? base)
       (eq? (callee-name (uir-call-callee base)) 'super)))

;; "Class.method" name symbol for a class name and method name (both symbols).
(define (qualified-name-symbol class-name method-name)
  (string->symbol (format "~a.~a" class-name method-name)))

;; The uir-fn wrapped by a JS/TS function value: either the fn itself, or
;; (call "async-fn"/"gen-fn" (list fn)). Returns #f otherwise.
(define (wrapped-fn val)
  (cond
    [(uir-fn? val) val]
    [(and (uir-call? val)
          (uir-symbol? (uir-call-callee val))
          (member (uir-symbol-name (uir-call-callee val)) '("async-fn" "gen-fn"))
          (= (length (uir-call-args val)) 1)
          (uir-fn? (first (uir-call-args val))))
     (first (uir-call-args val))]
    [else #f]))

;; (name . body) for a named JS/TS function definition:
;;   (set! NAME (fn #f ...))                       — function declaration
;;   (set! NAME (call "async-fn"/"gen-fn" (fn)))   — async / generator decl
;;   (set! NAME (call "=>" (list params body)))    — arrow bound to a name
;;   (set! NAME (call "function" (list params body))) — function expr bound
;; Returns #f for anything else.
(define (named-function n)
  (and (uir-set!? n)
       (uir-symbol? (uir-set!-name n))
       (let* ([nm (string->symbol (uir-symbol-name (uir-set!-name n)))]
              [val (uir-set!-value n)]
              [fn (wrapped-fn val)])
         (cond
           [fn (cons nm (uir-fn-body fn))]
           [(arrow-call? val) (cons nm (second (uir-call-args val)))]
           [(function-expr-call? val) (cons nm (second (uir-call-args val)))]
           [else #f]))))
