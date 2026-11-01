#lang racket

;; Python function parameter lists with star markers (Django front-end
;; catalogue item #7): a bare `*` separator, `*args` and `**kwargs` must keep
;; their calling convention through lowering and emit. Before this suite, every
;; star marker was dropped, so `def f(a, *, b=None)` emitted as
;; `def f(a, b = None)` — a signature that accepts a second positional argument
;; and silently changes call semantics.
;;
;; The single_input entry point is known to be broken for compound statements
;; (see LOOP_PLAN issue #1), so `def` sources parse through file_input, the
;; entry point the actual lowering pipeline uses. Run with a collection path
;; that reaches this worktree:
;;   cd racklr-test && racket -S .. lower-python-params-test.rkt

(require rackunit
         rackunit/text-ui
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-python
         racklr/emit-python)

;; ── Load the tracked modern Python3 parser ──────────────────────────

(define-values (_py-parse py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "../grammars/python3/Python3Parser.g4"))

(define py-parse-file
  (let ([mod-path (*last-module-path*)])
    (λ (src)
      (define tks ((dynamic-require mod-path 'tokenize) src))
      (define parsed ((dynamic-require mod-path 'parse-file_input) tks 0))
      (and (pair? parsed) (cadr parsed)))))

(define (parse-ok? src)
  (and (py-parse-file src) #t))

;; ── Lowering/emit helpers ──────────────────────────────────────────

(define (lower-source src)
  (define cst (py-parse-file src))
  (check-true (any-tree? cst) (format "parse ~s" src))
  (lower-python cst py-tok-type py-tok-value))

(define (emit-source src)
  (emit-python (lower-source src)))

(define (first-fn u)
  (cond
    [(uir-fn? u) u]
    [(uir-block? u) (for/or ([s (in-list (uir-block-stmts u))]) (and (uir-fn? s) s))]
    [else #f]))

(define (first-method u)
  (define klass
    (for/or ([s (in-list (if (uir-block? u) (uir-block-stmts u) (list u)))])
      (and (uir-class? s) s)))
  (and klass (first (uir-class-methods klass))))

(define (params-of u)
  (cond
    [(first-fn u) => uir-fn-params]
    [(first-method u) => uir-method-params]
    [else '()]))

(define (param-names u)
  (map (λ (p) (uir-symbol-name (uir-typed-param-name p))) (params-of u)))

(define (param-kinds u)
  (map uir-typed-param-kind (params-of u)))

;; The lowered default values, so assertions can pin which parameter a default
;; belongs to and what it lowered to.
(define (param-defaults u)
  (map uir-typed-param-default (params-of u)))

;; Lower, emit and re-lower: the emitted signature must parse and keep the same
;; parameter names and kinds. This catches marker loss in either direction.
(define (check-round-trip src)
  (define out (emit-source src))
  (check-true (parse-ok? (string-append out "\n"))
              (format "emitted Python re-parses: ~s -> ~s" src out))
  (check-equal? (param-kinds (lower-source (string-append out "\n")))
                (param-kinds (lower-source src))
                (format "emitted parameters keep kinds: ~s -> ~s" src out))
  out)

;; ── Tests ───────────────────────────────────────────────────────────

(define lower-python-params-tests
  (test-suite
   "Python parameter star markers"

   ;; ── Bare `*`: keyword-only region ──────────────────────────────
   (test-case
    "a bare `*` makes the following parameters keyword-only"
    (let* ([src "def f(a, *, b):\n    pass\n\n"]
           [u (lower-source src)])
      (check-equal? (param-names u) '("a" "b") "both parameters survive")
      (check-equal? (param-kinds u) (list #f 'kw-only)
                    "only `b` is keyword-only")
      (check-equal? (emit-source src) "def f(a, *, b):\n    pass"
                    "the `*` separator is emitted")))

   (test-case
    "keyword-only parameters keep their defaults (catalogue reproducer)"
    (let* ([src "def f(a, *, b=None):\n    return b\n\n"]
           [u (lower-source src)])
      (check-equal? (param-names u) '("a" "b"))
      (check-equal? (param-kinds u) (list #f 'kw-only))
      (check-equal? (param-defaults u) (list #f (uir-null))
                    "the default belongs to the keyword-only parameter")
      (check-equal? (emit-source src) "def f(a, *, b = None):\n    return b")
      (check-equal? (check-round-trip src) "def f(a, *, b = None):\n    return b")))

   (test-case
    "defaults are not shifted across the star marker"
    (let* ([src "def f(a, b=1, *, c, d=2):\n    pass\n\n"]
           [u (lower-source src)])
      (check-equal? (param-names u) '("a" "b" "c" "d"))
      (check-equal? (param-kinds u) (list #f #f 'kw-only 'kw-only))
      (check-equal? (param-defaults u) (list #f (uir-number "1") #f (uir-number "2"))
                    "each default stays on its own parameter")
      (check-equal? (emit-source src) "def f(a, b = 1, *, c, d = 2):\n    pass")))

   (test-case
    "annotated positional and keyword-only defaults both survive"
    (let* ([src "def f(a: int = 1, *, b: str = \"x\") -> bool:\n    pass\n\n"]
           [u (lower-source src)])
      (check-equal? (param-kinds u) (list #f 'kw-only))
      (check-equal? (param-defaults u) (list (uir-number "1") (uir-string "x")))
      (check-equal? (emit-source src)
                    "def f(a: int = 1, *, b: str = \"x\") -> bool:\n    pass")))

   (test-case
    "a star-only signature emits the bare separator"
    (check-equal? (emit-source "def f(*, a, b=2):\n    pass\n\n")
                  "def f(*, a, b = 2):\n    pass"))

   ;; ── *args / **kwargs ──────────────────────────────────────────
   (test-case
    "`*args` is var-positional and opens the keyword-only region"
    (let* ([src "def f(a, *args, b, c=3, **kw):\n    pass\n\n"]
           [u (lower-source src)])
      (check-equal? (param-names u) '("a" "args" "b" "c" "kw"))
      (check-equal? (param-kinds u)
                    (list #f 'var-positional 'kw-only 'kw-only 'var-keyword))
      (check-equal? (param-defaults u) (list #f #f #f (uir-number "3") #f))
      (check-equal? (emit-source src)
                    "def f(a, *args, b, c = 3, **kw):\n    pass")
      (check-equal? (check-round-trip src)
                    "def f(a, *args, b, c = 3, **kw):\n    pass")))

   (test-case
    "`*args` and `**kwargs` alone keep their markers"
    (let ([u (lower-source "def f(*args):\n    pass\n\n")])
      (check-equal? (param-kinds u) (list 'var-positional))
      (check-equal? (emit-source "def f(*args):\n    pass\n\n")
                    "def f(*args):\n    pass"))
    (let ([u (lower-source "def f(**kw):\n    pass\n\n")])
      (check-equal? (param-kinds u) (list 'var-keyword))
      (check-equal? (emit-source "def f(**kw):\n    pass\n\n")
                    "def f(**kw):\n    pass"))
    (check-equal? (emit-source "def f(a, *args):\n    pass\n\n")
                  "def f(a, *args):\n    pass")
    (check-equal? (emit-source "def f(a, **kw):\n    pass\n\n")
                  "def f(a, **kw):\n    pass"))

   ;; ── Composition and no-marker regression ───────────────────────
   (test-case
    "positional-only and keyword-only markers compose"
    (let* ([src "def f(a, /, b, *, c, **kw):\n    pass\n\n"]
           [u (lower-source src)])
      (check-equal? (param-names u) '("a" "b" "c" "kw"))
      (check-equal? (param-kinds u) (list #f #f 'kw-only 'var-keyword))
      ;; The `/` marker has no UIR representation yet (declared loss); the
      ;; keyword-only marker does.
      (check-equal? (emit-source src) "def f(a, b, *, c, **kw):\n    pass")))

   (test-case
    "parameters without star markers stay positional-or-keyword"
    (let ([u (lower-source "def f(a, b=1):\n    pass\n\n")])
      (check-equal? (param-kinds u) (list #f #f))
      (check-equal? (emit-source "def f(a, b=1):\n    pass\n\n")
                    "def f(a, b = 1):\n    pass")))

   (test-case
    "class methods keep their parameter kinds"
    (let* ([src "class C:\n    def m(self, *, x=1):\n        pass\n\n"]
           [u (lower-source src)])
      (check-equal? (param-names u) '("self" "x"))
      (check-equal? (param-kinds u) (list #f 'kw-only))
      (check-equal? (emit-source src)
                    "class C:\n    def m(self, *, x = 1):\n        pass"))
    (let* ([src "class C:\n    def m(self, *args, **kw):\n        pass\n\n"]
           [u (lower-source src)])
      (check-equal? (param-kinds u) (list #f 'var-positional 'var-keyword))
      (check-equal? (emit-source src)
                    "class C:\n    def m(self, *args, **kw):\n        pass")))

   ;; ── Serialization ─────────────────────────────────────────────
   (test-case
    "parameter kind survives sexp and JSON serialization"
    (let ([p (uir-typed-param (uir-symbol "a") #f (uir-number "1") 'kw-only)])
      (check-equal? (uir-typed-param-kind p) 'kw-only)
      (check-equal? (sexp->uir (uir->sexp p)) p "kind survives the sexp round-trip")
      (check-equal? (json->uir (uir->json p)) p "kind survives the JSON round-trip"))
    (let* ([ps (list (uir-typed-param (uir-symbol "a") #f #f #f)
                     (uir-typed-param (uir-symbol "args") #f #f 'var-positional)
                     (uir-typed-param (uir-symbol "x") #f (uir-number "1") 'kw-only)
                     (uir-typed-param (uir-symbol "kw") #f #f 'var-keyword))]
           [fn (uir-fn (uir-symbol "f") ps (uir-block '()) #f #f)])
      (check-equal? (sexp->uir (uir->sexp fn)) fn "fn sexp round-trip")
      (check-equal? (json->uir (uir->json fn)) fn "fn JSON round-trip")))

   (test-case
    "an invalid parameter kind is rejected at the serialization boundary"
    (check-exn exn:fail?
               (λ () (uir->sexp (uir-typed-param (uir-symbol "a") #f #f 'bogus)))
               "the param-kind codec refuses a value outside the closed set"))

   (test-case
    "a parameter with no kind keeps the legacy three-element payload"
    (check-equal? (uir->sexp (uir-typed-param (uir-symbol "a") #f #f #f))
                  '(typed-param (symbol "a") #f #f)
                  "legacy payloads stay byte-compatible"))

   (test-case
    "parameter kind survives the versioned envelope and v1 payloads migrate"
    (let ([p (uir-typed-param (uir-symbol "a") #f (uir-number "1") 'var-keyword)])
      (check-equal? (versioned-sexp->uir (uir->versioned-sexp p)) p
                    "kind survives the current schema envelope"))
    ;; A version-1 payload (three-element typed-param) written before parameter
    ;; kinds existed is still readable; the identity migration only bumps the
    ;; envelope version.
    (let ([legacy `(uir 1 (typed-param (symbol "a") #f #f))])
      (check-equal? (uir-typed-param-kind (versioned-sexp->uir legacy)) #f
                    "v1 payloads read as positional-or-keyword")))))

;; ── Run and clean up ────────────────────────────────────────────────

(define failure-count (run-tests lower-python-params-tests))
(cleanup)
(exit (if (zero? failure-count) 0 1))
