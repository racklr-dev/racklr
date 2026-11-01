#lang racket

;; Regression tests: emit-python must emit Python the tracked Python 3 grammar
;; can re-parse for real-world flask/requests constructs (ticket 01b06ee03a99).
;;
;; Every snippet below is a minimal reproducer distilled from the 83-file
;; flask + 37-file requests corpus scan (independent verifier harness,
;; /tmp/verify/, 2026-11-01). On the pre-fix emitter the re-parse cases failed
;; the parse -> lower -> emit -> re-parse round trip like this:
;;
;;   * prefixed string literals came out as `"b"hello""` / `"r"""doc` (the
;;     prefix and the original quotes were left in the value and the renderer
;;     wrapped the whole token in another pair of quotes);
;;   * a `"` inside a singly-quoted string was not escaped: `'"'` emitted as
;;     `"""`;
;;   * `is not` / `not in` comparisons were rendered as calls:
;;     `if is not(a, None):`, `if not in("a", d):`;
;;   * the effect-guard call wrapper rendered keyword arguments inside its
;;     list argument: `__perform("io", "warn", [None, category=DeprecationWarning],
;;     lambda: ...)`;
;;   * a `with ... as (host, port)` tuple target made emit-python raise
;;     `uir-var-name: contract violation` (raise, not bad text);
;;   * `async def` inside a class body emitted `async-fn(<?method>)` (a
;;     fallback placeholder that is not Python).
;;
;; The same corpus exposed lowering-side losses that stay syntactically valid
;; but change the program: chained assignment `a = b = 1` emitted `a = b`,
;; tuple unpack `a, b = c` emitted `a = c`, a decorator attribute chain
;; `@app.route(...)` emitted `@app(...)`, and call splats `*args` / `**kwargs`
;; were dropped (`f(a, **kwargs)` -> `f(a, kwargs)`, which CPython rejects).
;; Those cases assert emitted-text semantics, not byte identity: whitespace and
;; expression formatting may legitimately normalize (string literals are
;; emitted decoded and re-escaped).
;;
;; Each round-trip case checks: the snippet parses, lowers, emits a string, the
;; emitted text contains no fallback placeholder, and the emitted text
;; re-parses under the tracked grammar. Non-effect cases additionally assert
;; emission stability: emit(lower(parse(emit(src)))) reproduces the emitted
;; text (round-trip identity of the emitted text).
;;
;; Run: cd racklr-test && PLTCOLLECTS="<worktree>:" racket emit-python-roundtrip-test.rkt

(require rackunit
         rackunit/text-ui
         racket/list
         racket/string
         racklr/tree
         racklr/gen-test
         racklr/uir
         racklr/lower-python
         racklr/emit-python)

;; ── Load the tracked Python 3 parser ───────────────────────────────

(define-values (_py-parse py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "../grammars/python3/Python3Parser.g4"))

(define parser-module (*last-module-path*))
(define parse-file-input (dynamic-require parser-module 'parse-file_input))

(define (ensure-newline s)
  (if (regexp-match? #rx"\n$" s) s (string-append s "\n")))

;; parse-file_input returns (list #f cst) on success and #f on a grammar dead
;; end. It may raise (e.g. INDENT-ERROR); callers use `attempt` so one bad
;; snippet cannot abort the whole suite.
(define (parse-file src)
  (define parsed (parse-file-input (py-tokenize (ensure-newline src)) 0))
  (and (pair? parsed) (cadr parsed)))

;; (cons #t value) | (cons #f exn-message)
(define (attempt thunk)
  (with-handlers ([(lambda (e) (and (exn? e) (exn-message e)))
                   (lambda (e) (cons #f (exn-message e)))])
    (cons #t (thunk))))

(define (abbrev s [n 200])
  (if (> (string-length s) n) (string-append (substring s 0 n) "…") s))

;; ── Round-trip case harness ────────────────────────────────────────

(define failures 0)

(define (record-failure name msg)
  (set! failures (add1 failures))
  (eprintf "FAILURE ~a: ~a\n" name msg)
  #f)

;; Full parse -> lower -> emit stage; returns the emitted text or #f after
;; recording the failed stage.
(define (emit-text name src)
  (define parsed (attempt (λ () (parse-file src))))
  (cond
    [(not (car parsed)) (record-failure name (format "source snippet does not parse (~a)" (cdr parsed)))]
    [(not (any-tree? (cdr parsed))) (record-failure name (format "source snippet parsed to #f: ~s" src))]
    [else
     (define lowered (attempt (λ () (lower-python (cdr parsed) py-tok-type py-tok-value))))
     (cond
       [(not (car lowered)) (record-failure name (format "lower-python raised: ~a" (cdr lowered)))]
       [(not (uir? (cdr lowered))) (record-failure name "lower-python produced no UIR")]
       [else
        (define emitted (attempt (λ () (emit-python (cdr lowered)))))
        (cond
          [(not (car emitted)) (record-failure name (format "emit-python raised: ~a" (cdr emitted)))]
          [(not (string? (cdr emitted))) (record-failure name "emit-python returned a non-string")]
          [(equal? (cdr emitted) "") (record-failure name "emit-python returned the empty string")]
          [else
           (define text (cdr emitted))
           (define fallback-tag (regexp-match #rx"<(\\?[^>]*)>" text))
           (cond
             [fallback-tag
              (record-failure name (format "emitted text leaks fallback placeholder ~a: ~s"
                                           (cadr fallback-tag) (abbrev text)))]
             [else text])])])]))

;; `src` is the Python source; `stable?` opts into the stronger
;; emit(lower(parse(emitted))) = emitted check (skipped for snippets whose
;; emission carries an effect-guard preamble, whose re-emission is not
;; required to be byte-identical here). Returns the emitted text or #f.
(define (roundtrip name src [stable? #t])
  (define text (emit-text name src))
  (and text
       (let ([reparsed (attempt (λ () (parse-file text)))])
         (cond
           [(not (car reparsed))
            (record-failure name (format "emitted text made the tracked grammar raise: ~a; emitted ~s"
                                         (cdr reparsed) (abbrev text)))]
           [(not (any-tree? (cdr reparsed)))
            (record-failure name (format "emitted text does not re-parse (#f); emitted ~s"
                                         (abbrev text)))]
           [(not stable?) text]
           [else
            (define lowered2 (attempt (λ () (lower-python (cdr reparsed) py-tok-type py-tok-value))))
            (define emitted2 (and (car lowered2)
                                  (attempt (λ () (emit-python (cdr lowered2))))))
            (cond
              [(not (and emitted2 (car emitted2) (string? (cdr emitted2))))
               (record-failure name "emitted text lowered/emitted back to no string")]
              [(not (equal? (cdr emitted2) text))
               (record-failure name (format "emission is not stable; first ~s then ~s"
                                            (abbrev text) (abbrev (cdr emitted2))))]
              [else text])]))))

(define (check-roundtrip name src [stable? #t])
  (void (roundtrip name src stable?)))

;; Round trip plus emitted-text semantics: the emitted text must match `rx`.
(define (check-roundtrip-matching name src rx [stable? #t])
  (define text (roundtrip name src stable?))
  (when text
    (unless (regexp-match rx text)
      (record-failure name (format "emitted text does not match ~a: ~s" rx (abbrev text))))))

;; Round trip plus emitted-text semantics: the emitted text must contain the
;; literal `needle`.
(define (check-roundtrip-containing name src needle [stable? #t])
  (define text (roundtrip name src stable?))
  (when text
    (unless (string-contains? text needle)
      (record-failure name (format "emitted text does not keep ~s: ~s" needle (abbrev text))))))

;; ── Snippets distilled from the flask/requests corpus failures ─────

;; Prefixed string literals: flask `assert b"..." in response.data`,
;; requests docs/conf.py `u"Requests"`, requests _internal_utils.py
;; `rb"^[^:]+$"`, requests status_codes.py `r"""..."""` module docstring.
(check-roundtrip "bytes literal"
                 "x = b\"hello\"\n")
(check-roundtrip "unicode-prefixed literal"
                 "project = u\"Requests\"\n")
(check-roundtrip "raw-bytes regex literal"
                 "p = re.compile(rb\"^[^:]+$\")\n")
(check-roundtrip "raw triple-quoted module docstring"
                 "r\"\"\"raw doc\nline\n\"\"\"\nx = 1\n")

;; Quoting: requests cookies.py `value.replace('"', "")`.
(check-roundtrip "double quote inside a single-quoted string"
                 "x = value.replace('\"', \"\")\n")

;; Plain strings are emitted decoded and re-escaped rather than copied
;; verbatim; they must still round-trip and be stable (control case).
(check-roundtrip "plain string literal normalizes and round-trips"
                 "x = 'hi'\n")

;; Comparison operators: flask stores `if "methods" not in cls.__dict__`,
;; `if self._max_content_length is not None`.
(check-roundtrip "not-in condition"
                 "if \"a\" not in d:\n    x = 1\n")
(check-roundtrip "is-not condition"
                 "if a is not None:\n    x = 1\n")
(check-roundtrip "not-in expression"
                 "x = a not in d\n")
(check-roundtrip "is-not expression"
                 "x = a is not None\n")
(check-roundtrip "chained comparison"
                 "if a < b < c:\n    x = 1\n")

;; Keyword arguments in an effect-guarded call: flask `warnings.warn(...,
;; category=DeprecationWarning)`, `app.config.update(TESTING=True, ...)`.
;; The guard wrapper's list argument must not render `name=value` inside a
;; list literal. The emitted text also carries the `__perform` runtime
;; preamble, so the stability check is skipped here.
(check-roundtrip "effect call with a keyword argument"
                 "warnings.warn(None, category=DeprecationWarning)\n"
                 #f)
(check-roundtrip "effect call with two keyword arguments"
                 "app.config.update(TESTING=True, SECRET_KEY=\"k\")\n"
                 #f)

;; Statement forms with non-variable targets / wrappers: requests
;; tests/test_lowlevel.py `with server as (host, port):` (emit used to raise
;; uir-var-name contract violation); flask tests/test_async.py has `async def`
;; methods inside classes (emit used to fall through to `<?method>`).
(check-roundtrip "with-as tuple target"
                 "with server as (host, port):\n    pass\n")
(check-roundtrip "async method in a class body"
                 "class C:\n    async def m(self):\n        return 1\n")

;; Lowering-side representation losses (valid Python before the fix, wrong
;; program): chained assignment, tuple-unpack targets, decorator attribute
;; chains. Assert the emitted text keeps every target / the attribute chain,
;; not exact bytes.
(check-roundtrip-matching "chained assignment keeps every target"
                          "a = b = 1\n"
                          #px"a\\s*=\\s*b\\s*=\\s*1")
(check-roundtrip-matching "tuple unpack keeps every target"
                          "a, b = c\n"
                          #px"\\(?\\s*a\\s*,\\s*b\\s*\\)?\\s*=\\s*c")
(check-roundtrip-matching "decorator attribute chain is preserved"
                          "@app.route(\"/x\")\ndef f():\n    pass\n"
                          #rx"@app\\.route")

;; Call splat operators must survive emission: requests api.py
;; `session.request(method=method, url=url, **kwargs)` and flask
;; `super().__init__(*args, **kwargs)`. The tracked grammar accepts the mangled
;; `f(a, kwargs)` form, so re-parsing alone is not enough (CPython rejects it);
;; assert the operator spelling is preserved.
(define splat-tests
  (test-suite
   "call argument splat spellings survive emission"
   (test-case "**kwargs splat spelling survives emission"
              (check-roundtrip-containing "**kwargs splat" "f(a, **kwargs)\n" "**kwargs"))
   (test-case "*args splat spelling survives emission"
              (check-roundtrip-containing "*args splat" "f(a, *args)\n" "*args"))))

;; ── Run ────────────────────────────────────────────────────────────

(define failure-count (run-tests splat-tests))
(cleanup)
(unless (zero? failures)
  (set! failure-count (+ failure-count failures)))
(exit (if (zero? failure-count) 0 1))
