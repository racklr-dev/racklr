#lang racket

;; Real-world JavaScript front-end regression test: the constructs that the
;; express/axios corpus hardening (issue #122) added lowering support for.
;;
;; Each fixture is an authored, minimal program in the shape the corpus uses.
;; A fixture must parse, lower without a `?`-prefixed unhandled-node symbol,
;; emit JavaScript, and re-parse; the anchors prove the construct survived
;; lowering instead of being silently dropped (a degenerate emit can still
;; re-parse). Anchors are substrings of the emitted JavaScript.
;;
;; Run from racklr-test/:
;;
;;   cd racklr-test && PLTCOLLECTS="<checkout>:" racket js-real-world-test.rkt

(require rackunit
         rackunit/text-ui
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-javascript
         racklr/emit-javascript)

(define-runtime-path grammar-path "../grammars/javascript/JavaScriptParser.g4")

;; Generate the parser in a private scratch directory so the test leaves no
;; gen-tmp module behind and parallel runs cannot collide.
(define-values (js-parse tok-type tok-value)
  (let ([scratch (make-temporary-file "racklr-js-rwtest-~a" 'directory)])
    (dynamic-wind
      void
      (λ ()
        (parameterize ([current-directory scratch])
          (define-values (parse _tokenize tk-type tk-value)
            (gen-and-load (path->string grammar-path)))
          (cleanup)
          (values parse tk-type tk-value)))
      (λ ()
        (if (directory-exists? scratch)
            (delete-directory/files scratch)
            (void))))))

;; ── Fallback detection (a `?` symbol is an unhandled CST node) ──────

(define (fallback-symbols uir)
  (cond
    [(uir-symbol? uir)
     (define nm (uir-symbol-name uir))
     (if (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\?))
         (list nm)
         '())]
    [(struct? uir)
     (append* (for/list ([field (in-list (cdr (vector->list (struct->vector uir))))])
                (fallback-symbols field)))]
    [(pair? uir) (append (fallback-symbols (car uir))
                         (fallback-symbols (cdr uir)))]
    [else '()]))

;; ── Fixtures ────────────────────────────────────────────────────────

;; name, source, emitted-JS anchors
(define fixtures
  (list
   (list "class members: methods, fields, static members, blocks"
         "class Route {\n  constructor(path, options) {\n    this.path = path;\n    this.options = options;\n  }\n  handle(req, res) { return this.options.handler(req, res); }\n  toString() { return this.path; }\n  static from(path) { return new Route(path, {}); }\n  static { Route.count = 0; }\n  field = 1;\n  static staticField = 2;\n  get label() { return this.path; }\n}\n"
         '("class Route" "constructor(path" "handle(req, res)" "toString()"
           "static from(path)" "field = 1" "static staticField = 2"
           "get label()"))
   (list "optional chaining, optional call and nullish coalescing"
         "const value = config?.headers?.authorization;\nconst parsed = target?.(value);\nconst name = source ?? fallback ?? 'unknown';\nconst item = list?.[0];\n"
         '("config ?. headers ?. authorization" "target?.(" "??" "list?.[0]"))
   (list "logical assignment operators"
         "state.value ||= defaultValue;\nenabled &&= check();\ncache ??= create();\n"
         '("state.value ||= defaultValue" "enabled &&= check()" "cache ??= create()"))
   (list "nested template literals and template interpolation"
         "const nested = `outer ${`inner ${depth}`} done`;\nconst one = `a${b}c`;\nconst block = `x${ ({ a: 1 }) }y`;\n"
         '("`outer ${`inner ${depth}`} done`" "`a${b}c`" "`x${"))
   (list "object destructuring with rename, default, rest and computed keys"
         "const { a, b: renamed, c = 1, d: withDefault = 2, ...rest } = source;\nconst { 'quoted-key': quoted, 1: numeric, [computed]: dynamic } = source;\nconst nested = { outer: { inner } };\n"
         '("{ a, b: renamed, c = 1, d: withDefault = 2, ...rest }"
           "{\"quoted-key\": quoted, 1: numeric, [computed]: dynamic }"
           "outer: { inner }"))
   (list "array destructuring with holes, defaults, rest and nesting"
         "const [first, , third = 3, ...tail] = values;\nconst [[nested], { key }] = matrix;\n"
         '("[first, , third = 3, ...tail]" "[[nested], { key }]"))
   (list "for-of and for-in destructuring bindings"
         "for (const [key, value] of entries) { consume(key, value); }\nfor (const { name } of users) { emit(name); }\nfor (const [index] in sparse) { note(index); }\n"
         '("for (const [key, value] of entries)" "for (const {name} of users)"
           "for (const [index] in sparse)"))
   (list "statement if with return and throw branches keeps statement form"
         "function pick(value) {\n  if (!value) return null;\n  if (value.error) throw value.error;\n  if (value.ok) { return value.result; } else { return value; }\n}\n"
         '("if (! value) { return null; }" "if (value.error) { throw value.error; }"))
   (list "function expression parameters survive repetition groups"
         "runner.use(function (req, res, next) { next(); });\nconst handler = function named(a, b, c) { return a + b + c; };\n"
         '("function(req, res, next)" "function named(a, b, c)"))
   (list "keyword property names and private/import meta names"
         "promise.catch(onError).finally(onDone);\nmodule.new = 1;\nconst url = import.meta.url;\nif (#token in holder) { read(); }\n"
         '("promise.catch(onError).finally(onDone)" "module.new = 1"
           "import.meta.url" "#token in holder"))
   (list "anonymous class expression and class heritage"
         "const Base = class {};\nconst Derived = class extends Base { constructor() { super(); } };\n"
         '("class  {}" "class extends Base"))))

;; ── Round trip ──────────────────────────────────────────────────────

(define (round-trip name source anchors)
  (define cst (js-parse source))
  (check-true (and cst #t) (format "~a: parses" name))
  (define uir (and cst (lower-program cst tok-type tok-value)))
  (check-true (and uir #t) (format "~a: lowers" name))
  (define fallbacks (fallback-symbols uir))
  (check-equal? fallbacks '() (format "~a: no unhandled-node symbols" name))
  (define emitted (and uir (emit-javascript uir)))
  (check-true (and (string? emitted) (> (string-length emitted) 0))
              (format "~a: emits text" name))
  (check-true (and (js-parse emitted) #t)
              (format "~a: emitted JavaScript re-parses" name))
  (for ([anchor (in-list anchors)])
    (check-true (string-contains? emitted anchor)
                (format "~a: emitted text keeps ~s" name anchor))))

(test-suite
 "JavaScript real-world absorb"
 (for/list ([f (in-list fixtures)])
   (test-case (first f) (round-trip (first f) (second f) (third f)))))

(cleanup)
(displayln "All JavaScript real-world regression tests passed.")
