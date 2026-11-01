#lang racket

(require racklr/uir
         racklr/emit-contract
         racklr/emit-effects
         racklr/effects/guard-vocabulary
         racklr/effects/infer
         racklr/effects/python-annot)

(provide emit-python)

;; ── Emit helpers ───────────────────────────────────────────────────

(define (emit-body uir indent)
  (cond [(uir-block? uir)
         (string-join (map (λ (s) (emit-stmt s indent)) (uir-block-stmts uir)) "\n")]
        [else (emit-stmt uir indent)]))

;; ── Assignment targets ─────────────────────────────────────────────
;;
;; The Python front end encodes a destructuring target as `uir-list` whose
;; elements are uir-var / uir-get / nested uir-list / uir-spread; a chained
;; assignment is a nested `uir-set!` in the value position. A top-level target
;; list is rendered without parentheses (`a, b = ...`); a nested one keeps them
;; (`(a, b), c = ...`) because it is a tuple target, not an unpack list. A
;; one-element target list is a one-element tuple target and keeps its trailing
;; comma (`a, = ...`, `(a,), b = ...`), which distinguishes it from a plain
;; binding.
(define (target-str t [nested? #f])
  (cond
    [(uir-list? t)
     (define items (uir-list-items t))
     (define inner
       (string-append (string-join (map (λ (e) (target-str e #t)) items) ", ")
                      (if (= (length items) 1) "," "")))
     (if nested? (string-append "(" inner ")") inner)]
    [(uir-spread? t) (string-append "*" (target-str (uir-spread-expr t)))]
    [else (emit-expr t prec-primary)]))

(define (with-target-str t)
  (if (uir-list? t)
      (string-append "(" (target-str t) ")")
      (target-str t)))

;; Render an assignment statement, including any chain: `a = b = value`.
(define (emit-assign s)
  (define name-str (target-str (uir-set!-name s)))
  (define value (uir-set!-value s))
  (if (uir-set!? value)
      (string-append name-str " = " (emit-assign value))
      (string-append name-str " = " (emit-expr value 0))))

(define (emit-stmt uir indent)
  (define spc (make-string (* indent 4) #\space))
  (cond [(uir-null? uir) (string-append spc "pass")]
        [(uir-return? uir)
         (string-append spc "return " (emit-expr (uir-return-value uir)))]
        [(uir-set!? uir)
         (string-append spc (emit-assign uir))]
        [(uir-ann-set!? uir)
          (let ([lhs-str (target-str (uir-ann-set!-lhs uir))]
                [type-str (emit-expr (uir-ann-set!-type uir))])
            (string-append spc lhs-str ": " type-str
                           (if (uir-ann-set!-value uir)
                               (string-append " = " (emit-expr (uir-ann-set!-value uir) 0))
                               "")))]
        [(uir-type-alias? uir)
         (string-append spc (emit-type-alias uir))]
        [(uir-if? uir)
          ;; Emit as ternary if both branches are expressions (not blocks)
          (if (and (not (uir-block? (uir-if-then uir)))
                   (not (uir-null? (uir-if-then uir)))
                   (not (uir-block? (uir-if-else uir)))
                   (not (uir-null? (uir-if-else uir))))
              (string-append spc (emit-expr uir))
              (emit-if uir indent))]
        [(uir-for-each? uir)
         (emit-for-each uir indent)]
        [(uir-while? uir)
         (emit-while uir indent)]
        [(uir-try? uir)
          (emit-try uir indent)]
        [(uir-with? uir)
          (emit-with uir indent)]
        [(uir-decorated? uir)
         (emit-decorated uir indent)]
        [(uir-fn? uir)
         (if (uir-fn-name uir)
             (emit-funcdef uir indent)
             (string-append spc (emit-lambda uir)))]
        [(async-fn-call? uir)
         (emit-funcdef (first (uir-call-args uir)) indent #t)]
        [(uir-class? uir)
         (emit-classdef uir indent)]
        [(uir-import? uir)
         (emit-import uir spc)]
        [(uir-symbol? uir)
         (string-append spc (uir-symbol-name uir))]
        [(uir-var? uir)
         (string-append spc (uir-symbol-name (uir-var-name uir)))]
        [(uir-call? uir)
          (let ([callee (uir-call-callee uir)])
            (if (uir-symbol? callee)
                (let ([name (uir-symbol-name callee)])
                  (cond [(equal? name "del")
                         (string-append spc "del " (string-join (map (λ (a) (emit-expr a)) (uir-call-args uir)) ", "))]
                        [(equal? name "global")
                         (string-append spc "global " (string-join (map uir-symbol-name (uir-call-args uir)) ", "))]
                        [(equal? name "nonlocal")
                         (string-append spc "nonlocal " (string-join (map uir-symbol-name (uir-call-args uir)) ", "))]
                        [(equal? name "assert")
                         (let ([args (uir-call-args uir)])
                           (if (null? args)
                               (string-append spc "assert")
                               (string-append spc "assert " (emit-expr (car args))
                                              (if (>= (length args) 2)
                                                  (string-append ", " (emit-expr (cadr args)))
                                                  ""))))]
                        [(equal? name "raise")
                         (let ([args (uir-call-args uir)])
                           (if (null? args)
                               (string-append spc "raise")
                               (let ([expr-str (emit-expr (car args))])
                                 (if (>= (length args) 2)
                                     (string-append spc "raise " expr-str " from " (emit-expr (cadr args)))
                                     (string-append spc "raise " expr-str)))))]
                        [else (string-append spc (emit-expr uir))]))
                (string-append spc (emit-expr uir))))]
        [(uir-block? uir)
          (emit-body uir indent)]
        [(uir-yield? uir)
         (string-append spc (emit-yield uir))]
        [(uir-await? uir)
         (string-append spc (emit-await uir))]
        [(uir-list? uir)
         (string-append spc (emit-expr uir))]
        [(uir-record? uir)
          (string-append spc (emit-expr uir))]
        [(uir-get? uir)
          (string-append spc (emit-expr uir))]
        [(uir-paren? uir)
          (string-append spc (emit-expr uir))]
        [(uir-string? uir)
          (string-append spc (emit-expr uir))]
        [(uir-fstring? uir)
           (string-append spc (emit-expr uir))]
        [(uir-match? uir)
         (emit-match uir indent)]
        [else (format "~a# ?~a" spc (uir-tag uir))]))

(define (emit-if uir indent)
  (define spc (make-string (* indent 4) #\space))
  (string-append
   spc "if " (emit-expr (uir-if-test uir)) ":\n"
   (emit-body (uir-if-then uir) (+ indent 1))
   (if (uir-null? (uir-if-else uir))
       ""
       (string-append "\n" spc "else:\n" (emit-body (uir-if-else uir) (+ indent 1))))))

(define (emit-for-each uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define var-str (target-str (uir-for-each-var uir)))
  (define iter-str (emit-expr (uir-for-each-iterable uir)))
  (define body-str (emit-body (uir-for-each-body uir) (+ indent 1)))
  (define else-str
    (if (uir-null? (uir-for-each-else-body uir))
        ""
        (string-append "\n" spc "else:\n" (emit-body (uir-for-each-else-body uir) (+ indent 1)))))
  (string-append spc "for " var-str " in " iter-str ":\n" body-str else-str))

(define (emit-while uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define test-str (emit-expr (uir-while-test uir)))
  (define body-str (emit-body (uir-while-body uir) (+ indent 1)))
  (define else-str
    (if (uir-null? (uir-while-else-body uir))
        ""
        (string-append "\n" spc "else:\n" (emit-body (uir-while-else-body uir) (+ indent 1)))))
  (string-append spc "while " test-str ":\n" body-str else-str))

(define (emit-try uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define body-str (emit-body (uir-try-body uir) (+ indent 1)))
  (define catches-str
    (string-join
     (map (lambda (cat)
            (define exc-type (car cat))
            (define exc-name (cadr cat))
            (define handler-body (caddr cat))
            (define exc-str
              (if (uir-null? exc-type)
                  ""
                  (string-append " " (emit-expr exc-type))))
            (define as-str
              (if (uir-null? exc-name)
                  ""
                  (string-append " as " (uir-symbol-name (uir-var-name exc-name)))))
            (string-append spc "except" exc-str as-str ":\n"
                           (emit-body handler-body (+ indent 1))))
          (uir-try-catches uir))
     "\n"))
  (define else-str
    (if (uir-try-else-body uir)
        (string-append "\n" spc "else:\n" (emit-body (uir-try-else-body uir) (+ indent 1)))
        ""))
  (define finally-str
    (if (uir-try-finally-body uir)
        (string-append "\n" spc "finally:\n" (emit-body (uir-try-finally-body uir) (+ indent 1)))
        ""))
  (string-append spc "try:\n" body-str
                 (if (string=? catches-str "") "" (string-append "\n" catches-str))
                 else-str
                 finally-str))

(define (emit-with uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define items-str
    (string-join
     (for/list ([item (uir-with-items uir)])
       (define ctx-str (emit-expr (car item)))
       (if (cadr item)
           (string-append ctx-str " as " (with-target-str (cadr item)))
           ctx-str))
     ", "))
  (string-append spc "with " items-str ":\n"
                 (emit-body (uir-with-body uir) (+ indent 1))))

;; ── Effect-row runtime guard (pure Python, no FFI) ─────────────────
;;
;; The row's spelling is owned by the emit contract (racklr/emit-effects); this
;; section only places it: a `# @effects {...}` documented annotation plus the
;; live `@__withEffects` guard that enforces it at runtime.

;; Wrap a directly-performed effect call in the runtime guard: the ambient
;; effect context (established by the __withEffects decorator) enforces
;; must-not / ops / budget against the specific operation, then runs the
;; original call. Keyword arguments are passed as a dict so the positional
;; argument list stays a Python list.
(define (emit-perform name op args call-str)
  (format "__perform(~s, ~s, [~a], lambda: ~a~a)"
          (symbol->string name)
          (if op (symbol->string op) "")
          (emit-call-positional-args args)
          call-str
          (emit-call-kwargs-dict args)))

(define (emit-effects-guard fn indent)
  (define spc (make-string (* indent 4) #\space))
  (define row (uir-fn-effects fn))
  (define annotation (render-effect-annotation 'python row))
  (string-append
   (if annotation (string-append spc annotation "\n") "")
   spc "@__withEffects(" (render-effect-row 'python row) ")"))

;; Render a vocabulary list as a Python tuple literal. The scope read/write
;; verbs and the modality set come from the lexicon DATA module, so the guard
;; enforces exactly the vocabulary the classifier can emit.
(define (python-tuple strings)
  (string-append "(" (string-join (map (λ (v) (format "~s" v)) strings) ", ") ")"))

(define (effects-guard-preamble)
  (string-append
   "import inspect\n"
   "__effects_stack = []\n"
   "__handlers_stack = []\n"
   "__write_ops = " (python-tuple (scope-write-verbs)) "\n"
   "__read_ops = " (python-tuple (scope-read-verbs)) "\n"
   "def __effects_current():\n"
   "    return __effects_stack[-1] if __effects_stack else None\n"
   "def __current_handler(name):\n"
   "    for frame in reversed(__handlers_stack):\n"
   "        if frame[\"name\"] == name:\n"
   "            return frame[\"handler\"]\n"
   "    return None\n"
   "def __perform_checked(name, op, thunk):\n"
   "    ctx = __effects_current()\n"
   "    if ctx is not None:\n"
   "        entry = ctx[\"row\"].get(name)\n"
   "        if entry is None:\n"
   "            raise RuntimeError(\"effect prohibited: \" + name)\n"
   "        modality = entry.get(\"modality\", \"may\")\n"
   "        if modality == \"must-not\":\n"
   "            raise RuntimeError(\"effect prohibited: \" + name)\n"
   "        ops = entry.get(\"ops\", [])\n"
   "        if ops and op not in ops:\n"
   "            raise RuntimeError(\"effect operation prohibited: \" + name + \".\" + op)\n"
   "        scope = entry.get(\"scope\")\n"
   "        if scope:\n"
   "            if scope.startswith(\"reads=\") and op in __write_ops:\n"
   "                raise RuntimeError(\"effect scope violation: \" + name + \".\" + op + \" outside \" + scope)\n"
   "            if scope.startswith(\"writes=\") and op in __read_ops:\n"
   "                raise RuntimeError(\"effect scope violation: \" + name + \".\" + op + \" outside \" + scope)\n"
   "        performed = ctx[\"performed\"]\n"
   "        performed.setdefault(name, {\"any\": 0})\n"
   "        if entry.get(\"max\") is not None and performed[name][\"any\"] >= entry[\"max\"]:\n"
   "            raise RuntimeError(\"effect budget exceeded: \" + name)\n"
   "        performed[name][\"any\"] += 1\n"
   "        performed[name][op] = performed[name].get(op, 0) + 1\n"
   "    return thunk()\n"
   "def __perform(name, op, args, thunk, kwargs=None):\n"
   "    handler = __current_handler(name)\n"
   "    if handler is not None:\n"
   "        def resume():\n"
   "            return __perform_checked(name, op, thunk)\n"
   "        return handler(op, args, resume)\n"
   "    return __perform_checked(name, op, thunk)\n"
   "def __handle(name, handler, fn):\n"
   "    __handlers_stack.append({\"name\": name, \"handler\": handler})\n"
   "    try:\n"
   "        return fn()\n"
   "    finally:\n"
   "        __handlers_stack.pop()\n"
   "def __withEffects(row):\n"
   "    _valid_modalities = " (python-tuple (guard-modalities)) "\n"
   "    for _name, _entry in row.items():\n"
   "        _modality = _entry.get(\"modality\", \"may\")\n"
   "        if _modality not in _valid_modalities:\n"
   "            raise ValueError(\"invalid effect modality: %r\" % (_modality,))\n"
   "        _budget = _entry.get(\"max\")\n"
   "        if _budget is not None and (not isinstance(_budget, int) or _budget < 0):\n"
   "            raise ValueError(\"invalid effect budget: %r\" % (_budget,))\n"
   "        _scope = _entry.get(\"scope\")\n"
   "        if _scope is not None and (not isinstance(_scope, str) or not (_scope.startswith(\"reads=\") or _scope.startswith(\"writes=\"))):\n"
   "            raise ValueError(\"unsupported effect scope encoding: %r\" % (_scope,))\n"
   "    def _check(frame):\n"
   "        for _k, _entry in row.items():\n"
   "            if _entry.get(\"modality\", \"may\") == \"must\":\n"
   "                _perf = frame[\"performed\"].get(_k)\n"
   "                _ops = _entry.get(\"ops\", [])\n"
   "                if _ops:\n"
   "                    for _op in _ops:\n"
   "                        if not _perf or not (_perf.get(_op, 0) > 0):\n"
   "                            raise RuntimeError(\"effect obligation unmet: \" + _k + \".\" + _op)\n"
   "                elif not _perf or not (_perf.get(\"any\", 0) > 0):\n"
   "                    raise RuntimeError(\"effect obligation unmet: \" + _k)\n"
   "    def _decorator(func):\n"
   "        if inspect.iscoroutinefunction(func):\n"
   "            async def wrapped(*args, **kwargs):\n"
   "                _frame = {\"row\": row, \"performed\": {}}\n"
   "                __effects_stack.append(_frame)\n"
   "                try:\n"
   "                    _result = await func(*args, **kwargs)\n"
   "                    _check(_frame)\n"
   "                    return _result\n"
   "                finally:\n"
   "                    __effects_stack.pop()\n"
   "        else:\n"
   "            def wrapped(*args, **kwargs):\n"
   "                _frame = {\"row\": row, \"performed\": {}}\n"
   "                __effects_stack.append(_frame)\n"
   "                try:\n"
   "                    _result = func(*args, **kwargs)\n"
   "                    _check(_frame)\n"
   "                    return _result\n"
   "                finally:\n"
   "                    __effects_stack.pop()\n"
   "        wrapped.__effects__ = row\n"
   "        return wrapped\n"
   "    return _decorator"))

;; A call to the runtime handler installer `__handle(name, handler, fn)`: it
;; needs the preamble even when no direct effect call is visible in the program.
(define (handler-call? n)
  (and (uir-call? n)
       (let ([c (uir-call-callee n)])
         (cond
           [(uir-symbol? c) (string=? (uir-symbol-name c) "__handle")]
           [(uir-var? c) (string=? (uir-symbol-name (uir-var-name c)) "__handle")]
           [else #f]))))

(define (program-has-effects? uir)
  (cond
    [(struct? uir)
     (or (handler-call? uir)
         (and (uir-call? uir)
              (direct-effect-of-callee (uir-call-callee uir)))
         (and (uir-fn? uir) (uir-effect-row? (uir-fn-effects uir)))
         (for/or ([f (in-list (cdr (vector->list (struct->vector uir))))])
           (program-has-effects? f)))]
    [(pair? uir) (or (program-has-effects? (car uir)) (program-has-effects? (cdr uir)))]
    [else #f]))

;; The JS/TS async-fn wrapper: (call "async-fn" (list fn)). Python async defs
;; lower to the same shape so inference and the emitters share one convention.
;; Inside a class the wrapped node is a uir-method, at top level a uir-fn.
(define (async-fn-call? n)
  (and (uir-call? n)
       (uir-symbol? (uir-call-callee n))
       (string=? (uir-symbol-name (uir-call-callee n)) "async-fn")
       (= (length (uir-call-args n)) 1)
       (or (uir-fn? (first (uir-call-args n)))
           (uir-method? (first (uir-call-args n))))))

;; Render one parameter of a Python `def` signature. The `*` / `**` prefix
;; comes from the parameter kind: `var-positional` is `*args`,
;; `var-keyword` is `**kwargs`. Keyword-only parameters carry no prefix of
;; their own — emit-param-list inserts the bare `*` separator before them.
(define (emit-param p)
  (cond
    [(uir-symbol? p) (uir-symbol-name p)]
    [(uir-typed-param? p)
     (define pn (uir-symbol-name (uir-typed-param-name p)))
     (define pt (uir-typed-param-type p))
     (define pd (uir-typed-param-default p))
     (define pt-str (if pt (string-append ": " (emit-expr pt)) ""))
     (define pd-str (if pd (string-append " = " (emit-expr pd)) ""))
     (case (uir-typed-param-kind p)
       [(var-positional) (string-append "*" pn pt-str)]
       [(var-keyword) (string-append "**" pn pt-str)]
       [else (string-append pn pt-str pd-str)])]
    [else (emit-expr p)]))

;; Render a full Python parameter list (without the surrounding parentheses).
;; A bare `*` separator is emitted before the first keyword-only parameter
;; unless a var-positional parameter already opened the keyword-only region;
;; a `def f(**kw)` list needs no separator.
(define (emit-param-list params)
  (define pieces '())
  (define star-open? #f)
  (for ([p (in-list params)])
    (cond
      [(and (uir-typed-param? p) (eq? (uir-typed-param-kind p) 'kw-only))
       (unless star-open?
         (set! pieces (cons "*" pieces))
         (set! star-open? #t))
       (set! pieces (cons (emit-param p) pieces))]
      [(and (uir-typed-param? p) (eq? (uir-typed-param-kind p) 'var-positional))
       (set! pieces (cons (emit-param p) pieces))
       (set! star-open? #t)]
      [else (set! pieces (cons (emit-param p) pieces))]))
  (string-join (reverse pieces) ", "))

(define (emit-funcdef uir indent [async? #f])
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-fn-name uir))
  (define name-str (if name (uir-symbol-name name) "???"))
  (define params-str (emit-param-list (uir-fn-params uir)))
  (define return-str
    (let ([rt (uir-fn-return-type uir)])
      (if rt (string-append " -> " (emit-expr rt)) "")))
  (define guard-prefix
    (if (uir-effect-row? (uir-fn-effects uir))
        (string-append (emit-effects-guard uir indent) "\n")
        ""))
  (string-append
   guard-prefix
   spc (if async? "async def " "def ") name-str "(" params-str ")" return-str ":\n"
   (emit-body (uir-fn-body uir) (+ indent 1))))

(define (emit-classdef uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-symbol-name (uir-class-name uir)))
  (define supers (uir-class-super uir))
  (define super-str
    (if (null? supers) "" (string-append "(" (string-join (map (λ (a) (emit-expr a)) supers) ", ") ")")))
  (define fields (uir-class-fields uir))
  (define methods (uir-class-methods uir))
  (define indent1 (+ indent 1))
  (define spc1 (make-string (* indent1 4) #\space))
  (define field-lines
    (map (λ (f)
           (let* ([fn (uir-symbol-name (uir-field-name f))]
                  [ft (uir-field-type f)]
                  [fi (uir-field-init f)]
                  [type-part (if (uir-null? ft) "" (string-append ": " (emit-expr ft)))]
                  [init-part (if (uir-null? fi) "" (string-append " = " (emit-expr fi)))])
             (string-append spc1 fn type-part init-part)))
         fields))
  (define method-lines
    (filter values (map (λ (m) (emit-method-node m name indent1)) methods)))
  (define body-lines (append field-lines method-lines))
  (string-append
   spc "class " name super-str ":\n"
   (if (null? body-lines) (string-append spc1 "pass") (string-join body-lines "\n"))))

;; ── Methods ────────────────────────────────────────────────────────

;; Name of a parameter node (symbol or typed-param), or #f.
(define (param-name p)
  (cond [(uir-symbol? p) (uir-symbol-name p)]
        [(uir-typed-param? p) (uir-symbol-name (uir-typed-param-name p))]
        [else #f]))

;; The bound name of a decorator expression (@staticmethod, @x.setter, ...).
(define (decorator-name d)
  (define c (and (uir-call? d) (uir-call-callee d)))
  (cond [(uir-symbol? c) (uir-symbol-name c)]
        [(uir-var? c) (uir-symbol-name (uir-var-name c))]
        [else #f]))

;; How a method's first parameter is bound: a normal instance method gets an
;; implicit `self` receiver; `@staticmethod` has no receiver at all;
;; `@classmethod` keeps its written first parameter (`cls`).
(define (method-binding decos)
  (cond [(for/or ([d decos]) (equal? (decorator-name d) "staticmethod")) 'static]
        [(for/or ([d decos]) (equal? (decorator-name d) "classmethod")) 'class]
        [else 'instance]))

(define (emit-method uir class-name indent [async? #f] [decos '()])
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-symbol-name (uir-method-name uir)))
  (define params (uir-method-params uir))
  (define params-str
    (case (method-binding decos)
      [(static class) (emit-param-list params)]
      [else
       ;; The first parameter is the receiver; keep its name when the source
       ;; named it, and synthesize `self` only for a receiver-less method node.
       (define self-name (or (and (pair? params) (param-name (car params))) "self"))
       (define rest (if (pair? params) (cdr params) '()))
       (string-append self-name
                      (if (null? rest) "" (string-append ", " (emit-param-list rest))))]))
  (string-append
   spc (if async? "async def " "def ") name "(" params-str "):\n"
   (emit-body (uir-method-body uir) (+ indent 1))))

;; Render one class method-list node: a plain uir-method, a decorated method
;; ((uir-decorated decos method)), or an async def method ((call "async-fn"
;; (list method))). Anything else falls back to the generic stmt emitter.
(define (emit-method-node m class-name indent)
  (cond
    [(uir-method? m) (emit-method m class-name indent)]
    [(uir-decorated? m) (emit-decorated-method m class-name indent)]
    [(async-fn-call? m)
     (define inner (first (uir-call-args m)))
     (cond [(uir-method? inner) (emit-method inner class-name indent #t)]
           [(uir-decorated? inner) (emit-decorated-method inner class-name indent #t)]
           [else (emit-stmt inner indent)])]
    [else (emit-stmt m indent)]))

(define (emit-import uir spc)
  (define source (uir-symbol-name (uir-import-source uir)))
  (define names (uir-import-names uir))
  (if (null? names)
      (string-append spc "import " source)
      (string-append spc "from " source " import "
                     (string-join (map uir-symbol-name names) ", "))))

;; ── Async and yield emitters ───────────────────────────────────────

(define (emit-yield uir)
  (define val-str (emit-expr (uir-yield-value uir)))
  (if (uir-yield-from? uir)
      (string-append "yield from " val-str)
      (string-append "yield " val-str)))

(define (emit-await uir)
  (string-append "await " (emit-expr (uir-await-expr uir))))

(define (emit-lambda uir)
  (define params-str
    (string-join (map (lambda (p) (if (uir-symbol? p) (uir-symbol-name p) (emit-expr p)))
                      (uir-fn-params uir))
                 ", "))
  (string-append "lambda " params-str ": " (emit-expr (uir-fn-body uir))))

;; Emit a comprehension: (uir-call (uir-symbol "TYPE-comp") (list result var iterable filter))
(define (emit-comp uir)
  (define callee-name (uir-symbol-name (uir-call-callee uir)))
  (define args (uir-call-args uir))
  (define result (if (>= (length args) 1) (car args) (uir-null)))
  (define loop-var (if (>= (length args) 2) (cadr args) (uir-null)))
  (define iterable (if (>= (length args) 3) (caddr args) (uir-null)))
  (define filter (if (>= (length args) 4) (cadddr args) (uir-null)))
  (define result-str (emit-expr result))
  (define var-str
    (cond
      [(uir-symbol? loop-var) (uir-symbol-name loop-var)]
      [(uir-list? loop-var)
       (string-join (map (λ (v) (emit-expr v)) (uir-list-items loop-var)) ", ")]
      [else (emit-expr loop-var)]))
  (define iter-str (emit-expr iterable))
  (define body-str
    (string-append result-str " for " var-str " in " iter-str
                   (if (uir-null? filter) "" (string-append " if " (emit-expr filter)))))
  (cond [(equal? callee-name "list-comp") (string-append "[" body-str "]")]
        [(equal? callee-name "set-comp") (string-append "{" body-str "}")]
        [(equal? callee-name "dict-comp")
         ;; For dict comp, result-expr should be a key:value pair representation
         (string-append "{" body-str "}")]
        [(equal? callee-name "gen-exp") (string-append "(" body-str ")")]
        [else (string-append "[" body-str "]")]))

;; ── Decorated emitter ──────────────────────────────────────────────

;; Render one decorator line (`@name` for a bare decorator, `@expr(...)`
;; otherwise). A decorator expression is a uir-call by construction.
(define (emit-decorator d spc)
  (if (and (uir-call? d) (null? (uir-call-args d)))
      (string-append spc "@" (emit-expr (uir-call-callee d)))
      (string-append spc "@" (emit-expr d))))

(define (emit-decorated uir indent)
  (define spc (make-string (* indent 4) #\space))
  ;; The `@effects({...})` decorator is a static annotation, not a runtime
  ;; decorator: its row is carried in the inner fn's effects field and rendered
  ;; as `@__withEffects(...)`. Emitting it would evaluate the undefined
  ;; `effects` name at import time.
  (define decos (filter (λ (d) (not (effects-decorator? d)))
                        (uir-decorated-decorators uir)))
  (define deco-lines (string-join (map (λ (d) (emit-decorator d spc)) decos) "\n"))
  (define inner-str
    (let ([inner (uir-decorated-inner uir)])
      (cond [(uir-fn? inner) (emit-funcdef inner indent)]
            [(async-fn-call? inner)
             (emit-funcdef (first (uir-call-args inner)) indent #t)]
            [(uir-class? inner) (emit-classdef inner indent)]
            [else (emit-stmt inner indent)])))
  (string-append deco-lines "\n" inner-str))

;; A decorated method inside a class ((uir-decorated decos method)): render the
;; decorator lines and then the inner uir-method (async when wrapped).
;; `@staticmethod` / `@classmethod` change how the receiver parameter is bound;
;; `emit-method` reads that from the decorator list.
(define (emit-decorated-method uir class-name indent [async? #f])
  (define spc (make-string (* indent 4) #\space))
  (define decos (filter (λ (d) (not (effects-decorator? d)))
                        (uir-decorated-decorators uir)))
  (define deco-lines (string-join (map (λ (d) (emit-decorator d spc)) decos) "\n"))
  (define inner (uir-decorated-inner uir))
  (define-values (inner-node async-node?)
    (if (async-fn-call? inner)
        (values (first (uir-call-args inner)) #t)
        (values inner async?)))
  (define inner-str
    (cond
      [(uir-method? inner-node) (emit-method inner-node class-name indent async-node? decos)]
      [(uir-fn? inner-node) (emit-funcdef inner-node indent async-node?)]
      [else (emit-stmt inner-node indent)]))
  (string-append deco-lines "\n" inner-str))

;; ── Expression emitter ─────────────────────────────────────────────
;;
;; Expression emission is precedence-aware: a sub-expression is rendered at the
;; precedence level its parent context requires and parenthesized only when the
;; source tree would otherwise re-parse as a different tree. The levels are the
;; Python 3 operator precedence table, lowest first:
;;
;;   lambda (0) < conditional (1) < or (2) < and (3) < not (4) <
;;   comparisons (5) < | (6) < ^ (7) < & (8) < shifts (9) < + - (10) <
;;   * @ / // % (11) < unary + - ~ (12) < ** (13) < primary (14)

(define prec-lambda 0)
(define prec-conditional 1)
(define prec-primary 14)

(define comparison-ops '("<" ">" "==" "!=" "<=" ">=" "is" "is not" "in" "not in"))

;; `(list precedence associativity)` for an infix operator name, or #f. A list
;; rather than multiple values keeps it usable in a plain `and` guard.
(define (binary-op-info name)
  (cond
    [(equal? name "or") (list 2 'left)]
    [(equal? name "and") (list 3 'left)]
    [(member name comparison-ops) (list 5 'nonassoc)]
    [(equal? name "|") (list 6 'left)]
    [(equal? name "^") (list 7 'left)]
    [(equal? name "&") (list 8 'left)]
    [(or (equal? name "<<") (equal? name ">>")) (list 9 'left)]
    [(or (equal? name "+") (equal? name "-")) (list 10 'left)]
    [(member name '("*" "@" "/" "//" "%")) (list 11 'left)]
    [(equal? name "**") (list 13 'right)]
    [else #f]))

(define (binary-op? name)
  (and (binary-op-info name) #t))

(define (call-name n)
  (and (uir-call? n)
       (uir-symbol? (uir-call-callee n))
       (uir-symbol-name (uir-call-callee n))))

;; The precedence of a node as an expression. Nodes that need their own
;; parentheses (paren, walrus) count as primary: they always render wrapped.
(define (expr-prec n)
  (cond
    [(uir-if? n) prec-conditional]
    [(or (uir-fn? n) (uir-let? n) (uir-yield? n)) prec-lambda]
    [(uir-call? n)
     (define name (call-name n))
     (cond
       [(and name (regexp-match? #px"(?:-comp|-exp)$" name)) prec-primary]
       [(and name (equal? name "cmp-chain")) 5]
       [(and name (equal? name "not") (= (length (uir-call-args n)) 1)) 4]
       [(and name (member name '("+" "-" "~")) (= (length (uir-call-args n)) 1)) 12]
       [(and name (binary-op-info name))
        (car (binary-op-info name))]
       [else prec-primary])]
    [(uir-spread? n) 12]
    [(uir-await? n) prec-primary]
    [else prec-primary]))

;; Render `n` at context level `ctx`: wrap in parentheses only when the node's
;; precedence is below what the context requires. A primary node never needs
;; wrapping (paren/walrus already carry their own).
(define (emit-expr n [ctx prec-lambda])
  (define raw (emit-expr-raw n))
  (if (< (expr-prec n) ctx)
      (string-append "(" raw ")")
      raw))

;; ── Literal strings and f-strings ─────────────────────────────────
;;
;; Plain string literals are stored decoded (lowering processes the escapes);
;; the emitter re-encodes them as a double-quoted literal. A prefixed literal
;; (r/b/u prefix, or an r/b combination) is stored verbatim by the front end
;; because its escaping rules are prefix-dependent; it is emitted verbatim.
;;
;;   uir-string "a\"b"        → "a\"b"
;;   uir-string "r\"^\\d+\""  → r"^\d+"        (verbatim)
(define prefixed-string-rx #px"^[rRbBuU]{1,2}(\"\"\"|'''|\"|')")

;; True for a source literal stored verbatim: prefixed, with a matching
;; closing delimiter. A single-quoted literal may not carry a raw newline; a
;; triple-quoted one may (raw and bytes triple literals keep their embedded
;; newlines in the token text).
(define (verbatim-string-value? v)
  (and (regexp-match? prefixed-string-rx v)
       (let ([delim (match (regexp-match prefixed-string-rx v)
                      [(list _ d) d]
                      [_ "\"\"\""])])
         (and (>= (string-length v) (* 2 (string-length delim)))
              (string-suffix? v delim)
              (> (string-length v) (string-length delim))
              (or (= (string-length delim) 3)
                  (not (regexp-match? #rx"\n" v)))))))

;; Escape a decoded content string for a double-quoted Python literal. Control
;; characters use \xNN with exactly two hex digits, as Python requires.
(define (escape-python-string s)
  (apply string-append
         (for/list ([c (in-string s)])
           (case c
             [(#\\) "\\\\"]
             [(#\") "\\\""]
             [(#\newline) "\\n"]
             [(#\return) "\\r"]
             [(#\tab) "\\t"]
             [else
              (define code (char->integer c))
              (if (< code 32)
                  (let ([hex (string-upcase (number->string code 16))])
                    (format "\\x~a" (if (< (string-length hex) 2)
                                        (string-append "0" hex)
                                        hex)))
                  (string c))]))))

(define (emit-string-literal uir)
  (define v (uir-string-value uir))
  (if (verbatim-string-value? v)
      v
      (string-append "\"" (escape-python-string v) "\"")))

;; Pick the shortest delimiter that survives the f-string body: single-line
;; bodies use a single quote unless they contain it, multi-line bodies use a
;; triple quote that does not occur inside the body.
(define (emit-fstring-literal uir)
  (define body (uir-fstring-value uir))
  (define delim
    (cond
      [(and (not (regexp-match? #rx"\n" body))
            (not (string-contains? body "\""))
            (not (string-suffix? body "\\")))
       "\""]
      [(and (not (regexp-match? #rx"\n" body))
            (not (string-contains? body "'"))
            (not (string-suffix? body "\\")))
       "'"]
      [(and (not (string-contains? body "\"\"\""))
            (not (string-suffix? body "\"")))
       "\"\"\""]
      [(and (not (string-contains? body "'''"))
            (not (string-suffix? body "'")))
       "'''"]
      [else "\""]))
  (string-append "f" delim body delim))

;; ── Call arguments ─────────────────────────────────────────────────

(define (kwarg-name a)
  (and (uir-call? a)
       (call-name a)
       (equal? (call-name a) "kwarg")
       (= (length (uir-call-args a)) 2)
       (uir-call-args a)))

;; Render one call argument: keyword arguments use `name=value`, `*`/`**`
;; unpacking use the spread encodings the Python front end produces.
(define (emit-arg a)
  (cond
    [(kwarg-name a)
     (define args (kwarg-name a))
     (string-append (emit-expr (first args) 0) "=" (emit-expr (second args) 0))]
    [(and (call-name a) (equal? (call-name a) "kw-spread"))
     (string-append "**" (emit-expr (first (uir-call-args a)) 1))]
    [(uir-spread? a)
     (string-append "*" (emit-expr (uir-spread-expr a) 6))]
    [else (emit-expr a 0)]))

(define (emit-call-args args)
  (string-join (map emit-arg args) ", "))

;; The positional arguments of a call, as the runtime effect guard wants them:
;; keyword arguments and `**` expansion are passed separately so the list stays
;; a Python list. A `*` expansion is legal inside a list display.
(define (emit-call-positional-args args)
  (string-join (for/list ([a args]
                          #:unless (or (kwarg-name a)
                                       (kw-spread-call? a)))
                 (emit-arg a))
               ", "))

(define (kw-spread-call? a)
  (and (call-name a) (equal? (call-name a) "kw-spread")))

;; A dict display carrying the call's keyword arguments: `{"k": v, **rest}`.
(define (emit-call-kwargs-dict args)
  (define entries
    (for/list ([a args]
               #:when (or (kwarg-name a) (kw-spread-call? a)))
      (if (kwarg-name a)
          (kwarg-pair a)
          (string-append "**" (emit-expr (first (uir-call-args a)) 1)))))
  (if (null? entries)
      ""
      (string-append ", {" (string-join entries ", ") "}")))

(define (kwarg-pair a)
  (define args (kwarg-name a))
  (string-append (emit-expr (first args) 0) ": " (emit-expr (second args) 0)))

;; One dict-display entry. A `**` unpack entry is carried as a kw-spread call in
;; the key position with a null value (the same encoding call arguments use), so
;; it renders as `**expr` instead of `key: value`.
(define (emit-record-entry e)
  (if (kw-spread-call? (car e))
      (string-append "**" (emit-expr (first (uir-call-args (car e))) 1))
      (string-append (emit-expr (car e) 2) ": " (emit-expr (cdr e) 0))))

;; ── Operator expressions ───────────────────────────────────────────

(define (emit-binary-op name args)
  (define info (binary-op-info name))
  (define p (car info))
  (define assoc (cadr info))
  (cond
    [(= (length args) 2)
     (define lctx (if (eq? assoc 'right) (+ p 1) p))
     (define rctx (if (eq? assoc 'right) p (+ p 1)))
     (string-append (emit-expr (first args) lctx) " " name " " (emit-expr (second args) rctx))]
    [else
     ;; Not a shape the grammar produces, but keep it parseable rather than
     ;; emitting an invalid infix form.
     (string-append name "(" (emit-call-args args) ")")]))

;; Chained comparison: (cmp-chain operand op operand op operand ...).
(define (emit-cmp-chain args)
  (string-join
   (for/list ([a args] [i (in-naturals)])
     (if (even? i) (emit-expr a 6) (uir-symbol-name a)))
   " "))

;; Python keywords: a dotted attribute name must not be one of them (`x.class`
;; is invalid; `x["class"]` is the only spelling).
(define python-keywords
  '("False" "None" "True" "and" "as" "assert" "async" "await" "break" "class"
    "continue" "def" "del" "elif" "else" "except" "finally" "for" "from"
    "global" "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass"
    "raise" "return" "try" "while" "with" "yield"))

(define (python-identifier? s)
  (and (regexp-match? #rx"^[A-Za-z_][A-Za-z0-9_]*$" s)
       (not (member s python-keywords))
       #t))

;; A subscript field is a value expression, a tuple-index list (`a[i, j]`), or
;; a `slice` call (`a[i:j:k]`); rendering the last two with their source shape
;; keeps the emitted subscript semantically correct and re-absorbs identically.
(define (emit-subscript field)
  (cond
    [(uir-list? field)
     (string-join (map (λ (i) (emit-expr i 0)) (uir-list-items field)) ", ")]
    [(and (call-name field)
          (equal? (call-name field) "slice")
          (= (length (uir-call-args field)) 3))
     (define args (uir-call-args field))
     (define (part v) (if (uir-null? v) "" (emit-expr v 0)))
     (define step (list-ref args 2))
     (string-append (part (list-ref args 0)) ":"
                    (part (list-ref args 1))
                    (if (uir-null? step) "" (string-append ":" (part step))))]
    [else (emit-expr field 0)]))

;; Attribute access renders with a dotted name when the field is an identifier;
;; otherwise the subscript form is the only valid spelling.
(define (emit-get uir)
  (define base-str (emit-expr (uir-get-base uir) prec-primary))
  (define field (uir-get-field uir))
  (cond
    [(and (uir-string? field) (python-identifier? (uir-string-value field)))
     (string-append base-str "." (uir-string-value field))]
    [(uir-string? field)
     (string-append base-str "[" (emit-string-literal field) "]")]
    [else (string-append base-str "[" (emit-subscript field) "]")]))

;; ── Raw expression rendering ──────────────────────────────────────

(define (emit-expr-raw uir)
  (cond
    [(uir-null? uir) "None"]
    [(uir-bool? uir) (if (uir-bool-value uir) "True" "False")]
    [(uir-number? uir) (uir-number-value uir)]
    [(uir-string? uir) (emit-string-literal uir)]
    [(uir-fstring? uir) (emit-fstring-literal uir)]
    [(uir-list? uir)
     (string-append "[" (string-join (map (λ (i) (emit-expr i 0)) (uir-list-items uir)) ", ") "]")]
    [(uir-record? uir)
     (string-append "{" (string-join
                         (for/list ([e (in-list (uir-record-entries uir))])
                           (emit-record-entry e))
                         ", ") "}")]
    [(uir-symbol? uir) (uir-symbol-name uir)]
    [(uir-var? uir) (uir-symbol-name (uir-var-name uir))]
    [(uir-call? uir)
     (define callee (uir-call-callee uir))
     (define name (call-name uir))
     (define args (uir-call-args uir))
     (cond
       [(and name (regexp-match? #px"(?:-comp|-exp)$" name)) (emit-comp uir)]
       [(and name (equal? name "cmp-chain")) (emit-cmp-chain args)]
       [(and name (equal? name "not") (= (length args) 1))
        (string-append "not " (emit-expr (first args) 4))]
       [(and name (member name '("+" "-" "~")) (= (length args) 1))
        (string-append name (emit-expr (first args) 13))]
       [(and name (binary-op? name)) (emit-binary-op name args)]
       [(and name (equal? name "set") (pair? args))
        (string-append "{" (string-join (map (λ (a) (emit-expr a 0)) args) ", ") "}")]
       [(and name (equal? name "tuple"))
        (match (length args)
          [0 "()"]
          [1 (string-append "(" (emit-expr (first args) 0) ",)")]
          [_ (string-append "(" (string-join (map (λ (a) (emit-expr a 0)) args) ", ") ")")])]
       [(and name (equal? name "kw-spread"))
        (string-append "**" (emit-expr (first args) 1))]
       [else
        (define args-str (emit-call-args args))
        (define call-str (string-append (emit-expr callee prec-primary) "(" args-str ")"))
        (define eff (direct-effect-of-callee callee))
        (if eff
            (emit-perform eff (effect-op-of-callee callee) args call-str)
            call-str)])]
    [(uir-let? uir)
     (string-append "(" (uir-symbol-name (uir-let-name uir))
                    " := " (emit-expr (uir-let-value uir) 1)
                    "; " (emit-expr (uir-let-body uir) 0) ")")]
    [(uir-block? uir)
     (string-join (map (λ (s) (emit-expr s 0)) (uir-block-stmts uir)) "\n")]
    [(uir-return? uir) (string-append "return " (emit-expr (uir-return-value uir) 0))]
    [(uir-if? uir)
     (string-append (emit-expr (uir-if-then uir) 2)
                    " if " (emit-expr (uir-if-test uir) 2)
                    " else " (emit-expr (uir-if-else uir) 1))]
    [(uir-fn? uir) (emit-lambda uir)]
    [(uir-await? uir) (string-append "await " (emit-expr (uir-await-expr uir) prec-primary))]
    [(uir-yield? uir) (emit-yield uir)]
    [(uir-get? uir) (emit-get uir)]
    [(uir-paren? uir)
     ;; A walrus always renders parenthesized, so a source `(x := 1)` must not
     ;; gain a second pair of parentheses.
     (if (uir-walrus? (uir-paren-inner uir))
         (emit-expr (uir-paren-inner uir) 0)
         (string-append "(" (emit-expr (uir-paren-inner uir) 0) ")"))]
    [(uir-walrus? uir)
     (string-append "(" (emit-expr (uir-walrus-name uir) 0)
                    " := " (emit-expr (uir-walrus-value uir) 1) ")")]
    [(uir-type-alias? uir) (emit-type-alias uir)]
    [(uir-spread? uir)
     (string-append "*" (emit-expr (uir-spread-expr uir) 6))]
    [else (format "<?~a>" (uir-tag uir))]))

;; ── PEP 695 type alias emission ─────────────────────────────────────

(define (emit-type-alias uir)
  (define name (emit-expr (uir-type-alias-name uir)))
  (define tps (uir-type-alias-type-params uir))
  (define tp-str
    (if (null? tps)
        ""
        (string-append "[" (string-join (map (λ (a) (emit-expr a)) tps) ", ") "]")))
  (string-append "type " name tp-str " = " (emit-expr (uir-type-alias-value uir))))

;; ── Match/case emission ─────────────────────────────────────────────

(define (emit-match uir indent)
  (define spc (make-string (* indent 4) #\space))
  (string-append
   spc "match " (emit-expr (uir-match-subject uir)) ":\n"
   (string-join (map (λ (c) (emit-case c (+ indent 1))) (uir-match-cases uir)) "\n")))

(define (emit-case uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define pat-str (emit-pattern (uir-case-pattern uir)))
  (define guard-str (if (uir-case-guard uir)
                        (string-append " if " (emit-expr (uir-case-guard uir)))
                        ""))
  (define body-str (emit-body (uir-case-body uir) (+ indent 1)))
  (string-append spc "case " pat-str guard-str ":\n" body-str))

(define (emit-pattern pat)
  (cond [(uir-pat-literal? pat) (emit-pat-literal pat)]
        [(uir-pat-capture? pat) (uir-symbol-name (uir-pat-capture-name pat))]
        [(uir-pat-wildcard? pat) "_"]
        [(uir-pat-value? pat) (emit-expr (uir-pat-value-path pat))]
        [(uir-pat-or? pat) (string-join (map emit-pattern (uir-pat-or-alts pat)) " | ")]
        [(uir-pat-as? pat)
         (string-append (emit-pattern (uir-pat-as-pattern pat))
                        " as " (uir-symbol-name (uir-var-name (uir-pat-as-name pat))))]
        [(uir-pat-sequence? pat)
         (string-append "[" (emit-pat-seq-elements (uir-pat-sequence-elements pat)) "]")]
        [(uir-pat-star? pat)
         (string-append "*" (if (uir-pat-star-name pat)
                                (uir-symbol-name (uir-var-name (uir-pat-star-name pat)))
                                "_"))]
        [(uir-pat-mapping? pat) (emit-pat-mapping pat)]
        [(uir-pat-double-star? pat)
         (string-append "**" (uir-symbol-name (uir-pat-double-star-name pat)))]
        [(uir-pat-class? pat) (emit-pat-class pat)]
        [(uir-pat-group? pat)
         (string-append "(" (emit-pattern (uir-pat-group-pattern pat)) ")")]
        [else (format "<?pat ~a>" (uir-tag pat))]))

(define (emit-pat-literal pat)
  (define val (uir-pat-literal-value pat))
  (cond [(uir-number? val) (uir-number-value val)]
        [(uir-bool? val) (if (uir-bool-value val) "True" "False")]
        [(uir-symbol? val) (uir-symbol-name val)]
        [(uir-string? val) (emit-expr val)]
        [else (emit-expr val)]))

(define (emit-pat-seq-elements elems)
  (string-join
   (for/list ([e elems])
     (cond [(uir-pat-star? e) (emit-pattern e)]
           [else (emit-pattern e)]))
   ", "))

(define (emit-pat-mapping pat)
  (define entries (uir-pat-mapping-entries pat))
  (define rest (uir-pat-mapping-rest pat))
  (define entry-strs
    (for/list ([e entries])
      (string-append (emit-pattern (car e)) ": " (emit-pattern (cdr e)))))
  (define all-strs (if rest (append entry-strs (list (emit-pattern rest))) entry-strs))
  (string-append "{" (string-join all-strs ", ") "}"))

(define (emit-pat-class pat)
  (define cls-str (emit-expr (uir-pat-class-cls pat)))
  (define pos-strs (map emit-pattern (uir-pat-class-positional pat)))
  (define kw-strs
    (for/list ([k (uir-pat-class-keyword pat)])
      (string-append (uir-symbol-name (car k)) "=" (emit-pattern (cdr k)))))
  (string-append cls-str "(" (string-join (append pos-strs kw-strs) ", ") ")"))

;; ── Top-level ──────────────────────────────────────────────────────

;; `from __future__ import ...` must be the first statement after a module
;; docstring; the runtime guard preamble is therefore emitted after the leading
;; docstring / future-import prologue rather than before it.
(define (future-import? uir)
  (and (uir-import? uir)
       (equal? (uir-symbol-name (uir-import-source uir)) "__future__")))

(define (module-prologue-length stmts)
  ;; Optional leading docstring, then the run of future imports.
  (define after-doc
    (if (and (pair? stmts) (uir-string? (first stmts))) 1 0))
  (let loop ([i after-doc])
    (cond [(and (< i (length stmts)) (future-import? (list-ref stmts i)))
           (loop (add1 i))]
          [else i])))

;; A comment-only line can lower to a module-level uir-null. Such a no-op must
;; not be emitted (as `pass`) before the module docstring / future imports: it
;; would demote the docstring and push `from __future__` past a statement.
(define (trim-module-nulls stmts)
  (define (drop-leading lst)
    (cond [(and (pair? lst) (uir-null? (first lst))) (drop-leading (rest lst))]
          [else lst]))
  (define (drop-trailing lst)
    (reverse (drop-leading (reverse lst))))
  (drop-trailing (drop-leading stmts)))

;; Implements the emit contract (racklr/emit-contract): UIR plus an optional
;; explicit effect row in, Python 3 source out. An explicit row is authoritative
;; for the unit's entry function and is rendered as an `@__withEffects({...})`
;; guard. The other functions still get their row from the UIR or, when absent,
;; from inference, exactly as before the contract existed.
(define (emit-python uir [row #f])
  (define source (attach-root-row 'emit-python uir row))
  (define annotated
    (let ([a (annotate-effects source)])
      (if (uir-block? a)
          (uir-block (trim-module-nulls (uir-block-stmts a)))
          a)))
  (define guard? (program-has-effects? annotated))
  (cond
    [(and guard? (uir-block? annotated))
     (define stmts (uir-block-stmts annotated))
     (define n (module-prologue-length stmts))
     (define prologue (take stmts n))
     (define rest (drop stmts n))
     (string-append
      (if (null? prologue) "" (string-append (emit-body (uir-block prologue) 0) "\n\n"))
      (effects-guard-preamble)
      (if (null? rest) "" (string-append "\n\n" (emit-body (uir-block rest) 0))))]
    [guard?
     (string-append (effects-guard-preamble) "\n\n" (emit-stmt annotated 0))]
    [else
     ;; An empty module (empty file, only comments, or only `pass`) must still
     ;; emit a parseable statement.
     (define body (emit-body annotated 0))
     (if (string=? body "") "pass" body)]))

(module+ main
  (displayln "racklr/emit-python — UIR → Python3 text emitter loaded."))
