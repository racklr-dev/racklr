#lang racket

(require racklr/uir
         racklr/effects/infer
         racklr/effects/python-annot)

(provide emit-python)

;; ── Emit helpers ───────────────────────────────────────────────────

(define (emit-body uir indent)
  (cond [(uir-block? uir)
         (string-join (map (λ (s) (emit-stmt s indent)) (uir-block-stmts uir)) "\n")]
        [else (emit-stmt uir indent)]))

(define (emit-stmt uir indent)
  (define spc (make-string (* indent 4) #\space))
  (cond [(uir-null? uir) (string-append spc "pass")]
        [(uir-return? uir)
         (string-append spc "return " (emit-expr (uir-return-value uir)))]
        [(uir-set!? uir)
          (string-append spc (emit-expr (uir-set!-name uir)) " = " (emit-expr (uir-set!-value uir)))]
        [(uir-ann-set!? uir)
          (let ([lhs-str (emit-expr (uir-ann-set!-lhs uir))]
                [type-str (emit-expr (uir-ann-set!-type uir))])
            (string-append spc lhs-str ": " type-str
                           (if (uir-ann-set!-value uir)
                               (string-append " = " (emit-expr (uir-ann-set!-value uir)))
                               "")))]
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
                         (string-append spc "del " (string-join (map emit-expr (uir-call-args uir)) ", "))]
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
  (define var-str (uir-symbol-name (uir-for-each-var uir)))
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
           (string-append ctx-str " as " (uir-symbol-name (uir-var-name (cadr item))))
           ctx-str))
     ", "))
  (string-append spc "with " items-str ":\n"
                 (emit-body (uir-with-body uir) (+ indent 1))))

;; ── Effect-row runtime guard (pure Python, no FFI) ─────────────────

(define (emit-effect-entry-dict e)
  ;; {"modality":"may","max":5,"scope":"...","ops":["query"]}
  (let* ([base (list (format "~s: ~s" "modality"
                             (symbol->string (uir-effect-entry-modality e))))]
         [with-budget (if (uir-effect-entry-budget e)
                          (cons (format "~s: ~a" "max" (uir-effect-entry-budget e)) base)
                          base)]
         [with-scope (if (uir-effect-entry-scope e)
                         (cons (format "~s: ~s" "scope" (uir-symbol-name (uir-effect-entry-scope e))) with-budget)
                         with-budget)]
         [with-ops (if (pair? (uir-effect-entry-ops e))
                       (cons (format "~s: [~a]" "ops"
                                     (string-join
                                      (map (λ (o) (format "~s" (uir-symbol-name o)))
                                           (uir-effect-entry-ops e))
                                      ", "))
                             with-scope)
                       with-scope)])
    (string-append "{" (string-join (reverse with-ops) ", ") "}")))

(define (emit-effect-row row)
  (string-append
   "{"
   (string-join
    (map (λ (e) (format "~s: ~a"
                        (uir-symbol-name (uir-effect-entry-name e))
                        (emit-effect-entry-dict e)))
         (uir-effect-row-entries row))
    ", ")
   "}"))

;; Wrap a directly-performed effect call in the runtime guard: the ambient
;; effect context (established by the __withEffects decorator) enforces
;; must-not / ops / budget against the specific operation, then runs the
;; original call.
(define (emit-perform name op args-str call-str)
  (format "__perform(~s, ~s, [~a], lambda: ~a)"
          (symbol->string name)
          (if op (symbol->string op) "")
          args-str
          call-str))

(define (emit-effects-guard fn indent)
  (define spc (make-string (* indent 4) #\space))
  (string-append spc "@__withEffects(" (emit-effect-row (uir-fn-effects fn)) ")"))

(define (effects-guard-preamble)
  (string-append
   "import inspect\n"
   "__effects_stack = []\n"
   "__handlers_stack = []\n"
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
   "            write_ops = (\"write\", \"update\", \"create\", \"delete\", \"save\", \"insert\", \"put\", \"remove\", \"set\", \"push\", \"clear\", \"drop\", \"mutate\", \"add\")\n"
   "            read_ops = (\"query\", \"fetch\", \"get\", \"read\", \"find\", \"filter\", \"all\", \"select\", \"search\", \"list\", \"exists\", \"load\", \"peek\", \"count\")\n"
   "            if scope.startswith(\"reads=\") and op in write_ops:\n"
   "                raise RuntimeError(\"effect scope violation: \" + name + \".\" + op + \" outside \" + scope)\n"
   "            if scope.startswith(\"writes=\") and op in read_ops:\n"
   "                raise RuntimeError(\"effect scope violation: \" + name + \".\" + op + \" outside \" + scope)\n"
   "        performed = ctx[\"performed\"]\n"
   "        performed.setdefault(name, {\"any\": 0})\n"
   "        if entry.get(\"max\") is not None and performed[name][\"any\"] >= entry[\"max\"]:\n"
   "            raise RuntimeError(\"effect budget exceeded: \" + name)\n"
   "        performed[name][\"any\"] += 1\n"
   "        performed[name][op] = performed[name].get(op, 0) + 1\n"
   "    return thunk()\n"
   "def __perform(name, op, args, thunk):\n"
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
   "    _valid_modalities = (\"may\", \"must\", \"must-not\")\n"
   "    for _name, _entry in row.items():\n"
   "        _modality = _entry.get(\"modality\", \"may\")\n"
   "        if _modality not in _valid_modalities:\n"
   "            raise ValueError(\"invalid effect modality: %r\" % (_modality,))\n"
   "        _budget = _entry.get(\"max\")\n"
   "        if _budget is not None and (not isinstance(_budget, int) or _budget < 0):\n"
   "            raise ValueError(\"invalid effect budget: %r\" % (_budget,))\n"
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
(define (async-fn-call? n)
  (and (uir-call? n)
       (uir-symbol? (uir-call-callee n))
       (string=? (uir-symbol-name (uir-call-callee n)) "async-fn")
       (= (length (uir-call-args n)) 1)
       (uir-fn? (first (uir-call-args n)))))

(define (emit-funcdef uir indent [async? #f])
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-fn-name uir))
  (define name-str (if name (uir-symbol-name name) "???"))
  (define params-str
    (string-join (map (λ (p) (cond [(uir-symbol? p) (uir-symbol-name p)]
                                    [(uir-typed-param? p)
                                     (let* ([pn (uir-symbol-name (uir-typed-param-name p))]
                                            [pt (uir-typed-param-type p)]
                                            [pd (uir-typed-param-default p)]
                                            [pt-str (if pt (string-append ": " (emit-expr pt)) "")]
                                            [pd-str (if pd (string-append " = " (emit-expr pd)) "")])
                                       (string-append pn pt-str pd-str))]
                                    [else (emit-expr p)]))
                      (uir-fn-params uir))
                 ", "))
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
    (if (null? supers) "" (string-append "(" (string-join (map emit-expr supers) ", ") ")")))
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
    (map (λ (m) (emit-method-node m name indent1)) methods))
  (define body-lines (append field-lines method-lines))
  (string-append
   spc "class " name super-str ":\n"
   (if (null? body-lines) (string-append spc1 "pass") (string-join body-lines "\n"))))

(define (emit-method uir class-name indent [async? #f])
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-symbol-name (uir-method-name uir)))
  (define self-param (car (uir-method-params uir)))
  (define rest-params (cdr (uir-method-params uir)))
  ;; Don't emit self as a separate param — it's always implicit
  (define params-str
    (string-join (map (λ (p) (cond [(uir-symbol? p) (uir-symbol-name p)]
                                    [(uir-typed-param? p)
                                     (let* ([pn (uir-symbol-name (uir-typed-param-name p))]
                                            [pt (uir-typed-param-type p)]
                                            [pd (uir-typed-param-default p)]
                                            [pt-str (if pt (string-append ": " (emit-expr pt)) "")]
                                            [pd-str (if pd (string-append " = " (emit-expr pd)) "")])
                                       (string-append pn pt-str pd-str))]
                                    [else (emit-expr p)]))
                      rest-params)
                 ", "))
  (string-append
   spc (if async? "async def " "def ") name "(self"
   (if (string=? params-str "") "" (string-append ", " params-str)) "):\n"
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
     (and (uir-method? inner) (emit-method inner class-name indent #t))]
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
  (define var-str (if (uir-symbol? loop-var) (uir-symbol-name loop-var) (emit-expr loop-var)))
  (define iter-str (emit-expr iterable))
  (define body-str
    (string-append result-str " for " var-str " in " iter-str
                   (if (uir-null? filter) "" (string-append " if " (emit-expr filter)))))
  (cond [(equal? callee-name "list-comp") (string-append "[" body-str "]")]
        [(equal? callee-name "set-comp") (string-append "{" body-str "}")]
        [(equal? callee-name "dict-comp")
         ;; For dict comp, result-expr should be a key:value pair representation
         (string-append "{" body-str "}")]
        [else (string-append "[" body-str "]")]))

;; ── Decorated emitter ──────────────────────────────────────────────

(define (emit-decorated uir indent)
  (define spc (make-string (* indent 4) #\space))
  ;; The `@effects({...})` decorator is a static annotation, not a runtime
  ;; decorator: its row is carried in the inner fn's effects field and rendered
  ;; as `@__withEffects(...)`. Emitting it would evaluate the undefined
  ;; `effects` name at import time.
  (define decos (filter (λ (d) (not (effects-decorator? d)))
                        (uir-decorated-decorators uir)))
  (define deco-lines
    (string-join
     (map (lambda (d)
            ;; d is a uir-call: emit as @name or @name(args)
            (if (null? (uir-call-args d))
                (string-append spc "@" (emit-expr (uir-call-callee d)))
                (string-append spc "@" (emit-expr d))))
          decos)
     "\n"))
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
(define (emit-decorated-method uir class-name indent)
  (define spc (make-string (* indent 4) #\space))
  (define decos (filter (λ (d) (not (effects-decorator? d)))
                        (uir-decorated-decorators uir)))
  (define deco-lines
    (string-join
     (map (lambda (d)
            (if (null? (uir-call-args d))
                (string-append spc "@" (emit-expr (uir-call-callee d)))
                (string-append spc "@" (emit-expr d))))
          decos)
     "\n"))
  (define inner (uir-decorated-inner uir))
  (define inner-str
    (cond
      [(uir-method? inner) (emit-method inner class-name indent)]
      [(async-fn-call? inner)
       (define m (first (uir-call-args inner)))
       (and (uir-method? m) (emit-method m class-name indent #t))]
      [else (emit-stmt inner indent)]))
  (string-append deco-lines "\n" inner-str))

;; ── Expression emitter ─────────────────────────────────────────────

;; Render one call argument, special-casing the (call "kwarg" (list name value))
;; keyword-argument encoding produced by lower-argument.
(define (emit-arg a)
  (if (and (uir-call? a)
           (uir-symbol? (uir-call-callee a))
           (equal? (uir-symbol-name (uir-call-callee a)) "kwarg")
           (= (length (uir-call-args a)) 2))
      (string-append (emit-expr (first (uir-call-args a)))
                     "=" (emit-expr (second (uir-call-args a))))
      (emit-expr a)))

(define (emit-expr uir)
  (cond [(uir-null? uir) "None"]
        [(uir-bool? uir) (if (uir-bool-value uir) "True" "False")]
        [(uir-number? uir) (uir-number-value uir)]
        [(uir-string? uir) (string-append "\"" (uir-string-value uir) "\"")]
        [(uir-fstring? uir) (string-append "f\"" (uir-fstring-value uir) "\"")]
        [(uir-list? uir)
         (string-append "[" (string-join (map emit-expr (uir-list-items uir)) ", ") "]")]
        [(uir-record? uir)
         (string-append "{" (string-join
                             (map (lambda (e)
                                    (string-append (emit-expr (car e)) ": " (emit-expr (cdr e))))
                                  (uir-record-entries uir))
                             ", ") "}")]
        [(uir-symbol? uir) (uir-symbol-name uir)]
        [(uir-var? uir) (uir-symbol-name (uir-var-name uir))]
        [(uir-call? uir)
          (let ([callee (uir-call-callee uir)])
            (if (and (uir-symbol? callee)
                     (regexp-match #rx"-comp$" (uir-symbol-name callee)))
                (emit-comp uir)
                (let ([args (uir-call-args uir)]
                      [name (and (uir-symbol? callee) (uir-symbol-name callee))])
                  (cond [(and name (equal? name "set"))
                         (string-append "{" (string-join (map emit-expr args) ", ") "}")]
                        [(and name (equal? name "tuple"))
                         (match (length args)
                           [0 "()"]
                           [1 (string-append "(" (emit-expr (first args)) ",)")]
                           [_ (string-append "(" (string-join (map emit-expr args) ", ") ")")])]
                         [(and name (= (length args) 2)
                               (member name '("is" "in" "<" ">" "==" "!=" ">=" "<="
                                              "+" "-" "*" "/" "//" "%" "**" "@"
                                              "and" "or" "|" "^" "&" "<<" ">>")))
                         (string-append (emit-expr (first args)) " " name " " (emit-expr (second args)))]
                        [(and name (= (length args) 1) (equal? name "not"))
                         (string-append "not " (emit-expr (first args)))]
                        [else
                         (let* ([args-str (string-join (map emit-arg args) ", ")]
                                [call-str (string-append (emit-expr callee)
                                                         "(" args-str ")")]
                                [eff (direct-effect-of-callee callee)])
                           (if eff
                               (emit-perform eff (effect-op-of-callee callee) args-str call-str)
                               call-str))]))))]
        [(uir-let? uir)
         (string-append "(" (uir-symbol-name (uir-let-name uir))
                        " := " (emit-expr (uir-let-value uir))
                        "; " (emit-expr (uir-let-body uir)) ")")]
        [(uir-block? uir)
         (string-join (map emit-expr (uir-block-stmts uir)) "\n")]
        [(uir-return? uir) (string-append "return " (emit-expr (uir-return-value uir)))]
        [(uir-if? uir)
          (string-append (emit-expr (uir-if-then uir))
                         " if " (emit-expr (uir-if-test uir))
                         " else " (emit-expr (uir-if-else uir)))]
        [(uir-fn? uir)
          (emit-lambda uir)]
        [(uir-await? uir) (emit-await uir)]
        [(uir-yield? uir) (emit-yield uir)]
        [(uir-get? uir)
          (let ([base-str (emit-expr (uir-get-base uir))]
                [field (uir-get-field uir)])
            (if (uir-string? field)
                (string-append base-str "." (uir-string-value field))
                (string-append base-str "[" (emit-expr field) "]")))]
        [(uir-paren? uir)
          (string-append "(" (emit-expr (uir-paren-inner uir)) ")")]
        [(uir-spread? uir)
          (string-append "*(" (emit-expr (uir-spread-expr uir)) ")")]
        [else (format "<?~a>" (uir-tag uir))]))

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

(define (emit-python uir)
  (define annotated (annotate-effects uir))
  (define body
    (cond [(uir-block? annotated)
           (emit-body annotated 0)]
          [else (emit-stmt annotated 0)]))
  (if (program-has-effects? annotated)
      (string-append (effects-guard-preamble) "\n\n" body)
      body))

(module+ main
  (displayln "racklr/emit-python — UIR → Python3 text emitter loaded."))
