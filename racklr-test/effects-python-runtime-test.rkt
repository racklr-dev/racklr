#lang racket

;; Python effect-handler runtime enforcement (budget / must-not / ops / must).
;;
;; These tests emit Python via `emit-python`, append a harness that defines
;; stub effect sources, and run the result through `python3` to assert the
;; runtime guard actually raises (or not) against the operations a function
;; performs.

(require rackunit
         racket/file
         racket/system
         racklr/uir
         racklr/effects/python-annot
         racklr/emit-python)

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
;; Python attribute access `a.b` lowers to (uir-get a (uir-string "b")).
(define (pydot b f) (uir-get b (uir-string f)))

(define (str s) (uir-string s))
(define (num s) (uir-number s))

(define (effects-decorator . args)
  (uir-call (uir-var (uir-symbol "effects")) args))

(define (record . pairs)
  (uir-record (map (λ (p) (cons (str (car p)) (cadr p))) pairs)))

(define (prog . stmts) (uir-block stmts))

(define (fn name body #:params [params '()] #:effects [effects #f])
  (uir-fn (sym name) params body #f effects))

(define (llm-op op) (uir-call (pydot (v "llm") op) '()))
(define (db-op op) (uir-call (pydot (v "db") op) '()))
;; Await a directly-performed effect inside an async function.
(define (llm-op-await op) (uir-await (llm-op op)))

;; Build one effect entry: (name modality ops budget scope).
(define (entry name #:modality [modality 'may] #:max [max #f] #:ops [ops '()] #:scope [scope #f])
  (uir-effect-entry (sym name) modality (map sym ops) max (if scope (uir-symbol scope) #f)))

(define (row . entries)
  (uir-effect-row entries))

;; Program: @__withEffects(ROW)\ndef f(): STMTS
(define (guarded-program row-uir stmts)
  ;; An empty body must still emit a valid Python function (a `pass`).
  (prog (fn "f"
            (uir-block (if (null? stmts) (list (uir-null)) stmts))
            #:effects row-uir)))

;; ── python harness ──────────────────────────────────────────────────

(define (run-python py)
  (define script (make-temporary-file "racklr-effect-~a.py"))
  (define outfile (make-temporary-file "racklr-effect-out-~a.txt"))
  (display-to-file py script #:exists 'replace)
  (define ok? (system (format "python3 ~a > ~a 2>&1" script outfile)))
  (define output
    (with-handlers ([exn:fail:filesystem? (λ (e) "")])
      (file->string outfile)))
  (delete-file script)
  (delete-file outfile)
  (values ok? output))

(define (call-f-harness)
  (string-append
   "class _LLM:\n"
   "    def query(self): return \"q\"\n"
   "    def generate(self): return \"g\"\n"
   "llm = _LLM()\n"
   "class _DB:\n"
   "    def fetch(self): return \"d\"\n"
   "    def get(self): return \"d\"\n"
   "    def write(self): return \"w\"\n"
   "db = _DB()\n"
   "try:\n"
   "    f()\n"
   "    print(\"OK\")\n"
   "except Exception as e:\n"
   "    print(\"THREW:\" + str(e))\n"))

(define (check-throws program message)
  (define-values (ok? out)
    (run-python (string-append (emit-python program) "\n" (call-f-harness))))
  (check-true ok? (format "python3 exited nonzero: ~a" out))
  (check-true (string-contains? out (format "THREW:~a" message))
              (format "expected THREW:~a, got: ~a" message out)))

;; A guard that rejects the row at construction raises while the emitted module
;; is imported, so there is no THREW: line: the process must fail loudly.
(define (check-load-fails program message)
  (define-values (ok? out) (run-python (emit-python program)))
  (check-false ok? (format "python3 should exit nonzero: ~a" out))
  (check-true (string-contains? out message)
              (format "expected ~a, got: ~a" message out)))

(define (check-runs program)
  (define-values (ok? out)
    (run-python (string-append (emit-python program) "\n" (call-f-harness))))
  (check-true ok? (format "python3 exited nonzero: ~a" out))
  (check-true (string-contains? out "OK")
              (format "expected OK, got: ~a" out)))

;; Async guarded function: (call "async-fn" (list fn)) carrying a row.
(define (async-guarded-program row-uir stmts)
  (prog (uir-call (sym "async-fn")
                  (list (fn "f"
                            (uir-block (if (null? stmts) (list (uir-null)) stmts))
                            #:effects row-uir)))))

(define (call-f-async-harness)
  (string-append
   "import asyncio\n"
   "class _LLM:\n"
   "    async def query(self): return \"q\"\n"
   "    async def generate(self): return \"g\"\n"
   "llm = _LLM()\n"
   "class _DB:\n"
   "    async def fetch(self): return \"d\"\n"
   "    async def write(self): return \"w\"\n"
   "db = _DB()\n"
   "try:\n"
   "    asyncio.run(f())\n"
   "    print(\"OK\")\n"
   "except Exception as e:\n"
   "    print(\"THREW:\" + str(e))\n"))

(define (check-async-throws program message)
  (define-values (ok? out)
    (run-python (string-append (emit-python program) "\n" (call-f-async-harness))))
  (check-true ok? (format "python3 exited nonzero: ~a" out))
  (check-true (string-contains? out (format "THREW:~a" message))
              (format "expected THREW:~a, got: ~a" message out)))

(define (check-async-runs program)
  (define-values (ok? out)
    (run-python (string-append (emit-python program) "\n" (call-f-async-harness))))
  (check-true ok? (format "python3 exited nonzero: ~a" out))
  (check-true (string-contains? out "OK")
              (format "expected OK, got: ~a" out)))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-budget-exceeded)
  (check-throws (guarded-program (row (entry "llm" #:max 1))
                                 (list (llm-op "query") (llm-op "query")))
                "effect budget exceeded: llm"))

(define (test-must-not-prohibited)
  (check-throws (guarded-program (row (entry "db" #:modality 'must-not))
                                 (list (db-op "write")))
                "effect prohibited: db"))

(define (test-absence-prohibits)
  (check-throws (guarded-program (row)
                                 (list (llm-op "query")))
                "effect prohibited: llm"))

(define (test-op-whitelist)
  (check-throws (guarded-program (row (entry "llm" #:ops '("query")))
                                 (list (llm-op "generate")))
                "effect operation prohibited: llm.generate"))

(define (test-must-unmet)
  (check-throws (guarded-program (row (entry "llm" #:modality 'must))
                                 '())
                "effect obligation unmet: llm"))

(define (test-must-met)
  (check-runs (guarded-program (row (entry "llm" #:modality 'must))
                               (list (llm-op "query")))))

(define (test-must-ops-met)
  (check-runs (guarded-program (row (entry "llm" #:modality 'must #:ops '("query")))
                               (list (llm-op "query")))))

(define (test-may-within-budget-runs)
  (check-runs (guarded-program (row (entry "llm" #:max 5))
                               (list (llm-op "query")))))

;; ── scope enforcement ────────────────────────────────────────────────

(define (test-scope-reads-rejects-write)
  (check-throws (guarded-program (row (entry "db" #:scope "reads=search_index"))
                                 (list (db-op "write")))
                "effect scope violation: db.write outside reads=search_index"))

(define (test-scope-writes-rejects-read)
  (check-throws (guarded-program (row (entry "db" #:scope "writes=user"))
                                 (list (db-op "get")))
                "effect scope violation: db.get outside writes=user"))

;; The scope vocabulary comes from the classifier's lexicon: `fetchall` and
;; `bulk_update` are db verbs the old hand-written guard lists missed, so they
;; used to slip through a scope check unenforced.
(define (test-scope-enforces-lexicon-read-verb)
  (check-throws (guarded-program (row (entry "db" #:scope "writes=user"))
                                 (list (db-op "fetchall")))
                "effect scope violation: db.fetchall outside writes=user"))

(define (test-scope-enforces-lexicon-write-verb)
  (check-throws (guarded-program (row (entry "db" #:scope "reads=search_index"))
                                 (list (db-op "bulk_update")))
                "effect scope violation: db.bulk_update outside reads=search_index"))

(define (test-scope-reads-allows-read)
  (check-runs (guarded-program (row (entry "db" #:scope "reads=search_index"))
                               (list (db-op "get")))))

;; An unrecognized scope encoding has no enforcement branch; it must fail loudly
;; at guard construction rather than silently no-op.
(define (test-scope-unsupported-encoding-rejected)
  (check-load-fails (guarded-program (row (entry "db" #:scope "tenant_a"))
                                     (list (db-op "write")))
                    "unsupported effect scope encoding: 'tenant_a'"))

;; ── async enforcement ────────────────────────────────────────────────

(define (test-async-must-met)
  (check-async-runs (async-guarded-program (row (entry "llm" #:modality 'must))
                                           (list (llm-op-await "query")))))

(define (test-async-must-unmet)
  (check-async-throws (async-guarded-program (row (entry "llm" #:modality 'must))
                                             '())
                      "effect obligation unmet: llm"))

(define (test-async-budget-exceeded)
  (check-async-throws (async-guarded-program (row (entry "llm" #:max 1))
                                              (list (llm-op-await "query")
                                                    (llm-op-await "query")))
                      "effect budget exceeded: llm"))

(define (test-effects-decorator-not-reemitted)
  ;; A hand-written `@effects({...})` decorator must be stripped at emit time;
  ;; otherwise the emitted `@effects` raises NameError at import. The row it
  ;; declares still guards the function.
  (define deco
    (effects-decorator (record (list "llm" (record (list "max" (num "5")))))))
  (define inner
    (fn "f" (llm-op "query") #:effects (extract-effects-row deco)))
  (check-runs (prog (uir-decorated (list deco) inner))))

;; ── effect handlers ──────────────────────────────────────────────────

;; Program: def f(): return llm.query("hi") — inference guards f and the body
;; is instrumented with __perform, so a caller-installed handler intercepts it.
(define (performing-program)
  (prog (fn "f" (uir-return (uir-call (pydot (v "llm") "query") (list (str "hi")))))))

(define (run-handler handler-expr #:llm-impl [llm-impl "return \"real\""])
  (define py
    (string-append
     (emit-python (performing-program))
     "\n"
     "class _LLM:\n"
     (format "    def query(self, *a): ~a\n" llm-impl)
     "llm = _LLM()\n"
     "try:\n"
     "    _r = __handle(\"llm\", " handler-expr ", f)\n"
     "    print(\"RESULT:\" + str(_r))\n"
     "except Exception as e:\n"
     "    print(\"THREW:\" + str(e))\n"))
  (run-python py))

(define (check-handler-result handler-expr expected #:llm-impl [llm-impl "return \"real\""])
  (define-values (ok? out) (run-handler handler-expr #:llm-impl llm-impl))
  (check-true ok? (format "python3 exited nonzero: ~a" out))
  (check-true (string-contains? out (format "RESULT:~a" expected))
              (format "expected RESULT:~a, got: ~a" expected out)))

(define (test-handler-supplies-value)
  ;; The handler short-circuits: llm.query is never invoked (the stub raises if
  ;; it is), so the caller-supplied value is returned instead of the effect.
  (check-handler-result
   "lambda op, args, resume: \"mocked:\" + op"
   "mocked:query"
   #:llm-impl "raise RuntimeError(\"real llm called\")"))

(define (test-handler-resume)
  ;; A handler may resume to the real (row-checked) effect.
  (check-handler-result
   "lambda op, args, resume: \"wrapped:\" + resume()"
   "wrapped:real"))

(define (test-handler-receives-args)
  (check-handler-result
   "lambda op, args, resume: args[0]"
   "hi"
   #:llm-impl "raise RuntimeError(\"real llm called\")"))

(define (test-handler-innermost-wins)
  (define py
    (string-append
     (emit-python (performing-program))
     "\n"
     "class _LLM:\n"
     "    def query(self, *a): raise RuntimeError(\"real llm called\")\n"
     "llm = _LLM()\n"
     "try:\n"
     "    _r = __handle(\"llm\", lambda op, args, resume: \"outer\",\n"
     "                  lambda: __handle(\"llm\", lambda op, args, resume: \"inner\", f))\n"
     "    print(\"RESULT:\" + str(_r))\n"
     "except Exception as e:\n"
     "    print(\"THREW:\" + str(e))\n"))
  (define-values (ok? out) (run-python py))
  (check-true ok? (format "python3 exited nonzero: ~a" out))
  (check-true (string-contains? out "RESULT:inner")
              (format "expected RESULT:inner, got: ~a" out)))

(module+ main
  (test-budget-exceeded)
  (test-must-not-prohibited)
  (test-absence-prohibits)
  (test-op-whitelist)
  (test-must-unmet)
  (test-must-met)
  (test-must-ops-met)
  (test-may-within-budget-runs)
  (test-scope-reads-rejects-write)
  (test-scope-writes-rejects-read)
  (test-scope-enforces-lexicon-read-verb)
  (test-scope-enforces-lexicon-write-verb)
  (test-scope-reads-allows-read)
  (test-scope-unsupported-encoding-rejected)
  (test-async-must-met)
  (test-async-must-unmet)
  (test-async-budget-exceeded)
  (test-effects-decorator-not-reemitted)
  (test-handler-supplies-value)
  (test-handler-resume)
  (test-handler-receives-args)
  (test-handler-innermost-wins)
  (displayln "All Python effect-runtime enforcement tests passed."))
