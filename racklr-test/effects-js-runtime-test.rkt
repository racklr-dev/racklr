#lang racket

;; JS/TS effect-handler runtime enforcement (budget / must-not / ops / must).
;;
;; These tests emit JS via `emit-javascript`, append a small harness that
;; defines stub effect sources, and run the result through node to assert the
;; runtime guard actually throws (or not) against the operations a function
;; performs.

(require rackunit
         racket/file
         racket/system
         racklr/uir
         racklr/emit-javascript)

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (jsdot b f) (uir-call (sym "dot") (list b (sym f))))

(define (decl . stmts) (uir-block stmts))

(define (var-decl kind binding)
  (uir-call (sym kind) (list binding)))

(define (record . pairs)
  (uir-record (map (λ (p) (cons (uir-string (car p)) (cadr p))) pairs)))

;; (call (call (var withEffects) (list ROW)) (list FN)) — the UIR shape a
;; hand-written TS/JS `withEffects(row)(fn)` annotation lowers to.
(define (with-effects-wrapper row fn)
  (uir-call (uir-call (uir-var (sym "withEffects")) (list row))
            (list fn)))

(define (arrow body)
  (uir-call (sym "=>") (list (uir-list '()) body)))

(define (llm-op op) (uir-call (jsdot (v "llm") op) '()))
(define (db-op op) (uir-call (jsdot (v "db") op) '()))

(define (async-fn body)
  (uir-call (sym "async-fn") (list (uir-fn #f '() body #f #f))))

(define (await-promise)
  (uir-await (uir-call (jsdot (v "Promise") "resolve") '())))

;; Build a constraint record for one effect entry: {"modality","max","ops"}.
(define (entry #:modality [modality "may"] #:max [max #f] #:ops [ops #f] #:scope [scope #f])
  (define base (list (list "modality" (uir-string modality))))
  (define with-ops
    (if ops (cons (list "ops" (uir-list (map (λ (o) (uir-string o)) ops))) base) base))
  (define with-max
    (if max (cons (list "max" (uir-number (number->string max))) with-ops) with-ops))
  (define with-scope
    (if scope (cons (list "scope" (uir-string scope)) with-max) with-max))
  (apply record with-scope))

;; Build a full row record: ((name entry) ...).
(define (row . name-entries)
  (apply record name-entries))

;; Program: const f = withEffects(ROW)(() => { STMTS });
(define (guarded-program row-record stmts)
  (decl (var-decl "const"
                  (uir-set! (sym "f")
                            (with-effects-wrapper row-record
                                                  (arrow (uir-block stmts)))))))

;; Program: const f = withEffects(ROW)(async () => { STMTS });
(define (guarded-async-program row-record stmts)
  (decl (var-decl "const"
                  (uir-set! (sym "f")
                            (with-effects-wrapper row-record
                                                  (async-fn (uir-block stmts)))))))

;; ── node harness ────────────────────────────────────────────────────

(define (run-node js)
  (define script (make-temporary-file "racklr-effect-~a.js"))
  (define outfile (make-temporary-file "racklr-effect-out-~a.txt"))
  (display-to-file js script #:exists 'replace)
  (define ok? (system (format "node ~a > ~a 2>&1" script outfile)))
  (define output
    (with-handlers ([exn:fail:filesystem? (λ (e) "")])
      (file->string outfile)))
  (delete-file script)
  (delete-file outfile)
  (values ok? output))

(define (call-f-harness)
  (string-append
   "globalThis.llm = { query: function(){ return \"q\"; }, generate: function(){ return \"g\"; } };\n"
   "globalThis.db = { fetch: function(){ return \"d\"; }, get: function(){ return \"d\"; }, write: function(){ return \"w\"; } };\n"
   "try { f(); console.log(\"OK\"); } catch (e) { console.log(\"THREW:\" + e.message); }\n"))

(define (check-throws program message)
  (define-values (ok? out)
    (run-node (string-append (emit-javascript program) "\n" (call-f-harness))))
  (check-true ok? (format "node exited nonzero: ~a" out))
  (check-true (string-contains? out (format "THREW:~a" message))
              (format "expected THREW:~a, got: ~a" message out)))

;; A guard that rejects the row at construction throws while the emitted module
;; loads, so there is no THREW: line: the process must fail loudly.
(define (check-load-fails program message)
  (define-values (ok? out) (run-node (emit-javascript program)))
  (check-false ok? (format "node should exit nonzero: ~a" out))
  (check-true (string-contains? out message)
              (format "expected ~a, got: ~a" message out)))

(define (check-runs program)
  (define-values (ok? out)
    (run-node (string-append (emit-javascript program) "\n" (call-f-harness))))
  (check-true ok? (format "node exited nonzero: ~a" out))
  (check-true (string-contains? out "OK")
              (format "expected OK, got: ~a" out)))

(define (call-f-async-harness)
  (string-append
   "globalThis.llm = { query: function(){ return \"q\"; }, generate: function(){ return \"g\"; } };\n"
   "globalThis.db = { fetch: function(){ return \"d\"; }, get: function(){ return \"d\"; }, write: function(){ return \"w\"; } };\n"
   "(async function() { try { await f(); console.log(\"OK\"); } catch (e) { console.log(\"THREW:\" + e.message); } })();\n"))

(define (check-throws-async program message)
  (define-values (ok? out)
    (run-node (string-append (emit-javascript program) "\n" (call-f-async-harness))))
  (check-true ok? (format "node exited nonzero: ~a" out))
  (check-true (string-contains? out (format "THREW:~a" message))
              (format "expected THREW:~a, got: ~a" message out)))

(define (check-runs-async program)
  (define-values (ok? out)
    (run-node (string-append (emit-javascript program) "\n" (call-f-async-harness))))
  (check-true ok? (format "node exited nonzero: ~a" out))
  (check-true (string-contains? out "OK")
              (format "expected OK, got: ~a" out)))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-budget-exceeded)
  (check-throws (guarded-program (row (list "llm" (entry #:max 1)))
                                 (list (llm-op "query") (llm-op "query")))
                "effect budget exceeded: llm"))

(define (test-must-not-prohibited)
  (check-throws (guarded-program (row (list "db" (entry #:modality "must-not")))
                                 (list (db-op "write")))
                "effect prohibited: db"))

(define (test-absence-prohibits)
  (check-throws (guarded-program (row)
                                 (list (llm-op "query")))
                "effect prohibited: llm"))

(define (test-op-whitelist)
  (check-throws (guarded-program (row (list "llm" (entry #:ops '("query"))))
                                 (list (llm-op "generate")))
                "effect operation prohibited: llm.generate"))

(define (test-must-unmet)
  (check-throws (guarded-program (row (list "llm" (entry #:modality "must")))
                                 '())
                "effect obligation unmet: llm"))

(define (test-must-met)
  (check-runs (guarded-program (row (list "llm" (entry #:modality "must")))
                               (list (llm-op "query")))))

(define (test-must-ops-met)
  (check-runs (guarded-program (row (list "llm" (entry #:modality "must" #:ops '("query"))))
                               (list (llm-op "query")))))

(define (test-may-within-budget-runs)
  (check-runs (guarded-program (row (list "llm" (entry #:max 5)))
                               (list (llm-op "query")))))

;; ── scope enforcement ────────────────────────────────────────────────

(define (test-scope-reads-rejects-write)
  (check-throws (guarded-program (row (list "db" (entry #:scope "reads=search_index")))
                                 (list (db-op "write")))
                "effect scope violation: db.write outside reads=search_index"))

(define (test-scope-writes-rejects-read)
  (check-throws (guarded-program (row (list "db" (entry #:scope "writes=user")))
                                 (list (db-op "get")))
                "effect scope violation: db.get outside writes=user"))

;; The scope vocabulary comes from the classifier's lexicon: `fetchall` and
;; `bulk_update` are db verbs the old hand-written guard lists missed, so they
;; used to slip through a scope check unenforced.
(define (test-scope-enforces-lexicon-read-verb)
  (check-throws (guarded-program (row (list "db" (entry #:scope "writes=user")))
                                 (list (db-op "fetchall")))
                "effect scope violation: db.fetchall outside writes=user"))

(define (test-scope-enforces-lexicon-write-verb)
  (check-throws (guarded-program (row (list "db" (entry #:scope "reads=search_index")))
                                 (list (db-op "bulk_update")))
                "effect scope violation: db.bulk_update outside reads=search_index"))

(define (test-scope-reads-allows-read)
  (check-runs (guarded-program (row (list "db" (entry #:scope "reads=search_index")))
                               (list (db-op "get")))))

;; An unrecognized scope encoding has no enforcement branch; it must fail loudly
;; at guard construction rather than silently no-op.
(define (test-scope-unsupported-encoding-rejected)
  (check-load-fails (guarded-program (row (list "db" (entry #:scope "tenant_a")))
                                     (list (db-op "write")))
                    "unsupported effect scope encoding: tenant_a"))

(define (test-async-budget-after-await)
  ;; Effects performed after an await must still count against the ambient
  ;; frame: the context stays pushed until the returned promise settles.
  (check-throws-async
   (guarded-async-program (row (list "llm" (entry #:max 1)))
                          (list (await-promise) (llm-op "query") (llm-op "query")))
   "effect budget exceeded: llm"))

(define (test-async-must-met)
  (check-runs-async
   (guarded-async-program (row (list "llm" (entry #:modality "must")))
                          (list (await-promise) (llm-op "query")))))

(define (test-async-must-unmet)
  (check-throws-async
   (guarded-async-program (row (list "llm" (entry #:modality "must")))
                          (list (await-promise)))
   "effect obligation unmet: llm"))

;; ── effect handlers ──────────────────────────────────────────────────

;; Program: const f = () => llm.query("hi"); (inference guards f and the body
;; is instrumented with __perform, so a caller-installed handler intercepts it).
(define (performing-program)
  (decl (var-decl "const"
                  (uir-set! (sym "f")
                            (arrow (uir-call (jsdot (v "llm") "query")
                                             (list (uir-string "hi"))))))))

(define (run-handler handler-expr #:llm-impl [llm-impl "return \"real\";"])
  (define js
    (string-append
     (emit-javascript (performing-program))
     "\n"
     (format "globalThis.llm = { query: function(){ ~a } };\n" llm-impl)
     "try {\n"
     "  var _r = __handle(\"llm\", " handler-expr ", function() { return f(); });\n"
     "  console.log(\"RESULT:\" + _r);\n"
     "} catch (e) { console.log(\"THREW:\" + e.message); }\n"))
  (run-node js))

(define (check-handler-result handler-expr expected #:llm-impl [llm-impl "return \"real\";"])
  (define-values (ok? out) (run-handler handler-expr #:llm-impl llm-impl))
  (check-true ok? (format "node exited nonzero: ~a" out))
  (check-true (string-contains? out (format "RESULT:~a" expected))
              (format "expected RESULT:~a, got: ~a" expected out)))

(define (test-handler-supplies-value)
  ;; The handler short-circuits: llm.query is never invoked (the stub throws if
  ;; it is), so the caller-supplied value is returned instead of the effect.
  (check-handler-result
   "function(op, args, resume) { return \"mocked:\" + op; }"
   "mocked:query"
   #:llm-impl "throw new Error(\"real llm called\");"))

(define (test-handler-resume)
  ;; A handler may resume to the real (row-checked) effect.
  (check-handler-result
   "function(op, args, resume) { return \"wrapped:\" + resume(); }"
   "wrapped:real"))

(define (test-handler-receives-args)
  (check-handler-result
   "function(op, args, resume) { return args[0]; }"
   "hi"
   #:llm-impl "throw new Error(\"real llm called\");"))

(define (test-handler-innermost-wins)
  (define js
    (string-append
     (emit-javascript (performing-program))
     "\n"
     "globalThis.llm = { query: function(){ throw new Error(\"real llm called\"); } };\n"
     "try {\n"
     "  var _r = __handle(\"llm\", function(op, args, resume) { return \"outer\"; }, function() {\n"
     "    return __handle(\"llm\", function(op, args, resume) { return \"inner\"; }, function() { return f(); });\n"
     "  });\n"
     "  console.log(\"RESULT:\" + _r);\n"
     "} catch (e) { console.log(\"THREW:\" + e.message); }\n"))
  (define-values (ok? out) (run-node js))
  (check-true ok? (format "node exited nonzero: ~a" out))
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
  (test-async-budget-after-await)
  (test-async-must-met)
  (test-async-must-unmet)
  (test-handler-supplies-value)
  (test-handler-resume)
  (test-handler-receives-args)
  (test-handler-innermost-wins)
  (displayln "All JS effect-runtime enforcement tests passed."))
