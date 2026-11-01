#lang racket

;; Python `@__withEffects` emission with inference + runtime instrumentation.
;;
;; `emit-python` must run effect inference on its input, so an unannotated
;; function that performs an effect (directly or transitively) is emitted with
;; the `@__withEffects(...)` guard and the runtime preamble, while a pure
;; function stays unannotated. Direct effect calls are instrumented with
;; `__perform`.

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/effects/python-annot
         racklr/emit-python)

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (str s) (uir-string s))
(define (num s) (uir-number s))

(define (effects-decorator . args)
  (uir-call (uir-var (uir-symbol "effects")) args))

(define (record . pairs)
  (uir-record (map (λ (p) (cons (str (car p)) (cadr p))) pairs)))

(define (fn name body #:params [params '()])
  (uir-fn (if name (uir-symbol name) #f) params body #f #f))

;; Async function as a top-level statement: (call "async-fn" (list fn)).
(define (async-fn-stmt name body)
  (uir-call (sym "async-fn")
            (list (uir-fn (uir-symbol name) '() body #f #f))))

(define (prog . stmts)
  (uir-block stmts))

(define (count-substr s sub)
  (length (regexp-match* (regexp-quote sub) s)))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-no-effects-no-preamble)
  (define out (emit-python (prog (fn "plain" (uir-null)))))
  (check-false (string-contains? out "__withEffects")
               "a pure function must not emit the guard"))

(define (test-inferred-effects-emit-guard)
  (define p (prog (fn "summarize"
                      (uir-call (dot (v "llm") "query")
                                (list (uir-string "hi"))))))
  (define out (emit-python p))
  (check-true (string-contains? out "def __withEffects")
              "preamble emitted for inferred effects")
  (check-true (string-contains? out "@__withEffects(")
              "effectful function guarded")
  (check-true (string-contains? out "\"llm\"")
              "inferred llm row emitted"))

(define (test-transitive-inferred-effects)
  ;; main calls summarize, which performs llm; both functions are guarded.
  (define summarize
    (fn "summarize" (uir-call (dot (v "llm") "query")
                              (list (uir-string "hi")))))
  (define p (prog summarize
                  (fn "main" (uir-call (v "summarize") '()))))
  (define out (emit-python p))
  (check-equal? (count-substr out "@__withEffects(") 2
                "both summarize and main carry the guard"))

(define (test-direct-effect-call-instrumented)
  (define out (emit-python (prog (fn "summarize"
                                     (uir-call (dot (v "llm") "query")
                                               (list (uir-string "hi")))))))
  (check-true (string-contains? out "__perform(\"llm\", \"query\", [\"hi\"], lambda:")
              "direct llm.query call is instrumented"))

(define (test-preamble-defines-perform)
  (define out (emit-python (prog (fn "summarize"
                                     (uir-call (dot (v "llm") "query")
                                               (list (uir-string "hi")))))))
  (check-true (string-contains? out "def __perform(")
              "preamble defines __perform")
  (check-true (string-contains? out "__effects_stack")
              "preamble defines the effect-context stack"))

(define (test-top-level-effect-call-emits-preamble)
  ;; A direct effect call outside any function has no effect row, but it is
  ;; instrumented, so the preamble (with __perform) must still be emitted.
  (define out (emit-python (prog (uir-call (dot (v "llm") "query")
                                           (list (uir-string "hi"))))))
  (check-true (string-contains? out "def __perform(")
              "top-level effect call still emits the runtime preamble")
  (check-true (string-contains? out "__perform(\"llm\", \"query\",")
              "top-level effect call is instrumented"))

(define (test-async-inferred-effects-emit-guard)
  ;; An unannotated async function that performs an effect must be emitted as
  ;; `async def` and guarded, so its coroutine is awaited before `must` checks.
  (define out (emit-python (prog (async-fn-stmt "summarize"
                                                (uir-call (dot (v "llm") "query")
                                                          (list (uir-string "hi")))))))
  (check-true (string-contains? out "async def summarize")
              "async function keeps the async keyword")
  (check-true (string-contains? out "@__withEffects(")
              "async effectful function is guarded"))

(define (test-async-preamble-awaits-coroutine)
  (define out (emit-python (prog (async-fn-stmt "summarize"
                                                (uir-call (dot (v "llm") "query")
                                                          (list (uir-string "hi")))))))
  (check-true (string-contains? out "iscoroutinefunction")
              "preamble detects coroutine functions")
  (check-true (string-contains? out "async def wrapped")
              "preamble defines an async wrapper for async functions"))

(define (test-effects-decorator-stripped)
  ;; A hand-written `@effects({...})` annotation decorator is not a runtime
  ;; decorator; it must not be re-emitted (executing `@effects` would raise
  ;; NameError). The row it declares still guards the function via
  ;; `@__withEffects`.
  (define deco
    (effects-decorator (record (list "llm" (record (list "max" (num "5")))))))
  (define inner
    (uir-fn (sym "summarize") '()
            (uir-call (dot (v "llm") "query") (list (str "hi")))
            #f
            (extract-effects-row deco)))
  (define out (emit-python (prog (uir-decorated (list deco) inner))))
  ;; The row is documented as `# @effects {...}`, but the *decorator* must not
  ;; be re-emitted: executing an undefined `effects` would raise NameError.
  (check-false (regexp-match? #rx"(?m:^[ \t]*@effects\\()" out)
               "@effects annotation decorator must be stripped")
  (check-true (string-contains? out "@__withEffects(")
               "the declared row still guards the function")
  (check-true (string-contains? out "def summarize")
               "the function is emitted"))

(module+ main
  (test-no-effects-no-preamble)
  (test-inferred-effects-emit-guard)
  (test-transitive-inferred-effects)
  (test-direct-effect-call-instrumented)
  (test-preamble-defines-perform)
  (test-top-level-effect-call-emits-preamble)
  (test-async-inferred-effects-emit-guard)
  (test-async-preamble-awaits-coroutine)
  (test-effects-decorator-stripped)
  (displayln "All python-emit effect-guard tests passed."))
