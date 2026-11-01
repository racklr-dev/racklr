#lang racket

;; racklr/effects/pure-roots.rkt
;;
;; Predicates that classify a callee as PURE (no domain effect): front-end
;; constructs, side-effect-free builtins, protocol methods, exception
;; constructors, translation lookups, declarative constructors, config
;; methods, and string / collection methods. Every predicate is consulted by
;; `classify-nonedge` before a callee falls through to `unknown`.
;;
;; The names live in the injected token tables (library-tokens.rkt and
;; library-pure-names.rkt) and the host conventions in the injected dialect
;; (library-dialect.rkt); this module carries no language-specific literals.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/library-tokens
         racklr/effects/lexicon)

(provide
  synthetic-callee?
  runtime-primitive-callee?
  pure-builtin-callee?
  pure-stdlib-bare-callee?
  pure-native-builtin-callee?
  pure-dunder-callee?
  exception-constructor-callee?
  pure-op-callee?
  pure-constructor-callee?
  str-literal-method-callee?
  pure-method-callee?)

;; Operator / statement / special-form names the front ends encode as literal
;; `(uir-call (uir-symbol NAME) ...)`. These are language constructs, not
;; runtime-dispatched functions, so they perform no effect and are not
;; `unknown`. (Real calls lower to a uir-var callee in Python/TS and are
;; checked against known-names before reaching here.) Names come from the
;; injected dialect.
(define (synthetic-callee? c [tokens default-effect-tokens])
  (and (uir-symbol? c)
       (set-member? (tokens-synthetic-callees tokens) (uir-symbol-name c))))

;; Runtime primitives the emitters inject (a host names them in its dialect).
;; They are called as ordinary (uir-var) callees in user source, perform no
;; effect themselves, and their handler/thunk arguments are separate walk
;; boundaries.
(define (runtime-primitive-callee? c [tokens default-effect-tokens])
  (define nm (callee-name c))
  (and nm (set-member? (tokens-runtime-primitives tokens) (symbol->string nm))))

;; Side-effect-free builtins lower to uir-var callees; without this table a
;; call like `len(x)` / `dict(a=1)` / `isinstance(x, T)` is classified
;; `unknown`. Effectful builtins (open, input, print, eval, exec, compile,
;; breakpoint, help) are deliberately absent. Names live in the injected token
;; lexicon.
(define (pure-builtin-callee? c [tokens default-effect-tokens])
  (define nm (callee-name c))
  (and nm (set-member? (effect-tokens-pure-builtin-names tokens) (symbol->string nm))))

;; Directly-imported stdlib/ctypes/functools value constructors and decorators
;; that lower to a BARE callee (`from functools import partial`) and therefore
;; skip `pure-stdlib-callee?` (which only inspects member accesses). All build
;; or wrap values with no effect; a call to a C function pointer built by these
;; has a different callee and is classified separately. Names come from the
;; injected token lexicon.
(define (pure-stdlib-bare-callee? c [tokens default-effect-tokens])
  (define nm (callee-name c))
  (and nm (set-member? (effect-tokens-pure-stdlib-bare-names tokens)
                       (symbol->string nm))))

;; Host-NATIVE pure procedures (the compiler's own language: Racket standard-
;; library names, from the injected `pure-native-builtin-names` table). This is
;; a separate table from `pure-stdlib-bare-names` on purpose: receiver-type.rkt
;; reads that table as a *value-constructor* list for the Python/JS hosts, and a
;; native name such as Racket's `values` must not turn a Django `.values()` call
;; into a pure constructor.
(define (pure-native-builtin-callee? c [tokens default-effect-tokens])
  (define nm (callee-name c))
  (and nm
       (set-member? (effect-tokens-pure-native-builtin-names tokens)
                    (symbol->string nm))))

;; Protocol methods on an arbitrary receiver: `x.__init__()`, `x.__repr__()`,
;; ... implement the object protocol with no domain effect. A dunder call that
;; resolves as an intra-program edge (`self.__str__()`) never reaches this
;; tail. `__call__`, `__enter__`, `__exit__`, and `__getattr__` are
;; deliberately absent: they dispatch to arbitrary code. Names live in the
;; injected token lexicon.
;; injected token lexicon.
(define (pure-dunder-callee? c [tokens default-effect-tokens])
  (and (member-access-callee? c)
       (let* ([base (cond [(uir-get? c) (uir-get-base c)]
                          [(dot-call? c) (first (uir-call-args c))]
                          [else #f])]
              [op (effect-op-of-callee c)])
         (and op
              ;; `super().__init__()` targets a specific parent method that may
              ;; do real work; it stays a real edge or `unknown`, not swept pure.
              (not (super-call? base))
              (set-member? (effect-tokens-protocol-method-names tokens)
                           (symbol->string op))))))

;; Exception constructors (`ValueError`, `TypeError`, `ValidationError`, ...)
;; build an exception object to raise and perform no effect themselves; the
;; `raise` statement that throws it is already covered by the synthetic "raise"
;; callee. Detected by a bare callee name / final dotted field ending in one of
;; the host's exception-name suffixes. The suffixes are dialect data (Python's
;; PascalCase exceptions end in "Error"/"Exception"/"Warning" while effectful
;; callables are snake_case, so the case-sensitive suffix discriminates).
(define (exception-constructor-callee? c [tokens default-effect-tokens])
  (define op (effect-op-of-callee c))
  (and op
       (for/or ([suffix (in-set (tokens-exception-suffixes tokens))])
         (string-suffix? (symbol->string op) suffix))))

;; Translation-lookup calls are pure string-mapping functions with no effect
;; (no I/O, no network). They are usually imported as a bare alias, most often
;; `_`, so the name must be checked as a bare callee AND a dotted operation.
;; Names come from the injected token lexicon.
(define (pure-op-callee? c [tokens default-effect-tokens])
  (define op (effect-op-of-callee c))
  (and op (set-member? (effect-tokens-pure-op-names tokens) (symbol->string op))))

;; Framework / host exception classes that do NOT end in an exception suffix,
;; plus declarative framework constructors that are pure class instantiation
;; rather than effectful calls (model fields and query-expression objects).
;; Constructing one performs no effect. Names come from the injected token
;; lexicon.
(define (pure-constructor-callee? c [tokens default-effect-tokens])
  (define op (effect-op-of-callee c))
  (and op (set-member? (effect-tokens-pure-constructor-names tokens)
                       (symbol->string op))))

;; String-literal method calls (`"".join(x)`, `"{}".format(v)`) are pure string
;; operations; the uir-string base disambiguates them from `obj.join(...)`.
;; Names live in the injected token lexicon.
(define (str-literal-method-callee? c [tokens default-effect-tokens])
  (and (uir-get? c)
       (uir-string? (uir-get-base c))
       (let ([op (get-field-symbol (uir-get-field c))])
         (and op (set-member? (effect-tokens-string-method-names tokens)
                              (symbol->string op))))))

;; Pure method calls on an arbitrary receiver (list/dict/set/deque read or
;; in-place mutation, framework config/registration methods); mutating a local
;; collection is not a domain effect. The name list deliberately omits DB
;; receiver collisions (`get`, `update`, `filter`, `save`, `delete`, ...):
;; those classify `db` first, so anything still carrying one here is an
;; arbitrary object and stays `unknown`. Names live in the injected token
;; lexicon.
(define (pure-method-callee? c [tokens default-effect-tokens])
  (and (member-access-callee? c)
       (let ([op (effect-op-of-callee c)])
         (and op (set-member? (effect-tokens-pure-method-names tokens)
                              (symbol->string op))))))
