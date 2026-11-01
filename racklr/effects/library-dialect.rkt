#lang racket

;; racklr/effects/library-dialect.rkt
;;
;; The DEFAULT (Python) host dialect as DATA: the naming and encoding
;; conventions the effect classifier needs beyond the library token tables.
;; This is the Python front end's language knowledge kept out of engine code;
;; a different host supplies its own `dialect` value through its lexicon (see
;; racklr/effects/lexicons/javascript.rkt).
;;
;; Naming conventions:
;;   self-names          — Python's implicit receiver parameters. `cls` is
;;                         included because classmethods name it so.
;;   exception-suffixes  — Python's PascalCase exception classes end in these;
;;                         constructing one is pure (the `raise` is the effect).
;; Encodings the Python / JS / TS / JSX / Clojure lowerers emit as literal
;; `(uir-call (uir-symbol NAME) ...)` nodes:
;;   synthetic-callees   — operators / statements / special forms. They are
;;                         language constructs, not runtime-dispatched calls.
;;   comprehension-callees — comprehension encodings (`list-comp`, ...).
;;   wrapper-callees     — named-function wrappers (`async-fn`, `gen-fn`).
;;   runtime-primitives  — primitives the emitters inject (`__handle`, ...).
;;   binding-callees     — JS binding forms (`var`, `let`, `const`).
;; Object-model conventions:
;;   init-method-name    — the constructor method a class call maps to.
;;   method-receiver-offset — how many implicit parameters a method takes
;;                         before its explicit ones (Python skips `self`).
;;   keyword-arg-callee  — the keyword-argument encoding's callee name.
;;   annotation-callee   — the effect-row annotation wrapper callee.

(require racklr/effects/lexicon-types)

(provide default-dialect)

(define default-dialect
  (dialect
   (set "self" "cls")
   ;; `cls` is the class receiver: `cls(...)` constructs the enclosing class.
   (set "cls")
   (set "Error" "Exception" "Warning")
   (set
    ;; JS/TS/JSX structural calls
    "dot" "index" "while" "dowhile" "with" "for" "forin" "forof" "switch"
    "break" "continue" "debugger" "label" "regex" "function" "async-fn"
    "gen-fn" "import" "export" "throw" "try" "=>" "var" "let" "const"
    "void" "typeof" "delete" "array-bind" "object-bind"
    ;; arithmetic / comparison / assignment / logical / unary
    "+" "-" "*" "/" "//" "%" "=" "==" "===" "!=" "!==" "<" ">" "<=" ">="
    "&&" "||" "**" "+=" "-=" "*=" "/=" "**=" "!" "~" "&" "|" "^"
    "<<" ">>" ">>>" "postfix++" "postfix--" "prefix++" "prefix--"
    ;; Python identity / membership / boolean operators (lower-comparison /
    ;; lower-binop emit these as uir-symbol callees)
    "is" "is not" "in" "not in" "and" "or"
    ;; `super` is the UIR encoding marker for a `super()` call: the Python
    ;; lowerer emits `(uir-call (uir-var "super") ...)` as the base of a
    ;; parent-method access, and the classifier matches that marker
    ;; structurally (`shape.rkt:super-call?`). It is a construct encoding, not
    ;; a runtime-dispatched function, so it is dialect data here.
    "super"
    ;; Python structural calls and comprehensions
    "not" "tuple" "set" "list" "assert" "raise" "del" "global" "nonlocal"
    "list-comp" "dict-comp" "set-comp" "gen-comp" "gen-exp" "kwarg"
    ;; Clojure special forms / data constructors
    "quote" "syntax-quote" "unquote" "unquote-splicing" "deref" "var"
    "vector" "hash-set" "fn*" "with-meta" "namespace" "host-expr")
   (set "list-comp" "dict-comp" "set-comp" "gen-exp")
   (set "async-fn" "gen-fn")
   ;; `__handle` is the emitter's effect-guard primitive. The `__withEffects`
   ;; row-annotation wrapper is only introduced by write-back (`annotate-effects`)
   ;; through `annotation-callee` below, so it is not a synthetic callee here.
   (set "__handle")
   (set "var" "let" "const")
   1
   "__init__"
   "kwarg"
   "__withEffects"))
