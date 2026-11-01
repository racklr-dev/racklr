#lang racket

;; racklr/self/seed/compiler.rkt
;;
;; Facade for the P6.2 seed compiler: the self-image of the compiler as a UIR
;; program, plus the sample contract the end-to-end test drives it with.
;;
;; `kernel-compiler-uir` returns the UIR program built in program.rkt. Emitting
;; it, e.g. `(emit 'python (kernel-compiler-uir) #f)`, yields a runnable Python
;; compiler for the seed language; running that compiler on
;; `kernel-sample-source` prints `kernel-sample-compiled`, and running that
;; compiled program prints `kernel-sample-result`.
;;
;; Provides:
;;   kernel-compiler-uir       (-> uir?)   the seed program (definitions + main call)
;;   kernel-compiler-program   uir?        the same program value (not a thunk)
;;   seed-compiler-uir         (-> uir?)   alias kept for callers using the seed name
;;   kernel-compiler-language  string      human description of the seed language
;;   kernel-sample-source      string      the sample seed-language source
;;   kernel-sample-compiled    string      expected compiler stdout: a runnable Python program
;;   kernel-sample-result      string      expected stdout of running that compiled program
;;   seed-sample-source        string      alias for kernel-sample-source
;;   seed-sample-compiled      string      alias for kernel-sample-compiled
;;   seed-sample-result        string      alias for kernel-sample-result
;;   kernel-compiled-filename  string      filename the sample compiled program is written to

(require racket/contract
         racklr/uir
         "program.rkt")

(provide kernel-compiler-uir
         kernel-compiler-program
         seed-compiler-uir
         kernel-compiler-language
         kernel-sample-source
         kernel-sample-compiled
         kernel-sample-result
         seed-sample-source
         seed-sample-compiled
         seed-sample-result
         kernel-compiled-filename)

;; The seed compiler's program value: named function definitions, then the
;; `(main)` call that runs it.
(define/contract (kernel-compiler-uir) (-> uir?)
  seed-program)

;; The same program as a value, for callers that want to destructure it.
(define kernel-compiler-program seed-program)

;; Alias under the seed-* prefix.
(define (seed-compiler-uir) (kernel-compiler-uir))

;; The seed language: newline-separated top-level s-expressions. An atom is an
;; integer or a symbol; a form is '(' operator expression* ')'.
(define kernel-compiler-language
  "newline-separated s-expressions: atom = integer | symbol, form = (<op> <expr>*)")

;; Sample source: one top-level arithmetic form with a nested form.
(define kernel-sample-source "(+ 1 (* 2 3))")

;; What the emitted compiler prints for that source: a runnable Python 3
;; program, one print line per top-level form, arithmetic infix.
(define kernel-sample-compiled "print((1 + (2 * 3)))")

;; What running the compiled program prints.
(define kernel-sample-result "7")

;; seed-* aliases for the sample strings.
(define seed-sample-source kernel-sample-source)
(define seed-sample-compiled kernel-sample-compiled)
(define seed-sample-result kernel-sample-result)

;; The filename the end-to-end test writes the compiled program to.
(define kernel-compiled-filename "compiled_program.py")
