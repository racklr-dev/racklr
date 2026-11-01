#lang racket

;; tools/compiler-effects.rkt — the compiler as a UIR program, with effect rows.
;;
;; Prints the compiler-UIR report (modules, named functions, rows, unknown
;; counts, effect histogram, functions per component), then every compiler
;; module's named functions with their inferred effect row, then a one-line
;; summary.
;;
;; Usage (from the checkout root — PLTCOLLECTS must resolve this checkout):
;;
;;   PLTCOLLECTS="$(pwd):" racket tools/compiler-effects.rkt
;;   PLTCOLLECTS="$(pwd):" racket tools/compiler-effects.rkt --root /path/to/checkout
;;
;; Exit codes: 0 = report produced, 1 = failure (bad root, lowering, inference).

(require racket/cmdline
         racket/list
         racklr/compiler-uir)

(define root-override #f)

(command-line
 #:program "compiler-effects"
 #:once-each
 [("--root") dir
  "Use DIR as the repository root (default: auto-discover from the checkout)"
  (set! root-override dir)])

(define (main)
  (define programs (compiler-programs #:root root-override))
  (define rows (compiler-effect-rows #:programs programs))
  (define report (compiler-report #:programs programs))

  (displayln "compiler as UIR — functions and inferred effect rows")
  (displayln (compiler-report->string report))

  (for ([cp (in-list programs)])
    (define comp (car cp))
    (define names (program-named-functions (cdr cp)))
    (printf "~a (~a functions)\n" comp (length names))
    (for ([nm (in-list names)])
      (printf "  ~a  ~a\n"
              nm
              (row->string (hash-ref rows (cons comp nm) #f)))))

  (printf "compiler UIR: ~a modules, ~a functions, ~a with effect rows, ~a unknown\n"
          (hash-ref report 'modules 0)
          (hash-ref report 'functions 0)
          (hash-ref report 'functions-with-row 0)
          (hash-ref report 'unknown-function-count 0))
  (void))

(with-handlers ([exn:fail?
                 (λ (e)
                   (eprintf "ERROR: compiler-effects: ~a\n" (exn-message e))
                   (exit 1))])
  (main)
  (exit 0))
