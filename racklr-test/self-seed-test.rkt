#lang racket

;; racklr-test/self-seed-test.rkt
;;
;; End-to-end gate for the P6.2 seed compiler (racklr/self/seed): the compiler
;; is a UIR program, so the test proves the whole tower by real execution, not
;; by inspecting strings alone.
;;
;;   1. emit kernel-compiler-uir to Python with racklr/emit 'python;
;;   2. assert the emitted source is the compiler: every seed function has a
;;      `def`, every one carries an explicit `@__withEffects(...)` decorator,
;;      and main's decorator is the native io row;
;;   3. write the emitted source to a temp .py, run `python3 file` with
;;      kernel-sample-source on stdin, and assert stdout is exactly
;;      kernel-sample-compiled (a runnable Python program);
;;   4. write that compiler stdout to a second temp .py, run it, and assert
;;      stdout is kernel-sample-result;
;;   5. run the compiler on a small table of additional sources (nested forms,
;;      non-arithmetic call forms, integer atoms, several top-level forms) and
;;      assert the exact compiled output.
;;
;; Every python3 invocation is a real subprocess with a checked exit code; a
;; non-zero exit or stderr is reported in the failure message, never swallowed.

(require rackunit
         racket/file
         racket/string
         racklr/emit
         racklr/self/seed/compiler)

;; ── Failure accounting ─────────────────────────────────────────────
;;
;; Rackunit prints a FAILURE block but a bare `#lang racket` suite still exits
;; 0, so the gate's exit code would not carry the verdict. Count failed checks
;; through the check handler (chaining the default handler keeps the FAILURE
;; output) and raise at the end, so a red run is red in the exit code too.
(define rackunit-failures (box 0))
(define default-check-handler (current-check-handler))
(current-check-handler
 (λ (check)
   (set-box! rackunit-failures (add1 (unbox rackunit-failures)))
   (default-check-handler check)))

;; ── Subprocess helpers ─────────────────────────────────────────────

;; Run `python3 script` with `stdin-text` on stdin. Returns (values ok? stdout
;; stderr) where ok? is #t on exit 0; files are cleaned up before checking.
(define (run-python py-source stdin-text)
  (define script (make-temporary-file "racklr-self-seed-~a.py"))
  (define stdin-file (make-temporary-file "racklr-self-seed-in-~a.txt"))
  (define stdout-file (make-temporary-file "racklr-self-seed-out-~a.txt"))
  (define stderr-file (make-temporary-file "racklr-self-seed-err-~a.txt"))
  (display-to-file py-source script #:exists 'replace)
  (display-to-file stdin-text stdin-file #:exists 'replace)
  (define status
    (system (format "python3 ~a < ~a > ~a 2> ~a"
                    script stdin-file stdout-file stderr-file)))
  (define stdout
    (with-handlers ([exn:fail:filesystem? (λ (e) "")])
      (file->string stdout-file)))
  (define stderr
    (with-handlers ([exn:fail:filesystem? (λ (e) "")])
      (file->string stderr-file)))
  (for ([f (in-list (list script stdin-file stdout-file stderr-file))])
    (when (file-exists? f) (delete-file f)))
  (values (eq? status #t) stdout stderr))

;; Normalize captured stdout for comparison with a one-line expected program.
(define (trim-trailing-newline s) (string-trim s "\n"))

;; The decorator line immediately above `def NAME(...)` in the emitted source,
;; or #f. Used to assert each function carries its explicit row.
(define (decorator-for src def-line)
  (define lines (string-split src "\n"))
  (for/first ([line (in-list lines)]
              [prev (in-list (cons #f lines))]
              #:when (string=? (string-trim line) def-line))
    prev))

(define (count-matches rx s) (length (regexp-match* rx s)))

;; ── Emit the compiler and assert its shape ─────────────────────────

(define emitted (emit 'python (kernel-compiler-uir) #f))

(check-true (and (string? emitted) (> (string-length emitted) 0))
            "emitted seed compiler source is a non-empty string")

;; Every stage of the compiler is a named Python function.
(for ([name (in-list '("tokenize" "parse" "token_atom" "emit_expr"
                       "emit_program" "compile_source" "main"))])
  (check-true (string-contains? emitted (string-append "\ndef " name "("))
              (format "emitted source defines ~a" name)))

;; The emit is complete: no unrendered UIR nodes leaked into the target.
(check-false (string-contains? emitted "<?") "no unrendered UIR nodes in emitted source")
(check-false (string-contains? emitted "# ?") "no tag fallthrough markers in emitted source")

;; Every one of the seven seed functions carries an explicit row: six pure
;; `@__withEffects({})` guards plus main's io guard.
(check-true (string-contains? emitted "@__withEffects(")
            "emitted source uses the native Python row form @__withEffects(...)")
(check-equal? (count-matches #rx"@__withEffects\\(" emitted) 7
              "every seed function carries an explicit effect row")
(check-equal? (count-matches #rx"@__withEffects\\(\\{\\}\\)" emitted) 6
              "the six pure stages carry the explicit empty (total) row")

;; main's own row is the native io row.
(define main-decorator (decorator-for emitted "def main():"))
(check-true (and main-decorator (string-prefix? main-decorator "@__withEffects("))
            (format "main has an effect decorator; got: ~a" main-decorator))
(check-true (and main-decorator (string-contains? main-decorator "\"io\""))
            (format "main's decorator carries the io row; got: ~a" main-decorator))

;; ── Execute the emitted compiler on the sample source ──────────────

(define-values (compiler-ok? compiled-stdout compiled-stderr)
  (run-python emitted (string-append kernel-sample-source "\n")))

(check-true compiler-ok?
            (format "python3 seed compiler exited non-zero; stderr: ~a" compiled-stderr))
(check-equal? (trim-trailing-newline compiled-stdout) kernel-sample-compiled
              (format "seed compiler stdout for ~s; stderr: ~a"
                      kernel-sample-source compiled-stderr))

;; ── Execute the program the compiler produced ──────────────────────

(define-values (compiled-ok? result-stdout result-stderr)
  (run-python compiled-stdout ""))

(check-true compiled-ok?
            (format "compiled program exited non-zero; stderr: ~a" result-stderr))
(check-equal? (trim-trailing-newline result-stdout) kernel-sample-result
              (format "compiled program stdout; stderr: ~a" result-stderr))

;; The filename the compiler's product is written to is a Python file.
(check-true (string-suffix? kernel-compiled-filename ".py")
            "kernel-compiled-filename names a .py file")

;; ── Additional real compilations (compiler behavior table) ─────────

;; Each case is (source on stdin, expected stdout). Covers nested forms,
;; arithmetic infix rendering, non-arithmetic call forms, a bare integer atom,
;; a symbol atom, and several top-level forms on one source line. main() reads
;; one input() line (per the kernel contract), so multi-form sources stay on a
;; single line; compile_source itself accepts newlines as whitespace.
(define compile-cases
  (list (cons "(+ 1 (* 2 3))" "print((1 + (2 * 3)))")
        (cons "(- 10 4)" "print((10 - 4))")
        (cons "(* 2 (+ 3 4))" "print((2 * (3 + 4)))")
        (cons "(foo 1 2)" "print(foo(1, 2))")
        (cons "(+ 1 2 3)" "print(+(1, 2, 3))")
        (cons "42" "print(42)")
        (cons "hello" "print(hello)")
        (cons "(+ 1 2) (* 3 4)" "print((1 + 2))\nprint((3 * 4))")))

(for ([case (in-list compile-cases)])
  (define source (car case))
  (define expected (cdr case))
  (define-values (ok? stdout stderr)
    (run-python emitted (string-append source "\n")))
  (check-true ok? (format "compiler exited non-zero for ~s; stderr: ~a" source stderr))
  (check-equal? (trim-trailing-newline stdout) expected
                (format "compiler output for ~s; stderr: ~a" source stderr)))

;; ── Language description is present and honest ─────────────────────

(check-true (and (string? kernel-compiler-language)
                 (string-contains? kernel-compiler-language "s-expressions"))
            "kernel-compiler-language describes the seed language")

;; A red check above makes the suite exit non-zero, not just print a FAILURE.
(when (positive? (unbox rackunit-failures))
  (error 'self-seed-test
         "~a rackunit check(s) failed; see the FAILURE blocks above"
         (unbox rackunit-failures)))

(displayln "self-seed: all checks passed")
