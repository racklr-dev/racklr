#lang racket

;; P6.2 end-to-end integration: the tower emits the compiler into a target.
;;
;; The seed self-image (racklr/self/seed/compiler) is a compiler written as a
;; UIR program. This suite drives the whole reflexive path against the real
;; modules (no fixture):
;;
;;   seed UIR program
;;     --(emit-self 'python)-->            Python source with native effect rows
;;     --(run-emitted-source)-->           the emitted compiler runs
;;     --(compile the sample source)-->    compiled Python program text
;;     --(run the compiled program)-->     expected value
;;
;; It also covers the P6.1 seam: a serialized self-image loads from disk and
;; emits identically, so the tower's own lowered UIR can be fed to the same
;; path without a driver change. The CLI is exercised as the last case because
;; that is the shape a person or loop actually invokes.
;;
;; Run from inside racklr-test/: `racket self-emit-integration-test.rkt`.
;; grammars and collection paths resolve relative to this working directory.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/string
         racket/system
         racklr/uir
         racklr/emit
         racklr/self-emit
         racklr/self/seed/compiler)

;; ── Failure accounting ─────────────────────────────────────────────
;;
;; A bare `#lang racket` suite prints a FAILURE block but still exits 0, so the
;; gate's exit code would not carry the verdict. Count failed checks through the
;; check handler (chaining the default keeps the FAILURE output) and raise at
;; the end, so a red run is red in the exit code too.
(define rackunit-failures (box 0))
(define default-check-handler (current-check-handler))
(current-check-handler
 (λ (check)
   (set-box! rackunit-failures (add1 (unbox rackunit-failures)))
   (default-check-handler check)))

;; ── fixtures ────────────────────────────────────────────────────────

(define repo-root
  (path->string (simplify-path (build-path (current-directory) ".."))))

(define (call-with-temp-dir thunk)
  (define dir (make-temporary-file "racklr-self-emit-it-~a" 'directory))
  (dynamic-wind void
                (λ () (thunk dir))
                (λ ()
                  (if (directory-exists? dir)
                      (delete-directory/files dir)
                      (void)))))

(define seed-program (kernel-compiler-uir))

(define seed-forms
  (if (uir-block? seed-program)
      (uir-block-stmts seed-program)
      (list seed-program)))

(define seed-fns (filter uir-fn? seed-forms))

(define (named-fn name)
  (findf (λ (f)
           (define n (uir-fn-name f))
           (and n (equal? (uir-symbol-name n) name)))
         seed-fns))

(define (row-effect-names fn)
  (map (λ (e) (string->symbol (uir-symbol-name (uir-effect-entry-name e))))
       (uir-effect-row-entries (uir-fn-effects fn))))

;; ── the self-image is a compiler program with a row on every function ──

(define (test-seed-self-image)
  (check-true (uir? seed-program) "kernel-compiler-uir returns a UIR program")
  (check-true (uir-block? seed-program) "the self-image is a program (uir-block)")
  (check-equal? (length seed-fns) 7
                "tokenize, parse, token_atom, emit_expr, emit_program, compile_source, main")
  (for ([f (in-list seed-fns)])
    (check-true (uir-effect-row? (uir-fn-effects f))
                (format "every compiler function carries a row: ~a"
                        (uir-symbol-name (uir-fn-name f)))))
  ;; main performs the program's io (stdin + stdout); the pure stages are total.
  (define main-fn (named-fn "main"))
  (check-not-false main-fn "main exists")
  (check-not-false (memq 'io (row-effect-names main-fn))
                   "main's row names the io effect")
  (define pure-fns (filter (λ (f) (not (eq? f main-fn))) seed-fns))
  (for ([f (in-list pure-fns)])
    (check-equal? (row-effect-names f) '()
                  (format "pure stage ~a declares an empty (total) row"
                          (uir-symbol-name (uir-fn-name f))))))

;; ── the tower emits the compiler into Python through the emit contract ──

(define (test-emit-self-native-rows)
  (call-with-temp-dir
   (λ (dir)
     (define path (emit-self 'python seed-program dir))
     (check-true (file-exists? path) "the emitted compiler source was written")
     (check-equal? (path->string (file-name-from-path path)) (target-filename 'python))
     (define source (file->string path))
     (check-true (positive? (string-length source)))
     ;; Every row the UIR carries is reified in Python's native vocabulary.
     (check-equal? (length (regexp-match* #rx"@__withEffects\\(" source))
                   (length seed-fns)
                   "one @__withEffects(...) decorator per compiler function")
     (check-true (emitted-rows-native? 'python source)
                 "rows survive in Python's native vocabulary (guard-decorator)")
     (check-true (string-contains? source "@__withEffects({\"io\": {\"modality\": \"may\"}})")
                 "main's io row is rendered natively, not dropped")
     ;; The program is a compiler: the stages the UIR defines are in the source.
     (check-regexp-match #rx"def tokenize\\(src\\):" source)
     (check-regexp-match #rx"def parse\\(tokens, pos\\):" source)
     (check-regexp-match #rx"def token_atom\\(tok\\):" source)
     (check-regexp-match #rx"def emit_expr\\(node\\):" source)
     (check-regexp-match #rx"def emit_program\\(forms\\):" source)
     (check-regexp-match #rx"def compile_source\\(src\\):" source)
     (check-regexp-match #rx"def main\\(\\):" source)
     (check-true (string-suffix? (string-trim source) "main()")
                 "the program ends with the (main) call"))))

;; ── the emitted compiler is runnable source that compiles a program ──

(define (test-runnable-emitted-compiler)
  (call-with-temp-dir
   (λ (dir)
     (define report (emit-self-and-run 'python seed-program dir
                                       #:stdin kernel-sample-source))
     (check-true (hash-ref report 'rows-native?) "rows-native? reported true")
     (check-true (hash-ref report 'ok?)
                 (format "the emitted compiler ran (exit 0); output: ~a"
                         (hash-ref report 'output)))
     (define compiled (string-trim (hash-ref report 'output)))
     (check-equal? compiled kernel-sample-compiled
                   "the emitted compiler compiled the sample to the expected program")
     (check-equal? compiled "print((1 + (2 * 3)))")
     ;; The compiler's output is itself runnable target source.
     (define compiled-path (build-path dir kernel-compiled-filename))
     (display-to-file compiled compiled-path #:exists 'replace)
     (define-values (ok? output) (run-emitted-source compiled-path))
     (check-true ok?
                 (format "the compiled program ran (exit 0); output: ~a" output))
     (check-equal? (string-trim output) kernel-sample-result)
     (check-equal? (string-trim output) "7"))))

;; ── the P6.1 seam: a serialized self-image emits identically ──

(define (test-serialized-self-image)
  (call-with-temp-dir
   (λ (dir)
     ;; Versioned s-expression encoding (the on-disk schema envelope).
     (define sexp-path (build-path dir "self-image.sexp"))
     (call-with-output-file sexp-path
       (λ (out) (write (uir->versioned-sexp seed-program) out))
       #:exists 'replace)
     (define from-disk (load-self-image-file sexp-path))
     (check-true (equal? from-disk seed-program))
     (check-equal? (emit 'python from-disk #f)
                   (emit 'python seed-program #f)
                   "the same compiler is emitted from a serialized self-image")
     ;; JSON encoding of the same program.
     (define json-path (build-path dir "self-image.json"))
     (display-to-file (uir->json seed-program) json-path #:exists 'replace)
     (check-equal? (emit 'python (load-self-image-file json-path #:format 'json) #f)
                   (emit 'python seed-program #f)))))

;; ── what a person/loop actually invokes: the CLI ──

(define (sh-quote s)
  (string-append "'" (string-replace s "'" "'\\''") "'"))

(define (test-cli-runnable-compiler)
  (call-with-temp-dir
   (λ (dir)
     (define log (build-path dir "cli.log"))
     (define command
       (format "cd ~a && PLTCOLLECTS=~a racket tools/emit-self.rkt --image kernel --target python --out ~a --run --stdin ~a > ~a 2>&1"
               (sh-quote repo-root)
               (sh-quote (string-append repo-root ":"))
               (sh-quote (path->string dir))
               (sh-quote kernel-sample-source)
               (sh-quote (path->string log))))
     (define exit-code (system command))
     (define output (if (file-exists? log) (file->string log) ""))
     (check-equal? exit-code #t (format "the CLI exits 0; output:\n~a" output))
     (check-true (string-contains? output "emitted python -> ")
                 "the CLI reports the emitted source")
     (check-true (string-contains? output "rows-native?: #t")
                 "the CLI reports rows rendered in Python's native vocabulary")
     (check-true (string-contains? output "run: ok (exit 0)")
                 "the CLI reports the emitted compiler ran")
     (check-true (string-contains? output "print((1 + (2 * 3)))")
                 "the CLI shows the compiled program")
     (check-true (file-exists? (build-path dir (target-filename 'python)))
                 "the emitted compiler source persists at --out"))))

;; A red check above makes the suite exit non-zero, not just print a FAILURE.
(module+ main
  (test-seed-self-image)
  (test-emit-self-native-rows)
  (test-runnable-emitted-compiler)
  (test-serialized-self-image)
  (test-cli-runnable-compiler)
  ;; A red check above makes the suite exit non-zero, not just print a
  ;; FAILURE block: rackunit records failures and keeps going.
  (if (positive? (unbox rackunit-failures))
      (error 'self-emit-integration-test
             "~a rackunit check(s) failed; see the FAILURE blocks above"
             (unbox rackunit-failures))
      (displayln "All self-emit integration tests passed.")))
