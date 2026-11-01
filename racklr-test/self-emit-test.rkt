#lang racket

;; Self-emit driver tests (P6.2): a compiler-as-UIR self-image in, a target
;; language out through the P3 emit contract, a real run of the emitted source,
;; and the effect-row check in the target's native vocabulary.
;;
;; The driver itself is image-agnostic; the tiny fixture programs here keep the
;; unit tests independent of the seed compiler. The seed compiler integration
;; assertion registers racklr/self/seed/compiler as 'kernel when that module is
;; present, and prints an explicit SKIP line when it is absent (never a silent
;; skip). The driver module itself never requires the seed compiler.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/string
         racklr/emit
         racklr/emit-contract
         racklr/self-emit
         racklr/uir)

;; Rackunit prints a FAILURE block but a bare `#lang racket` suite still exits
;; 0, so the exit code would not carry the verdict. Count failed checks through
;; the check handler (chaining the default handler keeps the FAILURE output) and
;; raise at the end of `module+ main`: a red run is red in the exit code too.
(define rackunit-failures (box 0))
(define default-check-handler (current-check-handler))
(current-check-handler
 (λ (check)
   (set-box! rackunit-failures (add1 (unbox rackunit-failures)))
   (default-check-handler check)))

;; ── Fixtures ────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (text s) (uir-string s))

(define (row-of name)
  (uir-effect-row (list (uir-effect-entry (sym name) 'may '() #f #f))))

;; A function that carries an io row, called at the top level: emitting it to
;; Python renders the guard decorator and the preamble, and running it prints
;; its body's output.
(define (guarded-program)
  (uir-block
   (list (uir-fn (sym "greet")
                 '()
                 (uir-block (list (uir-call (sym "print") (list (text "ok")))))
                 #f
                 (row-of "io"))
         (uir-call (sym "greet") '()))))

;; A row-less body: valid source with no effect row anywhere.
(define (rowless-program)
  (uir-block (list (uir-fn (sym "noop") '() (uir-null) #f #f))))

;; Straight-line program whose only job is to print.
(define (printing-program)
  (uir-block (list (uir-call (sym "print") (list (text "ok"))))))

;; Fails at runtime, for the failure path of run-emitted-source.
(define (failing-program)
  (uir-block (list (uir-call (sym "undefined_function") '()))))

;; Reads one line from stdin, for --stdin / #:stdin plumbing. The read is a
;; standalone assignment: the Python effect guard evaluates a direct call's
;; argument expressions for its bookkeeping, so an `input()` nested inside a
;; guarded `print(...)` would be read twice.
(define (stdin-program)
  (uir-block (list (uir-set! (sym "line") (uir-call (sym "input") '()))
                   (uir-call (sym "print") (list (uir-var (sym "line")))))))

(define (with-temp-dir thunk)
  (define dir (make-temporary-file "racklr-self-emit-test-~a" 'directory))
  (dynamic-wind
    void
    (λ () (thunk dir))
    (λ () (delete-directory/files dir))))

;; ── emit-self: contract output on disk ──────────────────────────────

(define (test-emit-self-writes-the-contract-output)
  (with-temp-dir
   (λ (dir)
     (define path (emit-self 'python (guarded-program) dir))
     (check-equal? path (build-path dir "seed-compiler.py")
                   "the default filename comes from target-filename")
     (check-true (file-exists? path) "the emitted source was written")
     (check-equal? (bytes->string/utf-8 (path-get-extension path)) ".py"
                   "the extension is the target language's")
     (check-equal? (file->string path) (emit 'python (guarded-program) #f)
                   "the file is exactly the emit-contract output")
     ;; Nested output directories are created, not expected to exist.
     (define nested (build-path dir "nested" "deeper"))
     (define nested-path (emit-self 'python (guarded-program) nested))
     (check-true (file-exists? nested-path) "nested output directories are created")
     ;; #:filename overrides the default; #:row goes through the contract.
     (define custom (emit-self 'python (guarded-program) dir #:filename "custom.py"
                               #:row (row-of "llm")))
     (check-equal? (file-name-from-path custom) (string->path "custom.py")
                   "#:filename names the file")
     (check-equal? (file->string custom)
                   (emit 'python (guarded-program) (row-of "llm"))
                   "#:row is passed to the emit contract")
     (check-true (string-contains? (file->string custom) "\"llm\"")
                 "the explicit row reaches the emitted source"))))

;; ── filenames and native row markers ────────────────────────────────

(define (test-target-filenames)
  (check-equal? (target-filename 'python) "seed-compiler.py")
  (check-equal? (target-filename 'javascript) "seed-compiler.js")
  (check-equal? (target-filename 'koka) "seed-compiler.kk")
  (check-equal? (target-filename 'clojure) "seed-compiler.clj")
  (check-equal? (target-filename 'json) "seed-compiler.json")
  (check-equal? (self-emit-targets) (supported-targets)
                "self-emit covers exactly the P3 dispatch targets"))

(define (test-row-markers-derive-from-the-contract)
  (check-equal? (target-row-marker 'python) "@__withEffects(")
  (check-equal? (target-row-marker 'javascript) " __withEffects(")
  (check-equal? (target-row-marker 'koka) " : <")
  ;; Koka has two native spellings: the typed row (when a result type is
  ;; declared) and the documented `// @effects` row otherwise.
  (check-not-false (member "// @effects" (target-row-markers 'koka)))
  (check-equal? (target-row-marker 'clojure) ":__effects__")
  (check-false (target-row-marker 'json) "json has no native row vocabulary")
  (check-false (target-row-marker 'jsx) "jsx has no native row vocabulary")
  ;; The marker table is derived from the emit contract, not a second list.
  (for ([t (in-list (self-emit-targets))])
    (define syntax (emit-target-row-syntax (emit-target-for t)))
    (if (eq? syntax 'none)
        (check-false (target-row-marker t)
                     (format "~a declares 'none but has a marker" t))
        (check-true (string? (target-row-marker t))
                    (format "~a declares ~a but has no marker" t syntax)))))

(define (test-emitted-rows-native?)
  ;; Row-capable targets reify the row in their own vocabulary.
  (for ([t (in-list '(python javascript koka clojure html))])
    (check-true (emitted-rows-native? t (emit t (guarded-program) #f))
                (format "~a carries the row natively" t)))
  (check-true (emitted-rows-native? 'python (emit 'python (guarded-program) #f)))
  (check-true (string-contains? (emit 'python (guarded-program) #f)
                                "@__withEffects("))
  ;; A row-less body is valid source with no native row marker.
  (check-false (emitted-rows-native? 'python (emit 'python (rowless-program) #f)))
  ;; A 'none target is never claimed, even if the marker text appears.
  (check-false (emitted-rows-native? 'json "@__withEffects("))
  (check-false (emitted-rows-native? 'effervesce ":__effects__")))

;; ── running the emitted source ──────────────────────────────────────

(define (test-run-emitted-source)
  (with-temp-dir
   (λ (dir)
     (define path (emit-self 'python (printing-program) dir))
     (define-values (ok? output) (run-emitted-source path))
     (check-true ok? "the emitted Python program exits 0")
     (check-equal? output "ok\n" "stdout is captured")
     ;; stdin is piped through to the emitted program.
     (define stdin-path (emit-self 'python (stdin-program) dir #:filename "stdin.py"))
     (define-values (stdin-ok? stdin-output)
       (run-emitted-source stdin-path #:stdin "hello\n"))
     (check-true stdin-ok? "the stdin-fed program exits 0")
     (check-equal? stdin-output "hello\n" "#:stdin reaches the emitted program")
     ;; #:args are appended to the runtime command line.
     (define args-path (build-path dir "args.py"))
     (display-to-file "import sys\nprint('|'.join(sys.argv[1:]))\n"
                      args-path #:exists 'replace)
     (define-values (args-ok? args-output)
       (run-emitted-source args-path #:args '("a" "b")))
     (check-true args-ok? "a program run with #:args exits 0")
     (check-equal? args-output "a|b\n" "#:args reach the runtime command line")
     ;; A program that raises is reported as a failed run, not raised.
     (define fail-path (emit-self 'python (failing-program) dir #:filename "fail.py"))
     (define-values (fail-ok? fail-output) (run-emitted-source fail-path))
     (check-false fail-ok? "a raising emitted program is a failed run")
     (check-true (string-contains? fail-output "NameError")
                 "the runtime error is reported in the output")
     ;; A target with no wired-up runtime is an explicit failed run.
     (define koka-path (emit-self 'koka (guarded-program) dir))
     (define-values (koka-ok? koka-output) (run-emitted-source koka-path))
     (check-false koka-ok? "no runtime is wired up for Koka yet")
     (check-true (string-contains? koka-output "no runtime")
                 "the loss model is stated, not hidden")
     ;; A missing source file is a caller error.
     (check-exn exn:fail?
                (λ () (run-emitted-source (build-path dir "absent.py")))
                "running a nonexistent source raises"))))

;; ── emit-self-and-run ───────────────────────────────────────────────

(define (test-emit-self-and-run)
  (with-temp-dir
   (λ (dir)
     (define result (emit-self-and-run 'python (guarded-program) dir))
     (check-equal? (sort (hash-keys result) symbol<?)
                   '(ok? output path rows-native? source target)
                   "the report hash has exactly the agreed keys")
     (check-equal? (hash-ref result 'target) 'python)
     (check-true (hash-ref result 'ok?) "the emitted program runs")
     (check-true (hash-ref result 'rows-native?) "the io row is native Python")
     (check-equal? (hash-ref result 'output) "ok\n")
     (check-equal? (hash-ref result 'source) (file->string (hash-ref result 'path))
                   "the reported source is what was written")
     ;; Runtime failure is reported in the hash, not raised.
     (define failing (emit-self-and-run 'python (failing-program) dir
                                        #:filename "fail-and-run.py"))
     (check-false (hash-ref failing 'ok?) "a runtime failure is in the report"))))

;; ── unsupported targets and refused rows ────────────────────────────

(define (test-unsupported-target-raises)
  (with-temp-dir
   (λ (dir)
     (check-exn exn:fail? (λ () (emit-self 'cobol (guarded-program) dir)))
     (check-exn exn:fail? (λ () (target-filename 'cobol)))
     (check-exn exn:fail? (λ () (target-row-marker 'cobol)))
     (check-exn exn:fail? (λ () (self-image-uir 'no-such-image))))))

(define (test-row-less-target-refuses-an-explicit-row)
  (with-temp-dir
   (λ (dir)
     (check-exn exn:fail?
                (λ () (emit-self 'json (guarded-program) dir #:row (row-of "llm")))
                "a 'none target refuses an explicit row through the driver"))))

;; ── self-image registry ─────────────────────────────────────────────

(define (test-self-image-registry)
  (register-self-image! 'fixture "tiny fixture program" guarded-program)
  (check-not-false (member 'fixture (self-image-ids)))
  (check-equal? (self-image-uir 'fixture) (guarded-program))
  (check-equal? (self-image-description 'fixture) "tiny fixture program")
  (check-exn exn:fail? (λ () (register-self-image! "not-a-symbol" "d" guarded-program)))
  (check-exn exn:fail? (λ () (register-self-image! 'bad "d" "not a procedure")))
  (check-exn exn:fail? (λ () (register-self-image! 'bad2 "" guarded-program))))

;; ── serialized-UIR seam (--uir-file) ────────────────────────────────

(define (write-sexp-file! path value)
  (call-with-output-file path
    (λ (out) (write value out) (newline out))
    #:exists 'replace))

(define (test-load-self-image-file)
  (with-temp-dir
   (λ (dir)
     ;; Bare uir->sexp encoding, as a caller would write it.
     (define sexp-path (build-path dir "image.sexp"))
     (write-sexp-file! sexp-path (uir->sexp (guarded-program)))
     (check-equal? (load-self-image-file sexp-path) (guarded-program)
                   "the bare sexp encoding round-trips")
     ;; Versioned envelope.
     (define versioned-path (build-path dir "image.versioned.sexp"))
     (write-sexp-file! versioned-path (uir->versioned-sexp (guarded-program)))
     (check-equal? (load-self-image-file versioned-path) (guarded-program)
                   "the versioned sexp encoding round-trips")
     ;; Versioned JSON encoding: rows must survive it, not just row-less forms.
     (check-equal? (json->uir (uir->json (guarded-program)))
                   (guarded-program)
                   "the JSON encoding round-trips a program with an effect row")
     (define json-path (build-path dir "image.json"))
     (display-to-file (uir->json (guarded-program)) json-path #:exists 'replace)
     (define json-loaded (load-self-image-file json-path #:format 'json))
     (check-equal? json-loaded (guarded-program)
                   "the JSON-loaded image equals the in-memory image")
     (check-equal? (emit 'python json-loaded #f)
                   (emit 'python (guarded-program) #f)
                   "the JSON-loaded image emits identically to the in-memory image")
     (define json-path-emitted (emit-self 'python json-loaded dir
                                          #:filename "json-image.py"))
     (define-values (json-run-ok? json-run-output)
       (run-emitted-source json-path-emitted))
     (check-true json-run-ok? "the JSON-loaded image emits a runnable program")
     (check-equal? json-run-output "ok\n"
                   "the JSON-loaded image runs identically to the in-memory image")
     (check-exn exn:fail? (λ () (load-self-image-file sexp-path #:format 'yaml)))
     (check-exn exn:fail?
                (λ () (load-self-image-file (build-path dir "absent.sexp")))))))

;; ── seed compiler integration (the P6.1 self-image) ─────────────────

(define repo-root
  (simplify-path
   (build-path (path->complete-path (find-system-path 'run-file)) 'up 'up)))

(define seed-compiler-path
  (build-path repo-root "racklr" "self" "seed" "compiler.rkt"))

(define (test-seed-compiler-present)
  (define kernel-uir-proc
    (dynamic-require 'racklr/self/seed/compiler 'kernel-compiler-uir))
  (define kernel-language
    (dynamic-require 'racklr/self/seed/compiler 'kernel-compiler-language))
  (define sample-source
    (dynamic-require 'racklr/self/seed/compiler 'kernel-sample-source))
  (define sample-compiled
    (dynamic-require 'racklr/self/seed/compiler 'kernel-sample-compiled))
  (define sample-result
    (dynamic-require 'racklr/self/seed/compiler 'kernel-sample-result))
  (register-self-image! 'kernel kernel-language kernel-uir-proc)
  (check-not-false (member 'kernel (self-image-ids))
                   "the CLI/test seam registers the seed compiler as 'kernel")
  (check-equal? (self-image-uir 'kernel) (kernel-uir-proc)
                "--image kernel resolves the seed compiler UIR")
  ;; The P6.1 seam: the seed image survives the versioned JSON round trip and
  ;; emits identically from disk as in memory.
  (define json-loaded-seed (json->uir (uir->json (kernel-uir-proc))))
  (check-equal? json-loaded-seed (kernel-uir-proc)
                "the seed self-image round-trips through the versioned JSON encoding")
  (check-equal? (emit 'python json-loaded-seed #f)
                (emit 'python (kernel-uir-proc) #f)
                "the JSON-loaded seed image emits identically to the in-memory image")
  (with-temp-dir
   (λ (dir)
     ;; Emit the compiler, run it on the sample, check what it prints.
     (define report (emit-self-and-run 'python (kernel-uir-proc) dir
                                       #:stdin sample-source))
     (check-true (hash-ref report 'ok?)
                 "the emitted seed compiler runs under python3")
     (check-true (hash-ref report 'rows-native?)
                 "the seed compiler's rows are native Python")
     (check-true (string-contains? (hash-ref report 'source) "@__withEffects(")
                 "the emitted compiler source carries the io row in native form")
     (define compiled (string-trim (hash-ref report 'output)))
     (check-equal? compiled sample-compiled
                   "the emitted compiler compiles the sample source")
     (check-false (string-contains? compiled "#")
                  "the compiled sample is comment-free")
     ;; Then run the program the compiler produced.
     (define compiled-path (build-path dir "compiled_program.py"))
     (display-to-file (string-append compiled "\n") compiled-path
                      #:exists 'replace)
     (define-values (run-ok? run-output)
       (run-emitted-source compiled-path))
     (check-true run-ok? "the compiled sample program runs")
     (check-equal? (string-trim run-output) sample-result
                   "the compiled sample program prints the expected result"))))

(define (test-seed-compiler-self-image)
  (if (file-exists? seed-compiler-path)
      (test-seed-compiler-present)
      (displayln "SKIP test-seed-compiler-self-image: racklr/self/seed/compiler.rkt is absent from this checkout; driver unit tests above still ran.")))

;; ── main ────────────────────────────────────────────────────────────

(module+ main
  (test-emit-self-writes-the-contract-output)
  (test-target-filenames)
  (test-row-markers-derive-from-the-contract)
  (test-emitted-rows-native?)
  (test-run-emitted-source)
  (test-emit-self-and-run)
  (test-unsupported-target-raises)
  (test-row-less-target-refuses-an-explicit-row)
  (test-self-image-registry)
  (test-load-self-image-file)
  (test-seed-compiler-self-image)
  ;; A red check above must make the suite exit nonzero, not just print a
  ;; FAILURE block: rackunit records failures and keeps going.
  (if (positive? (unbox rackunit-failures))
      (error 'self-emit-test
             "~a rackunit check(s) failed; see the FAILURE blocks above"
             (unbox rackunit-failures))
      (displayln "All self-emit tests passed.")))
