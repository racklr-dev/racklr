#lang racket

;; tools/emit-self.rkt — CLI for the P6.2 self-emit path.
;;
;;   racket tools/emit-self.rkt --target python --out DIR [--run] [--stdin STR] [--image kernel]
;;   racket tools/emit-self.rkt --target python --out DIR --uir-file IMAGE.sexp [--uir-format sexp|json]
;;   racket tools/emit-self.rkt --list
;;
;; Takes a compiler-as-UIR program (a "self-image"), emits it through the P3
;; emit contract into the requested target, writes the generated source, and —
;; with --run — runs it and reports the emitted program's own output. Effect
;; rows must survive in the target's native vocabulary for a row-capable
;; target, and the run must succeed when --run is given; either failure is a
;; nonzero exit, never a silent pass.
;;
;; The driver module stays image-agnostic. This CLI is where concrete images are
;; registered: the seed compiler (racklr/self/seed/compiler) as 'kernel, plus
;; any serialized UIR program passed with --uir-file (the P6.1 seam).
;;
;; Exit codes: 0 = emitted (and ran) as requested; 1 = a verification failure
;; (rows not native, run failed); 2 = usage/emit error (unknown target or image,
;; unreadable image, conflicting flags).

(require racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/string
         racklr/self-emit
         racklr/self/seed/compiler
         racklr/uir)

;; ── Self-image registry ─────────────────────────────────────────────
;;
;; The image seam is data: registering here (not inside racklr/self-emit) keeps
;; the driver usable with any P6.1 self-image.

(define registered-kernel-image
  (register-self-image! 'kernel kernel-compiler-language kernel-compiler-uir))

;; ── Does the image declare effect rows? ─────────────────────────────
;;
;; A row-capable target is only required to show a native row when the image
;; actually declares rows; a row-less image legitimately emits no guard. This
;; walks the forms that can carry a row (the same ones the emit contract
;; attaches one to), not the whole UIR tree indiscriminately.
(define (program-declares-rows? uir)
  (cond
    [(uir-fn? uir)
     (or (uir-effect-row? (uir-fn-effects uir))
         (program-declares-rows? (uir-fn-body uir)))]
    [(uir-block? uir)
     (for/or ([stmt (in-list (uir-block-stmts uir))])
       (program-declares-rows? stmt))]
    [(uir-module? uir) (program-declares-rows? (uir-module-body uir))]
    [(uir-decorated? uir) (program-declares-rows? (uir-decorated-inner uir))]
    [(uir-set!? uir) (program-declares-rows? (uir-set!-value uir))]
    [(uir-call? uir)
     (define callee (uir-call-callee uir))
     (or (and (uir-symbol? callee)
              (member (uir-symbol-name callee) '("async-fn" "gen-fn"))
              (pair? (uir-call-args uir))
              (program-declares-rows? (first (uir-call-args uir))))
         (for/or ([arg (in-list (uir-call-args uir))])
           (program-declares-rows? arg)))]
    [else #f]))

;; ── Default output directory ────────────────────────────────────────

(define (default-out-dir image uir-file)
  (define name
    (if uir-file
        (let ([base (file-name-from-path uir-file)])
          (if base
              (path->string (path-replace-extension base ""))
              "uir-file"))
        (symbol->string image)))
  (build-path "self-emitted" name))

;; ── Reporting ───────────────────────────────────────────────────────

(define (report path target uir source run? stdin-string)
  (printf "emitted ~a -> ~a\n" target path)
  (define rows-native? (emitted-rows-native? target source))
  (printf "rows-native?: ~a\n" rows-native?)
  (if (target-row-marker target)
      (void)
      (printf "row vocabulary: none (target ~a has no native effect-row form)\n" target))
  (define run-ok? #t)
  (if run?
      (let-values ([(ok? output) (run-emitted-source path #:stdin stdin-string)])
        (set! run-ok? ok?)
        (printf "run: ~a\n" (if ok? "ok (exit 0)" "FAILED (nonzero exit or runtime unavailable)"))
        (printf "output:\n~a" output)
        (unless (string-suffix? "\n" output) (newline)))
      (void))
  (define rows-required?
    (and (target-row-marker target) (program-declares-rows? uir)))
  (if (and rows-required? (not rows-native?))
      (eprintf "emit-self: effect rows were not rendered in ~a's native vocabulary\n" target)
      (void))
  (values rows-native? run-ok?))

;; ── CLI ─────────────────────────────────────────────────────────────

(define target 'python)
(define out-dir #f)
(define image 'kernel)
(define image-supplied? #f)
(define uir-file #f)
(define uir-format 'sexp)
(define stdin-string #f)
(define run? #f)
(define list-requested? #f)

(command-line
 #:program "emit-self"
 #:once-each
 [("--target") lang "(default: python) target language to emit"
  (set! target (string->symbol lang))]
 [("--out") dir "output directory (default: self-emitted/<image>)"
  (set! out-dir dir)]
 [("--image") name "(default: kernel) registered self-image to emit"
  (set! image (string->symbol name))
  (set! image-supplied? #t)]
 [("--uir-file") path "serialized UIR program to emit instead of a self-image"
  (set! uir-file path)]
 [("--uir-format") format-name "sexp (default) or json"
  (set! uir-format (string->symbol format-name))]
 [("--stdin") text "stdin to feed the emitted program (used with --run)"
  (set! stdin-string text)]
 [("--run") "run the emitted source and report its output and exit"
  (set! run? #t)]
 [("--list") "list registered self images and supported targets"
  (set! list-requested? #t)])

(define (usage-check)
  (if (and uir-file image-supplied?)
      (error 'emit-self "--image and --uir-file are mutually exclusive")
      (void))
  (unless (memq uir-format '(sexp json))
    (error 'emit-self "unsupported --uir-format ~a; expected sexp or json" uir-format))
  (unless (member target (self-emit-targets))
    (error 'emit-self "unsupported target: ~a; supported targets: ~a"
           target (self-emit-targets))))

(define (show-list)
  (printf "self images:\n")
  (for ([id (in-list (self-image-ids))])
    (printf "  ~a  ~a\n" id (self-image-description id)))
  (printf "supported targets: ~a\n"
          (string-join (map symbol->string (self-emit-targets)) " ")))

(define (run-emit!)
  (usage-check)
  (define uir (if uir-file
                  (load-self-image-file uir-file #:format uir-format)
                  (self-image-uir image)))
  (define out (or out-dir (default-out-dir image uir-file)))
  (define path (emit-self target uir out))
  (define source (file->string path))
  (define-values (rows-native? run-ok?)
    (report path target uir source run? stdin-string))
  (define rows-required?
    (and (target-row-marker target) (program-declares-rows? uir)))
  (if (or (and rows-required? (not rows-native?)) (not run-ok?))
      (exit 1)
      (exit 0)))

(define (main)
  (if list-requested?
      (begin
        (show-list)
        (exit 0))
      (run-emit!)))


(with-handlers ([exn:fail?
                 (λ (e)
                   ;; Messages from this tool and from the emit contract already
                   ;; name their source, so print them without a second prefix.
                   (eprintf "~a\n" (exn-message e))
                   (exit 2))])
  (main))
