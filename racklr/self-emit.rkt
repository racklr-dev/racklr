#lang racket

;; ── Self-emit driver: emit a compiler-as-UIR program into a target ──
;;
;; P6.2 takes the reflexive step: the compiler is a UIR program (a
;; "self-image"), and this module emits that program into a target language
;; through the P3 emit contract (racklr/emit, racklr/emit-contract), writes the
;; generated source to disk, runs it when the target has a runtime here, and
;; reports whether the effect rows survived in the target's *native* vocabulary
;; (Python `@__withEffects({...})`, JavaScript/HTML `__withEffects(...)`, Koka
;; `: <effects>`, Clojure `:__effects__`). A target whose row syntax is 'none
;; has no native vocabulary, so it can neither carry a row nor be claimed to.
;;
;; The self-image seam is a registry, not a static dependency: this module never
;; requires a concrete compiler. A caller (the CLI, a test, P6.1) registers an
;; image by name, or hands in a serialized UIR program via
;; `load-self-image-file`. `emit-self` itself is image-agnostic.
;;
;; Loss model (honest, not papered over): Python is the runnable, verified
;; target today (python3). The .js targets have runtime plumbing to node, but JS
;; emission still has gaps for arbitrary UIR, so that path is not a verified
;; runnable target. Other targets emit source through the same contract but
;; their runtimes/toolchains are not wired up here; `run-emitted-source` says so
;; with a #f success flag instead of pretending a run happened.

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         racklr/emit
         racklr/emit-contract
         racklr/uir)

(provide emit-self
         self-emit-targets
         target-filename
         target-row-marker
         target-row-markers
         emitted-rows-native?
         run-emitted-source
         emit-self-and-run
         register-self-image!
         self-image-ids
         self-image-uir
         self-image-description
         load-self-image-file)

;; ── Targets, filenames, native row markers ──────────────────────────

;; Every target the P3 dispatch supports, in table order.
(define (self-emit-targets)
  (supported-targets))

;; One emitted-source filename per target. The stem is fixed so an emitted
;; self-image is recognizable; the extension is the target language's.
(define target-extensions
  (hash 'python ".py"
        'javascript ".js"
        'jsx ".jsx"
        'koka ".kk"
        'clojure ".clj"
        'dom-js ".dom.js"
        'html ".html"
        'json ".json"
        'effervesce ".eff"))

(define emitted-source-stem "seed-compiler")

;; Default filename for a target's emitted source. An unknown target raises the
;; emit contract's error (naming the supported targets) rather than inventing a
;; name for something that cannot be emitted.
(define (target-filename target)
  (define extension
    (hash-ref target-extensions
              (emit-target-target (emit-target-for target))
              #f))
  (if extension
      (string-append emitted-source-stem extension)
      (string-append emitted-source-stem ".txt")))

;; Every native spelling a row can take in a target, derived from the row syntax
;; the emit contract declares for the target (not from a duplicate target list).
;; Most targets have one spelling; Koka has two legitimate native forms —
;; a typed `: <effects>` row when the function declares a result type, and the
;; documented `// @effects {...}` row otherwise, because the grammar only
;; accepts `<effects>` in front of a result type (see racklr/emit-effects).
;; A 'none target has no native form for a row: the empty list, not a marker
;; that could be found in source that never carried a row.
(define (target-row-markers target)
  (case (emit-target-row-syntax (emit-target-for target))
    [(guard-decorator) '("@__withEffects(")]           ; Python
    [(guard-wrapper) '(" __withEffects(")]             ; JavaScript, HTML
    [(effect-annotation) '(" : <" "// @effects")]      ; Koka (typed / documented)
    [(metadata) '(":__effects__")]                     ; Clojure
    [else '()]))

;; The primary native row marker for a target, or #f when it has no native
;; vocabulary. `target-row-markers` lists every accepted spelling.
(define (target-row-marker target)
  (define markers (target-row-markers target))
  (and (pair? markers) (car markers)))

;; #t iff the target has a native row vocabulary and the emitted source
;; contains at least one of its native row spellings. This is the check the
;; driver reports: it never claims native rows for a 'none target and never
;; claims a row was reified when no native spelling is present.
(define (emitted-rows-native? target source)
  (and (string? source)
       (for/or ([marker (in-list (target-row-markers target))])
         (string-contains? source marker))))

;; ── Emitting and writing ────────────────────────────────────────────

;; Emit `uir` through the P3 dispatch `(emit target uir row)`, write the source
;; into `out-dir`, and return the written path. The target is validated through
;; the contract before anything is written; a target with no native row
;; vocabulary refuses an explicit row at the emit call, exactly as the contract
;; requires. `#:filename` overrides the default `(target-filename target)`.
(define (emit-self target uir out-dir
                   #:row [row #f]
                   #:filename [filename #f])
  (unless (uir? uir)
    (error 'emit-self "self-image must be a UIR value, got: ~a" uir))
  (define default-name (target-filename target))  ; validates the target
  (define name (or filename default-name))
  (unless (and (string? name) (positive? (string-length name)))
    (error 'emit-self "#:filename must be a non-empty string, got: ~s" name))
  (define source (emit target uir row))
  (define path (build-path out-dir name))
  (define directory (path-only path))
  (if (and directory (not (directory-exists? directory)))
      (make-directory* directory)
      (void))
  (display-to-file source path #:exists 'replace)
  path)

;; ── Running the emitted source ──────────────────────────────────────

;; The runtime executable for an emitted source, chosen from its extension.
;; Only the targets whose toolchains this driver actually wires up appear here;
;; everything else reports "not runnable here" instead of faking a run.
(define runtime-executables
  (hash ".py" "python3"
        ".js" "node"
        ".jsx" "node"))

(define (extension-string extension)
  (cond
    [(bytes? extension) (bytes->string/utf-8 extension)]
    [(path? extension) (path->string extension)]
    [else (format "~a" extension)]))

(define (runtime-executable-for path)
  (define extension (path-get-extension path))
  (and extension
       (hash-ref runtime-executables (extension-string extension) #f)))

;; Run a program with an optional stdin string, capturing stdout+stderr merged
;; (stderr is directed onto the stdout stream so the order is the child's).
;; Returns (values exit-zero? combined-output). Temp files keep stdin and the
;; output out of this process's own streams and out of the repository.
(define (run-process executable argv stdin-string)
  (define stdin-path (make-temporary-file "racklr-self-emit-stdin-~a"))
  (define stdout-path (make-temporary-file "racklr-self-emit-stdout-~a"))
  (if stdin-string
      (display-to-file stdin-string stdin-path #:exists 'replace)
      (void))
  (dynamic-wind
    void
    (λ ()
      (define stdin-port (open-input-file stdin-path))
      (define stdout-port (open-output-file stdout-path #:exists 'replace))
      (dynamic-wind
        void
        (λ ()
          (define-values (proc _stdout-pipe _stdin-pipe _stderr-pipe)
            (apply subprocess stdout-port stdin-port 'stdout executable argv))
          (subprocess-wait proc)
          (define exit-code (subprocess-status proc))
          (close-input-port stdin-port)
          (close-output-port stdout-port)
          (values (equal? exit-code 0) (file->string stdout-path)))
        (λ ()
          (unless (port-closed? stdin-port)
            (close-input-port stdin-port))
          (unless (port-closed? stdout-port)
            (close-output-port stdout-port)))))
    (λ ()
      (for ([temporary (list stdin-path stdout-path)])
        (if (file-exists? temporary)
            (delete-file temporary)
            (void))))))

;; Run the source written at `path` in its target runtime. python3 runs .py;
;; node runs .js/.jsx. Returns (values success? combined-output); a target with
;; no wired-up runtime is a failed run whose output says exactly that, never a
;; silent skip. A missing source file or missing runtime executable is a
;; truthful failure too, not an exception to be mistaken for success.
(define (run-emitted-source path
                            #:stdin [stdin-string #f]
                            #:args [args '()])
  (unless (file-exists? path)
    (error 'run-emitted-source "emitted source does not exist: ~a" path))
  (define runtime (runtime-executable-for path))
  (cond
    [(not runtime)
     (values #f
             (format "no runtime wired up for emitted source ~a (targets that run here: Python via python3, JavaScript via node)\n"
                     path))]
    [(not (find-executable-path runtime))
     (values #f (format "target runtime not found on PATH: ~a\n" runtime))]
    [else
     (run-process (find-executable-path runtime)
                  (cons (path->string path) args)
                  stdin-string)]))

;; ── Self-image registry (the P6.1 seam) ─────────────────────────────

;; A registered compiler-as-UIR image: a name, a human description, and a
;; nullary builder returning the UIR program.
(struct self-image-entry (id description build) #:transparent)

(define self-images (make-hasheq))

(define (self-image-ids)
  (sort (hash-keys self-images) symbol<?))

(define (self-image-entry-for who id)
  (hash-ref self-images id
            (λ () (error who "unknown self-image: ~a; registered: ~a"
                         id (self-image-ids)))))

;; Register a self-image under `id`. The driver itself never registers one --
;; that is the caller's seam -- so the module works with any P6.1 image.
(define (register-self-image! id description build)
  (unless (symbol? id)
    (error 'register-self-image! "self-image id must be a symbol, got: ~a" id))
  (unless (and (string? description) (positive? (string-length description)))
    (error 'register-self-image! "description for '~a must be a non-empty string" id))
  (unless (procedure? build)
    (error 'register-self-image! "builder for '~a must be a procedure" id))
  (hash-set! self-images id (self-image-entry id description build))
  id)

(define (self-image-uir id)
  (define entry (self-image-entry-for 'self-image-uir id))
  (define uir ((self-image-entry-build entry)))
  (unless (uir? uir)
    (error 'self-image-uir "self-image '~a built a non-UIR value: ~a" id uir))
  uir)

(define (self-image-description id)
  (self-image-entry-description (self-image-entry-for 'self-image-description id)))

;; ── Serialized-UIR seam ─────────────────────────────────────────────

;; Load a self-image from disk. 'sexp accepts either the bare `uir->sexp` form
;; or the versioned `(uir <version> <payload>)` envelope; 'json uses the
;; versioned `uir->json` encoding. Both encodings come from racklr/uir, so a
;; P6.1 image can be emitted with no driver change.
(define (load-self-image-file path #:format [format 'sexp])
  (unless (file-exists? path)
    (error 'load-self-image-file "no such serialized UIR file: ~a" path))
  (define uir
    (case format
      [(sexp)
       (define payload (call-with-input-file path read))
       (cond
         [(eof-object? payload)
          (error 'load-self-image-file "serialized UIR file is empty: ~a" path)]
         [(and (pair? payload) (eq? (car payload) 'uir))
          (versioned-sexp->uir payload)]
         [else (sexp->uir payload)])]
      [(json) (json->uir (file->string path))]
      [else
       (error 'load-self-image-file
              "unsupported UIR format ~a; expected 'sexp or 'json" format)]))
  (unless (uir? uir)
    (error 'load-self-image-file "~a does not contain a UIR program" path))
  uir)

;; ── Emit, run, report ───────────────────────────────────────────────

;; Emit the self-image, run the emitted source if its runtime is wired up, and
;; report the outcome. Runtime failures (a nonzero exit, a runtime that is not
;; installed, a target with no runtime here) are reported in the hash, not
;; raised: only emit/write errors from `emit-self` escape.
(define (emit-self-and-run target uir out-dir
                           #:stdin [stdin-string #f]
                           #:filename [filename #f])
  (define path (emit-self target uir out-dir #:filename filename))
  (define source (file->string path))
  (define-values (ok? output)
    (with-handlers ([exn:fail?
                     (λ (e) (values #f (string-append (exn-message e) "\n")))])
      (run-emitted-source path #:stdin stdin-string)))
  (hash 'path path
        'target target
        'ok? ok?
        'output output
        'rows-native? (emitted-rows-native? target source)
        'source source))

(module+ main
  (displayln "racklr/self-emit — emit a compiler-as-UIR self-image into a target language."))
