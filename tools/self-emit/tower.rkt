#lang racket

;; tools/self-emit/tower.rkt — the P6.2 seam for the P6.3 acceptance run.
;;
;; P6.2 (issue #39) emits a compiler-as-UIR program — the seed self-image — into
;; a target language and runs it. This module is P6.3's differential against
;; that artifact:
;;
;;   1. emit the self-image to Python once;
;;   2. run the emitted compiler on the seed-language corpus and compare every
;;      translation with the original's pinned translation contract
;;      (racklr/self/seed/compiler: kernel-sample-* plus the documented
;;      behavior table);
;;   3. run the compiled programs and compare their output with the reference;
;;   4. check the emitted compiler carries its rows in Python's native
;;      vocabulary;
;;   5. read the emitted compiler back through the tower (parse → lower →
;;      infer) and require every named function to carry a row — the reflexive
;;      half of the gate.
;;
;; P6.2 is not merged on main yet, so this module treats its absence as a
;; reported state instead of a crash: `tower-available?` is #f and the report
;; names the missing modules. Once the P6.2 branch is present (P6.3 stacks on
;; it), the full differential runs.

(require json
         racket/file
         racket/list
         racket/path
         racket/string
         racket/system
         racklr/uir
         "canonical.rkt"
         "gate.rkt")

(provide tower-available?
         self-image-availability
         tower-corpus
         extract-emitted-rows
         self-image-row-map
         run-tower-differential)

;; ── availability ────────────────────────────────────────────────────

(define self-emit-module 'racklr/self-emit)
(define seed-module 'racklr/self/seed/compiler)

(define (module-present? path)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (dynamic-require path #f)
    #t))

;; The P6.2 modules the differential needs. The report names which are absent
;; rather than pretending the emitted tower ran.
(define (self-image-availability)
  (hash 'self-emit (module-present? self-emit-module)
        'seed (module-present? seed-module)))

(define (tower-available?)
  (define availability (self-image-availability))
  (and (hash-ref availability 'self-emit) (hash-ref availability 'seed)))

;; ── the corpus and its reference outputs ────────────────────────────

;; (listof (hash 'source 'expected-compiled 'expected-result)). The first case
;; is the seed compiler's own sample contract; the rest are the documented
;; translation table. `expected-compiled` is the translation the original
;; produces; `expected-result` is what that compiled program prints when run
;; (declared for the cases where the reference states one).
(define (tower-corpus)
  (define sample-source (dynamic-require seed-module 'kernel-sample-source))
  (define sample-compiled (dynamic-require seed-module 'kernel-sample-compiled))
  (define sample-result (dynamic-require seed-module 'kernel-sample-result))
  (list
   (hash 'source sample-source
         'expected-compiled sample-compiled
         'expected-result sample-result)
   (hash 'source "(- 10 4)" 'expected-compiled "print((10 - 4))" 'expected-result "6")
   (hash 'source "(* 2 (+ 3 4))" 'expected-compiled "print((2 * (3 + 4)))" 'expected-result "14")
   (hash 'source "(foo 1 2)" 'expected-compiled "print(foo(1, 2))" 'expected-result #f)
   (hash 'source "(+ 1 2 3)" 'expected-compiled "print(+(1, 2, 3))" 'expected-result #f)
   (hash 'source "42" 'expected-compiled "print(42)" 'expected-result "42")
   (hash 'source "hello" 'expected-compiled "print(hello)" 'expected-result #f)
   (hash 'source "(+ 1 2) (* 3 4)"
         'expected-compiled "print((1 + 2))\nprint((3 * 4))"
         'expected-result #f)))

;; ── the emitted rows ────────────────────────────────────────────────
;;
;; The emit contract reifies the self-image's effect rows in Python's native
;; vocabulary: one `@__withEffects(<row JSON>)` guard immediately above each
;; emitted function. Reading those guards back and comparing them with the
;; original UIR program's rows is the UIR-plus-row half of the differential.

(define emitted-guard-rx
  #px"(?m:^[ \t]*@__withEffects\\((\\{[^\n]*\\})\\)[ \t]*\n[ \t]*def[ \t]+([A-Za-z_][A-Za-z0-9_]*))")

;; One entry object of a Python guard row → the canonical entry shape
;; canonical.rkt uses, so the comparison is order- and format-insensitive.
(define (guard-entry->canonical effect-name dict)
  (unless (hash? dict)
    (error 'extract-emitted-rows "guard entry for ~a is not an object: ~s" effect-name dict))
  (define modality (hash-ref dict 'modality "may"))
  (define ops (hash-ref dict 'ops '()))
  (hasheq 'effect (if (symbol? effect-name) (symbol->string effect-name) effect-name)
          'modality (if (string? modality) modality (format "~a" modality))
          'ops (sort (if (list? ops) ops '()) string<?)
          'budget (hash-ref dict 'max 'null)
          'scope (hash-ref dict 'scope 'null)))

(define (guard-json->row-entries json-text)
  (define parsed (string->jsexpr json-text))
  (unless (hash? parsed)
    (error 'extract-emitted-rows "guard row is not a JSON object: ~s" json-text))
  (sort (for/list ([(effect-name dict) (in-hash parsed)])
          (guard-entry->canonical effect-name dict))
        string<?
        #:key (λ (entry) (hash-ref entry 'effect))))

;; Map of emitted function name → canonical row entries, read from the native
;; `@__withEffects(...)` guards in the emitted compiler source.
(define (extract-emitted-rows source)
  (for/hash ([match (in-list (regexp-match* emitted-guard-rx source #:match-select values))])
    (values (string->symbol (third match))
            (guard-json->row-entries (second match)))))

;; Map of original self-image function name → canonical row entries, read from
;; the UIR program the tower emitted. This is the row side of "same output as
;; the original".
(define (self-image-row-map)
  (define program ((dynamic-require seed-module 'kernel-compiler-uir)))
  (define stmts (if (uir-block? program) (uir-block-stmts program) (list program)))
  (for/hash ([stmt (in-list stmts)]
             #:when (and (uir-fn? stmt) (uir-fn-name stmt)))
    (values (string->symbol (uir-symbol-name (uir-fn-name stmt)))
            (row->jsexpr (or (uir-fn-effects stmt) (uir-effect-row '()))))))

;; ── subprocess helpers ──────────────────────────────────────────────

(define (read-or-empty path)
  (with-handlers ([exn:fail:filesystem? (λ (_) "")])
    (file->string path)))

;; Run `executable file-path` with `stdin-text` on stdin. Returns
;; (values ok? stdout stderr). Temp files keep the run out of our streams and
;; out of the repository.
(define (run-file executable file-path stdin-text)
  (define stdin-file (make-temporary-file "racklr-self-emit-in-~a"))
  (define stdout-file (make-temporary-file "racklr-self-emit-out-~a"))
  (define stderr-file (make-temporary-file "racklr-self-emit-err-~a"))
  (display-to-file stdin-text stdin-file #:exists 'replace)
  (dynamic-wind
    void
    (λ ()
      (define stdin-port (open-input-file stdin-file))
      (define stdout-port (open-output-file stdout-file #:exists 'replace))
      (define stderr-port (open-output-file stderr-file #:exists 'replace))
      (dynamic-wind
        void
        (λ ()
          (define code
            (parameterize ([current-input-port stdin-port]
                           [current-output-port stdout-port]
                           [current-error-port stderr-port])
              (system*/exit-code executable (path->string file-path))))
          (close-input-port stdin-port)
          (close-output-port stdout-port)
          (close-output-port stderr-port)
          (values (zero? code)
                  (read-or-empty stdout-file)
                  (read-or-empty stderr-file)))
        (λ ()
          (unless (port-closed? stdin-port) (close-input-port stdin-port))
          (unless (port-closed? stdout-port) (close-output-port stdout-port))
          (unless (port-closed? stderr-port) (close-output-port stderr-port)))))
    (λ ()
      (for ([temporary (list stdin-file stdout-file stderr-file)])
        (when (file-exists? temporary) (delete-file temporary))))))

(define python-executable
  (or (find-executable-path "python3") "python3"))

(define (trim-output s) (string-trim s #rx"[\r\n]+"))

;; Run a Python file (the emitted compiler, or the program it produced) with
;; source text on stdin.
(define (run-python py-path stdin-text)
  (run-file python-executable py-path stdin-text))

;; ── the differential ────────────────────────────────────────────────

(define (emit-tower out-dir #:target [target 'python])
  (define emit-self (dynamic-require self-emit-module 'emit-self))
  (define image-builder (dynamic-require seed-module 'kernel-compiler-uir))
  (emit-self target (image-builder) out-dir))

(define (check-case emitted-path temp c)
  (define source (hash-ref c 'source))
  (define expected (hash-ref c 'expected-compiled))
  (define-values (ok? stdout stderr)
    (run-python emitted-path (string-append source "\n")))
  (define translated (trim-output stdout))
  (define match? (and ok? (string=? translated expected)))
  (define expected-result (hash-ref c 'expected-result))
  ;; A compiled program is checked only when the reference declares a result
  ;; and the translation itself already matched.
  (define result-ok?
    (cond
      [(not expected-result) #t]
      [(not match?) #f]
      [else
       (define result-file (build-path temp "compiled-program.py"))
       (display-to-file translated result-file #:exists 'replace)
       (define-values (rok? rout _rerr) (run-python result-file ""))
       (and rok? (string=? (trim-output rout) expected-result))]))
  (hash 'source source
        'ok? ok?
        'match-compiled? match?
        'match-result? result-ok?
        'output translated
        'expected expected
        'stderr stderr))

;; The full report. `('available? . #f)` carries the modules that are missing;
;; otherwise the report has the emitted path, the native-row check, the
;; reflexive absorb result, every corpus case, and a top-level `ok?`.
(define (run-tower-differential #:target [target 'python])
  (define availability (self-image-availability))
  (cond
    [(not (tower-available?)) (hash 'available? #f 'availability availability)]
    [else
     (define temp (make-temporary-file "racklr-self-emit-tower-~a" 'directory))
     (dynamic-wind
       void
       (λ ()
         (define emitted (emit-tower temp #:target target))
         (define emitted-source (file->string emitted))
         (define rows-native?
           ((dynamic-require self-emit-module 'emitted-rows-native?) target emitted-source))
         (define absorb (absorb-tower-source 'python emitted))
         (define expected-rows (self-image-row-map))
         (define emitted-rows (extract-emitted-rows emitted-source))
         (define rows-match? (equal? expected-rows emitted-rows))
         (define cases
           (for/list ([c (in-list (tower-corpus))]) (check-case emitted temp c)))
         (hash 'available? #t
               'target target
               'emitted-path (path->string emitted)
               'rows-native? rows-native?
               'rows-match? rows-match?
               'expected-rows (sort (hash->list expected-rows) string<? #:key (λ (p) (symbol->string (car p))))
               'emitted-rows (sort (hash->list emitted-rows) string<? #:key (λ (p) (symbol->string (car p))))
               'absorb absorb
               'cases cases
               'ok? (and rows-native?
                         rows-match?
                         (hash-ref absorb 'ok?)
                         (for/and ([c (in-list cases)])
                           (and (hash-ref c 'match-compiled?)
                                (hash-ref c 'match-result?))))))
       (λ ()
         (when (directory-exists? temp) (delete-directory/files temp))))]))
