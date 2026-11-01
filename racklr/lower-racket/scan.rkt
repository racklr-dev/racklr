#lang racket

;; racklr/lower-racket/scan.rkt
;;
;; First pass of the self-host front end: scan every Racket module's datum
;; stream into the metadata the lowering pass and the name resolver need.
;;
;;   - module id: source path relative to the repo root, without ".rkt"
;;     (e.g. racklr/lower-python/helpers). Top-level definitions are emitted
;;     under the mangled name "<module id>::<name>" so cross-file calls resolve
;;     as call-graph edges in the shared inference fixpoint.
;;   - require clauses: normalized import specs (plain / only-in / except-in /
;;     rename-in / prefix-in) over a corpus module id or an external library
;;     (recorded as #f and left to the effect-token lexicon).
;;   - provide specs: normalized exports, including all-from-out,
;;     all-defined-out, struct-out, rename-out and prefix-out.
;;   - defines: local name -> fn / value / struct / generated (struct-generated
;;     constructor, predicate, accessor and setter names).
;;   - structs: struct name -> field names.
;;
;; A module+ / module submodule's definitions are scanned into the same tables:
;; they live in the module's namespace and are lowered with the module, but
;; they are not exported unless a provide names them.

(require racket/list
         racket/path
         racket/string
         "reader.rkt")

(provide module-info? module-info-id module-info-path module-info-datums
         module-info-requires module-info-provides module-info-defines
         module-info-structs
         scan-module
         scan-module-datums
         module-id-of-path
         struct-generated-names
         describe-require)

;; id: module path symbol; path: source path; datums: (listof datum);
;; requires: (listof require-clause); provides: (listof provide-spec);
;; defines: hash name-symbol -> kind; structs: hash name-symbol -> (listof field-symbol).
(struct module-info (id path datums requires provides defines structs) #:transparent)

;; require-clause = (list 'plain source) | (list 'only source (cons local remote) ...)
;;                | (list 'except source name ...) | (list 'rename source (cons local remote) ...)
;;                | (list 'prefix source prefix-string)
;; source: module id symbol (corpus) or #f (external / unresolved).
;; provide-spec   = (list 'name name) | (list 'all-from-out source)
;;                | (list 'all-defined-out) | (list 'struct-out struct-name)
;;                | (list 'rename-out (cons old new) ...) | (list 'prefix-out prefix spec ...)

;; ── module identity ────────────────────────────────────────────────

;; Absolute source path + repo root → module id symbol: relative path without
;; the ".rkt" suffix, with "/" separators.
(define (module-id-of-path path root)
  (define rel (find-relative-path (simplify-path root) (simplify-path path)))
  (define str (path->string rel))
  (define no-ext (regexp-replace #rx"[.]rkt$" str ""))
  (string->symbol no-ext))

;; Is `abs` inside (or equal to) `root`?
(define (path-under-root? abs root)
  (define rel (find-relative-path (simplify-path root) (simplify-path abs)))
  (equal? (simplify-path (build-path (simplify-path root) rel)) (simplify-path abs)))

;; ── entry point ────────────────────────────────────────────────────

(define (scan-module path root)
  (define path* (simplify-path (if (path? path) path (string->path path))))
  (define root* (simplify-path root))
  (define datums (read-file-datums path*))
  (define id (module-id-of-path path* root*))
  (scan-module-datums datums id path* root*))

;; Scan an already-read datum stream into a module-info. Used by the single
;; source-string entry point, which has no file path.
(define (scan-module-datums datums id path root)
  (define cur-dir (if (path? path) (path-only path) (current-directory)))
  (define root* (simplify-path root))
  (define defines (make-hash))
  (define structs (make-hash))
  (define requires '())
  (define provides '())
  (define (scan-forms! forms)
    (for ([form (in-list forms)]) (scan-form! form)))
  (define (scan-form! form)
    (if (not (and (pair? form) (symbol? (car form))))
        (void)
        (case (car form)
          [(require)
           (set! requires
                 (append requires
                         (append-map (λ (s) (parse-require s cur-dir root*)) (cdr form))))]
          [(provide)
           (set! provides
                 (append provides
                         (append-map (λ (s) (parse-provide s cur-dir root*)) (cdr form))))]
          [(define) (scan-define! defines form)]
          [(define-values) (scan-define-values! defines form)]
          [(define/match) (scan-define-match! defines form)]
          [(struct) (scan-struct! defines structs form)]
          [(define-syntax define-syntax-rule)
           (define nm (and (pair? (cdr form)) (second form)))
           (if (and (symbol? nm) (not (memq nm (hash-keys defines)))) (hash-set! defines nm 'syntax) (void))]
          [(module) (scan-forms! (cdddr form))]
          [(module+ begin) (scan-forms! (cddr form))]
          [else (void)])))
  (scan-forms! datums)
  (module-info id path datums requires provides defines structs))

;; ── defines ────────────────────────────────────────────────────────

(define (scan-define! defines form)
  (define target (second form))
  (cond
    [(pair? target)
     ;; (define (name args ...) body ...) or curried (define ((n a) b) ...)
     (define nm (if (pair? (car target)) (car (car target)) (car target)))
     (if (symbol? nm) (hash-set! defines nm 'fn) (void))]
    [(symbol? target)
     (hash-set! defines target
                (if (and (pair? (cddr form)) (lambda-valued? (third form))) 'fn 'value))]
    [else (void)]))

(define (lambda-valued? v)
  (and (pair? v) (memq (car v) '(lambda λ))))

(define (scan-define-values! defines form)
  (for ([n (in-list (names-of-binding-form (second form)))])
    (hash-set! defines n 'value))
  (void))

(define (scan-define-match! defines form)
  (define target (second form))
  (define nm (cond [(pair? target) (car target)] [(symbol? target) target] [else #f]))
  (if (symbol? nm) (hash-set! defines nm 'fn) (void)))

;; Flatten a binding target (`(a b)`, `(a . rest)`, symbol) to name symbols.
(define (names-of-binding-form b)
  (cond
    [(symbol? b) (list b)]
    [(pair? b) (append (names-of-binding-form (car b)) (names-of-binding-form (cdr b)))]
    [else '()]))

;; ── structs ────────────────────────────────────────────────────────

(define (scan-struct! defines structs form)
  (define nm (second form))
  (if (symbol? nm)
      (let* ([has-fields? (and (pair? (cddr form)) (list? (third form)))]
             [fields (if has-fields? (third form) '())]
             [options (if has-fields? (cdddr form) (cddr form))])
        (hash-set! structs nm (filter symbol? fields))
        (for ([(gen _) (in-hash (struct-generated-names nm fields options))])
          (hash-set! defines gen 'generated))
        (void))
      (void)))

;; Struct name, fields and options → hash of generated name -> one of
;; 'constructor / 'predicate / 'accessor / 'setter. The constructor is defined
;; by the `struct` form too; the lowering pass models it as the class
;; constructor (a pure instantiation), the rest as pure generated functions.
(define (struct-generated-names name fields options)
  (define out (make-hash))
  (define (option-value kw) (for/or ([o (in-list options)]) (and (eq? o kw) #t)))
  (define ctor-name (keyword-following options '#:constructor-name))
  (define extra-ctors
    (for/list ([i (in-range (length options))]
               #:when (eq? (list-ref options i) '#:extra-constructor-name)
               #:when (< (add1 i) (length options)))
      (list-ref options (add1 i))))
  (define mutable? (option-value '#:mutable))
  (hash-set! out (or ctor-name name) 'constructor)
  (for ([c (in-list extra-ctors)] #:when (symbol? c)) (hash-set! out c 'constructor))
  (hash-set! out (string->symbol (format "~a?" name)) 'predicate)
  (for ([f (in-list fields)] #:when (symbol? f))
    (hash-set! out (string->symbol (format "~a-~a" name f)) 'accessor)
    (if mutable?
      (hash-set! out (string->symbol (format "set-~a-~a!" name f)) 'setter)  (void)))
  out)

(define (keyword-following options kw)
  (for/or ([i (in-range (length options))]
           #:when (eq? (list-ref options i) kw)
           #:when (< (add1 i) (length options)))
    (let ([v (list-ref options (add1 i))])
      (and (symbol? v) v))))

;; ── requires ───────────────────────────────────────────────────────

(define (parse-require spec cur-dir root)
  (cond
    [(symbol? spec) (list (list 'plain (require-spec-source spec cur-dir root)))]
    [(path-string? spec) (list (list 'plain (require-spec-source spec cur-dir root)))]
    [(not (pair? spec)) '()]
    [else
     (case (car spec)
       [(file) (list (list 'plain (require-spec-source (second spec) cur-dir root)))]
       [(only-in) (append (list (append (list 'only (require-spec-source (second spec) cur-dir root))
                                        (parse-only-ids (cddr spec)))))]
       [(except-in) (list (append (list 'except (require-spec-source (second spec) cur-dir root))
                                  (cddr spec)))]
       [(rename-in) (list (append (list 'rename (require-spec-source (second spec) cur-dir root))
                                  (for/list ([r (in-list (cddr spec))])
                                    (if (and (pair? r) (symbol? (car r)) (pair? (cdr r)))
                                        (cons (car r) (cadr r))
                                        (cons r r)))))]
       [(prefix-in)
        ;; (prefix-in prefix module-spec)
        (define pfx (and (symbol? (second spec)) (symbol->string (second spec))))
        (list (list 'prefix (require-spec-source (third spec) cur-dir root) pfx))]
       [(all-except)
        (append (parse-require (second spec) cur-dir root)
                (list (append (list 'except (require-spec-source (second spec) cur-dir root))
                              (cddr spec))))]
       [(for-syntax for-template protect-out) (parse-require (second spec) cur-dir root)]
       [(for-meta only-meta-in)
        (if (pair? (cddr spec)) (parse-require (third spec) cur-dir root) '())]
       [else '()])]))

;; `only-in` entries: id | (rename old new) | (prefix-in p id) | (struct-out s)
(define (parse-only-ids specs)
  (for/list ([s (in-list specs)])
    (cond
      [(symbol? s) (cons s s)]
      [(and (pair? s) (eq? (car s) 'rename) (pair? (cdr s)) (pair? (cddr s)))
       (cons (cadr s) (caddr s))]
      [(and (pair? s) (eq? (car s) 'prefix-in) (pair? (cdr s)) (pair? (cddr s)))
       (cons (string->symbol (format "~a~a" (cadr s) (caddr s))) (caddr s))]
      [else (cons s s)])))

;; A require source spec → corpus module id symbol, or #f for an external
;; library (left to the effect-token lexicon). A symbol that does not name a
;; corpus module also returns #f; the resolver decides using the scanned set.
(define (require-spec-source spec cur-dir root)
  (cond
    [(symbol? spec) spec]
    [(path-string? spec)
     (define abs (simplify-path (build-path cur-dir spec)))
     (and (path-under-root? abs root) (module-id-of-path abs root))]
    [(and (pair? spec) (eq? (car spec) 'lib) (pair? (cdr spec)))
     (require-spec-source (cadr spec) cur-dir root)]
    [else #f]))

(define (describe-require clause)
  (define source (and (pair? clause) (cadr clause)))
  (format "~a ~a" (if (pair? clause) (car clause) '?) (or source 'external)))

;; ── provides ───────────────────────────────────────────────────────

(define (parse-provide spec cur-dir root)
  (cond
    [(symbol? spec) (list (list 'name spec))]
    [(not (pair? spec)) '()]
    [else
     (case (car spec)
       [(all-from-out)
        (list (list 'all-from-out (require-spec-source (second spec) cur-dir root)))]
       [(all-defined-out) (list (list 'all-defined-out))]
       [(struct-out) (list (list 'struct-out (second spec)))]
       [(rename-out)
        (list (append (list 'rename-out)
                      (for/list ([r (in-list (cdr spec))])
                        (if (and (pair? r) (symbol? (car r)) (pair? (cdr r)))
                            (cons (car r) (cadr r))
                            (cons r r)))))]
       [(prefix-out)
        (list (list 'prefix-out (second spec) (parse-provide (third spec) cur-dir root)))]
       [(combine-out) (append-map (λ (s) (parse-provide s cur-dir root)) (cdr spec))]
       [(for-syntax for-template protect-out contract-out) (parse-provide (second spec) cur-dir root)]
       [(for-meta only-meta-in)
        (if (pair? (cddr spec)) (parse-provide (third spec) cur-dir root) '())]
       [else '()])]))
