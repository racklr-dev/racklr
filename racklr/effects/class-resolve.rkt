#lang racket

;; racklr/effects/class-resolve.rkt
;;
;; Cross-file class/superclass reference resolution: map a class name as
;; written in one file to the (component . real-class-name) pair that defines
;; it, following the import graph for aliases, and enumerate the method-callee
;; keys a `self.m()` / `cls.m()` / `super().m()` / `KnownClass.m()` call could
;; target. Consumed by the whole-program fixpoint; single-file inference does
;; not use these.

(require racklr/uir
         racklr/effects/imports
         racklr/effects/shape)

(provide resolve-class-component
         modpath-index
         alias-class-ref
         imported-class-ref
         resolve-super-ref
         mro-method-keys
         method-callee-keys
         bare-function-key
         corpus-call-return-type)

;; Component defining a class named `class-name`, from the point of view of
;; `comp` (the file making the reference). A local definition wins; a
;; globally-unique definition resolves; anything else is ambiguous → #f.
(define (resolve-class-component class-name comp class-defs)
  (define defs (remove-duplicates (hash-ref class-defs class-name '())))
  (cond
    [(null? defs) #f]
    [(member comp defs) comp]
    [(= (length defs) 1) (car defs)]
    [else #f]))

;; Inverted component→module-path index (dotted module path → component), used
;; to find the file that defines a class reached through an import alias.
(define (modpath-index import-env)
  (and import-env
       (let ([h (make-hash)])
         (for ([(c mp) (in-hash (import-env-comp-modpath import-env))])
           (hash-set! h mp c))
         h)))

;; An import alias that names a corpus class (`from x import C as Alias`), when
;; `name` has no corpus definition of its own. Returns (component . real-name).
(define (alias-class-ref name comp import-env mp->comp)
  (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
         [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
         [h (and e (hash-ref e (symbol->string name) #f))])
    (and (handle? h)
         (eq? (handle-kind h) 'class)
         (let* ([real-name (string->symbol (handle-name h))]
                [real-comp (hash-ref mp->comp (handle-path h) #f)])
           (and real-name real-comp (cons real-comp real-name))))))

;; A name bound in `comp`'s namespace (a local definition or an import) that
;; resolves to a class handle — including classes OUTSIDE the scanned corpus
;; (`from django.db.migrations.loader import MigrationLoader`). Returns the
;; handle's real class-name symbol, or #f. This is the receiver-TYPE half of
;; imported-constructor tracking: `x = MigrationLoader(...)` tracks `x` as
;; MigrationLoader, and the effect half re-resolves that name through the same
;; namespace.
(define (imported-class-ref name comp import-env)
  (and import-env name
       (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string name) #f))])
         (and (handle? h)
              (eq? (handle-kind h) 'class)
              (string->symbol (handle-name h))))))

;; Resolve a superclass reference (as written in the file) to the
;; (component . real-class-name) pair that defines it. A bare name with a corpus
;; definition resolves like `resolve-class-component` (local wins, unique wins,
;; ambiguous → #f); a bare name with no corpus definition is an import alias and
;; is followed through the import graph. A qualified reference
;; (`base.Serializer`) resolves its first component through the import graph to
;; a module, then the last component to a class in that module. External names
;; keep (cons #f name).
(define (resolve-super-ref ref comp class-defs import-env mp->comp)
  (cond
    [(pair? ref) (resolve-qualified-super-ref ref comp import-env mp->comp)]
    [else
     (define name ref)
     (define defs (hash-ref class-defs name '()))
     (cond
       [(and (pair? defs) (resolve-class-component name comp class-defs))
        => (λ (c) (cons c name))]
       [(and import-env (null? defs))
        (or (alias-class-ref name comp import-env mp->comp) (cons #f name))]
       [else (cons #f name)])]))

;; A qualified super reference `(first . full)` where `full` is the dotted name
;; as written (`base.Serializer`) and `first` its leftmost component (`base`).
;; `first` is a local name in the importing file: a module alias bound by an
;; import resolves to the module, whose exports supply the class; anything else
;; (a stdlib module or an unresolvable alias) is external → (cons #f class-name).
(define (resolve-qualified-super-ref ref comp import-env mp->comp)
  (define first (car ref))
  (define full-str (symbol->string (cdr ref)))
  (define class-name (string->symbol (last-component full-str)))
  (define modpath (and import-env (hash-ref (import-env-comp-modpath import-env) comp #f)))
  (define e (and modpath (hash-ref (import-env-exports import-env) modpath #f)))
  (define h (and e (hash-ref e (symbol->string first) #f)))
  (cond
    [(and h (eq? (handle-kind h) 'module) (handle-path h))
     (define mod-exports (hash-ref (import-env-exports import-env) (handle-path h) #f))
     (define ch (and mod-exports (hash-ref mod-exports (symbol->string class-name) #f)))
     (cond
       [(and ch (eq? (handle-kind ch) 'class) (handle-path ch))
        (define real-comp (hash-ref mp->comp (handle-path ch) #f))
        (and real-comp (cons real-comp (string->symbol (handle-name ch))))]
       [else (cons #f class-name)])]
    [(and h (eq? (handle-kind h) 'class) (handle-path h))
     (define real-comp (hash-ref mp->comp (handle-path h) #f))
     (and real-comp (cons real-comp (string->symbol (handle-name h))))]
    [else (cons #f class-name)]))

;; Ordered list of `(component . Class.method)` keys a `self.m()`/`cls.m()`/
;; `KnownClass.m()` call could target, most-derived first, following the
;; superclass chain across files. Stops at a superclass whose name is ambiguous
;; or unknown (such calls stay `unknown` rather than resolving to the wrong
;; file's class).
(define (mro-method-keys comp class-name mname methods class-defs comp-supers)
  (define seen (make-hash))
  (define out '())
  (let loop ([c comp] [cls class-name])
    (when (and c cls (not (hash-has-key? seen (cons c cls))))
      (hash-set! seen (cons c cls) #t)
      (define key (cons c (qualified-name-symbol cls mname)))
      (when (hash-has-key? methods key) (set! out (cons key out)))
      (for ([sr (in-list (hash-ref comp-supers (cons c cls) '()))])
        (define c2 (car sr))
        (define s (cdr sr))
        (when (and c2 s) (loop c2 s)))))
  (reverse out))

;; `(component . name)` keys a method callee could resolve to (the whole-program
;; counterpart of `method-callee-qualified-names`).
(define (method-callee-keys callee comp owner methods class-defs comp-supers)
  (cond
    [(not (uir-get? callee)) '()]
    [else
     (let* ([mname (get-field-symbol (uir-get-field callee))]
            [base (uir-get-base callee)])
       (cond
         [(and mname (super-call? base))
          ;; super().m() → each parent of the enclosing class, resolved to the
          ;; file that defines it (most-derived first).
          (if owner
              (append-map
               (λ (sr)
                 (let* ([c2 (car sr)] [s (cdr sr)])
                   (if (and c2 s) (mro-method-keys c2 s mname methods class-defs comp-supers) '())))
               (hash-ref comp-supers (cons comp owner) '()))
              '())]
         [(and mname (uir-var? base))
          (let ([base-name (string->symbol (uir-symbol-name (uir-var-name base)))])
            (cond
              [(memq base-name '(self cls))
               (or (and owner (mro-method-keys comp owner mname methods class-defs comp-supers)) '())]
              [else
               (define c (resolve-class-component base-name comp class-defs))
               (or (and c (mro-method-keys c base-name mname methods class-defs comp-supers)) '())]))]
         [else '()]))]))

;; `(component . name)` key for a bare function call `f(...)`: the local
;; definition wins, else a globally-unique definition, else #f (ambiguous).
(define (bare-function-key nm comp fn-defs known)
  (cond
    [(not nm) #f]
    [else
     (define local (cons comp nm))
     (define defs (remove-duplicates (hash-ref fn-defs nm '())))
     (cond
       [(hash-has-key? known local) local]
       [(and (= (length defs) 1) (pair? defs))
        (define k (cons (car defs) nm))
        (and (hash-has-key? known k) k)]
       [else #f])]))

;; Projected result class of a corpus function/method call
;; (`get_cookie_signer(...)`, `self.method()`), resolved through the
;; return-type projection table built by whole-program's pass 2. A bare
;; function call resolves through its unique definition key; a method call
;; resolves through its candidate MRO keys and projects only when every
;; candidate key WITH a projected type agrees on one class (sound: a mixed
;; MRO is not projected).
(define (corpus-call-return-type callee comp class-defs owner methods fn-defs known comp-supers return-types)
  (and return-types
       (let ([nm (callee-name callee)])
         (or (and nm (let ([k (bare-function-key nm comp fn-defs known)])
                       (and k (hash-ref return-types k #f))))
             (let ([mks (method-callee-keys callee comp owner methods class-defs comp-supers)])
               (and (pair? mks)
                    (let loop ([mks mks] [t #f])
                      (cond
                        [(null? mks) t]
                        [else
                         (define kt (hash-ref return-types (car mks) #f))
                         (cond
                           [(not kt) (loop (cdr mks) t)]
                           [(not t) (loop (cdr mks) kt)]
                           [(eq? kt t) (loop (cdr mks) t)]
                           [else #f])]))))))))
