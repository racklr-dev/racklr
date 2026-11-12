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
         racklr/effects/shape
         racklr/effects/library-tokens
         racklr/effects/lexicon)

(provide resolve-class-component
         modpath-index
         alias-class-ref
         imported-class-ref
         resolve-super-ref
         mro-method-keys
         method-callee-keys
         bare-function-key
         module-function-key
         corpus-call-return-type
         call-element-spec
         imported-module-class-ref
         imported-binding-module-path
         member-access-fields
         mro-property-return-type)

;; Component defining a class named `class-name`, from the point of view of
;; `comp` (the file making the reference). A local definition wins; a
;; globally-unique definition resolves; anything else is ambiguous → #f.
(define (resolve-class-component class-name comp class-defs [import-env #f])
  (define defs (remove-duplicates (hash-ref class-defs class-name '())))
  (cond
    [(null? defs) #f]
    [(member comp defs) comp]
    [(= (length defs) 1) (car defs)]
    ;; A module-path-qualified class name (`django.core.serializers.base.Serializer`)
    ;; disambiguates a bare class name that several scanned components define
    ;; (`Serializer` in base/python/xml/json/jsonl/pyyaml). Resolve the dotted
    ;; prefix through the module-path index to the defining component, then
    ;; confirm that component actually defines the class.
    [(and import-env
          (let ([s (symbol->string class-name)])
            (string-contains? s ".")))
     (let* ([s (symbol->string class-name)]
            [parts (string-split s ".")]
            [cls (and (pair? parts) (string->symbol (last parts)))]
            [modpath (and (pair? parts) (string-join (drop-right parts 1) "."))]
            [mp->comp (modpath-index import-env)]
            [c (and modpath mp->comp (hash-ref mp->comp modpath #f))])
       (and c cls (member c (hash-ref class-defs cls '())) c))]
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
(define (method-callee-keys callee comp owner methods class-defs comp-supers
                           [tokens default-effect-tokens])
  (cond
    [(not (member-access-callee? callee)) '()]
    [else
     (let* ([mname (member-access-field callee)]
            [base (member-access-base callee)]
            [base-name (bare-name-symbol base)])
       (cond
         [(and mname (or (super-call? base)
                         ;; JS/TS `super.m()`: the `super` keyword lowers to a
                         ;; bare symbol, unlike the Python `super()` invocation.
                         (eq? base-name 'super)))
          ;; super().m() → each parent of the enclosing class, resolved to the
          ;; file that defines it (most-derived first).
          (if owner
              (append-map
               (λ (sr)
                 (let* ([c2 (car sr)] [s (cdr sr)])
                   (if (and c2 s) (mro-method-keys c2 s mname methods class-defs comp-supers) '())))
               (hash-ref comp-supers (cons comp owner) '()))
              '())]
         [(and mname base-name)
          (cond
            ;; The host's implicit receiver name (Python `self`/`cls`, JS `this`)
            ;; dispatches through the enclosing class's MRO.
            [(tokens-self-name? tokens base-name)
             (or (and owner (mro-method-keys comp owner mname methods class-defs comp-supers)) '())]
            [else
             (define c (resolve-class-component base-name comp class-defs))
             (or (and c (mro-method-keys c base-name mname methods class-defs comp-supers)) '())])]
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
;; Project the return type of a METHOD call from its candidate MRO keys:
;; project only when every candidate key WITH a projected type agrees on one
;; class (sound: a mixed MRO is not projected).
(define (project-method-return-type mks return-types)
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
              [else #f])]))))

;; return-type projection table built by whole-program's pass 2. A bare
;; function call resolves through its unique definition key; a method call
;; resolves through its candidate MRO keys and projects only when every
;; candidate key WITH a projected type agrees on one class (sound: a mixed
;; MRO is not projected). `#:receiver-class` (optional) resolves a
;; member-access receiver through the CALLER's receiver-type env — a param /
;; local / self-attr / chained value — so `message.message()` (param typed
;; EmailMessage) and `self.fetch_command(x)` project their corpus return types
;; the same way a bare `self.m()` does. The caller supplies the resolution to
;; avoid a receiver-chain dependency cycle.
(define (corpus-call-return-type callee comp class-defs owner methods fn-defs known comp-supers return-types
                                 [tokens default-effect-tokens]
                                 #:receiver-class [rcfn #f])
  (and return-types
       (let ([nm (callee-name callee)])
         (or (and nm (let ([k (bare-function-key nm comp fn-defs known)])
                       (and k (hash-ref return-types k #f))))
             (let ([mks (or (and rcfn (member-access-callee? callee)
                                 (let* ([mname (member-access-field callee)]
                                        [rcls (and mname (rcfn (member-access-base callee)))]
                                        [rc (and rcls
                                                 (resolve-class-component rcls comp class-defs))])
                                   (and rc rcls mname
                                        (mro-method-keys rc rcls mname methods class-defs comp-supers))))
                            (method-callee-keys callee comp owner methods class-defs comp-supers tokens))])
               (project-method-return-type mks return-types))))))


;; `(component . func)` key for a module-qualified corpus function call
;; (`mail.get_connection(...)`, `timezone.now()`): the root resolves through
;; the current file's import namespace to a module handle, and `func` is a bare
;; function defined in that module. Returns the known corpus fn key, or #f.
;; This is the RESULT-half mirror of `imported-fn-effect`'s effect half: a
;; corpus module's function call must project its declared/derived return type
;; through the same call chain, instead of only resolving unscanned imports.
(define (module-function-key root func comp import-env fn-defs known)
  (and import-env comp root func
       (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string root) #f))])
         (and (handle? h) (eq? (handle-kind h) 'module) (handle-path h)
              (let* ([target-mod (handle-path h)]
                     [defs (remove-duplicates (hash-ref fn-defs func '()))])
                (for/or ([c (in-list defs)])
                  (define cm (hash-ref (import-env-comp-modpath import-env) c #f))
                  (and cm (equal? cm target-mod)
                       (let ([k (cons c func)])
                         (and (hash-has-key? known k) k)))))))))

;; (module-path . qualified-name) candidate keys an iterable call's callee
;; could declare an element type under: corpus method keys (Class.method),
;; corpus bare/module-qualified function keys, and the imported-handle key
;; (root.func for a singleton binding like `engines.all`, or bare func for a
;; module handle like `concurrent.futures.as_completed`).
(define (callee-element-keys callee comp owner class-defs methods fn-defs known comp-supers import-env)
  (define out '())
  (define (add! modpath qn) (when (and modpath qn) (set! out (cons (cons modpath qn) out))))
  (for ([k (in-list (method-callee-keys callee comp owner methods class-defs comp-supers))])
    (add! (and import-env (hash-ref (import-env-comp-modpath import-env) (car k) #f)) (cdr k)))
  (define nm (callee-name callee))
  (let ([bk (and nm (bare-function-key nm comp fn-defs known))])
    (add! (and bk import-env (hash-ref (import-env-comp-modpath import-env) (car bk) #f)) nm))
  (when (and (member-access-callee? callee) (dotted-root callee) (effect-op-of-callee callee))
    (let ([mk (module-function-key (dotted-root callee) (effect-op-of-callee callee) comp import-env fn-defs known)])
      (when mk
        (add! (and import-env (hash-ref (import-env-comp-modpath import-env) (car mk) #f))
              (cdr mk)))))
  (when import-env
    (let* ([root (dotted-root callee)]
           [func (and (member-access-callee? callee) (effect-op-of-callee callee))]
           [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
           [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
           [h (and e root (hash-ref e (symbol->string root) #f))])
      (when (and h (handle-path h))
        (add! (handle-path h)
              (if (handle-name h)
                  (if func (string->symbol (format "~a.~a" (handle-name h) func))
                      (string->symbol (handle-name h)))
                  func)))))
  (reverse out))

;; ELEMENT SPEC of an iterable call, from the declared element-types lexicon
;; (library-config DATA): the first (module-path . qualified-name) key with an
;; entry. A symbol = single element class (possibly a module-qualified marker,
;; so an external framework class resolves to its module's domain); a list =
;; tuple-destructuring spec. #f = no declared element type.
(define (call-element-spec callee comp owner class-defs methods fn-defs known comp-supers import-env element-types)
  (and element-types
       (for/or ([k (in-list (callee-element-keys callee comp owner class-defs methods fn-defs known comp-supers import-env))])
         (hash-ref element-types k #f))))

;; ── imported receiver-TYPE projection ────────────────────────────────

;; Effective MODULE PATH of an import binding used as a receiver root. A plain
;; `import pkg.mod` binds a module handle on the dotted path; a `from pkg import
;; submod` binding for a lowercase member is synthesized as an fn handle whose
;; path is the PARENT package and whose name is the submodule, so its effective
;; module path is parent.submod (`from email import generator` →
;; "email.generator"). The discriminator is the handle name vs the path's last
;; component, NOT the local binding name: `from PIL import ImageFile as
;; PillowImageFile` aliases the local away from the submodule name, but the
;; handle still records path "PIL" + name "ImageFile", so appending the name
;; must still reconstruct "PIL.ImageFile". A lowercase class imported from its
;; own module (`from datetime import datetime`) keeps its path: its name IS the
;; path's last component, so appending would invent "datetime.datetime". The
;; local alias (root) is irrelevant to both cases.
(define (imported-binding-module-path h root)
  (and (handle? h) (handle-path h)
       (cond
         [(eq? (handle-kind h) 'module) (handle-path h)]
         [(and (eq? (handle-kind h) 'fn) (handle-name h)
               (not (string=? (handle-name h) (last-component (handle-path h)))))
          (string-append (handle-path h) "." (handle-name h))]
         [else (handle-path h)])))

;; Dotted field path of a member-access expression, root first:
;; `concurrent.futures.ThreadPoolExecutor` → (concurrent futures ThreadPoolExecutor).
;; Returns #f when any link is not a plain attribute access.
(define (member-access-fields c)
  (cond
    [(uir-get? c)
     (define f (get-field-symbol (uir-get-field c)))
     (define rest (member-access-fields (uir-get-base c)))
     (and f rest (append rest (list f)))]
    [(uir-var? c) (list (string->symbol (uir-symbol-name (uir-var-name c))))]
    [(uir-symbol? c) (list (string->symbol (uir-symbol-name c)))]
    [else #f]))

;; Receiver TYPE named by a module-qualified CLASS reference (`smtplib.SMTP`,
;; `ssl.SSLContext`, `concurrent.futures.ThreadPoolExecutor`): the callee's root
;; binding resolves through the import graph to a module handle, its intermediate
;; fields extend the module path, and the final field is looked up in that
;; module's class table (stdlib-module-class-refs-data). This is the
;; module-qualified mirror of `imported-class-ref`: `from x import C` binds a
;; class handle directly, while `import x; x.C(...)` names the same class
;; through x's module handle. Unknown attributes are NOT guessed from
;; capitalization, so a mis-guessed class cannot be tracked.
(define (imported-module-class-ref callee comp import-env tokens)
  (and import-env comp (member-access-callee? callee)
       (let* ([root (dotted-root callee)]
              [fields (member-access-fields callee)]
              [attr (effect-op-of-callee callee)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e root (hash-ref e (symbol->string root) #f))]
              [path0 (imported-binding-module-path h root)])
         (and path0 attr (pair? fields) (>= (length fields) 2)
              (let* ([middle (drop-right (rest fields) 1)]
                     [base-path (if (null? middle)
                                    path0
                                    (string-append path0 "."
                                                   (string-join (map symbol->string middle) ".")))]
                     ;; A `from pkg import Name` binding synthesized as a CLASS
                     ;; handle may actually name a MODULE (`from PIL import
                     ;; ImageFile as PillowImageFile`): the handle records path
                     ;; "PIL" + name "ImageFile". The DATA module-class-refs
                     ;; table is authoritative for whether `pkg.Name` is a
                     ;; module: if it has a class table, resolve the root as
                     ;; that module instead of as a class in `pkg`. A module/fn
                     ;; handle resolves exactly as before.
                     [module-path
                      (or (and (memq (handle-kind h) '(module fn)) base-path)
                          (and (eq? (handle-kind h) 'class)
                               (handle-name h)
                               (let ([cand (string-append (handle-path h) "."
                                                          (handle-name h))])
                                 (and (hash-has-key? (effect-tokens-module-class-refs tokens) cand)
                                      cand))))]
                     [table (and module-path
                                 (hash-ref (effect-tokens-module-class-refs tokens) module-path #f))]
                     [t (and table (hash-ref table (symbol->string attr) #f))])
                (and t (string->symbol t)))))))

;; Projected return class of METHOD `attr` on `owner` (or the nearest class in
;; its MRO that defines it): a `@cached_property` / `@property` read is projected
;; to the class its body returns, so a subclass method reading a property
;; inherited from a base class resolves too.
(define (mro-property-return-type comp owner attr return-types class-defs comp-supers)
  (define start (resolve-class-component owner comp class-defs))
  (define seen (make-hash))
  (let loop ([c start] [cls owner])
    (cond
      [(not (and c cls)) #f]
      [(hash-has-key? seen (cons c cls)) #f]
      [else
       (hash-set! seen (cons c cls) #t)
       (or (hash-ref return-types (cons c (qualified-name-symbol cls attr)) #f)
           (for/or ([sr (in-list (hash-ref comp-supers (cons c cls) '()))])
             (let ([c2 (car sr)] [s (cdr sr)])
               (and c2 s (loop c2 s)))))])))
