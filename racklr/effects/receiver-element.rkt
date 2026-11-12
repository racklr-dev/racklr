#lang racket

;; racklr/effects/receiver-element.rkt
;;
;; Element-type projection for iterables: the class of the values an iterable
;; yields, so a for-loop / comprehension variable (`for c in xs.values()`,
;; `[f(x) for x in iterable]`) resolves its methods instead of `unknown`.
;;
;; Split out of receiver-type.rkt (which keeps the value→class projection) so
;; both stay under the 400-line obligation. The value→class lookups arrive as
;; a callback, exactly as type-projection.rkt does, so this module has no
;; dependency cycle with receiver-type.rkt.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/class-resolve
         racklr/effects/imports
         racklr/effects/library-tokens)

(provide imported-module-element-type
         stdlib-method-element-type
         class-attr-element-type
         iterable-element-spec)

;; ELEMENT TYPE(S) of an imported module FUNCTION used as an iterable
;; (`for dirpath, dirnames, filenames in os.walk(...)`, `with os.scandir(p) as
;; entries`): the mirror of imported-module-result-type, reading the module's
;; element-type table (`os.walk` → (str list list)). The value is a symbol
;; (single element class) or a list of symbols (tuple destructuring).
(define (imported-module-element-type callee comp import-env tokens)
  (and import-env comp (member-access-callee? callee)
       (let* ([root (dotted-root callee)]
              [func (effect-op-of-callee callee)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e root (hash-ref e (symbol->string root) #f))])
         (and root func (handle? h) (handle-path h) (eq? (handle-kind h) 'module)
              (let* ([path (handle-path h)]
                     [table (hash-ref (effect-tokens-module-element-types tokens)
                                      path #f)]
                     [spec (and table (hash-ref table (symbol->string func) #f))])
                spec)))))

;; ELEMENT TYPE of a METHOD call on a tracked stdlib/framework receiver, used
;; when the method's result is ITERATED (`for c in response.cookies.values()`):
;; resolve the receiver's class through rt (via the caller's value-class
;; callback), then the method's element class through
;; stdlib-method-element-types-data. The iterable mirror of
;; stdlib-method-result-type: a concrete tracked receiver class + a declared
;; element entry, never a name-only guess.
(define (stdlib-method-element-type callee rt tokens class-defs import-env comp owner value-class)
  (and rt (uir-get? callee)
       (let* ([mname (get-field-symbol (uir-get-field callee))]
              [rcls (value-class (uir-get-base callee))])
         (and mname rcls
              (let* ([m (hash-ref (effect-tokens-stdlib-method-element-types tokens)
                                  (symbol->string rcls) #f)]
                     [r (and m (hash-ref m (symbol->string mname) #f))])
                (and r (string->symbol r)))))))

;; ELEMENT TYPE of an ATTRIBUTE on a tracked framework class, used when the
;; attribute is ITERATED (`for operation in migration.operations`):
;; (class . attr) -> element class, resolved after the receiver's class.
(define (class-attr-element-type rcls attr tokens)
  (and rcls attr
       (let* ([m (hash-ref (effect-tokens-class-attr-element-types tokens)
                           (symbol->string rcls) #f)]
              [r (and m (hash-ref m (symbol->string attr) #f))])
         (and r (string->symbol r)))))

;; ELEMENT TYPE(S) of an expression used as a loop iterable. Consults, in order:
;;   1. an imported module function's element-type table (os.walk / os.scandir);
;;   2. a declared element type for a corpus/imported call (`engines.all()` →
;;      Engine, `apps.get_models()` → Model, `as_completed(...)` → Future),
;;      resolved through the same callee→(module . qualified-name) keys as
;;      corpus-call-return-type;
;;   3. the receiver-type env's recorded elem-spec for a local/with alias
;;      (`entries = os.scandir(path)` then `for entry in entries`).
;; `value-class` is the caller's value→class projection (receiver-type.rkt's
;; value-class-of bound to the caller's receiver env). Returns a class symbol,
;; a list of class symbols (a destructured tuple), or #f.
;; The receiver-type class-union marker (same symbol receiver-type.rkt exports;
;; this module must not require receiver-type, which requires this one). A
;; tuple destructure whose conditional branches name DIFFERENT classes yields
;; this marker for the element position, so a later bare call of that element
;; classifies as pure construction of SOME class.
(define class-union-marker 'class-union)

;; Merge two element specs (each a class symbol or a list of class symbols)
;; across a conditional's branches: same class folds, different classes become
;; the class-union marker, and anything else is unprojected (#f).
(define (merge-elem-specs t e)
  (cond
    [(and (symbol? t) (symbol? e)) (and (eq? t e) t)]
    [(and (list? t) (list? e) (= (length t) (length e)))
     (for/list ([a (in-list t)] [b (in-list e)])
       (cond
         [(eq? a b) a]
         [(and (symbol? a) (symbol? b)) class-union-marker]
         [else #f]))]
    [else #f]))

(define (iterable-element-spec iter rt tokens import-env comp value-class
                               #:class-defs [class-defs (hash)]
                               #:methods [methods #f]
                               #:fn-defs [fn-defs #f]
                               #:known [known #f]
                               #:comp-supers [comp-supers #f]
                               #:owner [owner #f]
                               #:element-types [element-types #f]
                               #:elem-spec-lookup [elem-spec-lookup (λ (nm) #f)])
  (define (recur v)
    (iterable-element-spec v rt tokens import-env comp value-class
                           #:class-defs class-defs #:methods methods
                           #:fn-defs fn-defs #:known known
                           #:comp-supers comp-supers #:owner owner
                           #:element-types element-types
                           #:elem-spec-lookup elem-spec-lookup))
  (cond
    ;; A conditional iterable (`(A, b) if c else (C, d)` destructured through
    ;; the lowerer's tmp subscript): project the union of the two branches'
    ;; element specs, position by position, so `error_cls = tmp[0]` types the
    ;; first element as SOME exception class instead of staying unknown.
    [(uir-if? iter)
     (merge-elem-specs (recur (uir-if-then iter)) (recur (uir-if-else iter)))]
    [(and (uir-call? iter) (member-access-callee? (uir-call-callee iter)))
     (or (imported-module-element-type (uir-call-callee iter) comp import-env tokens)
         (and rt (stdlib-method-element-type (uir-call-callee iter) rt tokens
                                             class-defs import-env comp owner
                                             value-class))
         (call-element-spec (uir-call-callee iter) comp owner class-defs
                            methods fn-defs known comp-supers import-env
                            element-types))]
    [(and (uir-call? iter))
     (or
      ;; A tuple literal used as a destructuring assignment value
      ;; (`open_method, kwargs, file_path = (open, {}, output)`): the element
      ;; spec is the class of each element (open, dict, str), so the lowerer's
      ;; tmp[0] projection types open_method as the file-opener `open`.
      (and rt
           (let ([c (uir-call-callee iter)])
             (and (uir-symbol? c)
                  (eq? (string->symbol (uir-symbol-name c)) 'tuple)
                  (let ([args (uir-call-args iter)])
                    ;; `tuple(<if>)` (a bare tuple RHS that is itself a
                    ;; conditional of tuples) is the same value as `<if>`:
                    ;; delegate to the conditional's merged spec.
                    (cond
                      [(and (= (length args) 1) (uir-if? (first args)))
                       (recur (first args))]
                      [else
                       (let ([classes (for/list ([a (in-list args)])
                                        (value-class a))])
                         ;; A tuple element may be an int-valued module attr
                         ;; (`errno.ENOTDIR`) with no tracked class: keep the
                         ;; known positions and leave the rest #f, so the
                         ;; later tmp[0]/tmp[1] projection types only the
                         ;; positions it can, instead of dropping the whole
                         ;; destructure. Require at least one known element.
                         (and (pair? classes) (ormap (λ (x) x) classes) classes))])))))
      (call-element-spec (uir-call-callee iter) comp owner class-defs
                         methods fn-defs known comp-supers import-env
                         element-types))]
    [(and rt (uir-var? iter))
     (define nm (string->symbol (uir-symbol-name (uir-var-name iter))))
     (elem-spec-lookup nm)]
    [(and rt (uir-get? iter))
     (define attr (get-field-symbol (uir-get-field iter)))
     (and attr
          (let ([rcls (value-class (uir-get-base iter))])
            (class-attr-element-type rcls attr tokens)))]
    [else #f]))
