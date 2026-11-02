#lang racket

;; racklr/emit-koka/common.rkt — shared pieces of the Koka 3.x emitter.
;;
;; The emitter is split so the expression and declaration passes can recurse
;; through each other without a module cycle: the two entry points are
;; parameters installed when their modules load (emit-expr-fn / emit-stmt-fn /
;; emit-block-fn). This file holds the parts neither owns: escaping, the
;; canonical one-line block spelling, declaration decoration, pattern/parameter
;; rendering, type text and the Koka effect-row spelling.
;;
;; Emission is deliberately layout-safe: every block is a braced, one-line
;; `{ a; b; c }`, so the Koka layout pass inserts no implicit braces or
;; semicolons inside a declaration and the re-parsed text is identical.

(require racklr/uir
         racklr/emit-effects)

(provide emit-expr-fn emit-stmt-fn emit-block-fn emit-handler-expr-fn
         escape-string
         emit-string-value
         decorators-of flatten-decorated
         emit-type emit-pattern emit-literal emit-params emit-param
         emit-name-node
         tuple-class? tuple-class-name-of
         join-semis block-spelling stmt-expression? statement-list-expression?
         koka-row-string koka-row-documented koka-row-empty?)

(define emit-expr-fn (make-parameter #f))
(define emit-stmt-fn (make-parameter #f))
(define emit-block-fn (make-parameter #f))
(define emit-handler-expr-fn (make-parameter #f))

(define (install-common-hooks!) (void))

;; ── Strings ─────────────────────────────────────────────────────────

;; Emit a string value. Long or multi-line values are emitted as raw strings
;; (`r"..."`, `r#"..."#`): the value is preserved exactly and the lexer matches
;; a raw string in linear time, while a run of thousands of escaped characters
;; in one STRING token is quadratic in the generated lexer's repetition matcher.
;; The exact raw delimiter of the source is not preserved (a documented loss);
;; the string value is.
(define (emit-string-value s)
  (define (raw-with-hashes h)
    (format "r~a\"~a\"~a" (make-string h #\#) s (make-string h #\#)))
  (cond
    [(or (string-contains? s "\n") (> (string-length s) 256))
     (define h (for/first ([h (in-range 0 4)]
                           #:unless (string-contains? s (string-append "\"" (make-string h #\#))))
                 h))
     (if h (raw-with-hashes h) (format "\"~a\"" (escape-string s)))]
    [else (format "\"~a\"" (escape-string s))]))

(define (escape-string s)
  (list->string
   (append* (for/list ([c (string->list s)])
              (cond [(char=? c #\\) '(#\\ #\\)]
                    [(char=? c #\") '(#\\ #\")]
                    [(char=? c #\newline) '(#\\ #\n)]
                    [(char=? c #\tab) '(#\\ #\t)]
                    [(char=? c #\return) '(#\\ #\r)]
                    [else (list c)])))))

;; ── Canonical block spelling ────────────────────────────────────────

;; Statements separated by `; ` inside braces. A block whose last statement is
;; not an expression gets a trailing `()` so the result is a valid Koka block.
(define (join-semis parts) (string-join parts "; "))

;; Koka's block must end with an expression statement. A statement list whose
;; last element is a declaration gets a trailing `()`; the check is on the UIR
;; node (not the rendered text), and a `with`-handler statement is transparent:
;; its body's last statement decides.
(define (block-spelling stmt-strings [expression-last? #t])
  (cond
    [(null? stmt-strings) "{ () }"]
    [expression-last? (string-append "{ " (join-semis stmt-strings) " }")]
    [else (string-append "{ " (join-semis stmt-strings) "; () }")]))

(define (stmt-expression? s)
  (define-values (_mods _tps inner) (decorators-of s))
  (cond
    [(uir-let? inner) (not (uir-null? (uir-let-body inner)))]
    [(uir-ann-set!? inner) #f]
    [(uir-fn? inner) (not (uir-fn-name inner))]
    [(uir-handler? inner)
     (if (memq (uir-handler-sort inner) '(with with-bind))
         (statement-list-expression? (uir-handler-body inner))
         #t)]
    [(uir-mask? inner)
     (if (eq? (uir-mask-form inner) 'with)
         (statement-list-expression? (uir-mask-body inner))
         #t)]
    [(or (uir-module? inner) (uir-import? inner) (uir-import-alias? inner)
         (uir-extern-import? inner) (uir-fixity? inner) (uir-struct? inner)
         (uir-enum? inner) (uir-type-alias? inner) (uir-effect-decl? inner)
         (uir-extern-value? inner) (uir-extern-fun? inner))
     #f]
    [else #t]))

(define (statement-list-expression? body)
  (cond
    [(not body) #t]
    [(uir-block? body) (if (pair? (uir-block-stmts body))
                           (stmt-expression? (last (uir-block-stmts body)))
                           #t)]
    [else #t]))

;; ── Declaration decoration ──────────────────────────────────────────

;; Decorators are the generic `uir-decorated` wrappers the lowering pass uses
;; for Koka modifiers and type-parameter binders (possibly nested). Flatten
;; them into the modifier symbol texts and the type-parameter nodes.
(define (decorators-of node)
  (cond
    [(not (uir-decorated? node)) (values '() '() node)]
    [else
     (define-values (m1 t1 inner1) (decorators-of (uir-decorated-inner node)))
     (define ds (uir-decorated-decorators node))
     (define texts
       (for/list ([d (in-list ds)]
                  #:when (and (uir-symbol? d)
                              (not (member (uir-symbol-name d) '("^")))))
         (uir-symbol-name d)))
     (define tps (append (for/list ([d (in-list ds)] #:when (uir-type-params? d)) d) t1))
     (values (append texts m1) tps inner1)]))

(define (decorations node)
  (define-values (mods tps inner) (decorators-of node))
  (values mods tps inner))

(define (flatten-decorated node)
  (define-values (_mods _tps inner) (decorators-of node))
  inner)

;; ── Type text ───────────────────────────────────────────────────────

(define (emit-type t)
  (cond
    [(not t) #f]
    [(uir-symbol? t) (uir-symbol-name t)]
    [(uir-var? t) (uir-symbol-name (uir-var-name t))]
    [else "?"]))

;; ── Patterns ────────────────────────────────────────────────────────

(define (emit-pattern pat)
  (cond
    [(uir-pat-wildcard? pat) "_"]
    [(uir-pat-capture? pat) (uir-symbol-name (uir-pat-capture-name pat))]
    [(uir-pat-value? pat) (emit-type (uir-pat-value-path pat))]
    [(uir-pat-literal? pat) (emit-literal (uir-pat-literal-value pat))]
    [(uir-pat-group? pat) (format "(~a)" (emit-pattern (uir-pat-group-pattern pat)))]
    [(uir-pat-as? pat)
     ;; Koka's as-pattern is `name as pattern`.
     (format "~a as ~a"
             (emit-type (uir-pat-as-name pat))
             (emit-pattern (uir-pat-as-pattern pat)))]
    [(uir-pat-sequence? pat)
     (format "[~a]" (string-join (map emit-pattern (uir-pat-sequence-elements pat)) ", "))]
    [(uir-pat-class? pat)
     (define cls (emit-type (uir-pat-class-cls pat)))
     (define positional (map emit-pattern (uir-pat-class-positional pat)))
     (define keyword
       (for/list ([kv (in-list (uir-pat-class-keyword pat))])
         (format "~a = ~a" (emit-type (car kv)) (emit-pattern (cdr kv)))))
     (define args (append positional keyword))
     (cond
       [(equal? cls "()") "()"]
       [(tuple-class? cls) (format "(~a)" (string-join args ", "))]
       [(null? args) cls]
       [else (format "~a(~a)" cls (string-join args ", "))])]
    [else "?"]))

(define (emit-literal v)
  (cond
    [(uir-number? v) (uir-number-value v)]
    [(uir-string? v) (emit-string-value (uir-string-value v))]
    [(uir-char? v) (uir-char-value v)]
    [(uir-bool? v) (if (uir-bool-value v) "True" "False")]
    [(uir-null? v) "()"]
    [(uir-symbol? v) (uir-symbol-name v)]
    [else "?"]))

(define (tuple-class? cls)
  (and (regexp-match? #rx"^\\(,*\\)$" cls) #t))

(define (tuple-class-name-of n)
  (string-append "(" (make-string (sub1 n) #\,) ")"))

;; ── Parameters ──────────────────────────────────────────────────────

(define (emit-params ps)
  (string-join (map emit-param ps) ", "))

(define (emit-param p)
  (cond
    ;; bound `^` borrow marker
    [(and (uir-decorated? p)
          (for/or ([d (in-list (uir-decorated-decorators p))])
            (and (uir-symbol? d) (equal? (uir-symbol-name d) "^"))))
     (string-append "^" (emit-param (uir-decorated-inner p)))]
    [(uir-typed-param? p)
     (define name (emit-name-node (uir-typed-param-name p)))
     (define ty (emit-type (uir-typed-param-type p)))
     (define default (uir-typed-param-default p))
     (string-append name
                    (if ty (format " : ~a" ty) "")
                    (if default (format " = ~a" ((emit-expr-fn) default)) ""))]
    [else "?"]))

(define (emit-name-node n)
  (cond
    [(uir-implicit-ref? n) (format "?~a" (emit-name-node (uir-implicit-ref-name n)))]
    [(or (uir-symbol? n) (uir-var? n)) (emit-type n)]
    [(or (uir-pat-capture? n) (uir-pat-wildcard? n) (uir-pat-class? n)
         (uir-pat-sequence? n) (uir-pat-group? n) (uir-pat-as? n)
         (uir-pat-literal? n) (uir-pat-value? n))
     (emit-pattern n)]
    [else "_"]))


;; ── Effect rows (Koka spelling) ─────────────────────────────────────

(define (entry-name e) (uir-symbol-name (uir-effect-entry-name e)))

(define (tail-entry? e) (and (> (string-length (entry-name e)) 0)
                             (char=? (string-ref (entry-name e) 0) #\|)))

(define (koka-row-empty? row)
  (or (not (uir-effect-row? row)) (null? (uir-effect-row-entries row))))

;; The row has an extensible tail (`<a | e>`); the tail has no
;; uir-effect-entry field, so the lowering pass stores it as a `|e` entry.
(define (koka-row-extensible? row)
  (and (uir-effect-row? row)
       (for/or ([e (in-list (uir-effect-row-entries row))]) (tail-entry? e))))

(define (koka-row-string row)
  (cond
    [(koka-row-empty? row) "total"]
    [(koka-row-extensible? row)
     (define entries (uir-effect-row-entries row))
     (define names (for/list ([e (in-list entries)] #:unless (tail-entry? e)) (entry-name e)))
     (define tail (for/first ([e (in-list entries)] #:when (tail-entry? e)) (substring (entry-name e) 1)))
     (if (null? names)
         (format "<|~a>" tail)
         (format "<~a | ~a>" (string-join names ", ") tail))]
    [else (render-effect-row 'koka row)]))

;; The documented constraint comment the shared contract renders for row fields
;; Koka's typed row cannot spell (budgets, scopes, ops, prohibitions).
(define (koka-row-documented row)
  (render-effect-annotation 'koka row))
