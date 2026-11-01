#lang racket

(require racklr/uir
         racklr/emit-contract
         racklr/effects/infer)

(provide emit-javascript)

;; ── React event name helpers ─────────────────────────────────────────

;; e.g. "onClick" → #t, "className" → #f
(define (react-event-prop? name)
  (and (string? name)
       (>= (string-length name) 3)
       (string-prefix? name "on")
       (char-upper-case? (string-ref name 2))))

;; e.g. "onClick" → "click", "onChange" → "change"
(define (react-event->dom name)
  (string-downcase (substring name 2)))

;; Synthetic event wrapper: wraps a handler ref and creates {nativeEvent, preventDefault}
(define synthetic-event-template
  "(function(_e){return {nativeEvent:_e,preventDefault:function(){_e.preventDefault();},stopPropagation:function(){_e.stopPropagation();}};})")

(define (wrap-event-handler val-str)
  ;; val-str is the handler reference, e.g. "_ctx.handleClick"
  ;; Wrap it to receive a synthetic event
  (format "function(_e){var _se=~a(_e); (~a)(_se);}"
          synthetic-event-template val-str))

(define infix-ops
  (set "+" "-" "*" "/" "%" "=" "==" "===" "!=" "!=="
       "<" ">" "<=" ">=" "&&" "||" "**"
       "+=" "-=" "*=" "/=" "**="))

(define postfix-ops (set "postfix++" "postfix--"))

(define prefix-ops (set "prefix++" "prefix--"))

;; Wrap a directly-performed effect call in the runtime guard: the ambient
;; effect context (established by __withEffects) enforces must-not / ops /
;; budget against the specific operation, then runs the original call.
(define (emit-perform name op args-str call-str)
  (format "__perform(~s, ~s, [~a], function() { return ~a; })"
          (symbol->string name)
          (if op (symbol->string op) "")
          args-str
          call-str))

(define (emit-js uir)
  (match uir
    [(uir-null) "null"]
    [(uir-bool v) (if v "true" "false")]
    [(uir-number v) v]
    [(uir-string v) (format "~s" v)]
    [(uir-symbol v) v]
    
    ;; JSX element → DOM API calls (HTML) or component call
    [(uir-element tag-uir attrs children _events)
     (cond
       ;; Component call: tag is a uir-symbol (uppercase)
       [(uir-symbol? tag-uir)
        (define tag-name (uir-symbol-name tag-uir))
        (define props-str
          (if (null? attrs) ""
              (string-join
               (for/list ([attr attrs])
                 (match-define (uir-attribute name-uir value-uir) attr)
                 (format "~a: ~a" (uir-symbol-name name-uir) (emit-js value-uir)))
               ", ")))
        (format "~a({~a})" tag-name props-str)]
       
       ;; HTML element: tag is a uir-string (lowercase)
        [(uir-string? tag-uir)
         (define tag-str (format "~s" (uir-string-value tag-uir)))
          (define attr-code
           (string-join
            (for/list ([attr (in-list attrs)])
              (match-define (uir-attribute name-uir value-uir) attr)
              (define name-str (symbol->string (uir-symbol-name name-uir)))
              (define val-str (emit-js value-uir))
              (cond
                [(string=? name-str "style")
                 (format "Object.assign(_el.style,~a);" val-str)]
                [(react-event-prop? name-str)
                 (format "_el.addEventListener(\"~a\",~a);"
                         (react-event->dom name-str)
                         (wrap-event-handler val-str))]
                [else
                 (format "_el.setAttribute(\"~a\",~a);" name-str val-str)]))
            ""))
        (define child-code
          (string-join
           (for/list ([child (in-list children)])
             (define emitted (emit-js child))
              (define wrapped
                 (cond [(uir-text-node? child) emitted]
                       [(uir-element? child) emitted]
                       [(uir-if? child) emitted]
                       [else (format "document.createTextNode(~a)" emitted)]))
             (format "_el.appendChild(~a);" wrapped))
           ""))
        (string-append
         "(function(){"
         "var _el=document.createElement(" tag-str ");"
         attr-code
         child-code
         "return _el;"
         "})()")]
       
       [else
        (error "uir-element tag must be uir-symbol or uir-string")])]
    
    [(uir-text-node text-uir)
     (format "document.createTextNode(~a)" (emit-js text-uir))]
    
    [(uir-jsx-expr expr)
     expr]
    
    [(uir-attribute name-uir value-uir)
     (cons (format "~s" (uir-symbol-name name-uir))
           (emit-js value-uir))]
    
    [(uir-style _styles)
     ""]
    
    [(uir-list items)
     (format "[~a]" (string-join (map emit-js items) ", "))]

    [(uir-record entries)
     (format "{ ~a }"
             (string-join
              (for/list ([e entries])
                (define key-str (emit-key (car e)))
                (define val (cdr e))
                (cond
                  [(string=? key-str "...")
                   (emit-js val)]
                  [(and (uir-fn? val) (string-prefix? key-str "get "))
                   (format "get ~a() { ~a }"
                           (substring key-str 4)
                           (emit-body (uir-fn-body val)))]
                  [(and (uir-fn? val) (string-prefix? key-str "set "))
                   (format "set ~a(~a) { ~a }"
                           (substring key-str 4)
                           (string-join (map emit-js (uir-fn-params val)) ", ")
                           (emit-body (uir-fn-body val)))]
                  [(uir-fn? val)
                   (format "~a(~a) { ~a }"
                           key-str
                           (string-join (map emit-js (uir-fn-params val)) ", ")
                           (emit-body (uir-fn-body val)))]
                  [else
                   (format "~a: ~a" key-str (emit-js val))]))
              ", "))]
    
    [(uir-var name)
     (emit-js name)]
    
    [(uir-spread expr)
     (format "...~a" (emit-js expr))]
    
    [(uir-paren inner)
     (format "(~a)" (emit-js inner))]
     
    [(uir-get base field)
     (define base-str (emit-js base))
     (if (uir-string? field)
         (format "~a.~a" base-str (uir-string-value field))
         (format "~a[~a]" base-str (emit-js field)))]
    
    [(uir-set! name val)
     (cond [(and (uir-fn? val) (uir-effect-row? (uir-fn-effects val)))
            (format "const ~a = __withEffects(~a, ~a)"
                    (emit-js name)
                    (emit-effect-row (uir-fn-effects val))
                    (emit-raw-fn val))]
           [(and (uir-call? val) (uir-symbol? (uir-call-callee val))
                 (let ([op (uir-symbol-name (uir-call-callee val))])
                   (or (string=? op "async-fn") (string=? op "gen-fn")))
                 (uir-effect-row? (uir-fn-effects (first (uir-call-args val)))))
            (define args (uir-call-args val))
            (define fn (first args))
            (define raw (if (string=? (uir-symbol-name (uir-call-callee val)) "async-fn")
                            (emit-raw-async-fn fn)
                            (emit-raw-gen-fn fn)))
            (format "const ~a = __withEffects(~a, ~a)"
                    (emit-js name) (emit-effect-row (uir-fn-effects fn)) raw)]
           [(uir-fn? val)
            (format "function ~a(~a) { ~a }"
                    (emit-js name)
                    (string-join (map emit-js (uir-fn-params val)) ", ")
                    (emit-body (uir-fn-body val)))]
           [(and (uir-call? val) (uir-symbol? (uir-call-callee val))
                 (let ([op (uir-symbol-name (uir-call-callee val))])
                   (or (string=? op "async-fn") (string=? op "gen-fn"))))
            (define args (uir-call-args val))
            (define fn (first args))
            (define prefix (if (string=? (uir-symbol-name (uir-call-callee val)) "async-fn") "async function" "function*"))
            (format "~a ~a(~a) { ~a }"
                    prefix
                    (emit-js name)
                    (string-join (map emit-js (uir-fn-params fn)) ", ")
                    (emit-body (uir-fn-body fn)))]
           [else
            (format "~a = ~a" (emit-js name) (emit-js val))])]
    
    [(uir-fn _ params body _ fx)
     (define raw (emit-raw-fn (uir-fn #f params body #f #f)))
     (if (uir-effect-row? fx)
         (format "__withEffects(~a, ~a)" (emit-effect-row fx) raw)
         raw)]
    
    [(uir-call callee args)
     (define op-name (and (uir-symbol? callee) (uir-symbol-name callee)))
     (cond [(and op-name (string=? op-name "dot") (= (length args) 2))
            (format "~a.~a" (emit-js (first args)) (emit-js (second args)))]
           [(and op-name (string=? op-name "index") (= (length args) 2))
            (format "~a[~a]" (emit-js (first args)) (emit-js (second args)))]
           [(and op-name (string=? op-name "while") (= (length args) 2))
            (format "while (~a) { ~a }" (emit-js (first args)) (emit-body (second args)))]
            [(and op-name (string=? op-name "dowhile") (= (length args) 2))
             (format "do { ~a } while (~a)" (emit-body (first args)) (emit-js (second args)))]
            [(and op-name (string=? op-name "with") (= (length args) 2))
             (format "with (~a) { ~a }" (emit-js (first args)) (emit-body (second args)))]
           [(and op-name (string=? op-name "for") (= (length args) 4))
            (format "for (~a; ~a; ~a) { ~a }"
                    (if (uir-null? (first args)) "" (emit-js (first args)))
                    (if (uir-null? (second args)) "" (emit-js (second args)))
                    (if (uir-null? (third args)) "" (emit-js (third args)))
                    (emit-body (fourth args)))]
           [(and op-name (string=? op-name "forin") (= (length args) 3))
            (format "for (~a in ~a) { ~a }"
                    (emit-js (first args))
                    (emit-js (second args))
                    (emit-body (third args)))]
            [(and op-name (string=? op-name "forof") (= (length args) 3))
             (format "for (~a of ~a) { ~a }"
                     (emit-js (first args))
                     (emit-js (second args))
                     (emit-body (third args)))]
            [(and op-name (string=? op-name "switch") (= (length args) 3))
             (define test-str (emit-js (first args)))
             (define case-str
               (if (uir-null? (second args)) ""
                   (emit-switch-cases (second args))))
             (define default-str
               (if (uir-null? (third args)) ""
                   (format " default: ~a" (emit-switch-body (third args)))))
             (format "switch (~a) { ~a~a }" test-str case-str default-str)]
           [(and op-name (string=? op-name "break"))
            "break"]
            [(and op-name (string=? op-name "continue"))
             "continue"]
            [(and op-name (string=? op-name "debugger"))
             "debugger"]
            [(and op-name (string=? op-name "label") (= (length args) 2))
             (define label-name (emit-js (first args)))
             (define body-str (emit-js (second args)))
             (format "~a: ~a" label-name body-str)]
            [(and op-name (string=? op-name "regex") (= (length args) 1))
             (uir-string-value (first args))]
            [(and op-name (string=? op-name "function") (= (length args) 2))
             (define params-uir (first args))
             (define body-uir (second args))
             (define param-str
               (if (uir-list? params-uir)
                   (string-join (map emit-js (uir-list-items params-uir)) ", ")
                   ""))
             (format "function(~a) { ~a }" param-str (emit-body body-uir))]
            [(and op-name (string=? op-name "async-fn") (= (length args) 1))
             (define fn (first args))
             (format "async function(~a) { ~a }"
                     (string-join (map emit-js (uir-fn-params fn)) ", ")
                     (emit-body (uir-fn-body fn)))]
            [(and op-name (string=? op-name "gen-fn") (= (length args) 1))
             (define fn (first args))
             (format "function* (~a) { ~a }"
                     (string-join (map emit-js (uir-fn-params fn)) ", ")
                     (emit-body (uir-fn-body fn)))]
            [(and op-name (string=? op-name "import") (>= (length args) 1))
             (emit-import args)]
            [(and op-name (string=? op-name "export") (>= (length args) 1))
             (emit-export args)]
            [(and op-name (string=? op-name "throw") (= (length args) 1))
             (format "throw ~a" (emit-js (first args)))]
             [(and op-name (string=? op-name "try") (= (length args) 4))
              (define try-str (format "try { ~a }" (emit-body (first args))))
              (define catch-str
                (if (uir-null? (third args))
                    ""
                    (format " catch (~a) { ~a }"
                            (emit-js (second args))
                            (emit-body (third args)))))
              (define finally-str
                (if (uir-null? (fourth args))
                    ""
                    (format " finally { ~a }" (emit-body (fourth args)))))
              (string-append try-str catch-str finally-str)]
             [(and op-name (string=? op-name "=>") (= (length args) 2))
               (define params-uir (first args))
               (define body-expr (second args))
               (define param-str
                 (if (uir-list? params-uir)
                     (string-join (map emit-js (uir-list-items params-uir)) ", ")
                     (emit-js params-uir)))
               (define body-str (emit-js body-expr))
               (if (uir-block? body-expr)
                   (format "(~a) => { ~a }" param-str (emit-body body-expr))
                   (format "(~a) => ~a" param-str body-str))]
            [(and op-name (string=? op-name "__withEffects") (= (length args) 2))
             ;; annotate-effects wraps an inferred arrow/function expression in
             ;; (call "__withEffects" (list row fn)) so the emitter can render
             ;; the runtime guard around a function value with no effects slot.
             (format "__withEffects(~a, ~a)"
                     (emit-effect-row (first args))
                     (emit-js (second args)))]
            [(and op-name (set-member? (set "var" "let" "const") op-name) (= (length args) 1))
             (emit-declaration op-name (first args))]
            [(and op-name (= (length args) 1) (set-member? postfix-ops op-name))
            (format "~a~a" (emit-js (first args)) (substring op-name 7))]
           [(and op-name (= (length args) 1) (set-member? prefix-ops op-name))
            (format "~a~a" (substring op-name 6) (emit-js (first args)))]
           [(and op-name (= (length args) 1))
            (format "~a ~a" op-name (emit-js (first args)))]
           [(and op-name (set-member? infix-ops op-name) (= (length args) 2))
            (format "~a ~a ~a"
                    (emit-js (first args))
                    op-name
                    (emit-js (second args)))]
            [else
             (define fn-str (emit-js callee))
             (define args-str (string-join (map emit-js args) ", "))
             (define call-str (format "~a(~a)" fn-str args-str))
             (define eff (direct-effect-of-callee callee))
             (if eff
                 (emit-perform eff (effect-op-of-callee callee) args-str call-str)
                 call-str)])]
    
    [(uir-if test conseq altern)
     (if (or (uir-block? conseq) (uir-block? altern))
         (format "if (~a) { ~a } else { ~a }"
                 (emit-js test)
                 (emit-body conseq)
                 (emit-body altern))
         (format "~a ? ~a : ~a"
                 (emit-js test)
                 (emit-js conseq)
                 (emit-js altern)))]
    
    [(uir-new class args)
     (format "new ~a(~a)"
             (emit-js class)
             (string-join (map emit-js args) ", "))]
    
    [(uir-class name supers fields methods)
     (format "class ~a~a {~a}"
             (emit-js name)
             (if (null? supers) "" (format " extends ~a" (emit-js (car supers))))
             (if (null? methods)
                 ""
                 (string-append "" (string-join (map emit-statement methods) ""))))]
    
    [(uir-block stmts)
     (string-join (map emit-statement stmts) "\n")]
    
    [(uir-return val)
     (format "return ~a" (emit-js val))]
    
    [(uir-await expr)
     (format "await ~a" (emit-js expr))]
    
    [(uir-yield value from?)
     (format "~a ~a"
             (if from? "yield*" "yield")
             (emit-js value))]
    
    [(? uir? v)
     (format "/* unlowered: ~a */" (uir-tag v))]
    
    [else (format "~a" uir)]))

(define (emit-body node)
  (cond [(uir-null? node) ""]
        [(uir-block? node) (emit-js node)]
        [else (emit-js node)]))

(define (emit-statement node)
  (cond [(uir-null? node) "null;"]
        [(uir-block? node)
         (string-join (map emit-statement (uir-block-stmts node)) " ")]
        [(and (uir-set!? node)
               (or (uir-fn? (uir-set!-value node))
                   (and (uir-call? (uir-set!-value node))
                        (uir-symbol? (uir-call-callee (uir-set!-value node)))
                        (let ([op (uir-symbol-name (uir-call-callee (uir-set!-value node)))])
                          (or (string=? op "async-fn") (string=? op "gen-fn"))))))
          (emit-js node)]
        [(and (uir-if? node)
              (or (uir-block? (uir-if-then node))
                  (uir-block? (uir-if-else node))))
         ;; A real if-statement has block branches; a ternary used as an
         ;; expression statement (`x ? 1 : 2;`) has expression branches and is
         ;; left to fall through to the generic emit-js ternary path below.
         (format "if (~a) { ~a } else { ~a }"
                 (emit-js (uir-if-test node))
                 (emit-statement (uir-if-then node))
                 (emit-statement (uir-if-else node)))]
        [(loop-statement? node)
         (emit-js node)]
        [(try-statement? node)
         (emit-js node)]
        [(switch-statement? node)
         (emit-js node)]
        [(and (uir-call? node) (uir-symbol? (uir-call-callee node))
              (string=? (uir-symbol-name (uir-call-callee node)) "dowhile"))
         (format "~a;" (emit-js node))]
        [(declaration? node)
          (format "~a;" (emit-js node))]
        [(uir-method? node)
         (format "~a(~a) {~a}"
                 (emit-js (uir-method-name node))
                 (string-join (map emit-js (uir-method-params node)) ", ")
                 (emit-js (uir-method-body node)))]
        [(uir-var? node)
         (format "~a;" (emit-js node))]
        [else (format "~a;" (emit-js node))]))

(define (loop-statement? node)
  (and (uir-call? node)
       (uir-symbol? (uir-call-callee node))
       (let ([n (uir-symbol-name (uir-call-callee node))])
         (or (string=? n "while") (string=? n "for")
             (string=? n "forin") (string=? n "forof")))))

(define (try-statement? node)
  (and (uir-call? node)
       (uir-symbol? (uir-call-callee node))
       (string=? "try" (uir-symbol-name (uir-call-callee node)))))

(define (declaration? node)
  (and (uir-call? node)
       (uir-symbol? (uir-call-callee node))
       (let ([n (uir-symbol-name (uir-call-callee node))])
         (set-member? (set "var" "let" "const") n))))

(define (emit-declaration kind body)
  (cond [(uir-set!? body)
         (format "~a ~a = ~a" kind
                 (emit-js (uir-set!-name body))
                 (emit-js (uir-set!-value body)))]
        [(uir-var? body)
         (format "~a ~a" kind (emit-js (uir-var-name body)))]
        [(and (uir-call? body) (uir-symbol? (uir-call-callee body))
              (let ([op (uir-symbol-name (uir-call-callee body))])
                (or (string=? op "array-bind") (string=? op "object-bind"))))
         (match-define (list names-list rhs) (uir-call-args body))
         (define names-str
           (if (uir-list? names-list)
               (string-join (map emit-js (uir-list-items names-list)) ", ")
               (emit-js names-list)))
         (define bracket-open (if (string=? (uir-symbol-name (uir-call-callee body)) "array-bind") "[" "{"))
         (define bracket-close (if (string=? (uir-symbol-name (uir-call-callee body)) "array-bind") "]" "}"))
         (if (uir-null? rhs)
             (format "~a ~a" kind (format "~a~a~a" bracket-open names-str bracket-close))
             (format "~a ~a = ~a" kind
                     (format "~a~a~a" bracket-open names-str bracket-close)
                     (emit-js rhs)))]
        [else (format "~a ~a" kind (emit-js body))]))

(define (emit-switch-cases cases-block)
  (string-join
   (for/list ([stmt (uir-block-stmts cases-block)]
              #:when (and (uir-call? stmt)
                          (uir-symbol? (uir-call-callee stmt))
                          (string=? "case" (uir-symbol-name (uir-call-callee stmt)))))
     (match-define (list test body) (uir-call-args stmt))
     (format " case ~a: ~a" (emit-js test) (emit-switch-body body)))
   ""))

(define (emit-switch-body body)
  (if (uir-block? body)
      (string-join (map emit-statement (uir-block-stmts body)) " ")
      (emit-statement body)))

(define (switch-statement? node)
  (and (uir-call? node)
       (uir-symbol? (uir-call-callee node))
       (string=? "switch" (uir-symbol-name (uir-call-callee node)))))

(define (emit-key key)
  (if (uir-string? key)
      (uir-string-value key)
      (format "[~a]" (emit-js key))))

;; ── ES Modules emit ──────────────────────────────────────────────────

(define (emit-import args)
  (define source
    (let ([last-arg (last args)])
      (if (uir-string? last-arg)
          (format " from ~s" (uir-string-value last-arg))
          "")))
  (cond
    ;; import 'm';
    [(and (= (length args) 1) (uir-string? (first args)))
     (format "import ~s" (uir-string-value (first args)))]
    ;; import x from 'm';
    [(and (= (length args) 2) (uir-symbol? (first args)))
     (format "import ~a~a" (emit-js (first args)) source)]
    ;; import * as ns from 'm';
    [(and (= (length args) 2) (uir-list? (first args))
          (= (length (uir-list-items (first args))) 2))
     (define items (uir-list-items (first args)))
     (define ns-name (emit-js (second items)))
     (format "import * as ~a~a" ns-name source)]
    ;; import { x, y } from 'm';
    [(uir-record? (first args))
     (define entries (uir-record-entries (first args)))
     (define names
       (string-join
        (for/list ([e entries])
          (define export-name (uir-string-value (car e)))
          (define local-name (emit-js (cdr e)))
          (if (string=? export-name local-name)
              local-name
              (format "~a as ~a" export-name local-name)))
        ", "))
     (define src
       (if (and (= (length args) 2) (uir-string? (second args)))
           (format " from ~s" (uir-string-value (second args)))
           ""))
     (format "import { ~a }~a" names src)]
    [else (format "/* unlowered import */")]))

(define (emit-export args)
  (define first-arg (first args))
  (cond
    ;; export default expr
    [(and (uir-symbol? first-arg) (string=? (uir-symbol-name first-arg) "default"))
     (format "export default ~a" (emit-js (second args)))]
    ;; export const x = 1; / export function f() {} ...
    [(and (uir-symbol? first-arg) (string=? (uir-symbol-name first-arg) "decl"))
     (format "export ~a" (emit-js (second args)))]
    ;; export { x, y } or export { x, y } from 'm'
    [(uir-record? first-arg)
     (define entries (uir-record-entries first-arg))
     (define names
       (string-join
        (for/list ([e entries])
          (define export-name (uir-string-value (car e)))
          (define local-name (emit-js (cdr e)))
          (if (string=? export-name local-name)
              local-name
              (format "~a as ~a" local-name export-name)))
        ", "))
     (define src
       (if (and (= (length args) 2) (uir-string? (second args)))
           (format " from ~s" (uir-string-value (second args)))
           ""))
     (format "export { ~a }~a" names src)]
    [else (format "/* unlowered export */")]))

;; ── Effect-row runtime guard (gap 4: TypeScript / React emission) ───

(define (emit-effect-entry-obj e)
  ;; {"modality":"may","max":5,"scope":"...","ops":["query"]}
  (define base (list (format "~s: ~s" "modality"
                             (symbol->string (uir-effect-entry-modality e)))))
  (define with-budget
    (if (uir-effect-entry-budget e)
        (cons (format "~s: ~a" "max" (uir-effect-entry-budget e)) base)
        base))
  (define with-scope
    (if (uir-effect-entry-scope e)
        (cons (format "~s: ~s" "scope" (uir-symbol-name (uir-effect-entry-scope e))) with-budget)
        with-budget))
  (define with-ops
    (if (pair? (uir-effect-entry-ops e))
        (cons (format "~s: [~a]" "ops"
                      (string-join
                       (map (λ (o) (format "~s" (uir-symbol-name o)))
                            (uir-effect-entry-ops e))
                       ", "))
              with-scope)
        with-scope))
  (string-append "{" (string-join (reverse with-ops) ", ") "}"))

(define (emit-effect-row row)
  (string-append
   "{"
   (string-join
    (map (λ (e) (format "~s: ~a"
                        (uir-symbol-name (uir-effect-entry-name e))
                        (emit-effect-entry-obj e)))
         (uir-effect-row-entries row))
    ", ")
   "}"))

(define (emit-raw-fn fn)
  (define param-str (string-join (map emit-js (uir-fn-params fn)) ", "))
  (format "function(~a) { ~a }" param-str (emit-body (uir-fn-body fn))))

(define (emit-raw-async-fn fn)
  (format "async function(~a) { ~a }"
          (string-join (map emit-js (uir-fn-params fn)) ", ")
          (emit-body (uir-fn-body fn))))

(define (emit-raw-gen-fn fn)
  (format "function* (~a) { ~a }"
          (string-join (map emit-js (uir-fn-params fn)) ", ")
          (emit-body (uir-fn-body fn))))

(define (effects-guard-preamble)
  (string-append
   "var __effects_stack = [];\n"
   "var __handlers_stack = [];\n"
   "function __effects_current() {\n"
   "  return __effects_stack.length ? __effects_stack[__effects_stack.length - 1] : null;\n"
   "}\n"
   "function __current_handler(name) {\n"
   "  for (var _i = __handlers_stack.length - 1; _i >= 0; _i--) {\n"
   "    var _f = __handlers_stack[_i];\n"
   "    if (_f.name === name) return _f.handler;\n"
   "  }\n"
   "  return null;\n"
   "}\n"
   "function __perform_checked(name, op, thunk) {\n"
   "  var _ctx = __effects_current();\n"
   "  if (_ctx) {\n"
   "    var _e = _ctx.row[name];\n"
   "    if (!_e) throw new Error(\"effect prohibited: \" + name);\n"
   "    var _m = _e.modality || \"may\";\n"
   "    if (_m === \"must-not\") throw new Error(\"effect prohibited: \" + name);\n"
   "    var _ops = _e.ops || [];\n"
   "    if (_ops.length && _ops.indexOf(op) === -1) throw new Error(\"effect operation prohibited: \" + name + \".\" + op);\n"
   "    var _scope = _e.scope;\n"
   "    if (_scope) {\n"
   "      var _writeOps = { \"write\":1, \"update\":1, \"create\":1, \"delete\":1, \"save\":1, \"insert\":1, \"put\":1, \"remove\":1, \"set\":1, \"push\":1, \"clear\":1, \"drop\":1, \"mutate\":1, \"add\":1 };\n"
   "      var _readOps = { \"query\":1, \"fetch\":1, \"get\":1, \"read\":1, \"find\":1, \"filter\":1, \"all\":1, \"select\":1, \"search\":1, \"list\":1, \"exists\":1, \"load\":1, \"peek\":1, \"count\":1 };\n"
   "      if (_scope.indexOf(\"reads=\") === 0 && _writeOps[op]) throw new Error(\"effect scope violation: \" + name + \".\" + op + \" outside \" + _scope);\n"
   "      if (_scope.indexOf(\"writes=\") === 0 && _readOps[op]) throw new Error(\"effect scope violation: \" + name + \".\" + op + \" outside \" + _scope);\n"
   "    }\n"
   "    var _perf = _ctx.performed;\n"
   "    if (!_perf[name]) _perf[name] = { any: 0 };\n"
   "    if (_e.max != null && _perf[name].any >= _e.max) throw new Error(\"effect budget exceeded: \" + name);\n"
   "    _perf[name].any += 1;\n"
   "    _perf[name][op] = (_perf[name][op] || 0) + 1;\n"
   "  }\n"
   "  return thunk();\n"
   "}\n"
   "function __perform(name, op, args, thunk) {\n"
   "  var _handler = __current_handler(name);\n"
   "  if (_handler) {\n"
   "    var _resume = function() { return __perform_checked(name, op, thunk); };\n"
   "    return _handler(op, args, _resume);\n"
   "  }\n"
   "  return __perform_checked(name, op, thunk);\n"
   "}\n"
   "function __handle(name, handler, fn) {\n"
   "  __handlers_stack.push({ name: name, handler: handler });\n"
   "  try {\n"
   "    return fn();\n"
   "  } finally {\n"
   "    __handlers_stack.pop();\n"
   "  }\n"
   "}\n"
   "function __withEffects(row, fn) {\n"
   "  var _valid = [\"may\", \"must\", \"must-not\"];\n"
   "  for (var _k in row) {\n"
   "    var _e = row[_k];\n"
   "    var _m = _e.modality || \"may\";\n"
   "    if (_valid.indexOf(_m) === -1) throw new Error(\"invalid effect modality: \" + _m);\n"
   "    var _b = _e.max;\n"
   "    if (_b != null && (typeof _b !== \"number\" || _b < 0)) throw new Error(\"invalid effect budget: \" + _b);\n"
   "  }\n"
   "  function _check(frame) {\n"
   "    for (var _k in row) {\n"
   "      var _e = row[_k];\n"
   "      if ((_e.modality || \"may\") === \"must\") {\n"
   "        var _perf = frame.performed[_k];\n"
   "        var _ops = _e.ops || [];\n"
   "        if (_ops.length) {\n"
   "          for (var _i = 0; _i < _ops.length; _i++) {\n"
   "            if (!_perf || !(_perf[_ops[_i]] > 0)) throw new Error(\"effect obligation unmet: \" + _k + \".\" + _ops[_i]);\n"
   "          }\n"
   "        } else if (!_perf || !(_perf.any > 0)) {\n"
   "          throw new Error(\"effect obligation unmet: \" + _k);\n"
   "        }\n"
   "      }\n"
   "    }\n"
   "  }\n"
   "  function wrapped() {\n"
   "    var frame = { row: row, performed: {} };\n"
   "    __effects_stack.push(frame);\n"
   "    var _r;\n"
   "    try {\n"
   "      _r = fn.apply(this, arguments);\n"
   "    } catch (e) {\n"
   "      __effects_stack.pop();\n"
   "      throw e;\n"
   "    }\n"
   "    if (_r && typeof _r.then === \"function\") {\n"
   "      var _settle = function() { __effects_stack.pop(); };\n"
   "      return _r.then(function(v) { _settle(); _check(frame); return v; },\n"
   "                     function(e) { _settle(); throw e; });\n"
   "    }\n"
   "    try {\n"
   "      _check(frame);\n"
   "      return _r;\n"
   "    } finally {\n"
   "      __effects_stack.pop();\n"
   "    }\n"
   "  }\n"
   "  fn.__effects__ = row;\n"
   "  wrapped.__effects__ = row;\n"
   "  return wrapped;\n"
   "}"))

;; A call to the runtime handler installer `__handle(name, handler, fn)`: it
;; needs the preamble even when no direct effect call is visible in the program.
(define (handler-call? n)
  (and (uir-call? n)
       (let ([c (uir-call-callee n)])
         (cond
           [(uir-symbol? c) (string=? (uir-symbol-name c) "__handle")]
           [(uir-var? c) (string=? (uir-symbol-name (uir-var-name c)) "__handle")]
           [else #f]))))

(define (program-has-effects? uir)
  (cond
    [(uir-effect-row? uir) #t]
    [(struct? uir)
     (or (handler-call? uir)
         (and (uir-call? uir)
              (direct-effect-of-callee (uir-call-callee uir)))
         (and (uir-fn? uir) (uir-effect-row? (uir-fn-effects uir)))
         (for/or ([f (in-list (cdr (vector->list (struct->vector uir))))])
           (program-has-effects? f)))]
    [(pair? uir) (or (program-has-effects? (car uir)) (program-has-effects? (cdr uir)))]
    [else #f]))

;; Implements the emit contract (racklr/emit-contract): UIR plus an optional
;; explicit effect row in, JavaScript source out. An explicit row is
;; authoritative for the unit's entry function and is rendered as a
;; `__withEffects({...}, fn)` guard. The other functions still get their row
;; from the UIR or, when absent, from inference, exactly as before.
(define (emit-javascript uir [row #f])
  (define source (attach-root-row 'emit-javascript uir row))
  (define annotated (annotate-effects source))
  (define body (emit-js annotated))
  (if (program-has-effects? annotated)
      (string-append (effects-guard-preamble) "\n\n" body)
      body))

(module+ main
  (displayln "emit-javascript loaded."))
