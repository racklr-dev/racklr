#lang racket

(require racket/string
         racklr/tree)

(provide tok? cst-kids tag-of kids-of find-kid find-list find-node-or-list
         extract-var-kind extract-identifier-name cst-source-text)

;; ── CST-walking helpers for TypeScript lowering ───────────────────────

(define (tok? x tk-type)
  (and (not (cst-node? x)) (not (null? x)) (not (eq? x 'none))
       (not (pair? x))
       (with-handlers ([exn:fail? (λ (_) #f)])
         (tk-type x) #t)))

(define (cst-kids n) (filter cst-node? (cst-node-children n)))
(define (tag-of n) (cst-node-tag n))
(define (kids-of n) (cst-node-children n))

(define (find-kid n tag)
  (for/or ([k (kids-of n)] #:when (and (cst-node? k) (eq? (tag-of k) tag))) k))

(define (find-list n)
  (for/or ([k (kids-of n)] #:when (pair? k)) k))

(define (find-node-or-list n)
  (for/or ([k (kids-of n)] #:when (or (cst-node? k) (pair? k))) k))

(define (extract-var-kind vm tk-type)
  (define kid (first (kids-of vm)))
  (cond [(and (tok? kid tk-type) (eq? (tk-type kid) 'Var)) "var"]
        [(and (tok? kid tk-type) (eq? (tk-type kid) 'Const)) "const"]
        [(and (tok? kid tk-type) (eq? (tk-type kid) 'Let)) "let"]
        [(and (cst-node? kid) (eq? (tag-of kid) 'let_)) "let"]
        [else "var"]))

(define (extract-identifier-name node tk-type tk-value)
  (define in (find-kid node 'identifierName))
  (and in
       (let ([ident (find-kid in 'identifier)])
         (and ident
              (let ([tok (first (kids-of ident))])
                (and (tok? tok tk-type) (eq? (tk-type tok) 'Identifier)
                     (tk-value tok)))))))

;; ── Recovering a declaration's spelling ─────────────────────────────
;;
;; The lowering interface receives the CST and token accessors, not the source
;; string, so a declaration's literal text is recovered by walking its tokens
;; in order. The lexer drops whitespace and comments, so the rendering is
;; canonical (token order preserved) rather than byte-identical. Erased
;; type-level declarations ride into the UIR as this text so the emitter can
;; document them instead of dropping the file.

(define (cst-source-text node tk-type tk-value)
  (define toks
    (let loop ([x node] [acc '()])
      (cond [(cst-node? x)
             (for/fold ([acc acc]) ([k (in-list (cst-node-children x))])
               (loop k acc))]
            [(pair? x)
             (for/fold ([acc acc]) ([k (in-list x)]) (loop k acc))]
            [(and (tok? x tk-type) (not (eq? (tk-type x) 'EOF)))
             (cons (tk-value x) acc)]
            [else acc])))
  (normalize-whitespace (tokens->text (reverse toks))))

(define (normalize-whitespace text)
  (string-join (string-split text) " "))

;; Canonical spacing: punctuation hugs its operand, so the comment reads like
;; the declaration (`{ x: number; }`, `Array<T>`, `label?: string`).
(define (tokens->text toks)
  (define no-space-before '(")" "]" "." "," ";" "?" ":" ">"))
  (define keyword-before-paren
    '("=" "," ":" "|" "&" "=>" "extends" "?"))
  (define (separator prev text)
    (cond
      [(not prev) ""]
      [(member text no-space-before) ""]
      [(string=? text "}") (if (string=? prev "{") "" " ")]
      [(string=? text "[") (if (string=? prev "{") " " "")]
      [(string=? text "<") ""]
      [(and (string=? text ">") (string=? prev ">")) ""]
      [(string=? text "(") (if (member prev keyword-before-paren) " " "")]
      [(member prev '("(" "[" "." "<")) ""]
      [else " "]))
  (define (append-token out text)
    (define prev (and (pair? out) (car out)))
    (cons text (cons (separator prev text) out)))
  (string-join (reverse (for/fold ([out '()]) ([tok (in-list toks)])
                          (append-token out (format "~a" tok)))) ""))
