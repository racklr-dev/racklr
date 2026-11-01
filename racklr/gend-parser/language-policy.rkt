#lang racket

;; racklr/gend-parser/language-policy.rkt
;;
;; Absorb-layer language policy as INJECTABLE DATA: the host-language knowledge
;; the generic parser generator needs beyond the .g4 grammar itself.
;;
;;   layout-helper-source   — generated Racket source for the host's layout
;;                            (indentation) token post-pass, or #f.
;;   layout-tokenize-wrapper— generated `tokenize` wrapper that applies the
;;                            post-pass, or #f.
;;   newline-token-name     — the lexer token that carries line breaks, or #f.
;;   template-mode-name     — the lexer mode in which template literals are
;;                            lexed (whitespace handling differs there), or #f.
;;   rule-post-process      — a hook: (rule-name inner-body token-expr -> code
;;                            string or #f), for lexer rules whose matched text
;;                            needs host-specific bookkeeping.
;;   entry-rule-names       — preferred parser entry-rule names, in order.
;;
;; The default policy carries the Python / JS / TS / Clojure front ends' current
;; policy (their generated parsers are byte-identical to before this module
;; existed). A new front end supplies its own policy via
;; `generate-parser-module #:policy`, so it edits no core generator code.
;;
;; This module is DATA (a front end's generated-code policy), not the generic
;; engine: racklr/gend-parser/lexer.rkt and parser.rkt carry no host rule names.

(provide language-policy? language-policy
         language-policy-name
         language-policy-layout-helper-source
         language-policy-layout-tokenize-wrapper
         language-policy-newline-token-name
         language-policy-template-mode-name
         language-policy-rule-post-process
         language-policy-entry-rule-names
         default-language-policy)

;; The host's generated-code policy (see the module comment).
(struct language-policy
  (name
   layout-helper-source
   layout-tokenize-wrapper
   newline-token-name
   template-mode-name
   rule-post-process
   entry-rule-names)
  #:transparent)

;; Lexer-rule post-processing for ANTLR template-literal style grammars
;; (JavaScript / TypeScript / JSX): backticks open/close template mode, `${`
;; opens a brace-balanced expression that must pop back to template mode.
;; The generic engine only calls this hook; the rule names and the emitted
;; bookkeeping live here.
(define (template-rule-post-process raw-name inner-body token-expr)
  (cond
    [(string=? raw-name "BackTick")
     (string-append "(set-box! template-depth (+ (unbox template-depth) 1)) " inner-body)]
    [(string=? raw-name "BackTickInside")
     (string-append "(set-box! template-depth (- (unbox template-depth) 1)) " inner-body)]
    [(string=? raw-name "TemplateStringStartExpression")
     (string-append "(set-box! brace-depth 1) " inner-body)]
    [(string=? raw-name "OpenBrace")
     (string-append "(when (> (unbox template-depth) 0) (set-box! brace-depth (+ (unbox brace-depth) 1))) " inner-body)]
    [(string=? raw-name "CloseBrace")
     (string-append
      "(let ([new-bd (if (> (unbox template-depth) 0)"
      "                  (- (unbox brace-depth) 1)"
      "                  (unbox brace-depth))])"
      "  (set-box! brace-depth new-bd)"
      "  (if (and (> (unbox template-depth) 0) (= new-bd 0))"
      "      (let ([next-mode (if (null? mstack) 'default (car mstack))]"
      "            [next-mstack (if (null? mstack) '() (cdr mstack))])"
      "        (define sl (string-length v))"
      "        (define tk (token " token-expr " v (pos l c o) (pos l (+ c sl) (+ o sl))))"
      "        (loop np l (+ c sl) (+ o sl) (cons tk tks) next-mode next-mstack pending))"
      "      (let ()"
      "        (define sl (string-length v))"
      "        (define tk (token " token-expr " v (pos l c o) (pos l (+ c sl) (+ o sl))))"
      "        (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending))))")]
    [(string=? raw-name "TemplateCloseBrace")
     (string-append
      "(let ([new-bd (if (> (unbox template-depth) 0)"
      "                  (- (unbox brace-depth) 1)"
      "                  (unbox brace-depth))])"
      "  (set-box! brace-depth new-bd)"
      "  (if (and (> (unbox template-depth) 0) (= new-bd 0))"
      "      (let ([next-mode (if (null? mstack) 'default (car mstack))]"
      "            [next-mstack (if (null? mstack) '() (cdr mstack))])"
      "        (define sl (string-length v))"
      "        (define tk (token " token-expr " v (pos l c o) (pos l (+ c sl) (+ o sl))))"
      "        (loop np l (+ c sl) (+ o sl) (cons tk tks) next-mode next-mstack pending))"
      "      (let ()"
      "        (define sl (string-length v))"
      "        (define tk (token " token-expr " v (pos l c o) (pos l (+ c sl) (+ o sl))))"
      "        (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending))))")]
    [else #f]))

;; The layout (INDENT/DEDENT) token post-pass source the generated lexer embeds
;; for indentation-sensitive hosts (Python 3, Koka). This is the Python model:
;; spaces count 1, tabs advance to the next multiple of 8, brackets suppress
;; implicit newlines, and a NEWLINE's trailing whitespace is the next line's
;; indentation. A host with a different layout model supplies its own source.
(define python-layout-helper-source
  "\n;; Python indent computation: spaces count 1, tabs advance to next multiple of 8\n(define (compute-indent ws)\n  (let loop ([i 0] [col 0])\n    (if (>= i (string-length ws))\n        col\n        (let ([c (string-ref ws i)])\n          (if (char=? c #\\tab)\n              (loop (+ i 1) (+ col (- 8 (modulo col 8))))\n              (loop (+ i 1) (+ col 1)))))))\n\n(define (paren-open? type)\n  (member type '(OPEN_PAREN OPEN_BRACK OPEN_BRACE)))\n\n(define (paren-close? type)\n  (member type '(CLOSE_PAREN CLOSE_BRACK CLOSE_BRACE)))\n\n;; Strip the newline prefix from a NEWLINE token value, returning\n;; (values newline-str indent-ws).\n(define (split-newline-value v)\n  (define vlen (string-length v))\n  (cond [(and (>= vlen 2) (char=? (string-ref v 0) #\\return)\n              (char=? (string-ref v 1) #\\newline))\n         (values \"\\r\\n\" (substring v 2 vlen))]\n        [(and (>= vlen 1) (or (char=? (string-ref v 0) #\\newline)\n                              (char=? (string-ref v 0) #\\return)\n                              (char=? (string-ref v 0) #\\page)))\n         (values (substring v 0 1) (substring v 1 vlen))]\n        [else (values v \"\")]))\n\n;; Post-process token list to insert INDENT/DEDENT tokens\n;; raw-tokens: list of (token ...) including EOF at end\n;; Returns: new list with INDENT/DEDENT inserted (EOF preserved at end)\n(define (insert-indents raw-tokens)\n  (define indent-stack (list 0))\n  (define opened 0)\n  (define dummy-pos (pos 0 0 0))\n  (define result '())\n  (define (emit t) (set! result (cons t result)))\n  (define (emit-indent indent-ws)\n    (emit (token 'INDENT indent-ws dummy-pos dummy-pos)))\n  (define (emit-dedent)\n    (emit (token 'DEDENT \"\" dummy-pos dummy-pos)))\n  ;; Collapse a leading run of consecutive NEWLINE tokens. Blank and\n  ;; comment-only lines each contribute a spurious NEWLINE; only the run's\n  ;; final NEWLINE carries the next real line's indentation. Returns the\n  ;; final NEWLINE and the remaining tokens after the run.\n  (define (skip-newlines toks)\n    (let nl-loop ([prev (car toks)] [rest (cdr toks)])\n      (if (and (pair? rest) (eq? (token-type (car rest)) 'NEWLINE))\n          (nl-loop (car rest) (cdr rest))\n          (values prev rest))))\n\n  (let loop ([remaining raw-tokens])\n    (if (null? remaining)\n        (begin\n          (for ([_ (in-range (sub1 (length indent-stack)))])\n            (emit-dedent))\n          (reverse result))\n        (let ([t (car remaining)]\n              [rest (cdr remaining)])\n          (define type (token-type t))\n          (define value (token-value t))\n          (cond\n            [(eq? type 'EOF)\n             ;; EOF: emit pending DEDENTs then EOF\n             (for ([_ (in-range (sub1 (length indent-stack)))])\n               (emit-dedent))\n             (emit t)\n             (reverse result)]\n            [(paren-open? type)\n             (set! opened (+ opened 1))\n             (emit t)\n             (loop rest)]\n            [(paren-close? type)\n             (set! opened (max 0 (- opened 1)))\n             (emit t)\n             (loop rest)]\n            [(eq? type 'NEWLINE)\n             ;; Inside an open bracket a NEWLINE is Python implicit line\n             ;; joining: drop it entirely. At depth 0, collapse any run of\n             ;; consecutive NEWLINEs to the last one (blank/comment-only\n             ;; lines) and process that line's indentation.\n             (if (= opened 0)\n                 (let*-values ([(nl rest-after-run) (skip-newlines remaining)]\n                               [(nl-str indent-ws) (split-newline-value (token-value nl))])\n                   (let ([indent (compute-indent indent-ws)]\n                         [current-top (car indent-stack)])\n                     (emit (token 'NEWLINE nl-str (token-start nl) (token-end nl)))\n                     (cond\n                       [(> indent current-top)\n                        (set! indent-stack (cons indent indent-stack))\n                        (emit-indent indent-ws)]\n                       [(< indent current-top)\n                        (let pop-loop ()\n                          (when (and (pair? indent-stack) (> (car indent-stack) indent))\n                            (set! indent-stack (cdr indent-stack))\n                            (emit-dedent)\n                            (pop-loop)))\n                        (when (or (null? indent-stack) (not (= (car indent-stack) indent)))\n                          (error 'insert-indents\n                                 \"unindent does not match any outer indentation level\"))]\n                       [else (void)])\n                     (loop rest-after-run)))\n                 (loop rest))]\n            [else\n              (emit t)\n              (loop rest)])))))\n")

;; The front ends' default policy: Python/Koka layout + JS/TS template literals.
(define default-language-policy
  (language-policy
   'default
   python-layout-helper-source
   "
(define (tokenize in) (insert-indents (tokenize-raw in)))
"
   "NEWLINE"
   "TEMPLATE"
   template-rule-post-process
   '("program")))

(module+ main
  (displayln "racklr/gend-parser/language-policy loaded."))