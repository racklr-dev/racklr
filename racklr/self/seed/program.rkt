#lang racket

;; racklr/self/seed/program.rkt
;;
;; The P6.2 seed compiler, written entirely as a UIR program. The program value
;; is a compiler for this tiny language:
;;
;;   source ::= form*
;;   form   ::= atom | '(' op expr* ')'
;;   atom   ::= integer | symbol
;;   expr   ::= atom | form
;;
;; It compiles that language to runnable Python 3: one `print(<python-expr>)`
;; line per top-level form. Arithmetic forms (+ - * /) become infix
;; parenthesized expressions, every other form becomes call syntax
;; `op(args...)`, and symbols/integers pass through unchanged.
;;
;; The seven named uir-fn stages each carry an explicit uir-effect-row:
;; tokenize, parse, token_atom, emit_expr, emit_program and compile_source are
;; pure (the empty row renders `@__withEffects({})` in the emitted Python, so
;; their totality is visible in the target source); main performs io
;; (input/print) and carries the io row. The uir-block ends with the entry call
;; `main()`.
;;
;; Behavior contract, proved by real execution in racklr-test/self-seed-test.rkt:
;; emitting this value with racklr/emit 'python and running the result under
;; python3 with stdin `(+ 1 (* 2 3))` prints `print((1 + (2 * 3)))`, and running
;; that compiled program prints `7`.

(require "builders.rkt")

(provide seed-program)

;; ── tokenize: manual character scan ────────────────────────────────
;;
;; toks = []; i = 0; n = len(src)
;; while i < n:
;;   c = src[i]
;;   if c.isspace():            i += 1
;;   elif c == "(" or c == ")": toks.append(c); i += 1
;;   else:                      scan while c is neither space nor paren,
;;                              accumulate the token, then append it
(define tokenize-fn
  (pure-def "tokenize" '(src)
    (body
     (assign "toks" (lst '()))
     (assign "i" (num 0))
     (assign "n" (call "len" (list (var "src"))))
     (while-loop
      (op "<" (list (var "i") (var "n")))
      (body
       (assign "c" (index (var "src") (var "i")))
       (if-else
        (mcall (var "c") "isspace" '())
        (body (assign "i" (op "+" (list (var "i") (num 1)))))
        (if-else
         (or* (op "==" (list (var "c") (str "(")))
              (op "==" (list (var "c") (str ")"))))
         (body
          (mcall (var "toks") "append" (list (var "c")))
          (assign "i" (op "+" (list (var "i") (num 1)))))
         (body
          (assign "tok" (str ""))
          (while-loop
           (and* (op "<" (list (var "i") (var "n")))
                 (op "not" (list (mcall (index (var "src") (var "i")) "isspace" '())))
                 (op "!=" (list (index (var "src") (var "i")) (str "(")))
                 (op "!=" (list (index (var "src") (var "i")) (str ")"))))
           (body
            (assign "tok" (op "+" (list (var "tok") (index (var "src") (var "i")))))
            (assign "i" (op "+" (list (var "i") (num 1))))))
          (mcall (var "toks") "append" (list (var "tok"))))))))
     (ret (var "toks")))))

;; ── parse: recursive descent over the token list ───────────────────
;;
;; Precondition: tokens[pos] == "(" (this call consumes the "(" itself).
;; pos += 1; items = []
;; while pos < len(tokens) and tokens[pos] != ")":
;;   if tokens[pos] == "(": sub = parse(tokens, pos)
;;                          items.append(sub[0]); pos = sub[1]
;;   else:                  items.append(token_atom(tokens[pos])); pos += 1
;; return [items, pos + 1]
(define parse-fn
  (pure-def "parse" '(tokens pos)
    (body
     (assign "pos" (op "+" (list (var "pos") (num 1))))
     (assign "items" (lst '()))
     (while-loop
      (and* (op "<" (list (var "pos") (call "len" (list (var "tokens")))))
            (op "!=" (list (index (var "tokens") (var "pos")) (str ")"))))
      (body
       (if-else
        (op "==" (list (index (var "tokens") (var "pos")) (str "(")))
        (body
         ;; Nested form: recurse and unpack the [items, next-pos] pair. parse
         ;; takes the position OF the "(" and steps past it itself.
         (assign "sub" (call "parse" (list (var "tokens") (var "pos"))))
         (mcall (var "items") "append" (list (index (var "sub") (num 0))))
         (assign "pos" (index (var "sub") (num 1))))
        (body
         (mcall (var "items") "append"
                (list (call "token_atom"
                            (list (index (var "tokens") (var "pos"))))))
         (assign "pos" (op "+" (list (var "pos") (num 1))))))))
     (ret (lst (list (var "items")
                     (op "+" (list (var "pos") (num 1)))))))))

;; ── token_atom: integer literal or symbol string ───────────────────
;;
;; return int(tok) if tok.isdigit() else tok
(define token-atom-fn
  (pure-def "token_atom" '(tok)
    (body
     (if-else
      (mcall (var "tok") "isdigit" '())
      (body (ret (call "int" (list (var "tok")))))
      (body (ret (var "tok")))))))

;; ── emit_expr: one parsed node to one Python expression ────────────
;;
;; if type(node) == list:
;;     op = node[0]; parts = [emit_expr(x) for x in node[1:]]
;;     if op in "+-*/" and len(parts) == 2:
;;         return "(" + parts[0] + " " + op + " " + parts[1] + ")"
;;     return op + "(" + ", ".join(parts) + ")"
;; if type(node) == str: return node
;; return str(node)
(define emit-expr-fn
  (let* (;; (op == "+" or op == "-" or op == "*" or op == "/")
         [infix? (paren (or* (op "==" (list (var "op") (str "+")))
                             (op "==" (list (var "op") (str "-")))
                             (op "==" (list (var "op") (str "*")))
                             (op "==" (list (var "op") (str "/")))))]
         [two-parts? (op "==" (list (call "len" (list (var "parts"))) (num 2)))]
         [infix-result (concat (list (str "(")
                                     (index (var "parts") (num 0))
                                     (str " ")
                                     (var "op")
                                     (str " ")
                                     (index (var "parts") (num 1))
                                     (str ")")))]
         [call-result (concat (list (var "op")
                                    (str "(")
                                    (mcall (str ", ") "join" (list (var "parts")))
                                    (str ")")))])
    (pure-def "emit_expr" '(node)
      (body
       (if-else
        (op "==" (list (call "type" (list (var "node"))) (sym "list")))
        (body
         (assign "items" (var "node"))
         (assign "op" (index (var "items") (num 0)))
         (assign "parts" (lst '()))
         (assign "i" (num 1))
         (while-loop (op "<" (list (var "i") (call "len" (list (var "items")))))
                     (body
                      (mcall (var "parts") "append"
                             (list (call "emit_expr"
                                         (list (index (var "items") (var "i"))))))
                      (assign "i" (op "+" (list (var "i") (num 1))))))
         (if-else (and* infix? two-parts?)
                  (body (ret infix-result))
                  (body (ret call-result))))
        (if-else
         (op "==" (list (call "type" (list (var "node"))) (sym "str")))
         (body (ret (var "node")))
         (body (ret (call "str" (list (var "node")))))))))))

;; ── emit_program: one print line per top-level form ────────────────
;;
;; for each form: parts.append("print(" + emit_expr(form) + ")")
;; return "\n".join(parts)
(define emit-program-fn
  (let* ([print-line (concat (list (str "print(")
                                   (call "emit_expr"
                                         (list (index (var "forms") (var "i"))))
                                   (str ")")))])
    (pure-def "emit_program" '(forms)
      (body
       (assign "parts" (lst '()))
       (assign "i" (num 0))
       (while-loop (op "<" (list (var "i") (call "len" (list (var "forms")))))
                   (body
                    (mcall (var "parts") "append" (list print-line))
                    (assign "i" (op "+" (list (var "i") (num 1))))))
       ;; "\n".join(parts); the UIR string holds the decoded content (one real
       ;; newline character), which the Python emitter re-encodes as the escape
       ;; `\n` in the generated source.
       (ret (mcall (str "\n") "join" (list (var "parts"))))))))

;; ── compile_source: tokenize, parse every top-level form, emit ─────
;;
;; tokens = tokenize(src); forms = []; pos = 0
;; while pos < len(tokens):
;;   if tokens[pos] == "(": r = parse(tokens, pos)
;;                          forms.append(r[0]); pos = r[1]
;;   else:                  forms.append(token_atom(tokens[pos])); pos += 1
;; return emit_program(forms)
(define compile-source-fn
  (pure-def "compile_source" '(src)
    (body
     (assign "tokens" (call "tokenize" (list (var "src"))))
     (assign "forms" (lst '()))
     (assign "pos" (num 0))
     (while-loop
      (op "<" (list (var "pos") (call "len" (list (var "tokens")))))
      (body
       (if-else
        (op "==" (list (index (var "tokens") (var "pos")) (str "(")))
        (body
         (assign "r" (call "parse" (list (var "tokens") (var "pos"))))
         (mcall (var "forms") "append" (list (index (var "r") (num 0))))
         (assign "pos" (index (var "r") (num 1))))
        (body
         (mcall (var "forms") "append"
                (list (call "token_atom"
                            (list (index (var "tokens") (var "pos"))))))
         (assign "pos" (op "+" (list (var "pos") (num 1))))))))
     (ret (call "emit_program" (list (var "forms")))))))

;; ── main: read the source, print the compiled program, exit 0 ──────
;;
;; src = input(); out = compile_source(src); print(out); return 0
;;
;; The compiled program goes into a local before `print`: a directly-performed
;; effect call's argument expressions are re-evaluated for the runtime guard's
;; bookkeeping, so nesting compile_source inside print would compile twice.
(define main-fn
  (io-def "main" '()
    (body
     (assign "src" (call "input" '()))
     (assign "out" (call "compile_source" (list (var "src"))))
     (op "print" (list (var "out")))
     (ret (num 0)))))

;; ── The program value: definitions, then the entry call ────────────

(define seed-program
  (program
   tokenize-fn
   parse-fn
   token-atom-fn
   emit-expr-fn
   emit-program-fn
   compile-source-fn
   main-fn
   (call "main" '())))
