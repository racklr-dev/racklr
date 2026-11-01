#lang racket

;; Phase 1 (grammar verification) for the Haskell front end.
;;
;; Loads the tracked, authored pass-1 grammar `grammars/haskell/Haskell.g4`
;; with layout token insertion (NEWLINE/INDENT/DEDENT, the Koka model) and
;; asserts tokenizer behavior plus parser smoke coverage of the Phase-1
;; fixture set from docs/plans/haskell-pipeline.md. The grammar is authored/
;; curated, not the upstream antlr/grammars-v4 pair: see
;; grammars/haskell/README.md for the acquisition attempt and its evidence.

(require rackunit
         rackunit/text-ui
         racklr/tree
         racklr/gen-test)

;; ── Load the tracked grammar (relative to racklr-test/) ─────────────

(define-values (hs-parse hs-tokenize tok-type tok-value)
  (gen-and-load-layout "../grammars/haskell/Haskell.g4"))

;; ── Helpers ─────────────────────────────────────────────────────────

(define (tok-types src) (map tok-type (hs-tokenize src)))
(define (first-tok src) (first (hs-tokenize src)))
(define (count-type src type) (length (filter (lambda (t) (eq? t type)) (tok-types src))))

(define (parses? src)
  (with-handlers ([(lambda (e) (exn:fail? e)) (lambda (_) #f)])
    (any-tree? (hs-parse src))))

;; Rule tags in the CST. Generator nodes can hold child lists (star/plus),
;; so walk lists as well as nodes.
(define (cst-tags src)
  (define (walk x acc)
    (cond [(cst-node? x)
           (for/fold ([a (cons (cst-node-tag x) acc)])
                     ([ch (in-list (cst-node-children x))])
             (walk ch a))]
          [(list? x) (for/fold ([a acc]) ([ch (in-list x)]) (walk ch a))]
          [else acc]))
  (reverse (walk (hs-parse src) '())))

(define (has-tag? src tag) (and (member tag (cst-tags src)) #t))

;; ── Tokenizer cases ─────────────────────────────────────────────────

(define keyword-cases
  '(("module" . MODULE) ("where" . WHERE) ("import" . IMPORT)
    ("qualified" . QUALIFIED) ("as" . AS) ("hiding" . HIDING)
    ("data" . DATA) ("newtype" . NEWTYPE) ("type" . TYPE)
    ("class" . CLASS) ("instance" . INSTANCE) ("deriving" . DERIVING)
    ("let" . LET) ("in" . IN) ("do" . DO) ("case" . CASE) ("of" . OF)
    ("if" . IF) ("then" . THEN) ("else" . ELSE)
    ("infix" . INFIX) ("infixl" . INFIXL) ("infixr" . INFIXR)))

(define literal-cases
  '(("42" . DECIMAL)
    ("0x1F" . HEX)
    ("0o17" . OCTAL)
    ("2.5e3" . FLOAT)
    ("'a'" . CHAR)
    ("\"hi\"" . STRING)))

(define operator-cases
  '(("::" . DOUBLE_COLON) ("->" . ARROW) ("<-" . LARROW) ("=>" . FAT_ARROW)
    ("=" . EQUALS) ("|" . PIPE) ("\\" . BACKSLASH) ("@" . AT)
    ("." . DOT) ("," . COMMA) (";" . SEMI) (":" . COLON) (".." . DOUBLE_DOT)
    ("||" . OROR) ("&&" . AMPAMP) ("==" . EQEQ) ("/=" . NEQ)
    ("<=" . LTE) (">=" . GTE) ("<" . LT) (">" . GT)
    ("+" . PLUS) ("-" . MINUS) ("*" . STAR) ("/" . SLASH)
    ("%" . PERCENT) ("^" . CARET) ("++" . CONCAT) ("$" . DOLLAR)
    ("<>" . SYMBOL) (">>=" . SYMBOL) ("<*>" . SYMBOL)))

(define identifier-cases
  '(("foo" . VARID) ("Foo" . CONID) ("foo'" . VARID) ("foo_bar2" . VARID)
    ("_" . WILDCARD) ("_x" . VARID)))

(define (check-keywords)
  (for ([pair (in-list keyword-cases)])
    (define kw (car pair))
    (define tok (first-tok kw))
    (check-equal? (tok-type tok) (cdr pair) (format "keyword ~a" kw))
    (check-equal? (tok-value tok) kw (format "keyword value ~a" kw))))

(define (check-literals)
  (for ([pair (in-list literal-cases)])
    (define src (car pair))
    (define tok (first-tok src))
    (check-equal? (tok-type tok) (cdr pair) (format "literal ~s" src))
    (check-equal? (tok-value tok) src (format "literal value ~s" src))))

(define (check-operators)
  (for ([pair (in-list operator-cases)])
    (define op (car pair))
    (define tok (first-tok op))
    (check-equal? (tok-type tok) (cdr pair) (format "operator ~s" op))
    (check-equal? (tok-value tok) op (format "operator value ~s" op))))

(define (check-identifiers)
  (for ([pair (in-list identifier-cases)])
    (define src (car pair))
    (define tok (first-tok src))
    (check-equal? (tok-type tok) (cdr pair) (format "identifier ~s" src))
    (check-equal? (tok-value tok) src (format "identifier value ~s" src)))
  (check-equal? (tok-types "Data.List.sort") '(CONID DOT CONID DOT VARID EOF)
                "qualified value name")
  (check-equal? (tok-types "Data.Maybe.Just") '(CONID DOT CONID DOT CONID EOF)
                "qualified constructor name"))

(define (check-string-escapes)
  (check-equal? (tok-types "\"a\\nb\\t\\\"c\"") '(STRING EOF))
  (check-equal? (tok-types "'\\n'") '(CHAR EOF))
  (check-equal? (tok-types "'\\''") '(CHAR EOF))
  (check-exn exn:fail? (lambda () (hs-tokenize "\"unterminated"))))

(define (check-comments)
  (check-equal? (tok-types "x = 1 -- trailing\n") '(VARID EQUALS DECIMAL NEWLINE EOF))
  (check-equal? (tok-types "x = 1 {- block -}\ny = 2\n")
                '(VARID EQUALS DECIMAL NEWLINE VARID EQUALS DECIMAL NEWLINE EOF))
  (check-equal? (count-type "x = 1 -- only\n-- comment lines\ny = 2\n" 'COMMENT) 0))

;; ── Tokenizer: layout (NEWLINE / INDENT / DEDENT) ───────────────────

(define (check-layout-body)
  (check-equal?
   (tok-types "f x =\n  x + 1\n")
   '(VARID VARID EQUALS NEWLINE INDENT VARID PLUS DECIMAL NEWLINE DEDENT EOF)
   "one INDENT after the deeper newline, one DEDENT before EOF"))

(define (check-layout-do)
  (check-equal?
   (tok-types "main = do\n  a\n  b\n")
   '(VARID EQUALS DO NEWLINE INDENT VARID NEWLINE VARID NEWLINE DEDENT EOF)
   "same-indent statements are separated by NEWLINE, one INDENT/DEDENT"))

(define (check-layout-nested)
  (check-equal? (count-type "f x = y\n  where\n    y = x + 1\n" 'INDENT) 2)
  (check-equal? (count-type "f x = y\n  where\n    y = x + 1\n" 'DEDENT) 2))

(define (check-layout-blank-lines)
  (check-equal? (tok-types "x = 1\n\n\n") '(VARID EQUALS DECIMAL NEWLINE EOF)
                "blank lines collapse to one NEWLINE"))

(define (check-layout-joining)
  ;; racklr's layout pass drops NEWLINE while a bracket is open, so a
  ;; multi-line list/tuple is one contiguous token run.
  (check-equal? (tok-types "xs = [1,\n 2]\n")
                '(VARID EQUALS OPEN_BRACK DECIMAL COMMA DECIMAL CLOSE_BRACK
                  NEWLINE EOF))
  (check-equal? (count-type "xs = [1,\n 2]\n" 'NEWLINE) 1))

;; ── Parser: Phase-1 fixture set ─────────────────────────────────────

;; Each fixture: (label source intended-rule-tags ...).
(define phase1-fixtures
  (list
   (list "top-level binding + type signature"
         "f :: Int -> Int\nf x = x + 1\n"
         'type_sig 'value_decl 'funlhs)
   (list "polymorphic type signature"
         "map2 :: (a -> b) -> [a] -> [b]\n"
         'type_sig 'atype)
   (list "typeclass-constrained signature"
         "foo :: Eq a => a -> Bool\n"
         'type_sig 'context 'constraint)
   (list "module header + imports"
         "module Main where\n\nimport Data.List\nimport qualified Data.Map as M\n"
         'module_header 'import_decl)
   (list "module with export list"
         "module Foo (bar, Baz (..)) where\nbar = 1\n"
         'module_header 'exports 'value_decl)
   (list "if/then/else expression"
         "g x = if x > 0 then x else 0 - x\n"
         'if_expr 'rel_op)
   (list "multiline if/then/else in a body block"
         "g x =\n  if x > 0\n  then 1\n  else 2\n"
         'if_expr)
   (list "case with constructor patterns"
         "h x = case x of\n  Just y -> y\n  Nothing -> 0\n"
         'case_expr 'case_alts 'case_alt 'pat)
   (list "case with guards"
         "h x = case x of\n  n | n > 0 -> n\n  n -> 0 - n\n"
         'case_expr 'case_alt)
   (list "let expression (inline)"
         "f x = let y = x + 1 in y * 2\n"
         'let_expr 'let_binds)
   (list "let expression (multiline body block)"
         "f x =\n  let y = x + 1\n  in y * 2\n"
         'let_expr)
   (list "where clause (inline)"
         "f x = y\n  where y = x + 1\n"
         'where_clause 'value_decl)
   (list "where clause (layout block)"
         "f x = y\n  where\n    y = x + 1\n"
         'where_clause 'value_decl)
   (list "data with positional constructors"
         "data Shape = Circle Double | Square Double deriving (Show, Eq)\n"
         'data_decl 'data_body 'constr 'deriving_list)
   (list "data with layout constructors"
         "data Shape\n  = Circle Double\n  | Square Double\n  deriving (Show)\n"
         'data_decl 'data_body 'data_item)
   (list "data with named-field constructor"
         (string-append "data Person = Person { name :: String\n"
                        "                     , age :: Int\n"
                        "                     }\n")
         'data_decl 'constr 'field_decl)
   (list "data with type variables"
         "data Maybe a = Nothing | Just a\n"
         'data_decl 'tyvar)
   (list "newtype"
         "newtype UserId = UserId Int\n"
         'newtype_decl 'constr)
   (list "typeclass declaration"
         "class Eq a where\n  eq :: a -> a -> Bool\n"
         'class_decl 'class_body 'method_sig)
   (list "instance declaration"
         "instance Eq Shape where\n  eq (Circle a) (Circle b) = a == b\n  eq _ _ = False\n"
         'instance_decl 'instance_body 'value_decl)
   (list "do-notation"
         "main :: IO ()\nmain = do\n  putStrLn \"hi\"\n  name <- getLine\n  putStrLn name\n"
         'do_expr 'do_stmt 'pat)
   (list "do-notation with pattern bind"
         "main = do\n  Just x <- getLine\n  putStrLn x\n"
         'do_expr 'do_stmt 'pat)
   (list "lambda"
         "apply = \\x -> x + 1\n"
         'lambda_expr 'apat)
   (list "operator precedence ladder"
         "calc a b = a + b * 2 - a / b\n"
         'op_expr 'add_op 'mul_op)
   (list "boolean and comparison operators"
         "cmp x y = x == y || x < y && y >= 0\n"
         'op_expr 'rel_op)
   (list "composition, dollar and custom operators"
         "compose f g = f . g\npipeline x = x $ id\ncombine a b = a <> b\n"
         'op_expr 'op6 'op7 'op8)
   (list "lists and tuples"
         "xs = [1, 2, 3]\npair = (1, \"two\")\n"
         'list_expr 'atom)
   (list "fixity declaration"
         "infixl 6 <+>\n"
         'fixity_decl 'fixity 'prec 'ops)
   (list "parenthesised operator definition"
         "(<+>) a b = a + b\n"
         'funlhs 'value_decl)
   (list "unary minus"
         "neg = -1\n"
         'minus_expr)
   (list "comments between declarations"
         "x = 1 -- trailing\n-- standalone\n{- block\n   comment -}\ny = 2\n"
         'value_decl)
   (list "combined module"
         (string-append
          "module Shapes where\n\n"
          "import Data.List\n\n"
          "data Shape = Circle Double | Square Double deriving (Show)\n\n"
          "area :: Shape -> Double\n"
          "area s = case s of\n"
          "  Circle r -> r * r\n"
          "  Square w -> w * w\n\n"
          "offset :: Shape -> Shape\n"
          "offset s = let n = 1 in s\n\n"
          "main :: IO ()\n"
          "main = do\n"
          "  let s = Circle 1.0\n"
          "  print (area s)\n")
         'module_header 'import_decl 'data_decl 'case_expr 'do_expr 'let_expr)))

(define (check-phase1-fixtures)
  (for ([fixture (in-list phase1-fixtures)])
    (define label (first fixture))
    (define src (second fixture))
    (check-true (parses? src) (format "~a: should parse" label))
    (for ([tag (in-list (drop fixture 2))])
      (check-true (has-tag? src tag)
                  (format "~a: CST should contain rule ~a" label tag)))))

(define (check-empty-module)
  (check-true (parses? ""))
  (check-true (parses? "\n")))

(define (check-rejects)
  (define rejects
    (list
     (cons "missing binding body" "f x =\n")
     (cons "empty case" "f x = case x of\n")
     (cons "if without else" "f x = if x then 1\n")
     (cons "let without in" "f x = let y = 1\n")
     (cons "dangling operator" "f x = x +\n")
     (cons "unclosed paren" "f x = (x + 1\n")
     (cons "unclosed bracket" "f x = [1, 2\n")
     (cons "extra close paren" "f x = x)\n")
     (cons "data without constructor" "data Shape = \n")
     (cons "data without type name" "data = Circle\n")
     (cons "case alternative without arrow" "f x = case x of\n  Just y\n")
     (cons "empty do block" "f x = do\n")
     (cons "instance without head" "instance where\n")
     (cons "module without name" "module where\n")
     (cons "stray input" "%%% not haskell %%%\n")))
  (for ([pair (in-list rejects)])
    (check-false (parses? (cdr pair))
                 (format "~a: should be rejected" (car pair)))))

;; ── Run ────────────────────────────────────────────────────────────

(define suite
  (test-suite
   "Haskell pass-1 grammar (tracked grammars/haskell/Haskell.g4)"
   (test-case "tokenizer: keywords" (check-keywords))
   (test-case "tokenizer: literals" (check-literals))
   (test-case "tokenizer: operators" (check-operators))
   (test-case "tokenizer: identifiers" (check-identifiers))
   (test-case "tokenizer: string and character escapes" (check-string-escapes))
   (test-case "tokenizer: comments are skipped" (check-comments))
   (test-case "layout: indented body" (check-layout-body))
   (test-case "layout: do block" (check-layout-do))
   (test-case "layout: nested blocks" (check-layout-nested))
   (test-case "layout: blank lines" (check-layout-blank-lines))
   (test-case "layout: implicit line joining" (check-layout-joining))
   (test-case "parser: Phase-1 fixture set" (check-phase1-fixtures))
   (test-case "parser: empty module" (check-empty-module))
   (test-case "parser: reject cases" (check-rejects))))

;; Always remove the generated temp module from racklr-test/, even if an
;; assertion raises.
(define result (dynamic-wind void (lambda () (run-tests suite)) cleanup))
(exit result)
