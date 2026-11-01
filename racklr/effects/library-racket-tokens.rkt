#lang racket

;; racklr/effects/library-racket-tokens.rkt
;;
;; The Racket standard-library effect lexicon. This module is DATA, not
;; engine code: it names Racket procedures and their effects so the token
;; tables in library-tokens.rkt can classify calls in the compiler's own
;; (Racket) UIR. library-tokens.rkt requires this module and only unions the
;; sets below into the existing effect-tokens fields; no engine module
;; (effect-roots.rkt, pure-roots.rkt, classify.rkt, infer.rkt) carries Racket
;; names.
;;
;; Provenance: every name is either a call head in the compiler corpus
;; (racklr/**.rkt + effervesce/**.rkt, read as datums, special forms and
;; corpus-defined top-level names removed) or a Racket core/library procedure
;; the ticket's ground-truth list requires. Each name was checked as a bound
;; procedure in the Racket 9.3 library. Effect keys are grounded in the
;; concrete call chain: io = ports/streams/files/directories/display family;
;; process = process spawn/control; time = clock reads; network = sockets.
;;
;; Policy: names whose effect is not a fact about the procedure stay OUT, so
;; they remain tracked `unknown` with an elimination obligation. Deliberately
;; absent: dynamic-require / eval / eval-syntax (dynamic evaluation), random
;; and shuffle (RNG state), and higher-order application (`apply`) whose
;; callee is not known at the call site. `read` is io even when handed an
;; in-memory string port: a name table cannot see the receiver, so it takes
;; the conservative direction. `format`/`~a` return strings and are pure;
;; printing (`printf`/`fprintf`/`display`) is io.

(provide racket-pure-builtins
         racket-effect-builtins
         racket-effect-builtin-names
         racket-io-builtins
         racket-time-builtins
         racket-network-builtins
         racket-process-root-effects)

;; ── purely computational procedures ─────────────────────────────────
;;
;; Side-effect-free value construction, inspection, and transformation: no
;; port is touched, no directory is read, no clock is read, no process is
;; started. Mutating a locally allocated hash/vector/box is computation, not
;; a domain effect.
(define racket-pure-builtins
  (set
   ;; pairs, lists, and higher-order traversal
   "cons" "list" "cons?" "car" "cdr" "caar" "cadr" "cdar" "cddr" "caadr" "caddr" "cdadr"
   "cdaddr" "caaddr" "cadddr" "cdddr" "cddddr" "first" "second" "third" "fourth" "fifth"
   "sixth" "rest" "last" "length" "append" "append*" "append-map" "reverse" "list-ref"
   "list?" "pair?" "null?" "empty?" "member" "memq" "memv" "remove-duplicates" "take" "drop"
   "drop-right" "list-tail" "list-set" "list-update" "make-list" "range" "in-range"
   "in-list" "in-naturals" "in-value" "in-vector" "in-string" "in-hash" "in-hash-keys"
   "in-hash-values" "in-indexed" "list*" "assoc" "assq" "assv"
   ;; hash tables and sets
   "hash" "hasheq" "make-hash" "make-hasheq" "make-hasheqv" "hash-ref" "hash-ref!"
   "hash-set" "hash-set!" "hash-set*" "hash-remove" "hash-remove!" "hash-clear"
   "hash-clear!" "hash-update" "hash-update!" "hash-copy" "hash-has-key?" "hash-count"
   "hash-keys" "hash-values" "hash->list" "hash-empty?" "hash-union" "hash-union!"
   "hash-filter" "hash-for-each" "hash-map" "hash?" "set" "set-add" "set-add!" "set-member?"
   "set-count" "set->list" "list->set" "set-union" "set-remove" "set-intersect"
   "set-subtract" "set-map" "set-for-each" "set=?" "set?" "mutable-set"
   ;; strings, characters, symbols, and regexps
   "string" "string-append" "string-length" "string-ref" "substring" "string-copy"
   "string-copy!" "string-fill!" "string=?" "string<?" "string>?" "string<=?" "string>=?"
   "string-ci=?" "string-ci<?" "string-ci>?" "string-join" "string-split" "string-trim"
   "string-contains?" "string-prefix?" "string-suffix?" "string-replace" "string-downcase"
   "string-upcase" "string-titlecase" "string-foldcase" "string->list" "list->string"
   "string->symbol" "symbol->string" "symbol=?" "symbol<?" "string->number" "number->string"
   "make-string" "string->immutable-string" "string->bytes/utf-8" "bytes->string/utf-8"
   "bytes" "bytes?" "bytes-length" "bytes-ref" "subbytes" "bytes-append" "bytes->list"
   "list->bytes" "string->bytes/locale" "bytes->string/locale" "integer->char"
   "char->integer" "char?" "char=?" "char<=?" "char-alphabetic?" "char-upper-case?"
   "char-numeric?" "char-whitespace?" "char-upcase" "char-downcase" "keyword?"
   "keyword->string" "string->keyword" "regexp-match" "regexp-match?" "regexp-match*"
   "regexp-match-positions" "regexp-replace" "regexp-replace*" "regexp-split" "regexp-quote"
   "pregexp"
   ;; pure path arithmetic (filesystem-consulting helpers are io)
   "build-path" "path->string" "string->path" "split-path" "explode-path" "path?"
   "path-string?" "path->complete-path" "path-only" "file-name-from-path"
   ;; numbers, equality, predicates, values
   "+" "-" "*" "/" "=" "<" ">" "<=" ">=" "add1" "sub1" "abs" "min" "max" "modulo" "quotient"
   "remainder" "round" "floor" "ceiling" "truncate" "sqr" "sqrt" "expt" "exp" "log" "sin"
   "cos" "tan" "atan" "numerator" "denominator" "gcd" "lcm" "exact->inexact"
   "inexact->exact" "arithmetic-shift" "bitwise-and" "bitwise-ior" "bitwise-xor"
   "bitwise-not" "zero?" "positive?" "negative?" "even?" "odd?" "number?" "integer?"
   "exact-integer?" "exact-positive-integer?" "exact-nonnegative-integer?" "real?"
   "rational?" "complex?" "exact?" "inexact?" "nan?" "infinite?" "real->decimal-string"
   "eq?" "eqv?" "equal?" "not" "values" "void" "raise" "symbol?" "string?"
   "boolean?" "vector?" "procedure?" "struct?" "struct->vector" "eof-object?" "immutable?"
   "port?" "input-port?" "output-port?"
   ;; vectors
   "vector" "make-vector" "vector-ref" "vector-set!" "vector-length" "vector-map"
   "vector-fill!" "vector-copy" "vector-copy!" "vector-append" "vector-member" "vector-memq"
   "vector-sort" "vector-sort!" "vector->list" "list->vector"
   ;; boxes, parameters, exceptions, functions
   "box" "box?" "unbox" "unbox*" "set-box!" "make-parameter"
   "current-command-line-arguments" "with-input-from-string" "exn?" "exn:fail?"
   "exn-message" "exn:fail:contract?" "exn:fail:filesystem?" "exn-continuation-marks"
   "identity" "const" "compose" "curry" "negate" "call-with-values" "gensym" "format" "~a"
   "~s" "~v" "~e"
   ;; higher-order traversal and comparison (the combinator itself is pure)
   "for-each" "map" "filter" "andmap" "ormap" "findf" "filter-map" "memf" "foldl" "foldr"
   "sort" "count"
   ;; in-memory ports (string ports; no external I/O)
   "open-output-string" "get-output-string"
   ;; json serialization (pure)
   "jsexpr->string" "jsexpr->bytes" "string->jsexpr" "bytes->jsexpr" "jsexpr?"
   ;; rackunit test assertions (in-memory checks; raise on failure)
   "check-equal?" "check-true" "check-false" "check-not-false"
   ))

;; ── effectful procedures ────────────────────────────────────────────
;;
;; name → effect symbol. The effect is a property of the procedure itself
;; (`display` writes to a port), not of how it is called: no heuristic.
(define racket-effect-builtins
  (hash
   ;; io — ports, streams, files, directories, and the display/print/read family
   "display" 'io "displayln" 'io "display-to-file" 'io "print" 'io "eprintf" 'io "printf"
   'io "fprintf" 'io "newline" 'io "write" 'io "write-string" 'io "write-bytes" 'io
   "write-char" 'io "flush-output" 'io "read" 'io "read-line" 'io "read-string" 'io
   "read-bytes" 'io "read-char" 'io "peek-char" 'io "read-json" 'io "port->string" 'io
   "port->lines" 'io "port->bytes" 'io "file->string" 'io "file->bytes" 'io "file->lines"
   'io "open-input-file" 'io "open-output-file" 'io "call-with-input-file" 'io
   "call-with-output-file" 'io "with-input-from-file" 'io "with-output-to-file" 'io
   "close-input-port" 'io "close-output-port" 'io "file-exists?" 'io "directory-exists?" 'io
   "link-exists?" 'io "file-size" 'io "file-or-directory-modify-seconds" 'io
   "file-or-directory-permissions" 'io "delete-file" 'io "delete-directory" 'io "copy-file"
   'io "rename-file-or-directory" 'io "make-file-or-directory-link" 'io "make-directory" 'io
   "make-directory*" 'io "make-temporary-file" 'io "directory-list" 'io "current-directory"
   'io "find-executable-path" 'io "simplify-path" 'io "normalize-path" 'io
   "find-relative-path" 'io "write-json" 'io
   ;; process — spawn, control, and terminate OS processes
   "system" 'process "system*" 'process "system/exit-code" 'process "system*/exit-code"
   'process "subprocess" 'process "process" 'process "process*" 'process "process/ports"
   'process "process*/ports" 'process "shell-execute" 'process "exit" 'process
   ;; time — clock reads (and the ticket's seconds->date date conversion)
   "current-seconds" 'time "current-milliseconds" 'time "current-inexact-milliseconds" 'time
   "current-inexact-monotonic-milliseconds" 'time "current-date" 'time "seconds->date" 'time
   ;; network — TCP/UDP socket operations
   "tcp-connect" 'network "tcp-connect/enable-break" 'network "tcp-listen" 'network
   "tcp-accept" 'network "tcp-accept-ready?" 'network "tcp-accept/enable-break" 'network
   "tcp-close" 'network "tcp-abandon-port" 'network "udp-open-socket" 'network "udp-bind!"
   'network "udp-connect!" 'network "udp-close" 'network "udp-send-to" 'network
   "udp-send-to*" 'network "udp-send" 'network "udp-send*" 'network "udp-receive!" 'network
   "udp-receive!*" 'network
   ))

;; ── derived views consumed by library-tokens.rkt ────────────────────
;;
;; The pure and effect tables are disjoint by construction (a name is
;; classified once); library-tokens.rkt unions these sets into the matching
;; effect-tokens fields.
(define racket-effect-builtin-names
  (list->set (hash-keys racket-effect-builtins)))

(define (racket-effect-names-of effect)
  (for/set ([(name eff) (in-hash racket-effect-builtins)]
            #:when (eq? eff effect))
    name))

(define racket-io-builtins (racket-effect-names-of 'io))
(define racket-time-builtins (racket-effect-names-of 'time))
(define racket-network-builtins (racket-effect-names-of 'network))

;; `stdlib-root-effects` is keyed by symbol (the dotted root), so the process
;; names are projected to symbols here. A bare callee's dotted root is its own
;; name symbol, which is exactly how a bare `(system ...)` / `(exit ...)` call
;; resolves.
(define racket-process-root-effects
  (for/hash ([(name eff) (in-hash racket-effect-builtins)]
             #:when (eq? eff 'process))
    (values (string->symbol name) eff)))
