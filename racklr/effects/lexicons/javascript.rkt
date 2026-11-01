#lang racket

;; racklr/effects/lexicons/javascript.rkt
;;
;; A NON-PYTHON host lexicon, as DATA. Its purpose is to prove the injection
;; contract of the effect classifier: swapping `default-lexicon` for this value
;; at a classification entry point (infer-effects / infer-effects-all /
;; classify-call / classify-call-all) classifies JavaScript instead of
;; Python/Django with ZERO edits to engine code.
;;
;; A lexicon is: token tables (effect roots, bare-name / method effects, purity
;; tables, receiver-type tables) + the host dialect (implicit receiver name
;; `this`, the `Error` exception suffix, the JS lowerer's construct encodings,
;; a zero-width method receiver offset) + import-tag rules + the operation
;; vocabulary. None of these names appear in racklr/effects/*.rkt engine code.

(require racklr/effects/lexicon-types
         racklr/effects/library-tokens
         racklr/effects/imports/effect-tag-types)

(provide javascript-dialect javascript-lexicon)

;; ── dialect ─────────────────────────────────────────────────────────
(define javascript-dialect
  (dialect
   ;; `this` is JavaScript's implicit receiver (there is no `self`/`cls`).
   (set "this")
   ;; No implicit class-receiver name in JS (class constructors are not bound
   ;; to an implicit parameter); JS has no `cls(...)` convention.
   (set)
   ;; JS exception classes end in `Error`; constructing one is pure.
   (set "Error")
   ;; The JS/TS/JSX lowerers encode member access / operators / statements as
   ;; literal callee names. These are constructs, not runtime-dispatched calls.
   (set "dot" "index" "while" "dowhile" "for" "forin" "forof" "switch" "break"
        "continue" "debugger" "label" "regex" "function" "async-fn" "gen-fn"
        "import" "export" "throw" "try" "=>" "var" "let" "const" "void"
        "typeof" "delete" "array-bind" "object-bind"
        "+" "-" "*" "/" "%" "=" "==" "===" "!=" "!==" "<" ">" "<=" ">="
        "&&" "||" "**" "!" "~" "&" "|" "^"
        "<<" ">>" ">>>" "postfix++" "postfix--" "prefix++" "prefix--")
   ;; JS has no comprehension encoding.
   (set)
   ;; Named async / generator function wrappers.
   (set "async-fn" "gen-fn")
   (set "__handle" "__withEffects")
   ;; const/let/var binding encodings.
   (set "var" "let" "const")
   ;; JS methods receive no implicit receiver parameter: a call's first
   ;; argument maps to the method's first declared parameter.
   0
   ;; JS has no constructor-method convention; class calls are constructors.
   #f
   ;; The JS lowerer does not synthesize keyword-argument calls.
   #f
   "__withEffects"
   ;; JS literal receiver types are not modeled yet (no String/Array type
   ;; tables in this lexicon).
   (hash)
   ;; Callees/statements that always transfer control out of the block:
   ;; `throw` does not return; `break`/`continue` leave the loop body.
   (set "throw" "break" "continue")
   ;; Short-circuit boolean operators: the right operand runs conditionally.
   (set "&&" "||" "??")))

;; ── token tables ────────────────────────────────────────────────────
(define javascript-lexicon
  (lexicon
   'javascript
   (effect-tokens
    ;; effect roots: every call rooted at one of these is the domain
    (hash
     'io (set "console" "process.stdout" "process.stderr")
     'network (set "fetch" "axios" "XMLHttpRequest" "WebSocket")
     'process (set "child_process")
     'cache (set "localStorage" "sessionStorage")
     'time (set "Date"))
    '(io network cache process time)
    ;; bare-name effects: an effect on its own when called bare
    (hash
     "fetch" 'network
     "parseInt" 'pure
     "setTimeout" 'time
     "setInterval" 'time
     "requestAnimationFrame" 'time
     "structuredClone" 'pure)
    ;; method effects: a dotted method name that is an effect on its own
    (hash
     "log" 'io "info" 'io "warn" 'io "error" 'io "debug" 'io "trace" 'io
     "readFile" 'io "readFileSync" 'io "writeFile" 'io "writeFileSync" 'io
     "appendFile" 'io "appendFileSync" 'io "readdir" 'io "readdirSync" 'io
     "mkdir" 'io "mkdirSync" 'io "rmdir" 'io "rmdirSync" 'io "unlink" 'io
     "unlinkSync" 'io "stat" 'io "statSync" 'io
     "now" 'time "today" 'time)
    (hash)
    (hash)
    #f
    (hash)
    ;; builtin receiver types (pure to construct; method namespace tracked)
    (set "Array" "Object" "String" "Number" "Boolean" "Promise" "Map" "Set"
         "WeakMap" "WeakSet" "JSON" "Math" "Date" "RegExp" "Error" "BigInt"
         "Symbol" "Buffer" "Uint8Array" "ArrayBuffer")
    ;; module roots whose attribute namespace is not a name-only DB operation
    (set "fs" "path" "os" "crypto" "http" "https" "net" "url" "util" "events"
         "stream" "buffer" "process" "child_process")
    ;; whole-root effects
    (hash 'fs 'io 'os 'io 'http 'network 'https 'network 'net 'network
          'child_process 'process)
    ;; receiver-type → method → effect (result types of calls)
    (hash
     "fetch" (hash "json" 'pure "text" 'pure "arrayBuffer" 'pure
                   "blob" 'pure "status" 'pure "ok" 'pure)
     "Response" (hash "json" 'pure "text" 'pure "arrayBuffer" 'pure
                      "blob" 'pure "status" 'pure "ok" 'pure)
     "Promise" (hash "then" 'pure "catch" 'pure "finally" 'pure)
     "Map" (hash "get" 'pure "set" 'pure "has" 'pure "delete" 'pure
                 "keys" 'pure "values" 'pure "entries" 'pure "forEach" 'pure)
     "Set" (hash "add" 'pure "has" 'pure "delete" 'pure "values" 'pure)
     "Date" (hash "now" 'time "getTime" 'pure "toISOString" 'pure))
    ;; whole-module pure roots
    (set "JSON" "Math" "Object" "Array" "String" "Number" "Boolean" "Date"
         "RegExp" "Promise" "Map" "Set" "WeakMap" "WeakSet" "Reflect" "Intl")
    (hash)
    ;; pure bare names
    (set "parseInt" "parseFloat" "isNaN" "isFinite" "encodeURI"
         "encodeURIComponent" "decodeURI" "decodeURIComponent"
         "structuredClone" "queueMicrotask")
    (set)
    ;; pure constructor names
    (set "TypeError" "RangeError" "SyntaxError" "ReferenceError" "EvalError"
         "URIError")
    ;; pure builtin names (JS global functions / value constructors)
    (set "Array" "Object" "String" "Number" "Boolean" "Symbol" "BigInt"
         "parseInt" "parseFloat" "isNaN" "isFinite" "encodeURI"
         "decodeURI" "encodeURIComponent" "decodeURIComponent"
         "structuredClone" "queueMicrotask")
    ;; protocol method names
    (set "toString" "valueOf" "hasOwnProperty" "toJSON")
    ;; string-method names
    (set "toUpperCase" "toLowerCase" "trim" "trimStart" "trimEnd" "split"
         "replace" "replaceAll" "slice" "substring" "substr" "charAt"
         "charCodeAt" "codePointAt" "startsWith" "endsWith" "includes"
         "indexOf" "lastIndexOf" "padStart" "padEnd" "repeat" "concat"
         "match" "matchAll" "search" "localeCompare" "at")
    ;; pure method names on arbitrary receivers (collections / promises /
    ;; regexes); mutating a local collection is not a domain effect.
    (set "map" "filter" "forEach" "reduce" "reduceRight" "find" "findIndex"
         "findLast" "findLastIndex" "some" "every" "flat" "flatMap" "slice"
         "splice" "concat" "join" "push" "pop" "shift" "unshift" "reverse"
         "sort" "includes" "indexOf" "lastIndexOf" "keys" "values" "entries"
         "has" "get" "set" "delete" "clear" "add" "then" "catch" "finally"
         "toString" "valueOf" "hasOwnProperty" "toJSON" "test" "exec"
         "toUpperCase" "toLowerCase" "trim" "split" "replace" "startsWith"
         "endsWith" "padStart" "padEnd" "repeat")
    (hash)
    ;; module receivers resolved to a domain by receiver-type dataflow
    (hash "fs" 'io "axios" 'network "child_process" 'process)
    ;; module-qualified class references: none for the JS host yet
    (hash)
    ;; type-preserving call projections: none for the JS host yet
    (hash)
    ;; native pure procedures: none for the JS host (its pure names are the
    ;; whole-module / receiver-type tables above)
    (set)
    javascript-dialect)
   ;; import-tag rules: module path prefix → domain
   (list
    (effect-library "fs" (list (effect-rule "fs" 'io)))
    (effect-library "node:fs" (list (effect-rule "node:fs" 'io)))
    (effect-library "path" (list (effect-rule "path" 'pure)))
    (effect-library "node:path" (list (effect-rule "node:path" 'pure)))
    (effect-library "axios" (list (effect-rule "axios" 'network)))
    (effect-library "node-fetch" (list (effect-rule "node-fetch" 'network)))
    (effect-library "child_process" (list (effect-rule "child_process" 'process)))
    (effect-library "react" (list (effect-rule "react" 'pure)))
    ;; A host application module: an imported application module's domain is
    ;; per-import-rule data, never an engine assumption.
    (effect-library "app" (list (effect-rule "app" 'io))))
   ;; db operation verbs
   (set "find" "findMany" "findFirst" "findUnique" "count" "aggregate")
   (set "create" "createMany" "update" "updateMany" "upsert" "delete"
        "deleteMany" "save")
   ;; io operation verbs
   (set "read" "readFile" "readFileSync" "readdir" "readdirSync")
   (set "write" "writeFile" "writeFileSync" "appendFile" "appendFileSync")
   (set "close" "flush" "seek" "tell")
   ;; other-domain operation vocabulary: the JS host types only io/network
   ;; roots by name for now, so it supplies no method→op tables
   (hash)
   ;; builtin receiver method tables
   (hash
    "Array" (set "push" "pop" "shift" "unshift" "slice" "splice" "concat"
                 "join" "map" "filter" "forEach" "reduce" "find" "includes"
                 "indexOf" "sort" "reverse" "length")
    "String" (set "split" "replace" "slice" "substring" "toUpperCase"
                  "toLowerCase" "trim" "startsWith" "endsWith" "includes")
    "Map" (set "get" "set" "has" "delete" "keys" "values" "entries" "clear")
    "Set" (set "add" "has" "delete" "values" "clear"))))
