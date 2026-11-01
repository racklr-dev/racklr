# UIR (Universal Intermediate Representation) Schema

The UIR is a three-tier intermediate representation for cross-language transpilation.
Tier 0 (Core) captures computational semantics. Tier 1 (OOP/Module) adds object-oriented
and module-system constructs. Tier 2 (UI Component) models declarative component trees.

Currently implemented: **Tier 0** (partial — the subset needed for JSON).

## Node types

All UIR node types are defined as Racket structs in `uir.rkt`. They are transparent
(`#:transparent`) and support round-trip serialization via `uir->sexp` / `sexp->uir`
and `uir->json` / `json->uir`.

### `uir-null`

Represents the null value.

| Field     | Type | Description |
|-----------|------|-------------|
| _(none)_  | —    | Unit struct, no fields |

**Struct**: `(struct uir-null () #:transparent)`

**Example** — lowering JSON `null`:

- CST: `(cst-node 'value (list (token 'null "null" ...)))`
- UIR: `(uir-null)`

### `uir-bool`

Represents a boolean literal.

| Field   | Type      | Description |
|---------|-----------|-------------|
| `value` | `boolean?` | `#t` or `#f` |

**Struct**: `(struct uir-bool (value) #:transparent)`

**Example** — lowering JSON `true`:

- CST: `(cst-node 'value (list (token 'true "true" ...)))`
- UIR: `(uir-bool #t)`

### `uir-number`

Represents a numeric literal. Value is stored as a string to preserve
precision (important for big integers, exact decimals, etc.).

| Field   | Type     | Description |
|---------|----------|-------------|
| `value` | `string?` | String representation of the number |

**Struct**: `(struct uir-number (value) #:transparent)`

**Example** — lowering JSON `3.14`:

- CST: `(cst-node 'value (list (token 'NUMBER "3.14" ...)))`
- UIR: `(uir-number "3.14")`

### `uir-string`

Represents a string literal. The value is the *unescaped* content (without
surrounding quotes).

| Field   | Type     | Description |
|---------|----------|-------------|
| `value` | `string?` | The string content |

**Struct**: `(struct uir-string (value) #:transparent)`

**Example** — lowering JSON `"hello"`:

- CST: `(cst-node 'value (list (token 'STRING "\"hello\"" ...)))`
- UIR: `(uir-string "hello")`

### `uir-list`

Represents an ordered sequence / array of UIR values.

| Field   | Type         | Description |
|---------|--------------|-------------|
| `items` | `(listof uir?)` | The elements in order |

**Struct**: `(struct uir-list (items) #:transparent)`

**Example** — lowering JSON `[1, true]`:

- CST: `(cst-node 'arr (list [ ... (cst-node 'value (token 'NUMBER "1")) ...
                               (group , (cst-node 'value (token 'true "true"))) ... ]))`
- UIR: `(uir-list (list (uir-number "1") (uir-bool #t)))`

### `uir-record`

Represents a key-value mapping (object / struct / dictionary). Keys are
always `uir-string`; values are arbitrary UIR nodes. Entries are stored
as a list of `(cons uir-string uir?)` in insertion order.

| Field     | Type                          | Description |
|-----------|-------------------------------|-------------|
| `entries` | `(listof (cons uir-string uir?))` | Key-value pairs |

**Struct**: `(struct uir-record (entries) #:transparent)`

**Example** — lowering JSON `{"a": 1}`:

- CST: `(cst-node 'obj (list { (cst-node 'pair (token 'STRING "\"a\"" ...) : (cst-node 'value (token 'NUMBER "1")))) }))`
- UIR: `(uir-record (list (cons (uir-string "a") (uir-number "1"))))`

### `uir-symbol`

Represents an identifier (variable name, function name, field name).
Used for name references in later UIR constructs (lambda parameters,
variable references, etc.).

| Field  | Type     | Description |
|--------|----------|-------------|
| `name` | `string?` | The identifier name |

**Struct**: `(struct uir-symbol (name) #:transparent)`

**Example** — not used in JSON lowering, but would represent a bound name:

- UIR: `(uir-symbol "myVar")`

## Predicates

- `(uir? v)` — returns `#t` if `v` is any UIR node type
- `(uir-tag v)` — returns a symbol: `'null`, `'bool`, `'number`, `'string`, `'list`, `'record`, or `'symbol`

## Serialization

### S-expression format

```
(null)
(bool #t|#f)
(number "string-value")
(string "string-value")
(symbol "name")
(list item...)
(record (key value)...)
```

### JSON format

Uses the same structure with symbols replaced by strings:

```json
["null"]
["bool", true]
["number", "3.14"]
["string", "hello"]
["symbol", "myVar"]
["list", ["null"], ["bool", false]]
["record", [["string", "a"], ["number", "1"]]]
```

Round-trip: `(equal? (json->uir (uir->json u)) u)` holds for all UIR nodes.

## Planned (not yet implemented)

These are part of Tier 0 but not needed for JSON:

| Node type     | Fields                          | Purpose |
|---------------|---------------------------------|---------|
| `uir-fn`      | `params body`                   | Lambda / function expression |
| `uir-call`    | `callee args`                   | Function application |
| `uir-let`     | `name value body`               | Variable binding |
| `uir-var`     | `name`                          | Variable reference (uses `uir-symbol`) |
| `uir-set!`    | `name value`                    | Mutation / assignment |
| `uir-walrus`  | `name value`                    | Assignment expression (Python `:=`) |
| `uir-type-alias` | `name type-params value`     | Type alias declaration (Python 3.12 `type`) |
| `uir-if`      | `test then else`                | Conditional |
| `uir-block`   | `stmts`                         | Sequence of expressions |
| `uir-return`  | `value`                         | Return from function |
