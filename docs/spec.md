# Liesel Language Specification (alpha)

This specification captures the behaviour of the current Liesel interpreter implementation. It will evolve alongside the runtime; all future work should reference this document to maintain coherence.

## 1. Overview
- Paradigm: imperative with poetic aesthetics, lexical scoping, first-level routines.
- Execution model: source files interpreted top-to-bottom. Top-level statements execute immediately; routines must be invoked explicitly (commonly via `main()`).
- Encoding: UTF-8 source, but tokens are limited to ASCII for now (string literals may contain arbitrary bytes via escapes).

## 2. Lexical Structure
### 2.1 Tokens
- Identifiers: `[a-zA-Z_][a-zA-Z0-9_]*`. Qualified identifiers use `::` segments (e.g. `io::echo`).
- Keywords: `note`, `let`, `set`, `be`, `to`, `if`, `otherwise`, `whilst`, `halt`, `gather`, `true`, `false`, `nothing`, `and`, `or`, `not`, `is`, `isnt`, `break`, `continue`.
- Literals: numbers (decimal, optional fractional part), strings (`"..."` with `\n`, `\t`, `\\`, `\"` escapes), booleans, `nothing`.
- Operators/punctuation: `+ - * / %`, `< <= > >=`, `is`, `isnt`, `and`, `or`, parenthesis, brackets, braces, commas, colon, `::`.
- Comments: `whisper ` begins a comment to end-of-line.

### 2.2 Whitespace & Indentation
- Indentation uses spaces only. Tabs trigger a lex error.
- A colon (`:`) at the end of a statement introduces a block. The following line must have greater indentation.
- Blocks are closed by dedenting to a previous indentation level. Empty lines within blocks are permitted.

## 3. Grammar (informal)
```
program        → { declaration } EOF

declaration    → gatherStmt | functionDecl | letDecl | setStmt | statement

gatherStmt     → "gather" IDENTIFIER NEWLINE*
functionDecl   → "note" IDENTIFIER "(" [ parameters ] " )" " :" block
parameters     → IDENTIFIER { "," IDENTIFIER }
letDecl        → "let" IDENTIFIER "be" expression NEWLINE*
setStmt        → "set" IDENTIFIER "to" expression NEWLINE*

statement      → exprStmt
               | ifStmt
               | whilstStmt
               | block
               | haltStmt
               | breakStmt
               | continueStmt

ifStmt         → "if" expression " :" block { "otherwise" elseClause }
elseClause     → "if" expression " :" block
               | " :" block
whilstStmt     → "whilst" expression " :" block
block          → NEWLINE INDENT { declaration } DEDENT
haltStmt       → "halt" [ expression ] NEWLINE*
breakStmt      → "break" NEWLINE*
continueStmt   → "continue" NEWLINE*
exprStmt       → expression NEWLINE*

expression     → logic_or
logic_or       → logic_and { "or" logic_and }
logic_and      → equality { "and" equality }
equality       → comparison { ("is" | "isnt") comparison }
comparison     → term { ("<" | "<=" | " >" | " >=") term }
term           → factor { ("+" | "-") factor }
factor         → unary { ("*" | "/" | "%") unary }
unary          → ("not" | "-") unary | call
call           → primary { "(" [ arguments ] " )" | "[" expression " ]" }
arguments      → expression { "," expression }
primary        → NUMBER | STRING | "true" | "false" | "nothing"
               | IDENTIFIER
               | "(" expression " )"
               | listLiteral
               | recordLiteral
listLiteral    → "[" [ expression { "," expression } ] " ]"
recordLiteral  → "{" recordField { "," recordField } " }"
recordField    → (IDENTIFIER | STRING) "be" expression
```


## 4. Runtime Semantics
### 4.1 Values
- Dynamic typing with the following variants: number (double), string (owned copy), boolean, nothing, list, record, native function, user routine.
- Truthiness: numbers (`0` false, others true), strings (empty false), booleans (as-is), nothing (false), lists/records (true when non-empty), natives/routines (true).

### 4.2 Environments & Scope
- Each block (`block`) introduces a new environment that chains to the enclosing environment.
- `let` binds immutably within the current block environment.
- Bindings may include namespace qualifiers (`library::name`) to expose module exports.
- `set` walks outward to update the nearest existing binding; error if not found.
- Routines capture their defining environment (reference-counted). Invoking a routine creates a fresh environment enclosed by that captured environment for parameters & locals.
- Closures remain valid even if the original block has exited.

### 4.3 Evaluation
- Expressions use eager evaluation with short-circuiting for `and`/`or`.
- Arithmetic operators require numbers (string concatenation via `+` is supported when either operand is string). Errors produce runtime hints.
- Division by zero is a runtime error.
- Equality uses value semantics (type-sensitive equality, strings by contents).
- Indexing with `[ ]` retrieves list elements by integer index or record fields by string key.

### 4.4 Control Flow
- `if`/`otherwise` evaluate truthiness of the condition.
- `whilst` loops until condition becomes falsy; `halt` inside a loop returns from the innermost routine.
- `break` exits the current `whilst`; `continue` skips directly to the next iteration.
- `halt` outside a routine triggers a runtime error.

## 5. Modules & Libraries
- `gather <name>` loads modules once per interpreter run. Order:
  1. Native modules registered in the runtime (currently `core`).
  2. Script modules located at `libs/<name>.ls` (executed in the current interpreter).
- Circular gathers produce a runtime error with a hint.

### 5.1 Native Modules
- `core`:
  - `core::write_line(...)` — prints arguments separated by spaces with newline. Requires at least one argument.

### 5.2 Script Modules
- Stored under `libs/`. Executed the first time they are gathered; their top-level code runs, and any `note` definitions / bindings persist via stored ASTs.
- `libs/math.ls` currently exposes:
  - `math::pi` constant (`3.14159265358979323846`)
  - `math::abs(x)` — absolute value
  - `math::floor(x)` — greatest integer ≤ `x`
  - `math::ceil(x)` — smallest integer ≥ `x`
  - Implemented entirely in Liesel using loops and comparisons; relies on `core` only for eventual display helpers.

## 6. Errors
- Lexical, parse, runtime, and system errors emit `Hint:` lines with actionable suggestions.
- Runtime errors set a flag that stops further execution and propagate a non-zero exit code (`70`). Parse errors exit with status `65`; system errors with `74`.

## 7. Examples
- `examples/hello.ls` — simple IO and loops.
- `examples/story.ls` — nested loops with helper routine.
- `examples/math-demo.ls` — demonstrates math library and closures.
- `examples/control-demo.ls` — showcases break/continue and collection indexing.
- `examples/error-intentional.ls` — triggers runtime arity error to showcase diagnostics.

## 8. Implementation Notes
- Interpreter entry: `liesel` binary expects a single `.ls` script path.
- Memory management relies on manual `malloc` + reference counting for environments and routines; strings are copied and freed upon value destruction.
- Empty lines are permitted inside blocks; indentation depth must remain consistent.

## 9. Roadmap & Compliance
Any future change must update this spec to remain authoritative. Planned areas include:
- Additional standard libraries (`text`, `collections`).
- First-class data structures (records, lists).
- Gradual typing syntax (`: number`, etc.).
- Improved error recovery in the parser.
