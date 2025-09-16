# Liesel Syntax Sheet

Liesel is an imperative, poetic language with a soft, nostalgic flow. The syntax favors gentle keywords (`let`, `whilst`, `note`) and avoids heavy punctuation. Blocks use indentation after a leading colon, encouraging natural rhythm similar to writing prose.

## Design Principles
- **Core minimalism** — Only constructs essential for expressing logic live in the core runtime. Everything else, including I/O, mathematics, collections, and utilities, resides in importable libraries.
- **Readable cadence** — Keywords and operators are chosen to read almost like sentences without sacrificing clarity.
- **Structured but flexible** — Dynamic typing with gradual type annotations planned later. For now, runtime values carry their type.

## Program Shape
- Scripts execute top-to-bottom; top-level statements run immediately.
- Named routines (functions) are declared with `note` and may be called like ordinary expressions.
- Libraries are imported with `gather`, which exposes symbols into the current scope.

## Core Keywords
| Keyword | Meaning |
| --- | --- |
| `note` | Define a routine. `note greet(person): ...` |
| `let` | Introduce an immutable binding. |
| `set` | Reassign an existing binding. |
| `be` / `to` | Linker words used inside `let` and `set`. |
| `if` | Conditional branch. |
| `otherwise` | Else branch. |
| `whilst` | While-style looping. |
| `halt` | Return from a routine with a value. |
| `true` / `false` | Boolean literals. |
| `nothing` | Absence of a value (null). |
| `gather` | Import a module. |

_Poetic aliases planned_: synonyms like `whisper` for comments or `kindly` for future optional modifiers can be introduced through libraries/macros later without touching the core syntax.

## Comments
- Single-line comment: start with `whisper` followed by a space. Everything after the keyword is ignored.
  ```
  whisper this is ignored
  ```

## Identifiers & Qualified Names
- Identifiers use lowercase letters, underscores, and digits (not starting with a digit).
- Qualified names for library symbols use `::`, e.g. `io::echo`.

## Literals
- Numbers: decimal floating literals (`123`, `3.14`).
- Strings: double quotes support escapes `"`, `\`, `\n`, `\t`.
- Booleans: `true`, `false`.
- Nothing: `nothing`.

## Operators & Symbol Table
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `+` `-` `*` `/` `%` | Arithmetic | `%` currently numeric remainder. |
| `is` / `isnt` | Equality / inequality | Natural-language alternatives to `==` / `!=`. |
| `<` `<=` `>` `>=` | Comparisons | Return booleans. |
| `and` `or` | Short-circuit logic | Core keywords for boolean logic. |
| `not` | Unary negation | Applies to booleans and numbers (`not 0` ⇒ `true`). |
| `::` | Module separator | Used inside qualified names. |
| `,` | Separates arguments in calls. |
| `(` `)` | Grouping & calls | |
| `:` | Introduces an indented block. |

## Statements
### Let Binding
```
let mood be "wistful"
```
- Introduces a new name in the current scope.

### Reassignment
```
set mood to "hopeful"
```
- Requires the name to exist.

### Conditional
```
if mood is "hopeful":
    set tone to "bright"
otherwise:
    set tone to "soft"
```

### Loop
```
whilst count > 0:
    io::echo("Counting: " + count)
    set count to count - 1
```

### Routine Definition
```
note greet(person):
    io::echo("hello, " + person)
    halt nothing
```
- `halt` exits the routine. If omitted, `nothing` is returned by default.
- Routines may now be declared inside other routines or blocks for helper logic (escaping closures are on the roadmap).

## Expressions
- Expression statements evaluate the expression and discard the value unless used in a routine with `halt`.
- Function calls: `io::echo("hi")` or `greet("Ada")`.
- Parentheses group expressions.

## Libraries & Modules
- `gather <name>` loads each module once, first checking for native runtime bridges, then for a script library at `libs/<name>.ls`. Modules may depend on one another using nested `gather`.
- The runtime ships with a tiny native `core` module that exposes host bridges (currently `core::write_line`) intended for library authors.
- The user-facing `io` module lives in `libs/io.ls` and delegates to `core::write_line` to implement `io::echo`.

### Example Script
```
gather io

let count be 3
whilst count > 0:
    io::echo("petal " + count)
    set count to count - 1

note main():
    io::echo("memories bloom")

main()
```

## Minimal Core vs Libraries
- Core runtime implements: lexical rules, evaluation, scopes, control flow, routine declaration/invocation, module loader hooks.
- Libraries provide: math (`math::sin`), IO (`io::echo`), collections, string utilities, etc. The interpreter recognises modules by name and dispatches to registered providers.

## Next Steps
- Flesh out type annotations syntax (`: number`) once runtime type descriptors exist.
- Expand comment aesthetics (`whisper`, `hush` multi-line) via syntactic sugar.
- Add pattern matching constructs after loops and functions are fully implemented.

## Current Implementation Status (alpha)
- Implemented in the interpreter today: indentation-aware blocks (colon + leading spaces), nested routine declarations, `gather` with native/module loading cache, `let`, `set`, routines (`note` / `halt`), branching (`if` / `otherwise`), looping (`whilst`), expression statements, arithmetic/logic expressions, the `core::write_line` bridge + `io` library, booleans, numbers, strings, `nothing`, `and`/`or`/`not`.
- Planned next iterations: richer standard modules (`math`, `text`), structured data types, module packaging conventions (including versioning), resilient closures that survive scope exit, and future gradual typing.
