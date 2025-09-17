# Repository Guidelines

## Project Structure & Module Organization
- `src/` holds the C interpreter core (lexer, parser, runtime, library loader). Pairing headers live in `include/`.
- `libs/` stores Liesel-standard libraries (`io.ls`, `math.ls`, `list.ls`, `record.ls`, `time.ls`) that the loader imports at runtime.
- `examples/` demonstrates language features and intentional failure cases; use these as smoke tests.
- `docs/` provides reference material (`spec.md`, `syntax-sheet.md`) that must stay in sync with implementation changes.

## Build, Test, and Development Commands
- `make` compiles the interpreter to `build/liesel` and refreshes generated assets.
- `./build/liesel examples/hello.ls` runs a script with the freshly built binary; swap the example to exercise other flows.
- `make clean` removes build artifacts prior to a fresh compile.

## Coding Style & Naming Conventions
- C code uses 4-space indentation, Allman braces for multi-line blocks, and `snake_case` identifiers (functions prefixed with module context, e.g., `lex_next_token`).
- Header guards follow `LIESEL_<FILE>_H`. Keep top-level functions declared in matching headers under `include/`.
- Liesel source (`*.ls`) favors lowercase keywords and descriptive identifiers (`gather io`, `let total = ...`).

## Testing Guidelines
- Rebuild with `make` before running samples. Execute representative scripts from `examples/` to cover new grammar or runtime paths.
- Add new demonstrations under `examples/` mirroring the feature name (e.g., `examples/loops.ls`). Intentional error cases should live beside their positive pairs.
- For bug fixes, craft a failing `.ls` script first to reproduce and confirm resolution.

## Commit & Pull Request Guidelines
- Stage with `git add .` and commit using `git commit -m "<very short message>"`; keep messages imperative (`"add record literals"`).
- Pull requests should describe the feature scope, affected docs, and any library migrations. Link relevant issues and include output from key example runs.
- Highlight spec or syntax changes explicitly so reviewers can cross-check `docs/` and the interpreter updates.

## Architecture Notes
- Interpreter entrypoint is `main()` in `src/main.c`, delegating to the execution pipeline defined in `lie_run_file()`.
- Runtime errors flow through `error.h/error.c`; ensure new diagnostics stay structured and actionable.
- The module loader walks multiple roots (`./`, script directory parents, registered install paths, `$LIESEL_HOME`) when resolving `libs/<name>.ls`.
