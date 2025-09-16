<div align=center>
    <img src="./logo.png" width="50%" height="50%">
</div>

# Liesel

Liesel is a hand-crafted, interpreted language with a lyrical or poetic syntax. This repository houses the interpreter (written in C), a few libraries (authored in Liesel itself), and a growing suite of examples and documentation.

## Getting Started

1. **Build & Install**
   ```bash
   ./setup.sh
   ```
   The script compiles the interpreter, installs it under `/usr/local` or `~/.local`, and registers module search paths so bundled libraries resolve anywhere. After execution, ensure the reported `bin` directory is on your `PATH` and reopen your shell if you opted into alias creation.

2. **Run a Script**
   ```bash
   liesel examples/hello.ls
   ```
   Any `.ls` file can be run directly; positional arguments after the path are forwarded to the script.

3. **Ask for Help**
   ```bash
   liesel --help
   ```
   Displays usage tips, module loading rules, and documentation pointers.

## Language Primer

- Modules: Use `gather io` (or any library name) at the top of your file. The interpreter searches `./libs`, the current script’s directory lineage, the install directory, and `$LIESEL_HOME`.
- Definitions: Declare routines with `note name(args):` and return with `halt value`. Variables are introduced with `let` and reassigned via `set`.
- Control Flow: `if`/`otherwise`, `whilst`, `break`, and `continue` follow indentation-driven blocks. `halt` exits the current routine.
- Data: Numbers, strings, booleans, `nothing`, lists (`[...]`), and records (`{ key be value }`) are first-class. Index with `seq[index]` or `record["key"]`.
- Error Handling: Runtime and parse errors are annotated with hints such as “Perhaps you meant...”, guiding corrections without guesswork.

## Repository Layout

- `src/`, `include/`: C sources and headers for the interpreter runtime.
- `libs/`: Standard library modules (`io`, `math`, `list`, `record`) authored in Liesel.
- `docs/`: Reference material (`spec.md`, `syntax-sheet.md`).
- `examples/`: Working programs (`hello.ls`, `grand-tour.ls`, `fibonacci.ls`, etc.) plus intentional failure cases.

## Contributing

- Update `docs/spec.md` when language behavior changes.
- Pair new features or bug fixes with illustrative scripts under `examples/`.

Enjoy exploring Liesel’s and don’t hesitate to extend its library ecosystem with your own modules.
