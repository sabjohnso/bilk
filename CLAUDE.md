# Wile — Scheme R7RS Implementation in OCaml

## Project Overview

Wile is an R7RS Scheme implementation written in OCaml. It compiles rather
than interprets (SBCL-style: every `eval` compiles to bytecode, then
executes). It targets four use cases: standalone REPL, compiled executables,
embedding in OCaml applications, and embedding in C applications. Multiple
independent Scheme instances can coexist in a single process. A stable FASL
format provides reliable serialization of compiled code across releases.

The R7RS specification is available at `r7rs.pdf` in the project root.
Refer to it when implementing or verifying Scheme semantics.

The full development plan is in `PLAN.md`.

## Architecture

```
Source text → Reader → Expander → Compiler → VM
                                    (CPS → closure conv → bytecode)
```

- **Bytecode VM, not tree-walker.** No interpreter mode.
- **CPS transformation** for `call/cc` and proper tail recursion.
- **Instance-local state.** All mutable state lives in `Instance.t`. No
  module-level mutable globals. Functions take instance as a parameter.
- **Immutable readtable.** Reader takes a readtable value as a parameter.
- **Stack-based VM.** Value stack + call stack of frames.

## Build System

This project uses **dune** with a local opam switch (OCaml 5.3.0 with BER MetaOCaml).

Set up the environment before running commands:

```sh
eval $(opam env --switch=.)
dune build
dune test
```

## Project Structure

```
bin/          # Executable entry point (main.ml)
lib/          # Core library (the wile library)
test/         # Tests (per-topic test files)
r7rs.pdf      # R7RS specification
PLAN.md       # Development plan and milestones
_opam/        # Local opam switch (gitignored)
```

- `bin/dune` — declares the `wile` executable, depends on the `wile` library
- `lib/dune` — declares the `wile` library
- `test/dune` — declares test executables

## Current Milestone

**Milestone 0 (Foundation)** — complete.

| Module | Purpose |
|---|---|
| `Char_type` | Character classification for the readtable (6 variants) |
| `Datum` | Core Scheme value type (10 variants, structural equality, printer) |
| `Readtable` | Immutable readtable with functional updates, R7RS default table |

**Next: Milestone 1 (Reader)** — readtable-driven parser, source text → Datum.

## Development Workflow

**This project uses TDD (Test-Driven Development).** Follow this cycle:

1. **Red** — Write a failing test first
2. **Green** — Write the minimum code in `lib/` to make the test pass
3. **Refactor** — Clean up while keeping tests green

Always run tests after changes:

```sh
dune test
```

Build the project:

```sh
dune build
```

## Testing

Tests live in `test/` as per-topic files and are run via `dune test`.

| File | Scope |
|---|---|
| `test/test_char_type.ml` | Char_type (4 tests) |
| `test/test_datum.ml` | Datum (11 tests) |
| `test/test_readtable.ml` | Readtable (24 tests: 20 unit + 4 QCheck) |

Test dependencies:
- **alcotest** — unit test framework with readable output
- **qcheck-core** — property-based testing (use the `QCheck2` module)
- **qcheck-alcotest** — integration to run QCheck2 tests through Alcotest

When adding a new module, create a corresponding `test/test_<module>.ml`
file and add its name to the `(names ...)` list in `test/dune`.

## Code Conventions

- Follow standard OCaml formatting conventions
- Keep modules focused — one concern per module in `lib/`
- Use descriptive names that reflect Scheme R7RS terminology where appropriate
- Prefer pattern matching over conditional chains
- No module-level mutable globals — all state belongs in `Instance.t`
