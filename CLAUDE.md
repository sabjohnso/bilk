# Wile — Scheme R7RS Implementation in OCaml

## Project Overview

Wile is an R7RS Scheme implementation written in OCaml. The R7RS specification is available at `r7rs.pdf` in the project root. Refer to it when implementing or verifying Scheme semantics.

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
test/         # Tests (test/wile.ml)
r7rs.pdf      # R7RS specification
_opam/        # Local opam switch (gitignored)
```

- `bin/dune` — declares the `wile` executable, depends on the `wile` library
- `lib/dune` — declares the `wile` library
- `test/dune` — declares test executable

## Development Workflow

**This project uses TDD (Test-Driven Development).** Follow this cycle:

1. **Red** — Write a failing test first in `test/wile.ml`
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

Tests live in `test/wile.ml` and are run via `dune test`.

Test dependencies:
- **alcotest** — unit test framework with readable output
- **qcheck-core** — property-based testing (use the `QCheck2` module)
- **qcheck-alcotest** — integration to run QCheck2 tests through Alcotest

## Code Conventions

- Follow standard OCaml formatting conventions
- Keep modules focused — one concern per module in `lib/`
- Use descriptive names that reflect Scheme R7RS terminology where appropriate
- Prefer pattern matching over conditional chains
