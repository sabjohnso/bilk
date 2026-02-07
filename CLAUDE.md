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
              Syntax.t  Syntax.t              Datum.t
                          (CPS → closure conv → bytecode)
```

- **Two value representations.** `Syntax.t` is the compile-time tree — every
  node carries a `Loc.t` source location. `Datum.t` is the runtime value —
  no locations. The reader produces `Syntax.t`; the expander and compiler
  consume it; the VM operates on `Datum.t`.
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
| `Datum` | Core Scheme value type (17 variants + code/env/closure/continuation types, structural equality, printer) |
| `Readtable` | Immutable readtable with functional updates, R7RS default table |

**Milestone 1 (Reader)** — complete.

| Module | Purpose |
|---|---|
| `Loc` | Source location type: file, line, column |
| `Syntax` | Compile-time syntax tree mirroring `Datum.t`, every node carries `Loc.t` |
| `Port` | Input port abstraction (string ports), tracks line/column |
| `Reader` | Readtable-driven recursive-descent parser |

**Milestone 2 (Environments & Symbol Table)** — complete.

| Module | Purpose |
|---|---|
| `Symbol` | Interned symbols with integer ids for fast equality |
| `Env` | Lexical environments: chain of mutable frames (type alias for `Datum.env`) |
| `Instance` | Per-instance state: symbol table, global env, readtable, eval |

**Milestone 3 (Bytecode & VM)** — complete.

| Module   | Purpose                                                          |
|----------|------------------------------------------------------------------|
| `Opcode` | Bytecode instruction set (12 variants, accumulator machine)      |
| `Compiler` | Syntax.t → Datum.code compiler with tail-position tracking     |
| `Vm`     | Bytecode virtual machine with value stack and call stack         |

Changes to existing modules:
- `Datum`: Added `Void`, `Primitive`, `Closure` variants; `code`, `env`, `frame`, `primitive`, `closure` types
- `Env`: `type t = Datum.env` (no longer opaque)
- `Instance`: Added `eval_string`, `eval_syntax`; 14 primitives registered at creation

**Milestone 4 (Core Special Forms)** — complete.

Compiler-only changes (no new opcodes or VM modifications):
- `Compiler`: Added `and`, `or`, `when`, `unless`, `let`, `let*`, `letrec`, `letrec*`, named `let`, `cond`, `case`, `do`, internal `define`
- `Instance`: Added 5 primitives: `eqv?`, `eq?`, `list`, `>=`, `<=` (19 total)

**Milestone 5 (Continuations, Apply & Multiple Values)** — complete.

No new opcodes or compiler changes. All features are runtime values handled by
the VM's existing Call/TailCall dispatch via intrinsic dispatch.

Changes to existing modules:
- `Datum`: Added `Continuation`, `Values` variants; `intrinsic_id`, `call_frame`, `wind`, `continuation` types; `prim_intrinsic` field on `primitive`
- `Vm`: Stack-copying `call/cc`, intrinsic dispatch for `apply`/`call-with-values`/`dynamic-wind`, internal `vm_frame` type for multi-step operations, wind stack for continuation invocation
- `Instance`: Added `winds` field; registered 6 new primitives/intrinsics: `apply`, `call/cc`, `call-with-current-continuation`, `values`, `call-with-values`, `dynamic-wind` (25 total)

**Milestone 6 (R7RS Standard Library)** — complete.

Adds ~160 new primitives and 10 self-hosted boot definitions covering the
R7RS (scheme base) standard library.

Changes to existing modules:
- `Datum`: Mutable `Pair` (inline record), `Str of bytes`, `Error_object`
  variant with `error_tag`/`error_obj` types; updated `equal`/`pp` for all
- `Instance`: 182 registered primitives/intrinsics (up from 25), 10 boot
  definitions (self-hosted Scheme), `handlers` field for exception stack;
  covers type predicates, equivalence, numbers, pairs/lists, characters,
  strings, vectors, bytevectors, exceptions, and higher-order procedures
- `Syntax`: Updated `to_datum` for mutable Pair
- `Vm`: Updated for mutable Pair

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
| `test/test_datum.ml` | Datum (17 tests) |
| `test/test_readtable.ml` | Readtable (24 tests: 20 unit + 4 QCheck) |
| `test/test_loc.ml` | Loc (4 tests) |
| `test/test_syntax.ml` | Syntax (8 tests) |
| `test/test_port.ml` | Port (10 tests: 9 unit + 1 QCheck) |
| `test/test_reader.ml` | Reader (31 tests: 30 unit + 1 QCheck) |
| `test/test_symbol.ml` | Symbol (8 tests: 6 unit + 2 QCheck) |
| `test/test_env.ml` | Env (10 tests) |
| `test/test_instance.ml` | Instance (8 tests) |
| `test/test_opcode.ml`   | Opcode (4 tests) |
| `test/test_compiler.ml` | Compiler (14 tests) |
| `test/test_vm.ml`       | VM (189 tests: end-to-end via Instance.eval_string) |
| `test/test_m6_review.ml` | M6 bugfix regression (7 tests) |

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

### Modularity

- Every module in `lib/` **must** have a corresponding `.mli` interface file
  that defines its public API. Use the interface to hide internal helpers,
  intermediate types, and implementation details.
- Prefer abstract types in `.mli` files where the representation should not
  leak to clients. Expose concrete types only when pattern matching by
  callers is intended (e.g. `Datum.t`, `Char_type.t`).
- Keep interfaces minimal — export only what other modules need.

### Documentation

- All public values, types, and modules exposed in `.mli` files **must** have
  odoc documentation comments (`(** ... *)`).
- Use `(** ... *)` before the item it documents (the odoc convention).
- Include `@param`, `@return`, and `@raise` tags where they add clarity.
- Document invariants and non-obvious design decisions in module-level doc
  comments at the top of `.mli` files.

Generate documentation with:

```sh
dune build @doc
```

Output is in `_build/default/_doc/_html/`.

## Documentation Dependencies

- **odoc** — documentation generator for OCaml (`(odoc :with-doc)` in
  `dune-project`)
