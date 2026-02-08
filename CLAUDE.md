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

- `bin/dune` — declares the `wile` executable, depends on `wile`, `cmdliner`
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

**Milestone 7 (Hygienic Macros)** — complete.

| Module     | Purpose                                                        |
|------------|----------------------------------------------------------------|
| `Expander` | Macro expander: syntax-rules, define-syntax, let-syntax, letrec-syntax, quasiquote, guard, define-record-type, syntax-error |

Changes to existing modules:
- `Syntax`: Added `from_datum` helper (inverse of `to_datum`)
- `Instance`: Added `syn_env`, `gensym_counter` fields; expander wired into
  `eval_syntax`/`eval_boot` pipeline (Reader → Expander → Compiler → VM)

**Milestone 8 (R7RS Libraries)** — complete.

| Module    | Purpose                                                        |
|-----------|----------------------------------------------------------------|
| `Library` | Library types, registry, import-set parsing/resolution         |

Changes to existing modules:
- `Env`: Added `lookup_slot`, `define_slot` for slot-level sharing between
  environments (imported bindings share the same mutable cell)
- `Expander`: Added `cond-expand`, `include`, `include-ci` core forms with
  callback-based feature/library/file queries; exposed `binding` type and
  `lookup_binding`/`define_binding`/`binding_names` API; internal refactor
  to thread `expand_ctx` record through all mutual recursion
- `Port`: Added `of_file` constructor for file-based input
- `Instance`: Added `libraries` (registry), `search_paths`, `features`
  fields; top-level `import` and `define-library` handling; built-in
  `(scheme base)`, `(scheme char)`, `(scheme write)`, `(scheme cxr)`
  libraries built at creation; auto-loading of `.sld` library files from
  search paths with circular dependency detection

**Milestone 9 (FASL Format)** — complete.

| Module | Purpose                                                        |
|--------|----------------------------------------------------------------|
| `Fasl` | Binary serialization of compiled code and library FASL caching |

Changes to existing modules:
- `Instance`: Added `fasl_cache` field (opt-in); when enabled,
  `process_define_library` records declarations and writes `.fasl` cache
  alongside `.sld` source; `try_load_library` checks cache freshness and
  replays from FASL when valid, falling back to source compilation when
  stale or when library has syntax exports

**Milestone 10 (REPL & CLI)** — complete.

No new modules.  All REPL/CLI code lives in `bin/main.ml`.

Changes to existing modules:
- `Instance`: Added `eval_port` — reads and evaluates all expressions from a
  port until EOF, returning the last result (or `Void` if empty)
- `Symbol`: Added `all` — returns all interned symbols from a table
- `bin/main.ml`: Replaced placeholder with cmdliner-based CLI supporting
  three modes: interactive REPL (`wile`), file execution (`wile file.scm`),
  and expression evaluation (`wile -e '(+ 1 2)'`).  REPL features: custom
  line editor, multiline input, history (`~/.wile_history`), REPL commands
  (`,help`, `,quit`, `,load`, `,env`), SIGINT handling, FASL cache enabled

**Milestone 11 (OCaml Embedding API)** — complete.

No new modules.  Adds a clean OCaml embedding API for host applications.

Changes to existing modules:
- `Datum`: Added `is_true` (R7RS truthiness), `list_of` (build proper list),
  `to_list` (extract proper list)
- `Instance`: Added `lookup` (global env lookup by name),
  `define_primitive` (register OCaml function as Scheme primitive),
  `call` (call Scheme procedure via synthetic bytecode),
  `eval_datum` (evaluate a runtime datum as expression),
  `load_file` (load and execute a `.scm` file),
  `load_fasl` (load and execute a pre-compiled FASL file)
- `Expander`: Added `var_binding` (variable binding for registration)

**Milestone 12 (Ahead-of-Time Compiler)** — complete.

No new modules.  Adds `wile compile` and `wile run` CLI subcommands for
ahead-of-time compilation and execution of program FASL files.

Changes to existing modules:
- `Fasl`: Added `program_fasl` type (format_type=2), `write_program_fasl`,
  `read_program_fasl`, `write_program_bytes`, `read_program_bytes`
- `Instance`: Added `compile_port` (reads all top-level forms, processes
  imports at compile time, records declarations without executing),
  `run_program` (replays a program FASL: processes imports and executes code)
- `bin/main.ml`: Added `compile` subcommand
  (`wile compile file.scm [-o out.fasl] [--exe]`) and `run` subcommand
  (`wile run file.fasl`); manual dispatch on `Sys.argv.(1)` to avoid
  `Cmd.group` intercepting positional file arguments; `--exe` generates
  standalone native executable via `ocamlfind ocamlopt`

**R1 (Custom Line Editor)** — complete.

| Module        | Purpose                                                        |
|---------------|----------------------------------------------------------------|
| `Terminal`    | Raw terminal I/O: enter/leave raw mode, ANSI key parsing, cursor/screen control |
| `History`     | Line history ring buffer with navigation, dedup, file persistence |
| `Line_editor` | Editor engine: single-line editing, Emacs keybindings, history integration |

Changes to existing modules:
- `bin/main.ml`: Replaced all `LNoise.*` calls with `Line_editor` API
- `bin/dune`: Removed `linenoise` dependency
- `dune-project`: Removed `linenoise` from package depends

**R3 (Syntax Highlighting)** — complete.

| Module      | Purpose                                                        |
|-------------|----------------------------------------------------------------|
| `Tokenizer` | Fault-tolerant lexer for highlighting (never fails, covers all bytes) |
| `Highlight` | Theme engine + ANSI rendering with rainbow parens, paren matching |

Changes to existing modules:
- `Line_editor`: Added `highlight` callback to config; render uses it for
  ANSI-colored output with correct cursor positioning
- `bin/main.ml`: Added `--theme` CLI flag, `WILE_THEME` env var, `,theme`
  REPL command; dark theme enabled by default; `resolve_theme` helper

**R2 (Multi-line Editing)** — complete.

No new modules. Extends Line_editor for multi-line editing.

Changes to existing modules:
- `Terminal`: Added `Alt_enter` key variant
- `Line_editor`: Added `completeness_check` type, `is_complete` config field;
  buffer holds multi-line text with `\n` separators; Enter inserts newline
  when incomplete, submits when complete; Alt-Enter always inserts newline;
  Up/Down navigate between lines (history on first/last line); multi-line
  rendering with continuation prompts; Ctrl-A/E operate per-line; Ctrl-K
  kills to end of line (joins on newline)
- `bin/main.ml`: Removed `Buffer.t` accumulation and `continuation` ref;
  added `is_complete` callback using Reader; REPL now delegates all multi-line
  handling to Line_editor

**R4 (Paredit Mode)** — complete.

| Module | Purpose |
|--------|---------|
| `Paredit` | Structural editing: balanced insertion/deletion, sexp navigation, slurp/barf/wrap/splice/raise, auto-indentation |

Changes to existing modules:
- `Terminal`: Added `Ctrl_right`, `Ctrl_left`, `Alt_s`, `Alt_r`,
  `Alt_open_paren`, `Alt_9` key variants; CSI `1;5C/D` decoding for
  Ctrl-arrows; ESC+letter decoding for Alt-s/r/(
- `Line_editor`: Added `paredit` (bool ref option) and `readtable`
  (Readtable.t option) to config; intercepts `(`, `)`, `"`, Backspace,
  Delete when paredit active; structural keys (Ctrl-Right=slurp,
  Ctrl-Left=barf, Alt-(=wrap, Alt-s=splice, Alt-r=raise); dynamic prompt
  shows `[paredit]` indicator; Emacs-style backspace (moves inside closing
  delimiters); prefix-filtered history navigation; Scheme-aware
  auto-indentation on newline
- `Highlight`: Added semantic highlighting for defined names (`defn_name_style`)
  and parameters/bindings (`param_style`); cursor-on-identifier bolds both
  the identifier and its binding site; fixed matching-paren backward search
- `History`: Added `prev_matching`/`next_matching` for prefix-filtered
  navigation
- `bin/main.ml`: Added `,paredit` REPL command to toggle paredit mode;
  `paredit_ref` threaded through command handler

**Milestone 13 (Local Package Management)** — complete.

| Module        | Purpose                                                        |
|---------------|----------------------------------------------------------------|
| `Semver`      | Semantic versioning: parse, compare, constraint matching       |
| `Package`     | Package metadata parsing from `package.scm` files              |
| `Pkg_manager` | Package registry: install, remove, list, resolve dependencies  |

Changes to existing modules:
- `Instance`: Added `setup_package_paths` — resolves package dependencies
  and prepends dependency search paths to `inst.search_paths`
- `bin/main.ml`: Added `wile pkg` subcommand group (`install`, `list`,
  `remove`, `info`); auto-detection of `package.scm` in `run_file`,
  `run_repl`, `run_expr`, `compile_file`, `run_fasl`; catches
  `Package.Package_error` and `Pkg_manager.Pkg_error` in error handlers

**Milestone 14 (Output Ports & File I/O)** — complete.

No new modules. Adds bidirectional ports, first-class port values, file I/O,
string ports, the `read` procedure, and `(scheme file)`/`(scheme read)`
libraries.

Changes to existing modules:
- `Port`: Restructured from input-only to bidirectional with `input_source`,
  `output_sink`, `direction` internal types; added output constructors
  (`of_out_channel`, `open_output_string`, `open_output_file`), write
  operations (`write_char`, `write_uchar`, `write_string`, `write_u8`,
  `write_bytes`, `flush`), read operations (`read_line`, `read_u8`,
  `peek_u8`), `close`, predicates (`is_input`, `is_output`, `is_open`),
  `file_name`; readtable field changed to `Obj.t option` to break
  Datum→Port→Readtable→Datum cycle
- `Datum`: Added `Port of Port.t` variant; `equal` returns false (identity);
  `pp` shows `#<input-port file>` or `#<output-port file>`
- `Syntax`: `from_datum` maps `Port` to `Symbol "#<port>"`
- `Fasl`: `write_datum` raises `Fasl_error` for `Port`
- `Reader`: Updated to use `Port.readtable_obj`/`set_readtable_obj` with
  `Obj.repr`/`Obj.obj` casts
- `Instance`: Added `current_input`, `current_output`, `current_error` fields
  (Port.t refs for stdin/stdout/stderr); updated `display`/`write`/`newline`
  for optional port argument; added port predicates (`port?`, `input-port?`,
  `output-port?`, `input-port-open?`, `output-port-open?`, `textual-port?`,
  `binary-port?`), current port procedures, string port constructors
  (`open-input-string`, `open-output-string`, `get-output-string`), write
  primitives (`write-char`, `write-string`, `write-u8`, `write-bytevector`,
  `flush-output-port`), read primitives (`read-char`, `peek-char`,
  `read-line`, `read-string`, `read-u8`, `peek-u8`, `read-bytevector`,
  `char-ready?`), file I/O (`open-input-file`, `open-output-file`,
  `close-input-port`, `close-output-port`, `close-port`, `file-exists?`,
  `delete-file`), higher-order port procedures (`call-with-port`,
  `call-with-input-file`, `call-with-output-file`, `with-input-from-file`,
  `with-output-to-file`), `read`; added `write-shared`/`write-simple`
  aliases; built-in `(scheme file)`, `(scheme read)` libraries; updated
  `(scheme write)` with `write-shared`/`write-simple`

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
| `test/test_datum.ml` | Datum (21 tests) |
| `test/test_readtable.ml` | Readtable (24 tests: 20 unit + 4 QCheck) |
| `test/test_loc.ml` | Loc (4 tests) |
| `test/test_syntax.ml` | Syntax (12 tests) |
| `test/test_port.ml` | Port (31 tests: 30 unit + 1 QCheck) |
| `test/test_reader.ml` | Reader (31 tests: 30 unit + 1 QCheck) |
| `test/test_symbol.ml` | Symbol (8 tests: 6 unit + 2 QCheck) |
| `test/test_env.ml` | Env (14 tests) |
| `test/test_instance.ml` | Instance (31 tests) |
| `test/test_opcode.ml`   | Opcode (4 tests) |
| `test/test_compiler.ml` | Compiler (14 tests) |
| `test/test_vm.ml`       | VM (338 tests: end-to-end via Instance.eval_string) |
| `test/test_m6_review.ml` | M6 bugfix regression (7 tests) |
| `test/test_expander.ml` | Expander (11 tests) |
| `test/test_library.ml` | Library (25 tests) |
| `test/test_fasl.ml` | Fasl (40 tests) |
| `test/test_aot.ml` | AOT compiler (27 tests) |
| `test/test_terminal.ml` | Terminal key parsing (16 tests) |
| `test/test_history.ml` | History (20 tests) |
| `test/test_line_editor.ml` | Line_editor (21 tests) |
| `test/test_tokenizer.ml` | Tokenizer (26 tests) |
| `test/test_highlight.ml` | Highlight (18 tests) |
| `test/test_paredit.ml` | Paredit (73 tests) |
| `test/test_semver.ml` | Semver (34 tests) |
| `test/test_package.ml` | Package (16 tests) |
| `test/test_pkg_manager.ml` | Pkg_manager (25 tests) |

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

## Runtime Dependencies

- **cmdliner** — command-line argument parsing

## Documentation Dependencies

- **odoc** — documentation generator for OCaml (`(odoc :with-doc)` in
  `dune-project`)
