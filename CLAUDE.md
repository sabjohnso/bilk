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

| Module      | Purpose                                                                                                  |
|-------------|----------------------------------------------------------------------------------------------------------|
| `Char_type` | Character classification for the readtable (6 variants)                                                  |
| `Datum`     | Core Scheme value type (17 variants + code/env/closure/continuation types, structural equality, printer) |
| `Readtable` | Immutable readtable with functional updates, R7RS default table                                          |

**Milestone 1 (Reader)** — complete.

| Module   | Purpose                                                                  |
|----------|--------------------------------------------------------------------------|
| `Loc`    | Source location type: file, line, column                                 |
| `Syntax` | Compile-time syntax tree mirroring `Datum.t`, every node carries `Loc.t` |
| `Port`   | Input port abstraction (string ports), tracks line/column                |
| `Reader` | Readtable-driven recursive-descent parser                                |

**Milestone 2 (Environments & Symbol Table)** — complete.

| Module     | Purpose                                                                    |
|------------|----------------------------------------------------------------------------|
| `Symbol`   | Interned symbols with integer ids for fast equality                        |
| `Env`      | Lexical environments: chain of mutable frames (type alias for `Datum.env`) |
| `Instance` | Per-instance state: symbol table, global env, readtable, eval              |

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

| Module     | Purpose                                                                                                                     |
|------------|-----------------------------------------------------------------------------------------------------------------------------|
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

| Module        | Purpose                                                                         |
|---------------|---------------------------------------------------------------------------------|
| `Terminal`    | Raw terminal I/O: enter/leave raw mode, ANSI key parsing, cursor/screen control |
| `History`     | Line history ring buffer with navigation, dedup, file persistence               |
| `Line_editor` | Editor engine: single-line editing, Emacs keybindings, history integration      |

Changes to existing modules:
- `bin/main.ml`: Replaced all `LNoise.*` calls with `Line_editor` API
- `bin/dune`: Removed `linenoise` dependency
- `dune-project`: Removed `linenoise` from package depends

**R3 (Syntax Highlighting)** — complete.

| Module      | Purpose                                                               |
|-------------|-----------------------------------------------------------------------|
| `Tokenizer` | Fault-tolerant lexer for highlighting (never fails, covers all bytes) |
| `Highlight` | Theme engine + ANSI rendering with rainbow parens, paren matching     |

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

| Module    | Purpose                                                                                                          |
|-----------|------------------------------------------------------------------------------------------------------------------|
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

**Milestone 15 (Remaining R7RS Standard Libraries)** — complete.

No new modules. Implements 10 R7RS standard libraries covering the remaining
specification requirements.

Changes to existing modules:
- `Datum`: Added `Promise of promise` variant with mutable `promise_done`/
  `promise_value` fields; updated `equal` (identity), `pp` (shows forced state)
- `Syntax`: `from_datum` maps `Promise` to `Symbol "#<promise>"`
- `Fasl`: `write_datum` raises `Fasl_error` for `Promise`
- `Expander`: Added `delay`, `delay-force`, `case-lambda` to `core_forms`;
  `delay` expands to `(%make-promise #f (lambda () (make-promise expr)))`;
  `delay-force` expands to `(%make-promise #f (lambda () expr))`;
  `case-lambda` expands to variadic lambda with arity dispatch
- `Instance`: Added `command_line` (string list ref), `eval_envs` (side table
  for environment specifiers), `eval_env_counter` fields; ~30 new primitives:
  inexact math (`exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`,
  `finite?`, `infinite?`, `nan?`), complex stubs (`real-part`, `imag-part`,
  `magnitude`, `angle`, `make-rectangular`, `make-polar`), lazy (`%make-promise`,
  `make-promise`, `promise?`, `force`), process-context (`exit`,
  `emergency-exit`, `get-environment-variable`, `get-environment-variables`,
  `command-line`), time (`current-second`, `current-jiffy`,
  `jiffies-per-second`), eval/load/repl (`environment`, `eval`, `load`,
  `interaction-environment`); built-in libraries: `(scheme inexact)`,
  `(scheme complex)`, `(scheme lazy)`, `(scheme case-lambda)`,
  `(scheme process-context)`, `(scheme time)`, `(scheme eval)`,
  `(scheme load)`, `(scheme repl)`, `(scheme r5rs)`

**Milestone 16 (SRFI Support)** — complete.

Adds infrastructure for bundled SRFIs + implements 20 SRFIs. Embedded
`define-library` source strings in `Srfi` module, loaded lazily when
`(import (srfi N))` is first encountered and no on-disk `.sld` file exists.

| Module          | Purpose                                                       |
|-----------------|---------------------------------------------------------------|
| `Srfi`          | Bundled SRFI library sources with lazy loading infrastructure |
| `Regexp_engine` | Backtracking NFA regex engine for SRFI 115                    |

Bundled SRFIs (36 total):
- **Pure Scheme:** SRFI 1 (lists), 2 (and-let*), 8 (receive), 11 (let-values),
  16 (case-lambda), 26 (cut/cute), 28 (format), 31 (rec), 41 (streams),
  48 (intermediate format strings), 111 (boxes), 113 (sets/bags),
  117 (queues), 125 (intermediate hash tables), 128 (comparators),
  132 (sort), 133 (vectors), 145 (assumptions), 156 (syntactic combiners),
  158 (generators/accumulators), 162 (comparators sublibrary),
  175 (ASCII character library), 189 (Maybe and Either),
  195 (multiple-value boxes), 210 (multiple values procedures),
  214 (flexvectors), 219 (define higher-order lambda),
  223 (generalized binary search), 228 (composing comparators),
  234 (topological sorting), 235 (combinators)
- **OCaml primitives:** SRFI 14 (char-sets, ~37 primitives),
  SRFI 69 (basic hash tables, 26 primitives),
  SRFI 115 (regex, ~15 primitives + Regexp_engine module),
  SRFI 151 (bitwise operations, 20 primitives)
- **Mixed OCaml/Scheme:** SRFI 13 (strings, ~34 OCaml primitives)

Changes to existing modules:
- `Datum`: Added `Hash_table of hash_table` variant with mutable bucket
  array, configurable equality/hash, size tracking, mutability flag;
  added `Char_set of char_set` (256-bit bitset for Latin-1 range);
  added `Regexp of regexp` (compiled regex with SRE source);
  updated `equal` (identity for Hash_table/Regexp, structural for Char_set),
  `pp` (shows size/#<char-set>/#<regexp>)
- `Syntax`: `from_datum` maps `Hash_table`/`Char_set`/`Regexp` to symbols
- `Fasl`: `write_datum` raises `Fasl_error` for `Hash_table`/`Char_set`/`Regexp`
- `Expander`: Added `let-values`, `let*-values` to `core_forms`; expansion
  via nested `call-with-values` / `lambda`
- `Instance`: Added SRFI fallback in `try_load_library` (after file search
  fails, checks `Srfi.lookup`); added `Srfi.bundled_features` to
  `detect_features`; registered 26 SRFI 69 hash table primitives,
  20 SRFI 151 bitwise primitives, ~37 SRFI 14 char-set primitives,
  ~34 SRFI 13 string primitives, ~15 SRFI 115 regex primitives;
  added `let-values`/`let*-values` to `scheme_base_syntax_names`;
  built-in `(srfi 14)`, `(srfi 13)`, `(srfi 69)`, `(srfi 115)`, and
  `(srfi 151)` libraries via `build_library`

**V1 (Virtual Environments & Search Paths)** — complete.

| Module        | Purpose                                                        |
|---------------|----------------------------------------------------------------|
| `Venv`        | Virtual environment creation and validation                    |
| `Search_path` | Library search path resolution with Python-inspired ordering   |

Adds Python-inspired library path management and virtual environment support.

Environment variables:
- `WILE_VENV` — path to active virtual environment (its `lib/` is searched)
- `WILE_PATH` — colon-separated additional library search directories
- `WILE_HOME` — override for Wile home directory (default: `~/.wile/`)

Search path resolution order:
1. Script directory (or CWD for REPL/expr)
2. Package dependencies (from `package.scm`)
3. Virtual environment `lib/` (from `WILE_VENV`)
4. `WILE_PATH` entries
5. Site library (`~/.wile/lib/`)
6. Built-in libraries & bundled SRFIs (fallback in loader)

Changes to existing modules:
- `bin/main.ml`: All `search_paths` assignments use `Search_path.resolve`;
  added `wile venv` subcommand; `Venv.Venv_error` in error handler;
  AOT executable template uses `Search_path.resolve`; updated man page
  with environment variable documentation

**Milestone 17 (C Embedding API)** — complete.

| Module        | Purpose                                                        |
|---------------|----------------------------------------------------------------|
| `Wile_c_api`  | OCaml handle tables, error wrapping, callback registration     |

Exposes the existing OCaml embedding API to C programs via integer handles
and OCaml's C FFI (`Callback.register` + `caml_callback`).

Files:
- `lib/wile_c_api.ml` / `.mli` — Handle-based bridge: instance and value
  handle tables, error catching, all API functions, `Callback.register`
- `lib/wile_stubs.c` — C stubs: `caml_callback` wrappers + primitive dispatch
- `lib/wile.h` — Public C header with full API

Design:
- `wile_val_t` / `wile_inst_t` = `int32_t` handles into OCaml-side `Hashtbl`
- Handle 0 = `WILE_NULL` (error/not-found sentinel)
- Per-instance `last_error : string option` catches all Wile exceptions
- C functions registered as Scheme primitives via dispatch table (max 1024)
- Lazy callback caching (`ensure_cached()`) for all 40 registered callbacks

Changes to existing modules:
- `lib/dune`: Added `(foreign_stubs (language c) (names wile_stubs))` and
  `(install_c_headers wile)`

**Milestone 18 (OCaml Extensions)** — complete.

| Module      | Purpose                                                        |
|-------------|----------------------------------------------------------------|
| `Extension` | Static registry, Dynlink loading, search path resolution       |

Adds native extension support via `(include-shared "name")` in
`define-library`.  Extensions register Scheme primitives and integrate
with the R7RS library system.  Supports static linking (compile-time
`register_static`) and dynamic loading (`.cmxs` via `Dynlink`).

Changes to existing modules:
- `Instance`: Added `extension_lib_env` field (used during `include-shared`
  to redirect `define_primitive` into the library's env); added
  `load_native_ref` forward reference (filled by `Extension` module);
  `define_primitive` checks `extension_lib_env` and also registers in
  lib env when set; `process_define_library` handles `include-shared`;
  `replay_lib_fasl` handles `Lib_native`; `run_program` handles `Lib_native`
- `Fasl`: Added `Lib_native of string` to `lib_declaration`; tag 2
  serialization; bumped `version_minor` to 1
- `Wile_c_api`: Added `temporary_handle`/`release_handle` for C extension
  handles; fills in `Extension.c_temporary_handle_ref`/`c_release_handle_ref`
- `lib/dune`: Added `dynlink` to libraries
- `bin/main.ml`: Added `wile ext init` subcommand with `--lang ocaml`/`c`
  scaffolding; added `Extension.Extension_error` to error handlers

**Milestone 19 (C Extensions)** — complete.

No new modules.  Adds C extension loading via `dlopen`/`dlsym` to the
existing `Extension` module.

Files:
- `lib/ext_stubs.c` — C stubs wrapping POSIX `dlopen`/`dlsym`/`dlclose`
  and `wile_ext_call_init` for calling C extension entry points

Changes to existing modules:
- `Extension`: Added `ext_dlopen`/`ext_dlsym`/`ext_dlclose`/`ext_call_init`
  externals; `load_c` function; `load_native` searches `.so` after `.cmxs`;
  `c_temporary_handle_ref`/`c_release_handle_ref` forward refs for cycle
  breaking
- `lib/wile.h`: Added `WILE_EXT_API_VERSION`, `WILE_EXT_ENTRY`,
  `WILE_EXT_INIT` macros for C extension entry points
- `lib/dune`: Added `ext_stubs` to `foreign_stubs`; added `-ldl` to
  `c_library_flags`

**Milestone 20 (Shebang Script Execution)** — complete.

No new modules.  Adds shebang line support in the reader and script
argument passing in the CLI.

Changes to existing modules:
- `Reader`: `read_hash` `#!` branch detects shebang at file start
  (line 1, col 1) by checking if the character after `#!` is
  non-alphabetic (e.g. `/`, space); alphabetic characters fall through
  to directive handling (`#!fold-case`, `#!no-fold-case`); shebang
  lines are skipped as line comments
- `bin/main.ml`: `run_file` takes `script_args` parameter and sets
  `inst.command_line := path :: script_args`; main dispatch bypasses
  Cmdliner for positional file arguments (`wile file.scm arg1 arg2`),
  collecting remaining argv as script args

**Milestone 21 (Source Maps & VM Instrumentation)** — complete.

No new modules.  Adds source location tracking through compilation to
bytecode, binary serialization of source maps in FASL, and callback hooks
in the VM for tooling integration (debuggers, profilers, tracers).

Changes to existing modules:
- `Datum`: Added `source_map : Loc.t array` field to `code` record type,
  parallel to `instructions` (same length, maps each bytecode instruction
  to its source `Loc.t`)
- `Compiler`: Added `source_locs` (reversed list) and `current_loc`
  (mutable) fields to compiler state; `emit` appends `current_loc` to
  `source_locs` alongside each instruction; each `compile_*` function sets
  `current_loc <- s.loc` at entry; `compile_call` and `compile_define`
  restore location before emitting Call/TailCall/Define instructions;
  `package_code` converts `source_locs` to `source_map` array
- `Fasl`: Bumped `version_minor` from 2 to 3; `write_code_obj` serializes
  source map as `(string file, u32 line, u32 col)` per instruction after
  the instruction array; `read_code_obj` deserializes and reconstructs
  `Loc.t` values
- `Vm`: Extended `execute` signature with `?on_call` and `?on_return`
  optional callback parameters; `on_call` fires at Call and TailCall
  instructions with source location, procedure, and argument list;
  `on_return` fires at Return (Standard frames only) with source location
  and return value; hooks are NOT passed to internal `call_thunk_nested`
  (wind thunks)
- `Instance`: Added `on_call` and `on_return` fields (`(Loc.t -> ...)
  option ref`, default `None`); all `Vm.execute` call sites pass hooks
  from instance fields

**Milestone 22 (Debug Server — DAP)** — complete.

| Module          | Purpose                                               |
|-----------------|-------------------------------------------------------|
| `Dap`           | DAP wire format: message types, JSON helpers, framing |
| `Debug_server`  | Debug engine: breakpoints, stepping, inspection, DAP session |

Adds a Debug Adapter Protocol (DAP) server for interactive debugging of
Scheme programs.  Communication is over stdin/stdout.  Program output is
redirected and sent as DAP "output" events.

Changes to existing modules:
- `Vm`: Added `debug_state` type (mutable snapshot of env, frames, code, pc)
  and `make_debug_state` constructor; `execute` gains `?debug_state` optional
  parameter; `fire_on_call` populates debug state before firing callback
  (zero overhead when `None`)
- `Instance`: Added `debug_state : Vm.debug_state option ref` field (default
  `ref None`); all 10 `Vm.execute` call sites pass `?debug_state`
- `Env`: Added `frame_bindings` — returns all `(symbol_id, value)` pairs
  from a frame
- `bin/main.ml`: Added `wile debug` subcommand; `Dap.Dap_error` and
  `Debug_server.Debug_error` in error handlers

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

| File                       | Scope                                               |
|----------------------------|-----------------------------------------------------|
| `test/test_char_type.ml`   | Char_type (4 tests)                                 |
| `test/test_datum.ml`       | Datum (21 tests)                                    |
| `test/test_readtable.ml`   | Readtable (24 tests: 20 unit + 4 QCheck)            |
| `test/test_loc.ml`         | Loc (4 tests)                                       |
| `test/test_syntax.ml`      | Syntax (12 tests)                                   |
| `test/test_port.ml`        | Port (31 tests: 30 unit + 1 QCheck)                 |
| `test/test_reader.ml`      | Reader (33 tests: 32 unit + 1 QCheck)               |
| `test/test_symbol.ml`      | Symbol (8 tests: 6 unit + 2 QCheck)                 |
| `test/test_env.ml`         | Env (14 tests)                                      |
| `test/test_instance.ml`    | Instance (31 tests)                                 |
| `test/test_opcode.ml`      | Opcode (4 tests)                                    |
| `test/test_compiler.ml`    | Compiler (20 tests)                                 |
| `test/test_vm.ml`          | VM (425 tests: end-to-end via Instance.eval_string) |
| `test/test_m6_review.ml`   | M6 bugfix regression (7 tests)                      |
| `test/test_expander.ml`    | Expander (11 tests)                                 |
| `test/test_library.ml`     | Library (25 tests)                                  |
| `test/test_fasl.ml`        | Fasl (49 tests)                                     |
| `test/test_aot.ml`         | AOT compiler (27 tests)                             |
| `test/test_terminal.ml`    | Terminal key parsing (16 tests)                     |
| `test/test_history.ml`     | History (20 tests)                                  |
| `test/test_line_editor.ml` | Line_editor (21 tests)                              |
| `test/test_tokenizer.ml`   | Tokenizer (26 tests)                                |
| `test/test_highlight.ml`   | Highlight (18 tests)                                |
| `test/test_paredit.ml`     | Paredit (73 tests)                                  |
| `test/test_semver.ml`      | Semver (34 tests)                                   |
| `test/test_package.ml`     | Package (16 tests)                                  |
| `test/test_pkg_manager.ml` | Pkg_manager (25 tests)                              |
| `test/test_srfi.ml`        | SRFI (374 tests)                                    |
| `test/test_venv.ml`        | Venv (11 tests)                                     |
| `test/test_search_path.ml` | Search_path (13 tests)                              |
| `test/test_regexp_engine.ml` | Regexp_engine (32 tests)                            |
| `test/test_c_api.ml`        | Wile_c_api (49 tests)                               |
| `test/test_c_embed.ml`      | C integration tests (50 tests via test_c_embed_impl.c) |
| `test/test_extension.ml`    | Extension (23 tests: static registry, include-shared, FASL, C ext, scaffolding) |
| `test/test_dap.ml`          | Dap (11 tests)                                      |
| `test/test_debug_server.ml` | Debug_server (15 tests)                              |

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
- **yojson** — JSON parsing and generation (used by DAP debug server)

## Documentation Dependencies

- **odoc** — documentation generator for OCaml (`(odoc :with-doc)` in
  `dune-project`)
