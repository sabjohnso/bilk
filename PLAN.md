# Wile Development Plan

## Vision

Wile is an R7RS Scheme implementation in OCaml that compiles rather than
interprets (SBCL-style: every `eval` compiles to bytecode, then executes).
It targets four use cases: standalone REPL, compiled executables, embedding
in OCaml applications, and embedding in C applications. Multiple independent
Scheme instances can coexist in a single process. A stable FASL (Fast Load)
format provides reliable serialization of compiled code across releases.

## Architecture Overview

```
  Source text
      │
      ▼
  ┌────────┐
  │ Reader │  readtable-driven, produces Syntax.t (datum + source location)
  └───┬────┘
      │  Syntax.t (s-expressions with source locations)
      ▼
  ┌────────────┐
  │  Expander  │  hygienic macro expansion (syntax-rules)
  └───┬────────┘
      │  Syntax.t (core forms only)
      ▼
  ┌────────────┐
  │  Compiler  │  CPS transform → closure conversion → bytecode emission
  └───┬────────┘
      │  Bytecode (Code_object.t)
      ▼
  ┌────────┐
  │   VM   │  stack-based bytecode interpreter, values are Datum.t
  └────────┘
```

**Two value representations:**

- **`Syntax.t`** — compile-time representation. A recursive tree where every
  node (pair, vector element, atom) carries a `Loc.t` source location. Used
  by the reader, expander, and compiler. Mirrors the structure of `Datum.t`
  but is a separate type.
- **`Datum.t`** — runtime representation. Plain values with no source
  location. Used by the VM, the garbage collector, and the embedding API.
  `Syntax.to_datum` strips locations.

The reader's primary entry point is `read_syntax` (produces `Syntax.t`).
The `read` entry point is a thin wrapper that calls `read_syntax` then
`Syntax.to_datum`.

### Key architectural decisions

**Bytecode VM, not tree-walker.** Every expression is compiled to bytecode
before execution. There is no interpreter mode. This is the SBCL-style
guarantee: `eval` = compile + run. The bytecode is the unit of FASL
serialization.

**CPS transformation for continuations.** `call/cc` and proper tail
recursion are implemented via a CPS (continuation-passing style) intermediate
representation. The compiler transforms source into CPS form, then performs
closure conversion and emits bytecode. This is the proven approach used by
Rabbit, Orbit, and Chez Scheme.

**Instance-local state.** All mutable state (symbol table, global
environment, ports, loaded libraries) lives inside an `Instance.t` record.
No module-level mutable globals. This enables multiple independent Scheme
worlds in one process and makes embedding straightforward.

**Immutable readtable.** Already implemented. The reader takes a readtable
value as a parameter, supporting per-instance or per-port reader
configuration.

**Stack-based VM.** Simpler to implement than register-based, natural fit
for CPS-compiled code. The VM operates on a value stack plus a call stack
of frames.

---

## Milestones

### Milestone 0 — Foundation (done)

What exists today:

| Module      | Purpose                                                            |
|-------------|--------------------------------------------------------------------|
| `Char_type` | Character classification for the readtable (6 variants)            |
| `Datum`     | Core Scheme value type (10 variants, structural equality, printer) |
| `Readtable` | Immutable readtable with functional updates, R7RS default table    |

39 tests across 3 test files. TDD workflow established.

---

### Milestone 1 — Reader

Parse source text into syntax objects using the readtable.

**Modules:**

| Module   | Responsibility                                                                                                                            |
|----------|-------------------------------------------------------------------------------------------------------------------------------------------|
| `Loc`    | Source location type: file, line, column (and optionally span end)                                                                        |
| `Syntax` | Recursive syntax tree — mirrors `Datum.t` structure but every node carries a `Loc.t`. Includes `Syntax.to_datum` for stripping locations. |
| `Port`   | Input/output port abstraction (string ports first, file ports later). Tracks current line/column.                                         |
| `Reader` | Readtable-driven recursive-descent parser. Primary entry point `read_syntax` produces `Syntax.t`; `read` wraps it via `Syntax.to_datum`.  |

**Reader must handle (R7RS §2, §7.1):**

- Identifiers: `foo`, `+`, `...`, `|escaped identifier|`
- Booleans: `#t`, `#f`, `#true`, `#false`
- Numbers: integers, rationals (later), reals (later), `#b`, `#o`, `#d`, `#x` prefixes, `#e`, `#i` exactness
- Characters: `#\a`, `#\space`, `#\newline`, `#\x41`
- Strings: `"hello"`, escape sequences (`\\`, `\"`, `\n`, `\r`, `\t`, `\xNN;`)
- Lists: `(a b c)`, dotted pairs `(a . b)`
- Vectors: `#(1 2 3)`
- Bytevectors: `#u8(1 2 3)`
- Quote shorthands: `'x`, `` `x ``, `,x`, `,@x`
- Comments: `;` line comments, `#;` datum comments, `#| ... |#` block comments
- Datum labels: `#0=datum`, `#0#`
- `#!fold-case`, `#!no-fold-case` directives

**Approach:** Build incrementally — start with `Loc`, `Syntax`, and `Port`,
then the reader proper: symbols and lists, then booleans, numbers, strings,
characters, vectors, etc. Each form gets tests before implementation.

**`Syntax.t` sketch:**

```ocaml
(* Loc *)
type t = { file : string; line : int; col : int }

(* Syntax *)
type t = { loc : Loc.t; datum : datum }
and datum =
  | Bool of bool
  | Fixnum of int
  | Flonum of float
  | Char of Uchar.t
  | Str of string
  | Symbol of string
  | Pair of t * t
  | Vector of t array
  | Bytevector of bytes
  | Nil
  | Eof
```

**Number representation note:** `Datum.t` currently has only `Fixnum of int`.
This milestone adds `Flonum of float` and stubs the full numeric tower.
The complete tower (exact rationals, complex) is deferred to Milestone 6.

---

### Milestone 2 — Environments & Symbol Table

The runtime's variable binding infrastructure.

**Modules:**

| Module     | Responsibility                                                                          |
|------------|-----------------------------------------------------------------------------------------|
| `Symbol`   | Interned symbols with unique integer ids; global intern table per instance              |
| `Env`      | Lexical environment: chain of frames, each frame a mapping from Symbol.t to value slots |
| `Instance` | Top-level record holding all per-instance state                                         |

**`Instance.t` sketch:**

```ocaml
type t = {
  symbol_table : Symbol.Table.t;
  global_env   : Env.t;
  readtable    : Readtable.t;
  (* ports, libraries, etc. added in later milestones *)
}
```

**Design constraint:** `Instance.t` is passed explicitly — never stored in a
global ref. Functions that need instance state take it as a parameter.

---

### Milestone 3 — Bytecode & VM (minimal)

Define the instruction set and get a minimal compile-and-run loop working.

**Modules:**

| Module        | Responsibility                                                    |
|---------------|-------------------------------------------------------------------|
| `Opcode`      | Bytecode instruction set (variant type)                           |
| `Code_object` | Compiled unit: bytecode array + constant pool + metadata          |
| `Compiler`    | Syntax.t → bytecode (initially direct, CPS transform comes in M5) |
| `Vm`          | Stack-based bytecode interpreter                                  |

**Initial instruction set (grows over time):**

```
CONST k          — push constant pool entry k
LREF d i         — push local variable at depth d, index i
GREF sym         — push global variable
GSET sym         — pop and set global variable
LSET d i         — pop and set local at depth d, index i
PUSH             — push accumulator onto stack
POP              — pop stack into accumulator
CALL n           — call procedure with n arguments
TAILCALL n       — tail call
RETURN           — return from current frame
JUMP offset      — unconditional jump
JUMPFALSE offset — jump if accumulator is #f
CLOSURE code     — create closure from code object + current env
HALT             — stop VM
```

**Goal of this milestone:** Evaluate simple expressions end-to-end:

```scheme
42                    ;; constants
(+ 1 2)              ;; primitive calls
(if #t 1 2)          ;; conditionals
((lambda (x) x) 42)  ;; closures
(define x 10)        ;; global definitions
```

The compiler at this stage is a direct AST-to-bytecode pass (no CPS yet).
Tail calls use `TAILCALL` but full `call/cc` is not yet supported.

**Primitives:** A small set of built-in procedures (`+`, `-`, `*`, `<`, `=`,
`cons`, `car`, `cdr`, `null?`, `display`, `newline`) are registered in the
initial global environment as native OCaml functions.

---

### Milestone 4 — Core Special Forms

Expand the compiler to handle all R7RS primitive expression types (§4.1)
and the essential derived forms (§4.2).

**Primitive forms (compiled directly):**

- `quote`
- `lambda`, `case-lambda`
- `if`
- `set!`
- `define`, `define-values`
- `begin`

**Derived forms (desugared before compilation):**

- `let`, `let*`, `letrec`, `letrec*` → `lambda` + application
- `cond`, `case`, `when`, `unless` → nested `if`
- `and`, `or` → `if` chains
- `do` → named `let`
- `let-values`, `let*-values`
- Named `let` → `letrec` + `lambda`
- `define` in body position → `letrec*`

**Internal definitions:** Bodies (`lambda`, `let`, etc.) can begin with
`define` forms; these are converted to `letrec*`.

---

### Milestone 5 — CPS, Tail Calls & Continuations

Replace the direct compiler with a CPS-based pipeline. This is the major
architectural milestone that enables `call/cc` and guarantees proper tail
recursion.

**Compiler pipeline becomes:**

```
Syntax.t (core forms, with source locations)
  → CPS conversion       (every subexpression gets an explicit continuation)
  → CPS optimization     (beta reduction, eta reduction, dead code elimination)
  → Closure conversion   (free variable analysis, closure records)
  → Bytecode emission    (source locations preserved in debug info table)
```

**New/modified VM support:**

- Continuation objects (reified as first-class Scheme values)
- `call-with-current-continuation` / `call/cc`
- `dynamic-wind` (requires a wind/unwind stack per continuation)
- `values` / `call-with-values` (multiple return values)
- Proper tail position for `apply`, `call/cc`, `eval`

**Testing:** The R7RS §3.5 tail recursion examples must run in constant space.
Property tests: deeply recursive tail calls don't overflow.

---

### Milestone 6 — Standard Library (R7RS §6)

Implement the built-in procedures from the R7RS base library. Organized by
section:

| Section | Topic         | Key procedures                                                                                                                        |
|---------|---------------|---------------------------------------------------------------------------------------------------------------------------------------|
| §6.1    | Equivalence   | `eq?`, `eqv?`, `equal?`                                                                                                               |
| §6.2    | Numbers       | Full numeric tower: exact integers (arbitrary precision via Zarith), rationals, flonums, complex. Arithmetic, comparison, conversion. |
| §6.3    | Booleans      | `boolean?`, `not`, `boolean=?`                                                                                                        |
| §6.4    | Pairs & lists | `cons`, `car`, `cdr`, `list`, `append`, `map`, `for-each`, `assoc`, etc.                                                              |
| §6.5    | Symbols       | `symbol?`, `symbol->string`, `string->symbol`                                                                                         |
| §6.6    | Characters    | `char?`, `char->integer`, Unicode support                                                                                             |
| §6.7    | Strings       | String operations, UTF-8                                                                                                              |
| §6.8    | Vectors       | `vector`, `vector-ref`, `vector-set!`, `vector-map`, `vector-for-each`                                                                |
| §6.9    | Bytevectors   | `bytevector`, UTF-8 conversion                                                                                                        |
| §6.10   | Control       | `apply`, `map`, `for-each`, `call/cc`, `values`, `dynamic-wind` (wired in M5)                                                         |
| §6.11   | Exceptions    | `raise`, `guard`, `with-exception-handler`, `error`                                                                                   |
| §6.12   | Eval          | `eval`, `environment`                                                                                                                 |
| §6.13   | I/O           | Ports, `read`, `write`, `display`, file I/O                                                                                           |
| §6.14   | System        | `command-line`, `exit`, `features`                                                                                                    |

**Numeric tower design:** Add to `Datum.t`:

```ocaml
| Fixnum of int              (* small exact integers, unboxed *)
| Bignum of Zarith.Z.t       (* arbitrary-precision exact integers *)
| Ratnum of Zarith.Q.t       (* exact rationals *)
| Flonum of float            (* inexact reals *)
| Complex of float * float   (* inexact complex *)
```

Dispatch via a numeric promotion lattice: fixnum → bignum → ratnum →
flonum → complex. Exact arithmetic stays exact; inexact is contagious.

**Ports design:** Ports wrap OCaml channels or string buffers behind a
uniform interface. Each port carries its readtable (for `read`) and tracks
line/column for error reporting.

---

### Milestone 7 — Hygienic Macros

Implement the R7RS macro system.

- `syntax-rules` pattern matching and template instantiation
- `define-syntax`, `let-syntax`, `letrec-syntax`
- Hygiene via renaming (marks + substitutions, à la Dybvig)

**Expander module:** Sits between the reader and compiler. Operates on
`Syntax.t`, expands macros in a pre-pass, producing core forms only
(still as `Syntax.t` with source locations preserved). Tracks scopes for
hygiene.

This milestone also implements `define-record-type` (§5.5) as a macro that
expands to `define` forms.

---

### Milestone 8 — R7RS Libraries

Implement the library system (§5.6).

- `define-library` with `export`, `import`, `begin`, `include`, `include-ci`, `cond-expand`
- Import modifiers: `only`, `except`, `prefix`, `rename`
- Library registry per instance
- Library loading: source files → compile → cache as FASL
- Standard libraries: `(scheme base)`, `(scheme write)`, `(scheme read)`, `(scheme file)`, etc.

**Library search path:** Configurable per instance. Maps library names to
filesystem paths using a simple convention:
`(scheme base)` → `<lib-path>/scheme/base.sld`

---

### Milestone 9 — FASL Format

Design and implement a stable binary serialization format for compiled code.

**Design goals:**

- **Versioned:** Magic bytes `WFAS` + format version (major.minor). Major bump = breaking change. Minor bump = additive only.
- **Stable across releases:** Old FASL files remain loadable. New features use tagged extension sections that old readers skip.
- **Platform-independent:** Explicit endianness (little-endian), word sizes encoded in the header.
- **Self-describing:** Each section has a type tag and byte length, enabling forward-compatible skipping.

**FASL file structure:**

```
Header:
  magic          4 bytes   "WFAS"
  version        4 bytes   major u16 + minor u16
  flags          4 bytes   (reserved)
  section_count  4 bytes

Section (repeated):
  section_type   2 bytes   (CODE, CONSTANTS, SYMBOLS, METADATA, ...)
  section_len    4 bytes
  payload        variable

CODE section:
  bytecode instructions (the Opcode.t array, serialized)

CONSTANTS section:
  constant pool entries (tagged values: int, float, string, symbol, ...)

SYMBOLS section:
  interned symbol names (for relinking at load time)

METADATA section:
  source file, compile timestamp, library name, dependency list
```

**Modules:**

| Module       | Responsibility                                        |
|--------------|-------------------------------------------------------|
| `Fasl_write` | Serialize `Code_object.t` → bytes                     |
| `Fasl_read`  | Deserialize bytes → `Code_object.t`, version checking |
| `Fasl`       | High-level load/save, caching, path resolution        |

**Compile-and-cache workflow:**

```
source.sld  →  compile  →  source.fasl  (cached)
                              ↓
                    load (fast, no parsing/compiling)
```

FASL is recompiled when the source is newer or the FASL version is stale.

---

### Milestone 10 — REPL & CLI

The user-facing executable.

**REPL features:**

- Read-eval-print loop with the base library pre-loaded
- Line editing (via `linenoise` or OCaml `readline` bindings)
- Multiline input (detect incomplete expressions)
- Error reporting with source location
- `,help`, `,load`, `,env`, `,quit` REPL commands
- `(interaction-environment)` per §5.7

**CLI modes:**

```
wile                     # launch REPL
wile file.scm            # run a program
wile -e '(+ 1 2)'       # evaluate expression
wile compile file.scm    # compile to FASL
wile run file.fasl       # run a FASL file
```

---

### Milestone 11 — OCaml Embedding API (done)

**OCaml API** (`Wile` library):

```ocaml
val Instance.create : ?readtable:Readtable.t -> unit -> Instance.t

val eval_string : Instance.t -> string -> Datum.t
val eval_port   : Instance.t -> Port.t -> Datum.t
val eval_datum  : Instance.t -> Datum.t -> Datum.t

val call : Instance.t -> Datum.t -> Datum.t list -> Datum.t

val define_primitive : Instance.t -> string -> (Datum.t list -> Datum.t) -> unit
val lookup : Instance.t -> string -> Datum.t option

val load_file : Instance.t -> string -> unit
val load_fasl : Instance.t -> string -> unit
```

Multiple `Instance.t` values coexist independently. Each has its own symbol
table, environment, ports, and loaded libraries.

Also includes `Datum` helpers (`is_true`, `list_of`, `to_list`) and
`Expander.var_binding` for registration. Example programs in `examples/`.

---

### Milestone 12 — Ahead-of-Time Compiler

Compile Scheme programs to standalone executables.

**Approach:** Compile Scheme → bytecode → embed bytecode + VM runtime into
an OCaml executable. The "executable" is the Wile VM linked with the
program's FASL data as a static constant.

```
wile compile --output=program file.scm
```

This reuses the same compiler and VM — the only difference from REPL
execution is that the bytecode is pre-loaded rather than compiled at
startup.

**Whole-program mode:** When compiling a standalone program, the compiler
can see all code and perform:

- Dead library elimination (only include libraries actually imported)
- Constant folding across library boundaries
- Inlining of small procedures

---

### Milestone 13 — Package Management

**Package format:**

```
my-package/
  package.scm          # metadata: name, version, deps, entry points
  src/
    (my-package foo).sld
    (my-package bar).sld
  test/
    ...
```

`package.scm` example:

```scheme
(define-package
  (name my-package)
  (version "1.0.0")
  (description "A useful library")
  (license "MIT")
  (depends
    (scheme base)
    (srfi 1))
  (libraries
    (my-package foo)
    (my-package bar)))
```

**Features:**

- Local package registry (per-user or per-project)
- Remote repository protocol (HTTPS + signed checksums)
- Dependency resolution with version constraints
- `wile pkg install`, `wile pkg build`, `wile pkg test`
- Lock file for reproducible builds

---

### Milestone 14 — Output Ports & File I/O

The biggest gap in the current implementation. `Port` is currently
input-only. R7RS requires output ports, file I/O, and port-based
`read`/`write`/`display`.

**`Port` module** — extend with output ports:

- New type: output port (wraps `Buffer.t` for string ports, `out_channel`
  for file ports)
- `open-output-string`, `get-output-string`
- `open-input-file`, `open-output-file`
- `close-input-port`, `close-output-port`, `close-port`
- `input-port?`, `output-port?`, `port?`, `input-port-open?`,
  `output-port-open?`, `textual-port?`, `binary-port?`
- `write-char`, `write-string`, `write-u8`, `write-bytevector`
- `read-line`, `read-string`, `read-u8`, `read-bytevector`
- `peek-u8`
- `flush-output-port`
- Port as a `Datum.t` variant: `Port of port_obj` (with direction tag)

**`Instance` module** — current ports + updated primitives:

- `current-input-port`, `current-output-port`, `current-error-port`
  as parameter-like fields on `Instance.t`
- `display`, `write`, `newline` take optional port argument
- `with-input-from-file`, `with-output-to-file`
- `call-with-input-file`, `call-with-output-file`
- `call-with-port`
- `read` as Scheme procedure (wraps `Reader.read_syntax` + `Syntax.to_datum`)

**New libraries:**

- `(scheme file)` — file-specific operations
- `(scheme read)` — `read` procedure

**Tests:** ~40 (port creation, read/write round-trips, file I/O, string
ports, current port management, error cases)

---

### Milestone 15 — Remaining R7RS Standard Libraries

Complete the 10 unimplemented R7RS standard libraries. Most are small.
Grouped by dependency:

**No new infrastructure needed:**

1. **`(scheme case-lambda)`** — Add `case-lambda` to compiler (dispatch on
   argument count, try each clause). ~5 tests.

2. **`(scheme lazy)`** — `delay`, `force`, `make-promise`, `promise?`,
   `delay-force`. New `Promise` variant in `Datum.t` with mutable
   `forced`/`value` fields. ~10 tests.

3. **`(scheme inexact)`** — Transcendental functions on floats: `exp`,
   `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sqrt`, `finite?`,
   `infinite?`, `nan?`. Mostly direct OCaml `Float` module calls. ~10 tests.

4. **`(scheme complex)`** — Stub library. We don't implement full complex
   numbers but export the subset that works on reals: `real-part`,
   `imag-part`, `magnitude`, `angle`, `make-rectangular`, `make-polar`.
   Real numbers are trivially their own real-part. ~5 tests.

5. **`(scheme process-context)`** — `command-line`, `exit`,
   `emergency-exit`, `get-environment-variable`,
   `get-environment-variables`. Uses `Sys.argv`, `Sys.getenv`, `exit`.
   Instance needs a `command_line` field. ~8 tests.

6. **`(scheme time)`** — `current-second` (Unix epoch float),
   `current-jiffy` (monotonic clock), `jiffies-per-second`. Uses
   `Unix.gettimeofday` and `Mtime` or `clock_gettime`. ~5 tests.

**Depends on M14 (ports):**

7. **`(scheme eval)`** — `eval` (compile+run in given environment),
   `environment` (creates env from library imports). Wraps existing
   `Instance.eval_syntax` machinery. ~10 tests.

8. **`(scheme load)`** — `load` procedure (read+eval from file port).
   Wraps `Instance.eval_port`. ~5 tests.

9. **`(scheme repl)`** — `interaction-environment` (returns the global
   env). Tiny library. ~3 tests.

10. **`(scheme r5rs)`** — Compatibility library re-exporting R5RS subset.
    Optional per R7RS. Low priority. ~3 tests.

**Total: ~64 tests across all sub-libraries**

---

### Milestone 16 — SRFI Support

**Infrastructure:**

- **Naming convention:** `(srfi N)` maps to `srfi/N.sld` on the search
  path. E.g., `(srfi 1)` → `<search_dir>/srfi/1.sld`.
- **Bundled SRFIs:** Ship a `srfi/` directory alongside the wile library,
  added to default search paths. No separate installation needed.
- **Feature identifiers:** Each SRFI adds `srfi-N` to the features list
  for `cond-expand`.
- **Instance change:** Add `<lib-dir>/srfi/` to default `search_paths`.
- **Self-hosted where possible:** Most SRFIs are pure Scheme `.sld` files
  that import `(scheme base)` and define their exports.

**Tier 1 — Trivial (self-hosted Scheme, ~5 lines each):**

| SRFI | Name | Notes |
|------|------|-------|
| 8 | `receive` | Macro wrapping `call-with-values` |
| 11 | `let-values` | Already in base as syntax; re-export |
| 16 | `case-lambda` | Re-export from `(scheme case-lambda)` |
| 26 | `cut` / `cute` | Macro for partial application |
| 28 | Basic format strings | Simple `format` procedure |
| 31 | `rec` | Macro for self-referential definitions |
| 111 | Boxes | `box`, `unbox`, `set-box!` — trivial record type |

**Tier 2 — Moderate (mix of Scheme + OCaml primitives):**

| SRFI | Name | Notes |
|------|------|-------|
| 1 | List library | ~50 procedures, mostly Scheme, some OCaml for perf |
| 2 | `and-let*` | Macro |
| 69 | Basic hash tables | Needs OCaml `Hashtbl` backing |
| 125 | Intermediate hash tables | Supersedes 69, uses comparators |
| 128 | Comparators | Pure Scheme records |
| 132 | Sort libraries | Merge sort in Scheme + vector sort |
| 133 | Vector library | ~30 procedures |
| 151 | Bitwise operations | Needs OCaml integer primitives |

**Tier 3 — Future (not in initial milestone):**

SRFI 13 (strings), 14 (char-sets), 41 (streams), 113 (sets/bags),
115 (regex) — deferred until demand arises.

**Tests:** ~30 for infrastructure + tier 1, ~50 for tier 2

---

### Milestone 17 — C Embedding API

Built via OCaml's C FFI. Exposes the same operations as the OCaml embedding
API (M11) with C-friendly types:

```c
wile_instance_t *wile_create(void);
void             wile_destroy(wile_instance_t *);

wile_value_t     wile_eval_string(wile_instance_t *, const char *);
wile_value_t     wile_call(wile_instance_t *, wile_value_t proc, int argc, wile_value_t *argv);
void             wile_define_primitive(wile_instance_t *, const char *name, wile_cfunc_t fn);
```

Values crossing the C boundary are either immediate (tagged integers,
booleans) or heap-allocated handles. The C API manages GC roots for
handles.

**Build:** The C API produces `libwile.a` / `libwile.so` plus `wile.h`.
A `pkg-config` file is generated for easy integration.

---

### Milestone 20 — Shebang Script Execution

Allow Scheme files to be used as Unix scripts with `#!/usr/bin/env wile`.

**Changes to existing modules:**

- `Reader`: Skip `#!` line at start of file (line 1, column 0 only). When
  the first two characters are `#!`, consume the entire line as a comment.
  This is position-sensitive — `#!` anywhere else is handled normally
  (e.g., `#!fold-case`).
- `bin/main.ml`: In file execution mode (`wile file.scm`), pass remaining
  arguments after the script file to `inst.command_line` so the script
  can access them via `(command-line)`. Currently `command_line` is
  initialized from `Sys.argv` which includes `wile` itself and any wile
  flags — the script should see `(script-path arg1 arg2 ...)`.

**Example:**

```scheme
#!/usr/bin/env wile
(import (scheme base) (scheme write))
(for-each (lambda (arg) (display arg) (newline))
          (cdr (command-line)))
```

```
$ chmod +x hello.scm
$ ./hello.scm world
world
```

**Tests:** ~8 (shebang line skipped, non-shebang `#!` unaffected, arguments
passed correctly, `command-line` contents, edge cases)

**Depends on:** M10 (CLI)

---

### Milestone 21 — Source Maps & VM Instrumentation

Add source location tracking through compilation to bytecode, and provide
callback hooks in the VM for tooling integration.

**Source maps:**

- `Datum.code` gains `source_map : Loc.t array` — one location per
  instruction in the bytecode array. The compiler emits locations alongside
  bytecode by threading the current `Syntax.t` location through each
  emission call.
- `Fasl`: Serialize/deserialize `source_map` arrays. Add a `Loc` encoding
  (file string + line u32 + col u32). Bump `version_minor`.
- Source maps enable mapping a `(file, line)` breakpoint to a `(code, pc)`
  pair, and mapping a runtime PC back to a source location for stack traces.

**VM hooks:**

- `Instance.t` gains optional callback fields:
  - `on_call : (Loc.t -> Datum.t -> Datum.t list -> unit) option ref` —
    fired before every procedure call (location, procedure, arguments)
  - `on_return : (Loc.t -> Datum.t -> unit) option ref` — fired after
    every return (location, return value)
- Hooks are `None` by default. When `None`, the VM pays zero cost (a single
  `Option.is_some` check per call/return). When `Some`, the VM invokes the
  callback.
- Hooks are intentionally simple — they don't pause execution. Pausing
  (for debugging) is built on top by the debug server.

**Changes to existing modules:**

- `Datum`: Add `source_map` field to `code` record
- `Compiler`: Thread current `Loc.t` through emission; populate source map
- `Fasl`: Serialize `Loc.t array` in code sections; bump `version_minor`
- `Vm`: Check and invoke `on_call`/`on_return` hooks at call/return points
- `Instance`: Add `on_call`, `on_return` fields

**Tests:** ~20 (source map round-trip through FASL, compiler emits locations,
hooks fire correctly, hooks disabled by default have no overhead, source map
lookup by file+line)

**Depends on:** M12 (FASL + AOT)

---

### Milestone 22 — Debug Server (DAP)

Implement a Debug Adapter Protocol server for interactive debugging of
Scheme programs.

**Modules:**

| Module         | Responsibility                                              |
|----------------|-------------------------------------------------------------|
| `Dap`          | DAP message types, JSON serialization/deserialization       |
| `Debug_server` | Server loop: breakpoints, stepping, variable inspection     |

**Features:**

- **Breakpoints:** Set by source location (file + line). The debug server
  uses M21 source maps to find the corresponding `(code, pc)` pair. The
  VM `on_call` hook checks whether the current PC matches a breakpoint and
  pauses execution if so.
- **Stepping:** Step-in (stop at next call), step-over (stop at next call
  at same or lower depth), step-out (stop when current frame returns).
  Implemented by dynamically adjusting hook behavior based on call depth.
- **Variable inspection:** When paused, traverse the current `Env.t` frame
  chain to enumerate bindings. Display values using `Datum.pp`.
- **Call stack:** When paused, display the VM call stack with source
  locations from source maps.
- **Expression evaluation:** When paused, compile and evaluate an
  expression in the current environment using `Instance.eval_string` (or
  a variant that uses the paused frame's env).
- **Communication:** DAP over stdin/stdout (standard for VS Code and other
  editors). JSON messages with Content-Length headers.

**CLI:**

```
wile debug file.scm
```

**Changes to existing modules:**

- `bin/main.ml`: Add `debug` subcommand

**Tests:** ~25 (DAP message parsing, breakpoint hit detection, step-in/over/out
logic, variable enumeration, expression evaluation in paused context)

**Depends on:** M21 (source maps + VM hooks)

---

### Milestone 23 — Language Server (LSP)

Implement a Language Server Protocol server for IDE integration.

**Modules:**

| Module            | Responsibility                                         |
|-------------------|--------------------------------------------------------|
| `Lsp`             | LSP message types, JSON-RPC serialization              |
| `Language_server`  | Server loop: diagnostics, hover, completion, symbols  |

**Features:**

- **Diagnostics:** On file save/change, run the pipeline
  (Reader → Expander → Compiler) and report errors with source locations.
  Errors at each stage produce LSP `Diagnostic` messages with severity,
  range (from `Loc.t`), and message text.
- **Hover:** Look up the identifier at cursor position in `syn_env`.
  Display binding kind (variable, syntax, macro) and, for primitives, a
  brief signature. Uses the Tokenizer to find the identifier span.
- **Go to definition:** For variables, look up the binding in `syn_env`
  and report its definition location. For compiled procedures, use M21
  source maps to find the definition site.
- **Completion:** At cursor position, enumerate `syn_env` bindings and
  library exports that match the prefix. Include core forms, macros,
  and imported procedures.
- **Document symbols:** Walk top-level forms to find `define`,
  `define-syntax`, `define-record-type`, `define-library` and report
  them as LSP `DocumentSymbol` entries with location ranges.
- **Semantic tokens:** Reuse `Tokenizer` and `Highlight.analyze_semantics`
  to provide semantic token data (keywords, strings, comments, defined
  names, parameters).
- **Communication:** JSON-RPC over stdin/stdout (standard LSP transport).

**CLI:**

```
wile lsp
```

**Changes to existing modules:**

- `bin/main.ml`: Add `lsp` subcommand

**Tests:** ~30 (LSP message parsing, diagnostic generation from read/expand/
compile errors, hover lookup, completion filtering, document symbol
extraction, semantic token mapping)

**Depends on:** M21 (source maps for go-to-definition)

---

### Milestone 24 — Profiler

Collect runtime performance data and produce analysis reports.

**Modules:**

| Module           | Responsibility                                         |
|------------------|--------------------------------------------------------|
| `Profiler`       | Data collection via M21 hooks: call counts, timing     |
| `Profile_report` | Report generation: text summary, flame graph, trace    |

**Data collection:**

- Uses M21 `on_call`/`on_return` hooks to record:
  - Per-procedure call count
  - Per-procedure wall-clock time (total and self)
  - Call stack snapshots for flame graph construction
- Identified by procedure name (from `Datum.pp` of the procedure value)
  and source location (from source maps).
- Collection overhead is proportional to call frequency. The profiler
  is off by default and enabled only via the CLI subcommand.

**Report formats:**

- **Text summary:** Per-procedure table sorted by self time, showing
  call count, total time, self time, and source location. Similar to
  `gprof` flat profile.
- **Flame graph SVG:** Collapsed stack format → SVG rendering. Compatible
  with Brendan Gregg's FlameGraph tools. Self-contained SVG with
  interactive zoom.
- **Chrome Trace Event JSON:** Produces a JSON file in Trace Event Format
  (B/E events with timestamps and thread/process IDs). Viewable in
  Perfetto or `chrome://tracing`.

**CLI:**

```
wile profile file.scm                      # text summary to stderr
wile profile --format=flamegraph file.scm   # SVG to stdout
wile profile --format=trace file.scm        # Chrome trace JSON to stdout
```

**Changes to existing modules:**

- `bin/main.ml`: Add `profile` subcommand

**Tests:** ~20 (call count accuracy, timing consistency, flame graph SVG
validity, trace JSON format correctness, text report formatting, nested
call attribution)

**Depends on:** M21 (VM hooks)

---

## Cross-cutting concerns

### Error reporting

The reader produces `Syntax.t` where every node carries a `Loc.t` (file,
line, column). The expander and compiler work on `Syntax.t`, so source
locations are available at every stage of processing. The compiler emits a
debug info table mapping bytecode PCs to source locations. Error messages at
every level (read, expand, compile, runtime) include source locations.

`Datum.t` (runtime values) does not carry locations — source tracking is
strictly a compile-time concern.

### Testing strategy

Continue TDD throughout. Each milestone adds tests:

- **Unit tests** (Alcotest): specific behaviors, edge cases
- **Property tests** (QCheck2): round-trip invariants, fuzzing
- **Integration tests**: end-to-end `.scm` files with expected output
- **R7RS conformance**: adapt the R7RS test suite (Chibi's test suite is
  a good starting point)

### Performance

Not a primary focus until the language is complete, but the architecture
should not preclude good performance:

- Bytecode VM avoids AST allocation per eval
- Symbol interning makes comparisons O(1)
- Fixnum unboxing where possible
- Constant pool sharing in FASL

### Dependencies

| Dependency                | Purpose                                    | When         |
|---------------------------|--------------------------------------------|--------------|
| `zarith`                  | Arbitrary-precision integers and rationals | M6 (numbers) |
| `linenoise` or similar    | REPL line editing                          | M10          |
| `checkseum` or `digestif` | FASL/package checksums                     | M9, M13      |

MetaOCaml is available in the switch but is not required by the current
architecture. It remains an option for future native compilation or
staged optimizations.

---

## Milestone dependency graph

```
M0 (foundation) ─── done
 │
M1 (reader)
 │
M2 (environments & symbols)
 │
M3 (bytecode & VM, minimal)
 │
M4 (core special forms)
 │
M5 (CPS, continuations)
 ├──────────────┐
M6 (stdlib)     M7 (macros)
 │               │
 └───────┬───────┘
         │
       M8 (libraries)
         │
       M9 (FASL)
       ┌─┴─┐
  M10 (REPL)  M11 (OCaml embedding)
       │
  M12 (AOT compiler)
       │
  M13 (packages)
       │
  M14 (output ports & file I/O)
       │
  M15 (std libraries)
       │
  M16 (SRFIs)
       │
  M17 (C embedding)

  M10 ──► M20 (shebang scripts)

  M12 ──► M21 (source maps & hooks)
            ├──► M22 (debug server / DAP)
            ├──► M23 (language server / LSP)
            └──► M24 (profiler)
```

Each milestone is usable and testable on its own. The first meaningful
end-to-end demo (type expression, get result) arrives at the end of M3.
A usable R7RS subset is available after M8.
