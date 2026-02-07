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

### Milestone 14 — C Embedding API

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
  M14 (C embedding)
```

Each milestone is usable and testable on its own. The first meaningful
end-to-end demo (type expression, get result) arrives at the end of M3.
A usable R7RS subset is available after M8.
