# Wile

Wile is an R7RS Scheme implementation written in OCaml. It compiles Scheme
source to bytecode and executes it on a stack-based virtual machine (SBCL-style:
every `eval` compiles to bytecode, then executes — there is no interpreter
mode).

## Features

- **R7RS compliant** — full R7RS standard library including all 16
  `(scheme ...)` libraries
- **Hygienic macros** — `syntax-rules`, `define-syntax`, `let-syntax`,
  `letrec-syntax`, `define-record-type`, `guard`, quasiquote
- **R7RS library system** — `define-library`, `import`, `export`,
  `cond-expand`, `include`/`include-ci`, automatic `.sld` file loading
- **First-class continuations** — stack-copying `call/cc`, `dynamic-wind`,
  `call-with-values`
- **Full numeric tower** — fixnums, flonums, exact rationals, exact & inexact
  complex numbers, arbitrary-precision integers (GMP-backed via zarith)
- **36 bundled SRFIs** — including SRFI 1 (lists), 13 (strings), 14
  (char-sets), 69 (hash tables), 115 (regex), 151 (bitwise), and more
- **Interactive REPL** — syntax highlighting, rainbow parentheses, paredit
  mode, multi-line editing, history, tab completion
- **Ahead-of-time compilation** — compile to FASL bytecode or standalone
  native executables
- **FASL caching** — binary serialization of compiled libraries with
  mtime-based cache invalidation
- **Embedding APIs** — embed Scheme in OCaml or C applications; multiple
  independent instances per process
- **Native extensions** — write extensions in OCaml or C, loaded via
  `include-shared` in `define-library`
- **Package management** — local package manager with semantic versioning and
  dependency resolution
- **Virtual environments** — Python-inspired `WILE_VENV`/`WILE_PATH` for
  isolated library search paths
- **Developer tooling** — Language Server Protocol (LSP) server, Debug Adapter
  Protocol (DAP) server, runtime profiler with flame graph output
- **Shebang support** — use `#!/usr/bin/env wile` for executable scripts

## Building from Source

### Prerequisites

- **OCaml 5.3.0** with the BER MetaOCaml variant (`ocaml-variants.5.3.0+BER`)
- **opam** (OCaml package manager)
- **GMP** (GNU Multiple Precision Arithmetic Library)
  - Ubuntu/Debian: `sudo apt install libgmp-dev`
  - macOS: `brew install gmp`

### Setup

Clone the repository and create a local opam switch:

```sh
git clone https://github.com/wile-scheme/wile.git
cd wile
opam switch create . ocaml-variants.5.3.0+BER
eval $(opam env)
```

Install dependencies:

```sh
opam install . --deps-only --with-test -y
```

### Build

```sh
eval $(opam env)
dune build
```

### Test

```sh
dune test
```

The test suite has 1600+ tests across 40 test files covering the full
codebase.

### Install

To install the `wile` binary and library into the current opam switch:

```sh
dune install
```

## Usage

### Interactive REPL

```sh
wile
```

The REPL supports syntax highlighting, rainbow parentheses, paredit structural
editing (toggle with `,paredit`), multi-line input, and history. Type `,help`
for available REPL commands.

### Run a file

```sh
wile hello.scm
```

### Evaluate an expression

```sh
wile -e '(display (+ 1 2))'
```

### Script with arguments

```scheme
#!/usr/bin/env wile
(import (scheme base) (scheme write) (scheme process-context))
(for-each (lambda (arg) (display arg) (newline))
          (command-line))
```

```sh
chmod +x script.scm
./script.scm arg1 arg2
```

### Compile to bytecode

```sh
wile compile program.scm -o program.fasl
wile run program.fasl
```

### Compile to native executable

```sh
wile compile program.scm --exe -o program
./program
```

### Subcommands

| Command         | Description                                |
|-----------------|--------------------------------------------|
| `wile`          | Start the interactive REPL                 |
| `wile FILE`     | Execute a Scheme source file               |
| `wile -e EXPR`  | Evaluate an expression and print result    |
| `wile compile`  | Ahead-of-time compile to FASL or native    |
| `wile run`      | Execute a compiled FASL program            |
| `wile pkg`      | Manage local packages                      |
| `wile venv`     | Create a virtual environment               |
| `wile ext`      | Manage native extensions                   |
| `wile debug`    | Debug a program via DAP                    |
| `wile lsp`      | Start the LSP server                       |
| `wile profile`  | Profile a program                          |

## Environment Variables

| Variable      | Description                                              |
|---------------|----------------------------------------------------------|
| `WILE_VENV`   | Path to active virtual environment                       |
| `WILE_PATH`   | Colon-separated additional library search directories    |
| `WILE_HOME`   | Override for Wile home directory (default: `~/.wile/`)   |
| `WILE_THEME`  | Color theme: `dark`, `light`, `none`, or a file path     |
| `WILE_STDLIB` | Override for built-in stdlib directory                    |

## Embedding

### OCaml

```ocaml
let inst = Wile.Instance.create () in
let result = Wile.Instance.eval_string inst "(+ 1 2)" in
Printf.printf "%s\n" (Wile.Datum.to_string result)
```

### C

```c
#include "wile.h"

int main(void) {
    wile_inst_t inst = wile_create();
    wile_val_t result = wile_eval_string(inst, "(+ 1 2)");
    printf("%s\n", wile_to_string(inst, result));
    wile_destroy(inst);
    return 0;
}
```

## Project Structure

```
bin/            Executable entry point (main.ml)
lib/            Core library — 37 OCaml modules
test/           Test suite — 40 test files, 1600+ tests
stdlib/srfi/    Bundled SRFI library sources (.sld files)
```

## License

MIT
