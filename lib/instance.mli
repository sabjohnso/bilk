(** Per-instance Scheme state.

    An {!t} value holds all mutable state for one Scheme instance: the symbol
    table, the global environment, the readtable, the syntactic environment
    for macro expansion, the dynamic-wind/exception handler stacks, and the
    library registry.  Multiple independent instances can coexist in a
    single process.

    Instances are passed explicitly — never stored in a global ref. *)

(** {1 Types} *)

(** The instance record.  Fields are exposed for direct access. *)
type t = {
  symbols : Symbol.table;
  (** The symbol intern table for this instance. *)
  global_env : Env.t;
  (** The global (top-level) environment. *)
  readtable : Readtable.t;
  (** The readtable used by the reader. *)
  winds : Datum.wind list ref;
  (** The dynamic-wind stack, shared across evaluations. *)
  handlers : Datum.t list ref;
  (** The exception handler stack, shared across evaluations. *)
  syn_env : Expander.syn_env;
  (** The syntactic environment for macro expansion. *)
  gensym_counter : int ref;
  (** Counter for generating fresh identifiers during macro expansion. *)
  libraries : Library.registry;
  (** The library registry for this instance. *)
  search_paths : string list ref;
  (** Directories to search for library [.sld] files. *)
  features : string list;
  (** Feature identifiers for [cond-expand] (e.g. ["r7rs"; "wile"; "linux"]). *)
  loading_libs : string list list ref;
  (** Libraries currently being loaded from [.sld] files.
      Used to detect circular dependencies during auto-loading. *)
  fasl_cache : bool ref;
  (** When [true], compiled library code is cached to [.fasl] files
      alongside the source [.sld] files, and loaded from cache when
      the cache is fresh.  Defaults to [false]. *)
  current_input : Port.t ref;
  (** The current input port (default: stdin). *)
  current_output : Port.t ref;
  (** The current output port (default: stdout). *)
  current_error : Port.t ref;
  (** The current error port (default: stderr). *)
  command_line : string list ref;
  (** The command-line arguments for [command-line] (default: [Sys.argv]). *)
  eval_envs : (int, Datum.env * Expander.syn_env) Hashtbl.t;
  (** Side table mapping environment specifier IDs to env + syn_env pairs.
      Used by [(scheme eval)] to pass environments to [eval]. *)
  eval_env_counter : int ref;
  (** Counter for generating fresh environment specifier IDs. *)
  extension_lib_env : (Env.t * Expander.syn_env) option ref;
  (** When set, {!define_primitive} also registers into these envs.
      Used during [include-shared] processing.  Default: [ref None]. *)
  on_call : (Loc.t -> Datum.t -> Datum.t list -> unit) option ref;
  (** Optional callback fired before each procedure call in the VM.
      Receives the source location of the call site, the procedure
      being called, and the argument list.  Default: [ref None]. *)
  on_return : (Loc.t -> Datum.t -> unit) option ref;
  (** Optional callback fired after each procedure return in the VM.
      Receives the source location and the return value.
      Default: [ref None]. *)
  debug_state : Vm.debug_state option ref;
  (** Optional debug state populated by the VM before each [on_call]
      callback, providing access to the current environment and call
      stack.  Default: [ref None]. *)
}

(** {1 Constructors} *)

val create : ?readtable:Readtable.t -> unit -> t
(** [create ?readtable ()] returns a fresh instance with an empty symbol table,
    a global environment pre-populated with ~210 R7RS primitives
    and intrinsics, 10 self-hosted boot definitions, built-in libraries
    [(scheme base)], [(scheme char)], [(scheme write)], [(scheme cxr)],
    [(scheme file)], [(scheme read)], [(scheme inexact)], [(scheme complex)],
    [(scheme lazy)], [(scheme case-lambda)], [(scheme process-context)],
    [(scheme time)], [(scheme eval)], [(scheme load)], [(scheme repl)],
    [(scheme r5rs)],
    and the given [readtable] (defaults to {!Readtable.default}). *)

(** {1 Convenience} *)

val intern : t -> string -> Symbol.t
(** [intern inst name] is [Symbol.intern inst.symbols name]. *)

(** {1 Lookup and registration} *)

val lookup : t -> string -> Datum.t option
(** [lookup inst name] looks up a binding by name in the global environment.
    Returns [Some v] if bound, [None] otherwise. *)

val define_primitive : t -> string -> (Datum.t list -> Datum.t) -> unit
(** [define_primitive inst name fn] registers an OCaml function as a Scheme
    primitive in the global environment and syntactic environment, making it
    available to both compiled code and the macro expander. *)

(** {1 Calling} *)

val call : t -> Datum.t -> Datum.t list -> Datum.t
(** [call inst proc args] calls a Scheme procedure (closure, primitive, or
    continuation) with the given arguments.  Uses synthetic bytecode —
    no compiler or reader involved.
    @raise Vm.Runtime_error if [proc] is not callable or arity mismatch. *)

(** {1 Evaluation} *)

val eval_datum : t -> Datum.t -> Datum.t
(** [eval_datum inst d] evaluates a runtime datum as an expression.
    Self-evaluating values (numbers, booleans, strings) return themselves.
    Symbols are looked up.  Lists are treated as procedure calls.
    @raise Compiler.Compile_error on malformed syntax.
    @raise Vm.Runtime_error on runtime errors. *)

val eval_syntax : t -> Syntax.t -> Datum.t
(** [eval_syntax inst expr] expands macros, compiles, and executes a syntax
    object.  Also handles top-level [import] and [define-library] forms.
    @raise Compiler.Compile_error on malformed syntax or macro expansion error.
    @raise Vm.Runtime_error on runtime errors. *)

val eval_string : t -> string -> Datum.t
(** [eval_string inst src] reads one expression from [src], expands macros,
    compiles, and executes it.  Returns the result.
    @raise Reader.Read_error on malformed input.
    @raise Compiler.Compile_error on malformed syntax or macro expansion error.
    @raise Vm.Runtime_error on runtime errors. *)

val load_file : t -> string -> unit
(** [load_file inst path] loads and executes all expressions from a Scheme
    source file.  Equivalent to opening the file as a port and calling
    {!eval_port}, discarding the result.
    Raises [Sys_error] if the file cannot be opened.
    @raise Reader.Read_error on malformed input.
    @raise Compiler.Compile_error on malformed syntax.
    @raise Vm.Runtime_error on runtime errors. *)

val load_fasl : t -> string -> unit
(** [load_fasl inst path] loads and executes a pre-compiled FASL file.
    @raise Fasl.Fasl_error on format or I/O errors.
    @raise Vm.Runtime_error on runtime errors. *)

val eval_port : t -> Port.t -> Datum.t
(** [eval_port inst port] evaluates all expressions from [port] until EOF.
    Returns the last result, or [Void] if the port was empty.
    Handles [import] and [define-library] forms at top level.
    @raise Reader.Read_error on malformed input.
    @raise Compiler.Compile_error on malformed syntax or macro expansion error.
    @raise Vm.Runtime_error on runtime errors. *)

(** {1 Ahead-of-time compilation} *)

val compile_port : t -> Port.t -> Fasl.program_fasl
(** [compile_port inst port] reads all top-level forms from [port] and
    compiles them into a program FASL.  Imports are processed at compile time
    for binding resolution and recorded as [Lib_import] declarations.
    [define-library] forms are processed at compile time but not recorded.
    Expressions are expanded, compiled, and recorded as [Lib_code] declarations.
    @raise Reader.Read_error on malformed input.
    @raise Compiler.Compile_error on malformed syntax or macro expansion error. *)

val run_program : t -> Fasl.program_fasl -> Datum.t
(** [run_program inst prog] replays a program FASL: processes each import
    and executes each code object in order.  Returns the last result, or
    [Void] if the program is empty.
    @raise Vm.Runtime_error on runtime errors. *)

(** {1 Package integration} *)

val load_native_ref :
  (t -> search_dirs:string list -> sld_dir:string option -> string -> unit) ref
(** Forward reference for native extension loading.  Filled in by
    {!Extension} at module-init time to break the dependency cycle
    [Instance -> Extension -> Instance]. *)

val setup_package_paths : t -> registry_root:string -> Package.t -> unit
(** [setup_package_paths inst ~registry_root pkg] resolves the package's
    dependencies, computes search paths for the resolved packages, and
    prepends the package's own [src/] directory plus resolved dependency
    paths to [inst.search_paths].
    @raise Pkg_manager.Pkg_error on resolution errors. *)
