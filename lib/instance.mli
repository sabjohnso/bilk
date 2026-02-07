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
}

(** {1 Constructors} *)

val create : ?readtable:Readtable.t -> unit -> t
(** [create ?readtable ()] returns a fresh instance with an empty symbol table,
    a global environment pre-populated with 182 R7RS (scheme base) primitives
    and intrinsics, 10 self-hosted boot definitions, built-in libraries
    [(scheme base)], [(scheme char)], [(scheme write)], [(scheme cxr)],
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
