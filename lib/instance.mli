(** Per-instance Scheme state.

    An {!t} value holds all mutable state for one Scheme instance: the symbol
    table, the global environment, and the readtable.  Multiple independent
    instances can coexist in a single process.

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
}

(** {1 Constructors} *)

val create : ?readtable:Readtable.t -> unit -> t
(** [create ?readtable ()] returns a fresh instance with an empty symbol table,
    a global environment pre-populated with standard primitives ([+], [-], [*],
    [<], [=], [>], [<=], [>=], [cons], [car], [cdr], [null?], [pair?], [not],
    [display], [newline], [eqv?], [eq?], [list]), and the given [readtable]
    (defaults to {!Readtable.default}). *)

(** {1 Convenience} *)

val intern : t -> string -> Symbol.t
(** [intern inst name] is [Symbol.intern inst.symbols name]. *)

(** {1 Evaluation} *)

val eval_syntax : t -> Syntax.t -> Datum.t
(** [eval_syntax inst expr] compiles and executes a syntax object.
    @raise Compiler.Compile_error on malformed syntax.
    @raise Vm.Runtime_error on runtime errors. *)

val eval_string : t -> string -> Datum.t
(** [eval_string inst src] reads one expression from [src], compiles, and
    executes it.  Returns the result.
    @raise Reader.Read_error on malformed input.
    @raise Compiler.Compile_error on malformed syntax.
    @raise Vm.Runtime_error on runtime errors. *)
