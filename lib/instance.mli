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
    an empty global environment, and the given [readtable] (defaults to
    {!Readtable.default}). *)

(** {1 Convenience} *)

val intern : t -> string -> Symbol.t
(** [intern inst name] is [Symbol.intern inst.symbols name]. *)
