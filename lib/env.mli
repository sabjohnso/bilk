(** Lexical environments.

    An environment is a chain of frames, searched inner-to-outer.  Each frame
    maps symbol ids to mutable slots containing {!Datum.t} values.  Mutable
    slots support Scheme's [set!] semantics.

    The chain is extended by {!extend}, which prepends a new frame.
    {!define} always targets the top (innermost) frame.  {!set} walks the
    chain to find and mutate an existing binding. *)

(** {1 Types} *)

(** An environment (chain of frames).  Opaque. *)
type t

(** Raised by {!set} when the symbol has no binding in any frame. *)
exception Unbound_variable of Symbol.t

(** {1 Constructors} *)

val empty : unit -> t
(** [empty ()] returns a fresh environment with a single empty frame. *)

val extend : t -> (Symbol.t * Datum.t) list -> t
(** [extend env bindings] returns a new environment with a fresh frame
    containing [bindings] prepended to [env].  The original [env] is not
    modified. *)

(** {1 Operations} *)

val lookup : t -> Symbol.t -> Datum.t option
(** [lookup env sym] searches frames inner-to-outer for a binding of [sym].
    Returns [Some v] if found, [None] otherwise. *)

val set : t -> Symbol.t -> Datum.t -> unit
(** [set env sym value] mutates the existing binding of [sym] to [value].
    Searches frames inner-to-outer.
    @raise Unbound_variable if [sym] is not bound in any frame. *)

val define : t -> Symbol.t -> Datum.t -> unit
(** [define env sym value] adds or replaces the binding of [sym] in the
    top (innermost) frame of [env]. *)
