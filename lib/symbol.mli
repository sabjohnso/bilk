(** Interned symbols.

    Symbols are interned strings with unique integer identifiers.  Two symbols
    created via {!intern} with the same string and the same {!table} are
    guaranteed to have the same {!id}, enabling fast equality checks by
    integer comparison rather than string comparison.

    Each {!table} is independent — symbols interned in different tables may
    have different ids for the same name. *)

(** {1 Types} *)

(** An interned symbol.  Opaque — use the accessors below. *)
type t

(** A mutable intern table that maps strings to symbols. *)
type table

(** {1 Table Operations} *)

val create_table : unit -> table
(** [create_table ()] returns a fresh, empty intern table. *)

val intern : table -> string -> t
(** [intern tbl name] returns the symbol for [name] in [tbl].  If [name] has
    been interned before in [tbl], the same symbol (same {!id}) is returned.
    Otherwise a new symbol is created with a fresh id. *)

(** {1 Accessors} *)

val name : t -> string
(** [name sym] is the string name of [sym]. *)

val id : t -> int
(** [id sym] is the unique integer identifier of [sym] within its table. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same {!id}. *)

val compare : t -> t -> int
(** [compare a b] compares symbols by their {!id}. *)

val hash : t -> int
(** [hash sym] is [id sym].  Suitable for use as a hash key. *)

(** {1 Display} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt sym] prints the symbol's name. *)

val to_string : t -> string
(** [to_string sym] is the symbol's name. *)
