(** Compile‐time syntax objects.

    {!t} mirrors the structure of {!Datum.t} but every node carries a
    {!Loc.t} source location.  The reader produces [Syntax.t] values; the
    expander and compiler consume them.  Use {!to_datum} to strip locations
    when handing values to the runtime. *)

(** {1 Types} *)

(** A syntax object: a datum annotated with a source location. *)
type t = { loc : Loc.t; datum : datum }

(** The datum payload of a syntax node.  Variants parallel {!Datum.t}. *)
and datum =
  | Bool of bool
  | Fixnum of int
  | Bignum of Z.t
  | Rational of Z.t * Z.t
  | Flonum of float
  | Complex of t * t
  | Char of Uchar.t
  | Str of string
  | Symbol of string
  | Pair of t * t
  | Vector of t array
  | Bytevector of bytes
  | Nil
  | Eof

(** {1 Constructors} *)

val make : Loc.t -> datum -> t
(** [make loc d] wraps datum [d] with source location [loc]. *)

(** {1 Conversion} *)

val to_datum : t -> Datum.t
(** [to_datum s] recursively strips all source locations, converting a
    syntax tree to a plain runtime value. *)

val from_datum : Loc.t -> Datum.t -> t
(** [from_datum loc d] wraps a runtime datum [d] as a syntax tree with
    source location [loc].  Inverse of {!to_datum} for data types
    (Bool, Fixnum, Flonum, Char, Str, Symbol, Pair, Vector, Bytevector,
    Nil, Eof).  Non-data values (Void, Primitive, Closure, etc.) are
    represented as opaque symbols. *)

(** {1 Equality} *)

val equal_datum : t -> t -> bool
(** [equal_datum a b] compares the datum payloads of [a] and [b],
    ignoring source locations. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when both the location and datum payload match. *)

(** {1 Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt s] pretty‐prints the datum payload in Scheme external
    representation (delegates to {!Datum.pp} via {!to_datum}). *)

val to_string : t -> string
(** [to_string s] is [Format.asprintf "%a" pp s]. *)
