(** Runtime Scheme values.

    {!t} is the core value representation used by the VM and the embedding
    API.  It carries no source‐location information — that is the domain of
    {!Syntax.t}.  Every Scheme object that can exist at run time is a [Datum.t]. *)

(** The Scheme value type.  Exposed so callers can pattern‐match on values. *)
type t =
  | Bool of bool               (** [#t] / [#f] *)
  | Fixnum of int              (** Exact integer (machine word) *)
  | Flonum of float            (** Inexact real *)
  | Char of Uchar.t            (** Unicode character *)
  | Str of string              (** Immutable string *)
  | Symbol of string           (** Symbol (uninterned at this level) *)
  | Pair of t * t              (** Cons cell *)
  | Vector of t array          (** Mutable vector *)
  | Bytevector of bytes        (** Mutable bytevector *)
  | Nil                        (** Empty list [()] *)
  | Eof                        (** End‐of‐file object *)

val equal : t -> t -> bool
(** [equal a b] is recursive structural equality.  [Flonum] uses
    [Float.equal] (so [nan = nan] is [true]).  Vectors and bytevectors are
    compared element‐wise. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt d] pretty‐prints [d] in Scheme external representation.
    Uses dotted‐pair notation for improper lists and the standard [#t],
    [#f], [+inf.0], [-inf.0], [+nan.0] forms.

    Suitable for use with [Format.asprintf] and Alcotest's [testable]. *)

val to_string : t -> string
(** [to_string d] is [Format.asprintf "%a" pp d]. *)
