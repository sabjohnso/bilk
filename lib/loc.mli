(** Source locations.

    A {!t} identifies a position in a source file by file name, line number,
    and column number.  Locations are attached to every node of {!Syntax.t}
    to support precise error messages throughout the compiler pipeline. *)

(** A source location: file, line, column.  Lines and columns are 1‐based
    in normal use; {!none} uses 0 to indicate an unknown position. *)
type t = { file : string; line : int; col : int }

val make : string -> int -> int -> t
(** [make file line col] constructs a source location. *)

val none : t
(** A sentinel location for generated code or unknown positions.
    File is ["<unknown>"], line and column are [0]. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality on locations. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt loc] prints [file:line:col]. *)

val to_string : t -> string
(** [to_string loc] is [Format.asprintf "%a" pp loc]. *)
