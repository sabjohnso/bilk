(** Character classification for the readtable.

    Each character in a readtable is assigned a {!t} that controls how the
    reader treats it.  The six variants correspond to the character classes
    described in Common Lisp's readtable model, adapted for R7RS Scheme. *)

(** The character‐class type.  Exposed so callers can pattern‐match. *)
type t =
  | Constituent            (** Part of a token (identifier, number, etc.) *)
  | Whitespace             (** Separates tokens; skipped by the reader *)
  | Terminating_macro      (** Terminates the current token and invokes a
                               reader macro (e.g. parentheses, quote, comma) *)
  | Non_terminating_macro  (** Invokes a reader macro but does not terminate
                               a token already in progress (e.g. [#]) *)
  | Single_escape          (** Escapes the next character inside a token *)
  | Multiple_escape        (** Toggles escape mode for a span of characters
                               (e.g. [|...|]) *)

val equal : t -> t -> bool
(** [equal a b] is structural equality on character classes. *)

val to_string : t -> string
(** [to_string ct] returns a human-readable name such as
    ["constituent"] or ["terminating-macro"]. *)

val all : t list
(** A list of all six character‐class variants, useful for exhaustive
    testing. *)
