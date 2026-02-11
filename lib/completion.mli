(** Tab-completion logic for the REPL.

    Pure functions with no I/O — independently testable. *)

val extract_prefix : string -> int -> string * int
(** [extract_prefix text cursor] returns [(prefix, start_offset)].
    Walks backward from [cursor] over Scheme identifier characters
    (including comma for REPL commands). *)

val find_matches : string -> string list -> string list
(** [find_matches prefix candidates] returns sorted candidates
    that start with [prefix]. *)

val common_prefix : string list -> string
(** [common_prefix strs] returns the longest common prefix of all strings.
    Returns [""] for an empty list. *)

val format_columns : width:int -> string list -> string
(** [format_columns ~width candidates] arranges candidates in columns
    fitting within [width].  Returns the formatted string. *)
