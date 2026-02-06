(** Input ports with source‐location tracking.

    A port wraps a character source and maintains the current file name,
    line, and column.  The reader uses ports to produce accurate {!Loc.t}
    values for every syntax node.

    Currently only string ports are supported.  File‐backed ports will be
    added in a later milestone. *)

(** The port type.  Opaque — use the functions below. *)
type t

val of_string : ?file:string -> string -> t
(** [of_string ?file s] creates a port that reads from string [s].
    @param file  Source file name for locations (default ["<string>"]). *)

val peek_char : t -> char option
(** [peek_char p] returns the next character without consuming it, or
    [None] at end of input.  Idempotent — repeated calls return the same
    character. *)

val read_char : t -> char option
(** [read_char p] consumes and returns the next character, or [None] at
    end of input.  Updates the line and column counters.  Treats [\\r\\n]
    as a single newline. *)

val current_loc : t -> Loc.t
(** [current_loc p] returns the source location of the next character to
    be read (i.e. the position {i after} the most recently consumed
    character). *)
