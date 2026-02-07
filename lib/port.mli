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

val of_file : string -> t
(** [of_file path] reads the file at [path] into memory and returns a
    port.  The file name is used for source locations.
    Raises [Sys_error] if the file cannot be opened. *)

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

val readtable : t -> Readtable.t option
(** [readtable p] returns the readtable stored on this port, if any.
    Used by the reader to persist [#!fold-case] / [#!no-fold-case]
    directives across multiple [read] calls on the same port. *)

val set_readtable : t -> Readtable.t -> unit
(** [set_readtable p rt] stores readtable [rt] on the port.  Subsequent
    [read] calls on [p] will pick up this readtable. *)
