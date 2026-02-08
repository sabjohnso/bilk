(** Input and output ports with source-location tracking.

    A port wraps a character source or sink and maintains the current file
    name, line, and column.  The reader uses input ports to produce accurate
    {!Loc.t} values for every syntax node.  Output ports support writing
    characters, strings, and bytes. *)

(** The port type.  Opaque — use the functions below. *)
type t

(** {1 Input constructors} *)

val of_string : ?file:string -> string -> t
(** [of_string ?file s] creates an input port that reads from string [s].
    @param file  Source file name for locations (default ["<string>"]). *)

val of_file : string -> t
(** [of_file path] reads the file at [path] into memory and returns an
    input port.  The file name is used for source locations.
    Raises [Sys_error] if the file cannot be opened. *)

val of_in_channel : ?file:string -> in_channel -> t
(** [of_in_channel ?file chan] creates an input port that reads from
    [chan] with 1-char lookahead.  Useful for stdin.
    @param file  Source name for locations (default ["<channel>"]). *)

val open_input_file : string -> t
(** [open_input_file path] is equivalent to {!of_file}. *)

(** {1 Output constructors} *)

val of_out_channel : ?file:string -> out_channel -> t
(** [of_out_channel ?file chan] creates an output port that writes to [chan].
    @param file  Name for display (default ["<channel>"]). *)

val open_output_string : unit -> t
(** [open_output_string ()] creates an output port that accumulates
    written data in an internal buffer. *)

val open_output_file : string -> t
(** [open_output_file path] creates an output port that writes to the
    file at [path].
    Raises [Sys_error] if the file cannot be opened. *)

(** {1 Input operations} *)

val peek_char : t -> char option
(** [peek_char p] returns the next character without consuming it, or
    [None] at end of input.  Idempotent — repeated calls return the same
    character.
    Raises [Failure] if [p] is not an input port or is closed. *)

val read_char : t -> char option
(** [read_char p] consumes and returns the next character, or [None] at
    end of input.  Updates the line and column counters.  Treats [\\r\\n]
    as a single newline.
    Raises [Failure] if [p] is not an input port or is closed. *)

val read_line : t -> string option
(** [read_line p] reads characters up to (but not including) the next
    newline or end of input.  Returns [None] at EOF.
    Raises [Failure] if [p] is not an input port or is closed. *)

val read_u8 : t -> int option
(** [read_u8 p] reads and returns the next byte as an integer, or [None]
    at end of input.  Does not update line/column counters.
    Raises [Failure] if [p] is not an input port or is closed. *)

val peek_u8 : t -> int option
(** [peek_u8 p] returns the next byte without consuming it, or [None]
    at end of input.
    Raises [Failure] if [p] is not an input port or is closed. *)

(** {1 Output operations} *)

val write_char : t -> char -> unit
(** [write_char p c] writes character [c] to port [p].
    Raises [Failure] if [p] is not an output port or is closed. *)

val write_uchar : t -> Uchar.t -> unit
(** [write_uchar p uc] writes Unicode character [uc] as UTF-8 to port [p].
    Raises [Failure] if [p] is not an output port or is closed. *)

val write_string : t -> string -> unit
(** [write_string p s] writes string [s] to port [p].
    Raises [Failure] if [p] is not an output port or is closed. *)

val write_u8 : t -> int -> unit
(** [write_u8 p byte] writes a single byte to port [p].
    Raises [Failure] if [p] is not an output port or is closed. *)

val write_bytes : t -> bytes -> int -> int -> unit
(** [write_bytes p data off len] writes [len] bytes from [data] starting
    at offset [off] to port [p].
    Raises [Failure] if [p] is not an output port or is closed. *)

val flush : t -> unit
(** [flush p] flushes the output port [p].  No-op for string ports.
    Raises [Failure] if [p] is not an output port or is closed. *)

val get_output_string : t -> string
(** [get_output_string p] returns the accumulated output from a string
    output port.
    Raises [Failure] if [p] is not a string output port or is closed. *)

(** {1 Close} *)

val close : t -> unit
(** [close p] closes the port.  Flushes output ports.  Subsequent I/O
    operations on [p] will raise [Failure].  Closing an already-closed
    port is a no-op. *)

(** {1 Predicates} *)

val is_input : t -> bool
(** [is_input p] is [true] if [p] is an input port. *)

val is_output : t -> bool
(** [is_output p] is [true] if [p] is an output port. *)

val is_open : t -> bool
(** [is_open p] is [true] if [p] has not been closed. *)

(** {1 Location tracking} *)

val current_loc : t -> Loc.t
(** [current_loc p] returns the source location of the next character to
    be read (i.e. the position {i after} the most recently consumed
    character). *)

val readtable_obj : t -> Obj.t option
(** [readtable_obj p] returns the readtable stored on this port as a
    boxed value, if any.  The caller is responsible for casting to the
    appropriate type.  Used by the reader to persist [#!fold-case] /
    [#!no-fold-case] directives across multiple [read] calls. *)

val set_readtable_obj : t -> Obj.t -> unit
(** [set_readtable_obj p obj] stores a boxed readtable value on the port.
    Subsequent [read] calls on [p] will pick up this readtable. *)

val position : t -> int
(** [position p] returns the current byte offset in the port's content.
    Returns 0 for channel-based ports and output ports. *)

val file_name : t -> string
(** [file_name p] returns the file name associated with this port. *)
