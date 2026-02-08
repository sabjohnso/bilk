(** Terminal — Raw terminal I/O with ANSI escape sequence handling.

    Provides low-level terminal manipulation: entering/leaving raw mode,
    reading keystrokes (including multi-byte escape sequences), and
    output helpers for cursor movement and screen clearing. *)

(** {1 Types} *)

(** Decoded keystroke. *)
type key =
  | Char of char       (** Printable character *)
  | Enter              (** Enter/Return *)
  | Backspace          (** Backspace *)
  | Delete             (** Delete key *)
  | Tab                (** Tab *)
  | Up                 (** Arrow up *)
  | Down               (** Arrow down *)
  | Left               (** Arrow left *)
  | Right              (** Arrow right *)
  | Home               (** Home key *)
  | End_key            (** End key *)
  | Ctrl_a             (** Ctrl-A — beginning of line *)
  | Ctrl_b             (** Ctrl-B — backward char *)
  | Ctrl_c             (** Ctrl-C — interrupt *)
  | Ctrl_d             (** Ctrl-D — EOF / delete forward *)
  | Ctrl_e             (** Ctrl-E — end of line *)
  | Ctrl_f             (** Ctrl-F — forward char *)
  | Ctrl_k             (** Ctrl-K — kill to end of line *)
  | Ctrl_l             (** Ctrl-L — clear screen *)
  | Ctrl_n             (** Ctrl-N — next history *)
  | Ctrl_p             (** Ctrl-P — previous history *)
  | Ctrl_u             (** Ctrl-U — kill to beginning of line *)
  | Ctrl_w             (** Ctrl-W — kill previous word *)
  | Alt_left           (** Alt-Left — backward word *)
  | Alt_right          (** Alt-Right — forward word *)
  | Alt_backspace      (** Alt-Backspace — kill previous word *)
  | Alt_enter          (** Alt-Enter — force newline *)
  | Ctrl_right         (** Ctrl-Right — slurp forward (paredit) *)
  | Ctrl_left          (** Ctrl-Left — barf forward (paredit) *)
  | Alt_s              (** Alt-s — splice (paredit) *)
  | Alt_r              (** Alt-r — raise (paredit) *)
  | Alt_open_paren     (** Alt-( — wrap round (paredit) *)
  | Alt_9              (** Alt-9 — wrap round (paredit, alternative) *)
  | Unknown            (** Unrecognized sequence *)

(** Opaque terminal state handle. *)
type t

(** {1 Raw mode} *)

(** [enter_raw fd] saves current terminal attributes for file descriptor [fd]
    and switches to raw mode (no canonical processing, no echo, no signal
    generation, VMIN=1, VTIME=0).
    @return a handle for restoring the terminal later *)
val enter_raw : Unix.file_descr -> t

(** [leave_raw t] restores the terminal attributes saved by {!enter_raw}. *)
val leave_raw : t -> unit

(** {1 Key reading} *)

(** [read_key t] blocks until a complete keystroke is available and returns
    the decoded key. Handles multi-byte ANSI escape sequences. *)
val read_key : t -> key

(** {1 Key parsing (pure)} *)

(** [parse_key_bytes bytes len] decodes a byte sequence of length [len] into
    a key. This is the pure core of {!read_key}, exposed for testing.

    @param bytes a byte buffer (only the first [len] bytes are examined)
    @param len number of valid bytes *)
val parse_key_bytes : bytes -> int -> key

(** {1 Output helpers} *)

(** [write_string t s] writes string [s] to the terminal. *)
val write_string : t -> string -> unit

(** [move_cursor_to t row col] moves the cursor to the given 1-based
    row and column using ANSI CSI sequences. *)
val move_cursor_to : t -> int -> int -> unit

(** [clear_to_end t] clears from the cursor to the end of the line. *)
val clear_to_end : t -> unit

(** [clear_below t] clears from the cursor to the end of the screen. *)
val clear_below : t -> unit

(** [move_to_column t col] moves the cursor to the given 1-based column
    on the current line. *)
val move_to_column : t -> int -> unit

(** [get_cursor_pos t] queries the terminal for the current cursor position.
    @return [(row, col)] as 1-based coordinates *)
val get_cursor_pos : t -> int * int

(** [get_terminal_size t] queries the terminal for the current size.
    @return [(rows, cols)] *)
val get_terminal_size : t -> int * int

(** [fd t] returns the underlying file descriptor. *)
val fd : t -> Unix.file_descr
