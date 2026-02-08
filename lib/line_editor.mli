(** Line_editor — Interactive line editor with Emacs keybindings.

    A custom line editor built on {!Terminal} raw mode. Supports both
    single-line and multi-line editing with history navigation,
    Emacs-style keybindings (Ctrl-A/E/B/F/K/U/W), word movement
    (Alt-Left/Right), and interrupt/EOF handling.

    In multi-line mode (when [is_complete] is provided), Enter inserts a
    newline when the input is incomplete and submits when complete.
    Alt-Enter always inserts a newline. Up/Down navigate between lines
    within multi-line input, and access history when on the first/last line. *)

(** {1 Types} *)

(** Completeness check function. Returns [true] if the input text is a
    complete expression ready for submission. *)
type completeness_check = string -> bool

(** Editor configuration. *)
type config = {
  prompt : string;
  (** Primary prompt string (e.g. ["wile> "]). *)

  continuation_prompt : string;
  (** Continuation prompt for incomplete input (e.g. ["  ... "]). *)

  history_file : string option;
  (** Path to history file for save/load. [None] disables persistence. *)

  max_history : int;
  (** Maximum number of history entries to retain. *)

  is_complete : completeness_check option;
  (** When [Some f], Enter calls [f text]. If [true], the input is submitted;
      if [false], a newline is inserted (multi-line mode). When [None],
      Enter always submits (single-line mode). *)

  highlight : (string -> int -> string) option;
  (** When [Some f], each line is highlighted by calling [f line_text cursor_pos]
      which returns the line with ANSI escape codes inserted. The cursor
      position is relative to the line. When [None], no highlighting. *)

  paredit : bool ref option;
  (** When [Some r], enables paredit mode when [!r] is [true].
      Balanced insertion/deletion and structural editing keys are active.
      When [None], paredit is never active. *)

  readtable : Readtable.t option;
  (** Readtable for paredit operations. Required when [paredit] is [Some _]. *)
}

(** Result of a {!read_input} call. *)
type input_result =
  | Input of string   (** User submitted input (may contain newlines). *)
  | Eof               (** User pressed Ctrl-D on empty input. *)
  | Interrupted       (** User pressed Ctrl-C. *)

(** Opaque editor state. *)
type t

(** {1 Lifecycle} *)

(** [create config] creates a new line editor. Loads history from
    [config.history_file] if specified and the file exists. *)
val create : config -> t

(** [destroy t] restores the terminal and saves history to the configured
    file. Should be called before exiting. *)
val destroy : t -> unit

(** {1 Input} *)

(** [read_input t] displays the prompt and reads input from the user.
    In multi-line mode, blocks until a complete expression is entered
    (or Ctrl-D/Ctrl-C). In single-line mode, blocks until Enter. *)
val read_input : t -> input_result

(** {1 History} *)

(** [history_add t entry] adds an entry to the history. Empty strings and
    entries identical to the most recent entry are silently ignored. *)
val history_add : t -> string -> unit

(** [save_history t] saves history to the configured file (if any). *)
val save_history : t -> unit

(** {1 Multi-line helpers (exposed for testing)} *)

(** [cursor_row text cursor] returns the 0-based row for byte offset [cursor]
    in [text]. *)
val cursor_row : string -> int -> int

(** [cursor_col text cursor] returns the 0-based column for byte offset
    [cursor] in [text]. *)
val cursor_col : string -> int -> int

(** [num_lines text] returns the number of lines in [text]. *)
val num_lines : string -> int

(** [row_start text row] returns the byte offset of the start of the
    0-based [row] in [text]. *)
val row_start : string -> int -> int

(** [row_length text row] returns the length (in bytes) of 0-based [row]
    in [text]. *)
val row_length : string -> int -> int

(** [pos_of_row_col text row col] converts 0-based [(row, col)] to a byte
    offset, clamping [col] to the line length. *)
val pos_of_row_col : string -> int -> int -> int

(** [word_forward text pos] returns the byte offset of the start of the
    next word boundary after [pos]. *)
val word_forward : string -> int -> int

(** [word_backward text pos] returns the byte offset of the start of the
    previous word boundary before [pos]. *)
val word_backward : string -> int -> int
