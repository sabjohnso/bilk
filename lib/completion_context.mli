(** Completion context detection.

    Classifies the cursor position within REPL input into a completion
    context using the token stream. Pure analysis — no candidate
    generation. *)

(** The context at the cursor position. *)
type context =
  | Identifier of string * int
      (** [(prefix, start_offset)] — cursor follows an identifier. *)
  | String_literal of string * int
      (** [(content_so_far, start_offset)] — cursor is inside a string
          literal. [start_offset] is the byte after the opening quote. *)
  | Repl_command_arg of string * string * int
      (** [(command, arg_prefix, arg_start)] — cursor is in the argument
          position of a REPL comma-command. *)
  | Import_library of string list * string * int
      (** [(resolved_parts, current_prefix, start)] — cursor is inside
          an [import] form's library name. *)
  | No_context
      (** No meaningful completion context at the cursor. *)

val detect : Readtable.t -> string -> int -> context
(** [detect rt text cursor] analyzes [text] up to [cursor] and returns
    the completion context. Uses [rt] for tokenization. *)
