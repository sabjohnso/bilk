(** Paredit — Structural editing operations for Scheme code.

    All operations are pure functions of the form
    [(text, cursor) -> edit_result], making them fully testable without
    terminal I/O. Uses {!Tokenizer} to understand s-expression structure
    (respecting strings and comments).

    Paredit mode maintains balanced parentheses: opening delimiters always
    insert pairs, deletion skips unbalanced delimiters, and structural
    operations (slurp, barf, wrap, splice, raise) preserve balance. *)

(** {1 Types} *)

(** Result of an editing operation. *)
type edit_result = {
  text : string;    (** Updated text. *)
  cursor : int;     (** Updated cursor position. *)
}

(** {1 Navigation} *)

(** [find_matching_paren rt text pos] returns the position of the
    matching delimiter for the paren at [pos], or [None] if no match. *)
val find_matching_paren : Readtable.t -> string -> int -> int option

(** [sexp_forward rt text pos] moves past the next s-expression starting
    at or after [pos]. Returns [None] if no sexp found. *)
val sexp_forward : Readtable.t -> string -> int -> int option

(** [sexp_backward rt text pos] moves before the previous s-expression
    ending at or before [pos]. Returns [None] if no sexp found. *)
val sexp_backward : Readtable.t -> string -> int -> int option

(** [enclosing_paren rt text pos] returns the positions [(open_pos, close_pos)]
    of the innermost enclosing parentheses around [pos], or [None]. *)
val enclosing_paren : Readtable.t -> string -> int -> (int * int) option

(** {1 Balanced insertion} *)

(** [insert_open_paren text cursor] inserts ["()"] and places cursor between. *)
val insert_open_paren : string -> int -> edit_result

(** [insert_close_paren rt text cursor] moves past existing [")"] if present
    at cursor, otherwise inserts [")"] normally. *)
val insert_close_paren : Readtable.t -> string -> int -> edit_result

(** [insert_double_quote rt text cursor] inserts ['""'] pair or moves past
    closing ['"'] if cursor is inside a string at its end. *)
val insert_double_quote : Readtable.t -> string -> int -> edit_result

(** {1 Balanced deletion} *)

(** [backspace_paredit rt text cursor] deletes backward, but skips lone
    delimiters and removes empty pairs as a unit. *)
val backspace_paredit : Readtable.t -> string -> int -> edit_result

(** [delete_paredit rt text cursor] deletes forward, but skips lone
    delimiters and removes empty pairs as a unit. *)
val delete_paredit : Readtable.t -> string -> int -> edit_result

(** {1 Structural operations} *)

(** [slurp_forward rt text cursor] absorbs the next sexp after the closing
    paren of the enclosing list into the list. *)
val slurp_forward : Readtable.t -> string -> int -> edit_result

(** [barf_forward rt text cursor] ejects the last sexp from the enclosing
    list, placing it after the closing paren. *)
val barf_forward : Readtable.t -> string -> int -> edit_result

(** [slurp_backward rt text cursor] absorbs the previous sexp before the
    opening paren of the enclosing list into the list. *)
val slurp_backward : Readtable.t -> string -> int -> edit_result

(** [barf_backward rt text cursor] ejects the first sexp from the enclosing
    list, placing it before the opening paren. *)
val barf_backward : Readtable.t -> string -> int -> edit_result

(** [wrap_round rt text cursor] wraps the next sexp in parentheses. *)
val wrap_round : Readtable.t -> string -> int -> edit_result

(** [splice rt text cursor] removes the enclosing parentheses. *)
val splice : Readtable.t -> string -> int -> edit_result

(** [raise_sexp rt text cursor] replaces the enclosing list with the
    sexp at cursor. *)
val raise_sexp : Readtable.t -> string -> int -> edit_result

(** {1 Indentation} *)

(** [compute_indent rt text cursor] returns the number of spaces to indent
    a new line inserted at [cursor]. Uses Scheme indentation conventions:
    special forms (define, lambda, let, begin, etc.) indent body by 2 from
    the opening paren; regular calls align with the first argument if one
    appears on the same line as the operator, otherwise indent by 2. *)
val compute_indent : Readtable.t -> string -> int -> int
