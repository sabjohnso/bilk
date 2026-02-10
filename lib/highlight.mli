(** Highlight — Theme engine and ANSI syntax highlighting.

    Provides configurable syntax highlighting for Scheme source code
    using 256-color ANSI escape sequences. Includes built-in dark and
    light themes, theme loading from s-expression files, and rainbow
    parenthesis support with depth-based color cycling. *)

(** {1 Types} *)

(** ANSI text style. *)
type style = {
  fg : int option;          (** 256-color foreground index. *)
  bg : int option;          (** 256-color background index. *)
  bold : bool;              (** Bold text. *)
  italic : bool;            (** Italic text. *)
  underline : bool;         (** Underlined text. *)
}

(** A color theme mapping token kinds to styles. *)
type theme = {
  name : string;            (** Theme name. *)
  paren : style array;      (** Rainbow paren styles, depth-cycled. *)
  string_style : style;     (** String literal style. *)
  number_style : style;     (** Number literal style. *)
  keyword_style : style;    (** Special form / keyword style. *)
  comment_style : style;    (** Comment style. *)
  symbol_style : style;     (** Symbol / identifier style. *)
  boolean_style : style;    (** Boolean literal style. *)
  char_style : style;       (** Character literal style. *)
  quote_style : style;      (** Quote shorthand style. *)
  error_style : style;      (** Error / unrecognized token style. *)
  default_style : style;    (** Default / fallback style. *)
  defn_name_style : style;  (** Defined name style (function/variable names). *)
  param_style : style;      (** Parameter / binding style. *)
}

(** {1 Built-in themes} *)

(** Dark terminal theme (suitable for dark backgrounds). *)
val dark_theme : theme

(** Light terminal theme (suitable for light backgrounds). *)
val light_theme : theme

(** {1 Highlighting} *)

(** [highlight_line theme rt text cursor_pos] returns [text] with ANSI
    color escapes inserted according to [theme]. Uses {!Tokenizer.tokenize}
    to lex the input. [cursor_pos] is used for matching-paren highlighting
    (the paren adjacent to the cursor and its match are underlined).

    @param theme the color theme to use
    @param rt readtable for tokenization
    @param text the source text to highlight
    @param cursor_pos cursor byte offset for paren matching (-1 to disable) *)
val highlight_line : theme -> Readtable.t -> string -> int -> string

(** {1 ANSI helpers} *)

(** [style_to_ansi style] returns the ANSI SGR escape sequence for [style].
    Returns [""] for the default (empty) style. *)
val style_to_ansi : style -> string

(** [strip_ansi text] removes all ANSI escape sequences from [text]. *)
val strip_ansi : string -> string

(** {1 Theme loading} *)

(** [load_theme path] loads a theme from an s-expression file.
    Raises [Failure] if the file cannot be read or parsed. *)
val load_theme : string -> theme

(** {1 Style constructors} *)

(** [default_style] is the empty style (no formatting). *)
val default_style : style

(** [fg color] returns a style with only the foreground color set. *)
val fg : int -> style

(** [fg_bold color] returns a bold style with the foreground color set. *)
val fg_bold : int -> style

(** {1 Semantic analysis} *)

(** The role a token plays in a binding structure. *)
type sem_role = Defn_name | Param | Bound_var

(** A semantic mark recording a binding-site token's role, name, and
    the byte range of the enclosing scope. *)
type sem_mark = {
  role : sem_role;
  name : string;
  scope_start : int;
  scope_end : int;
}

(** [analyze_semantics tokens text] walks the token stream looking for
    binding-form patterns (define, lambda, let, do, etc.) and returns a
    hashtable mapping byte offsets of binding-site tokens to their
    {!sem_mark} records.

    @param tokens tokens from {!Tokenizer.tokenize}
    @param text the source text that was tokenized *)
val analyze_semantics :
  Tokenizer.token list -> string -> (int, sem_mark) Hashtbl.t

(** [find_cursor_binding tokens text cursor_pos marks] finds the
    identifier at [cursor_pos] and its innermost binding site.

    Returns [Some (tok_start, tok_stop, binding_opt)] where
    [binding_opt] is [Some (bind_start, bind_stop)] if a binding site
    was found, or [None] if the identifier is unbound or is itself
    a binding site.  Returns [None] if no identifier is at [cursor_pos].

    @param tokens tokens from {!Tokenizer.tokenize}
    @param text the source text
    @param cursor_pos byte offset of the cursor
    @param marks result of {!analyze_semantics} *)
val find_cursor_binding :
  Tokenizer.token list -> string -> int ->
  (int, sem_mark) Hashtbl.t ->
  (int * int * (int * int) option) option
