(** Tokenizer — Fault-tolerant lexer for syntax highlighting.

    Unlike the {!Reader} (which is strict and raises on errors), this
    tokenizer never fails. It produces a complete covering of the input —
    every byte belongs to exactly one token. Partial or malformed input
    is handled gracefully (e.g. unterminated strings span to EOF).

    Used by the syntax highlighter and paredit for understanding
    s-expression structure without requiring a full parse. *)

(** {1 Types} *)

(** A byte range within the input string. *)
type span = {
  start : int;  (** Start byte offset (inclusive). *)
  stop : int;   (** End byte offset (exclusive). *)
}

(** Token classification for syntax highlighting. *)
type token_kind =
  | Paren_open        (** Opening delimiter: [(], [\[], [#(], [#u8(] *)
  | Paren_close       (** Closing delimiter: [)], [\]] *)
  | String_lit        (** String literal including delimiters *)
  | Number_lit        (** Numeric literal *)
  | Boolean_lit       (** [#t], [#f], [#true], [#false] *)
  | Char_lit          (** Character literal: [#\\x] *)
  | Symbol            (** Identifier / symbol *)
  | Keyword           (** Known special form (define, lambda, if, ...) *)
  | Quote_shorthand   (** ['], [,], [,\@], [`] *)
  | Comment           (** Line comment or block comment *)
  | Datum_comment     (** [#;] datum comment prefix *)
  | Whitespace        (** Whitespace characters *)
  | Hash_prefix       (** Unrecognized [#] sequence *)
  | Error             (** Unrecognizable input *)

(** A token with its kind and position. *)
type token = {
  kind : token_kind;
  span : span;
}

(** {1 Tokenization} *)

(** [tokenize rt text] breaks [text] into a list of tokens covering the
    entire input. Uses [rt] for character classification (delimiter and
    whitespace detection). Never raises. *)
val tokenize : Readtable.t -> string -> token list

(** {1 Keyword detection} *)

(** [is_keyword name] returns [true] if [name] is a known Scheme special
    form or syntax keyword. *)
val is_keyword : string -> bool
