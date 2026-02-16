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

val should_complete_at : string -> int -> bool
(** [should_complete_at text cursor] returns [true] when the character
    before [cursor] is an identifier character, indicating that Tab
    should trigger completion rather than indentation. *)

val complete_path : string -> string list
(** [complete_path partial] returns filesystem completions for a partial
    path. Directories get a trailing [/]. Results are sorted. *)

val format_library_name : string list -> string
(** [format_library_name ["scheme"; "base"]] returns ["(scheme base)"]. *)

val match_library_name :
  string list -> string -> string list list -> string list list
(** [match_library_name known_parts prefix all_libs] filters library names
    where the first N parts match [known_parts] and the (N+1)th part
    starts with [prefix]. *)

val is_ident_char : char -> bool
(** [is_ident_char c] returns [true] if [c] is a Scheme identifier
    character (letters, digits, and special chars like [!?+*/-]). *)
