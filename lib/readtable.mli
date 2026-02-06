(** Immutable readtable for the Scheme reader.

    A readtable maps characters to {!Char_type.t} classes and optional
    reader‐macro functions.  It also carries a dispatch sub‐table for
    [#]‐prefixed syntax and a {!fold_case} flag for case‐insensitive reading.

    Readtables are immutable values — all "set" operations return a new
    table.  The {!default} table is configured for R7RS Scheme. *)

(** {1 Types} *)

type reader_macro = char -> Datum.t
(** A reader macro receives the triggering character and returns a datum.
    (Placeholder signature — the real reader uses its own dispatch.) *)

type dispatch_macro = char -> Datum.t
(** A dispatch macro receives the character after [#] and returns a datum. *)

(** The readtable type.  Opaque — use the accessors below. *)
type t

(** {1 Constructors} *)

val empty : t
(** The empty readtable.  Every character defaults to {!Char_type.Constituent}
    with no macro, fold‐case is off, and the dispatch table is empty. *)

val default : t
(** The R7RS default readtable with standard whitespace, terminating macros
    (parentheses, double-quote, semicolon, quote, backquote, comma),
    non-terminating macro [#], multiple-escape [|], and dispatch entries
    for booleans, characters, vectors, bytevectors, comments, directives,
    and number prefixes. *)

(** {1 Queries} *)

val char_type_of : t -> char -> Char_type.t
(** [char_type_of rt c] returns the character class of [c] in [rt].
    Characters not explicitly mapped default to {!Char_type.Constituent}. *)

val macro_of : t -> char -> reader_macro option
(** [macro_of rt c] returns the reader macro for [c], if any. *)

val dispatch_macro_of : t -> char -> dispatch_macro option
(** [dispatch_macro_of rt c] returns the dispatch macro for [c], if any. *)

val fold_case : t -> bool
(** [fold_case rt] is [true] when the readtable is in case‐folding mode. *)

val is_whitespace : t -> char -> bool
(** [is_whitespace rt c] is [true] when [c] has class {!Char_type.Whitespace}
    in [rt]. *)

val is_delimiter : t -> char -> bool
(** [is_delimiter rt c] is [true] when [c] is whitespace, a terminating
    macro, or a multiple-escape character — i.e. it terminates the current
    token.  Per R7RS §7.1.1, delimiters are whitespace, vertical line,
    parentheses, double-quote, and semicolon. *)

(** {1 Functional Updates} *)

val set_char_type : char -> Char_type.t -> t -> t
(** [set_char_type c ct rt] returns a new readtable where [c] has class [ct].
    Any existing macro for [c] is preserved. *)

val set_macro : char -> Char_type.t -> reader_macro -> t -> t
(** [set_macro c ct f rt] returns a new readtable where [c] has class [ct]
    and reader macro [f]. *)

val set_dispatch_macro : char -> dispatch_macro -> t -> t
(** [set_dispatch_macro c f rt] returns a new readtable where [c] is
    registered as a dispatch macro in the [#] sub‐table. *)

val with_fold_case : bool -> t -> t
(** [with_fold_case b rt] returns a new readtable with fold‐case set to [b]. *)
