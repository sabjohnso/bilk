(** Readtable‐driven Scheme reader.

    Parses source text into syntax objects ({!Syntax.t}) or plain values
    ({!Datum.t}).  The reader is a recursive‐descent parser driven by the
    character classes in a {!Readtable.t}.

    Handles all R7RS §2 / §7.1 lexical syntax:
    identifiers, escaped identifiers, booleans, integers (with
    [#b]  [#o]  [#d]  [#x] radix and [#e]  [#i] exactness prefixes),
    floats ([+inf.0], [-inf.0], [+nan.0]), characters (named and hex),
    strings (all escape sequences and line continuations), lists,
    dotted pairs, vectors, bytevectors, quote shorthands,
    line / block / datum comments, [#!fold-case] / [#!no-fold-case]
    directives, and datum labels. *)

(** {1 Exceptions} *)

exception Read_error of Loc.t * string
(** Raised when the reader encounters invalid syntax.  Carries the source
    location and a human‐readable message. *)

(** {1 Reading} *)

val read_syntax : Readtable.t -> Port.t -> Syntax.t
(** [read_syntax rt port] reads one datum from [port] using readtable [rt]
    and returns it as a {!Syntax.t} with source locations.  Returns a
    node with {!Syntax.Eof} at end of input.

    @raise Read_error on malformed input. *)

val read : Readtable.t -> Port.t -> Datum.t
(** [read rt port] is [Syntax.to_datum (read_syntax rt port)].

    @raise Read_error on malformed input. *)
