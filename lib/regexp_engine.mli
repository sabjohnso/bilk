(** Regular expression engine for SRFI 115.

    A backtracking NFA-based regex engine operating on byte strings.
    No dependency on [Datum] — operates on [bytes] and integer positions.

    Supports: literals, character classes (256-bit sets), [.] (any),
    alternation, sequence, repetition (greedy), submatch groups, and
    anchors (bos/eos/bol/eol). *)

(** {1 Abstract Syntax} *)

(** A regex AST node. *)
type node =
  | Lit of int           (** Match a single byte value *)
  | Any                  (** Match any byte (except newline) *)
  | Class of bytes       (** 256-bit character class (32 bytes) *)
  | NClass of bytes      (** Negated 256-bit character class *)
  | Seq of node list     (** Sequence of nodes *)
  | Alt of node * node   (** Alternation (try left first) *)
  | Rep of node * int * int option * bool
    (** Repetition: node, min, max (None=unlimited), greedy *)
  | Group of int * node  (** Submatch group with index *)
  | Bos                  (** Beginning of string anchor *)
  | Eos                  (** End of string anchor *)
  | Bol                  (** Beginning of line anchor *)
  | Eol                  (** End of line anchor *)

(** {1 Compiled Regex} *)

(** An opaque compiled regex. *)
type compiled

(** {1 Match Result} *)

(** Result of a match attempt. *)
type match_result = {
  matched : bool;
    (** Whether the match succeeded *)
  groups : (int * int) option array;
    (** Submatch groups: [Some (start, end_)] or [None] for unmatched.
        Group 0 is the overall match. *)
}

(** {1 Operations} *)

val compile : node -> int -> compiled
(** [compile node num_groups] compiles a regex AST into an executable form.
    [num_groups] is the total number of capture groups (including group 0). *)

val exec : compiled -> bytes -> int -> int -> match_result
(** [exec compiled input start end_] attempts to match [compiled] against
    [input] from position [start] to [end_].  The match must consume exactly
    the region [start..end_]. *)

val search : compiled -> bytes -> int -> int -> match_result
(** [search compiled input start end_] finds the first match of [compiled]
    anywhere in [input\[start..end_\]].  Returns a non-matching result if no
    match is found. *)
