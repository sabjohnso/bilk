(** Semantic versioning (SemVer) types and operations.

    Provides parsing, comparison, and constraint matching for
    {{:https://semver.org/} Semantic Versioning} version strings of
    the form [MAJOR.MINOR.PATCH]. *)

(** {1 Types} *)

(** A semantic version. *)
type t = { major : int; minor : int; patch : int }

(** A comparison operator for version constraints. *)
type constraint_op = Eq | Lt | Le | Gt | Ge

(** A single version constraint: an operator paired with a version. *)
type constraint_ = constraint_op * t

(** A conjunction (AND) of version constraints.  A version must satisfy
    all constraints in the set. *)
type constraint_set = constraint_ list

(** {1 Parsing} *)

exception Parse_error of string
(** Raised when a version string cannot be parsed. *)

val parse : string -> t
(** [parse s] parses a semantic version string ["MAJOR.MINOR.PATCH"].
    @raise Parse_error if [s] is not a valid version string. *)

val parse_constraint : string -> constraint_op
(** [parse_constraint s] parses a constraint operator string.
    Valid values: ["="], ["<"], ["<="], [">"], [">="].
    @raise Parse_error if [s] is not a valid operator. *)

(** {1 Display} *)

val to_string : t -> string
(** [to_string v] returns the version as ["MAJOR.MINOR.PATCH"]. *)

(** {1 Comparison} *)

val compare : t -> t -> int
(** [compare a b] returns a negative integer if [a < b], zero if [a = b],
    and a positive integer if [a > b].  Comparison is lexicographic on
    [(major, minor, patch)]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same major, minor, and
    patch numbers. *)

(** {1 Constraint matching} *)

val satisfies : t -> constraint_set -> bool
(** [satisfies v cs] returns [true] iff [v] satisfies every constraint
    in [cs].  An empty constraint set is satisfied by any version. *)

val latest_satisfying : t list -> constraint_set -> t option
(** [latest_satisfying versions cs] returns the latest (greatest) version
    from [versions] that satisfies all constraints in [cs], or [None] if
    no version matches. *)
