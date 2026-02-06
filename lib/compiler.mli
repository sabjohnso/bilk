(** Compiler from syntax trees to bytecode.

    Compiles {!Syntax.t} values into {!Datum.code} objects that can be
    executed by the VM.  The compiler takes a {!Symbol.table} (not
    {!Instance.t}) to avoid circular dependencies.

    Supports: self-evaluating literals, symbols, [quote], [if], [lambda],
    [define], [set!], [begin], and procedure calls.  Tracks tail position
    for proper tail calls. *)

(** {1 Exceptions} *)

exception Compile_error of Loc.t * string
(** Raised when the compiler encounters malformed syntax.  Carries the
    source location and a human-readable message. *)

(** {1 Compilation} *)

val compile : Symbol.table -> Syntax.t -> Datum.code
(** [compile symbols expr] compiles top-level expression [expr] to bytecode.
    Uses [symbols] to intern identifiers.
    @raise Compile_error on malformed syntax. *)
