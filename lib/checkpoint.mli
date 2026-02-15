(** FASL-based state snapshots for REPL session checkpoints.

    Serializes the essential parts of an {!Instance.t} into a compact binary
    format and restores them into a fresh instance.  Closures, hash tables,
    promises, and macro bindings survive the round-trip; continuations, ports,
    and regexps are replaced by [Void] on restore.

    The binary format extends the standard FASL encoding with additional tags
    for runtime values (primitives, closures, hash tables, etc.) and uses
    physical-identity tracking to preserve frame sharing across closures. *)

(** {1 Exceptions} *)

exception Checkpoint_error of string
(** Raised when snapshot or restore fails. *)

(** {1 Snapshot / Restore} *)

val snapshot : Instance.t -> bytes
(** [snapshot inst] serializes the instance state to binary.
    The global environment, reachable closure frames, gensym counter,
    loaded library names, and user syntax bindings are all captured.
    @raise Checkpoint_error if serialization fails. *)

val restore : bytes -> Instance.t
(** [restore data] deserializes binary into a fresh instance.
    Creates a new instance via {!Instance.create}, replaces its global
    bindings with the checkpoint data, and re-imports libraries.
    @raise Checkpoint_error if the data is malformed or incompatible. *)
