(** REPL session with named FASL-based checkpoints.

    Stores named snapshots of instance state as binary checkpoint data.
    Checkpoints can be reverted to restore a previous state, and the
    entire session (all checkpoints) can be saved to or loaded from a file. *)

exception Session_error of string

(** Mutable session state. *)
type t

(** Create an empty session with no checkpoints. *)
val create : unit -> t

(** [checkpoint t name inst] takes a snapshot of [inst] and stores it
    under [name].  If a checkpoint with the same name already exists,
    it is replaced. *)
val checkpoint : t -> string -> Instance.t -> unit

(** [revert t name] restores the instance from the named checkpoint.
    @raise Session_error if the checkpoint name is not found. *)
val revert : t -> string -> Instance.t

(** Return the names of all checkpoints in creation order. *)
val list_checkpoints : t -> string list

(** Write all checkpoints to a binary file.
    @raise Session_error on I/O errors. *)
val save : t -> string -> unit

(** Read checkpoints from a binary file.
    @raise Session_error if the file does not exist or is malformed. *)
val load : string -> t
