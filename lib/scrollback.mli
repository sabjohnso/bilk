(** Bounded scrollback buffer for terminal output.

    A ring buffer that stores recent terminal output up to a configured
    maximum byte count. When the buffer exceeds its limit, oldest data
    is dropped. Used by the remote REPL server to replay output on
    client reconnect. *)

type t

val create : max_bytes:int -> t
(** [create ~max_bytes] creates an empty scrollback buffer that holds
    at most [max_bytes] of output. *)

val append : t -> string -> unit
(** [append t s] adds [s] to the buffer, dropping oldest data if the
    total would exceed [max_bytes]. *)

val contents : t -> string
(** [contents t] returns all buffered output as a single string. *)

val clear : t -> unit
(** [clear t] empties the buffer. *)

val byte_count : t -> int
(** [byte_count t] returns the current number of bytes stored. *)
