(** REPL server for remote connections.

    Manages an Instance.t, line editor, scrollback buffer, and TCP
    connections. The server evaluates expressions from smart clients
    and sends back results, errors, and output. *)

(** Server configuration. *)
type config = {
  port : int;
  (** TCP port to listen on. 0 for tests (no TCP). *)

  scrollback_size : int;
  (** Maximum bytes of scrollback to keep for reconnect replay. *)

  auto_checkpoint : bool;
  (** If true, checkpoint session state on client disconnect. *)

  name : string;
  (** Session name for display and auto-checkpoint naming. *)
}

(** Opaque server state. *)
type t

val create : config -> t
(** [create config] creates a new REPL server without starting it. *)

val accept_client : t -> Unix.file_descr -> unit
(** [accept_client t fd] registers a connected client socket.
    Used for testing with socketpairs. *)

val append_output : t -> string -> unit
(** [append_output t s] adds [s] to the scrollback buffer. *)

val get_scrollback : t -> string
(** [get_scrollback t] returns the current scrollback contents. *)

val clear_scrollback : t -> unit
(** [clear_scrollback t] empties the scrollback buffer. *)

val is_alive : t -> bool
(** [is_alive t] returns whether the server is running. *)

val shutdown : t -> unit
(** [shutdown t] stops the server and cleans up resources. *)

val run : t -> unit
(** [run t] starts the server's TCP listener and eval loop. Blocks. *)
