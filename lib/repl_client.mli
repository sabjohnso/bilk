(** REPL client — thin terminal relay.

    Connects to a remote REPL server, enters raw terminal mode locally,
    and relays keystrokes to the server and terminal output back. *)

(** Client configuration. *)
type config = {
  host : string;
  (** Server hostname or IP address. *)

  port : int;
  (** Server TCP port. *)
}

val connect : config -> unit
(** [connect config] connects to the server and runs the relay loop.
    Blocks until the server sends [Server_exit] or the connection is lost.
    Enters and exits raw terminal mode. *)
