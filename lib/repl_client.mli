(** REPL client — smart client stub.

    Connects to a remote REPL server and relays expressions for
    evaluation. Currently a transitional implementation that wraps
    stdin reads as Eval messages; will be replaced with a full
    local REPL UI in a later phase. *)

(** Client configuration. *)
type config = {
  host : string;
  (** Server hostname or IP address. *)

  port : int;
  (** Server TCP port. *)
}

val connect : config -> unit
(** [connect config] connects to the server and runs the relay loop.
    Blocks until the server sends [Session_deny] or the connection
    is lost. *)
