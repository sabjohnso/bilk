(** REPL server — eval-only engine for remote clients.

    Maintains an {!Instance.t} (the Scheme environment) and handles
    smart-client protocol messages: evaluate expressions, provide
    completions, handle interrupts, and manage session resumption.
    The server owns the Scheme state; all UI is handled by the client. *)

(** {1 Configuration} *)

(** Server configuration. *)
type config = {
  port : int;
  (** TCP port to listen on. 0 for tests (no TCP). *)

  auto_checkpoint : bool;
  (** If true, checkpoint session state on client disconnect. *)

  name : string;
  (** Session name for display and auto-checkpoint naming. *)

  bind_address : Unix.inet_addr;
  (** Network address to bind the listener to.
      Default [Unix.inet_addr_loopback] for security. *)

  session_timeout : int;
  (** Session timeout in seconds.  Stored for future use. *)

  insecure : bool;
  (** If [true], skip encryption.  Only allowed with loopback bind. *)
}

(** {1 Server lifecycle} *)

(** Opaque server state. *)
type t

val create : config -> t
(** [create config] creates a new REPL server with a fresh Scheme
    instance.  Does not start listening. *)

val accept_client : t -> Unix.file_descr -> unit
(** [accept_client t fd] registers a connected client socket.
    Used for testing with socketpairs. *)

val is_alive : t -> bool
(** [is_alive t] returns whether the server is running. *)

val shutdown : t -> unit
(** [shutdown t] stops the server and cleans up resources. *)

val graceful_shutdown : t -> unit
(** [graceful_shutdown t] performs an auto-checkpoint (if enabled)
    and then shuts down the server. *)

val run : t -> unit
(** [run t] starts the server's TCP listener and eval loop. Blocks
    until shutdown or signal. *)

(** {1 Message handling (exposed for testing)} *)

val handle_eval : t -> string -> unit
(** [handle_eval t expr] evaluates [expr] in the server's instance,
    capturing output.  If [expr] starts with [,], dispatches as a
    server-side comma command instead.  Sends {!Repl_protocol.Output},
    {!Repl_protocol.Result}, or {!Repl_protocol.Error} to the
    connected client. *)

val handle_server_command : t -> string -> unit
(** [handle_server_command t line] dispatches a comma command
    (e.g. [",checkpoint foo"]) using the server's session and instance.
    Sends output via {!Repl_protocol.Output}, {!Repl_protocol.Result},
    or {!Repl_protocol.Error}. *)

val handle_complete : t -> string -> unit
(** [handle_complete t prefix] gathers completion candidates from the
    instance's symbol table and syntax environment, filters by [prefix],
    and sends {!Repl_protocol.Completions} to the client. *)

val handle_interrupt : t -> unit
(** [handle_interrupt t] sets the interrupt flag.  The next procedure
    call in the VM will raise an error, aborting the current eval. *)

val handle_resume : t -> string -> unit
(** [handle_resume t token] verifies [token] against the server's
    session token and sends {!Repl_protocol.Session_ok} or
    {!Repl_protocol.Session_deny}. *)

val handle_client_msg : t -> Repl_protocol.client_msg -> unit
(** [handle_client_msg t msg] dispatches a client protocol message.
    Exposed for testing with socketpairs. *)

val session_token : t -> string
(** [session_token t] returns the server's session token.
    Exposed for testing resume verification. *)

val instance : t -> Instance.t
(** [instance t] returns the server's Scheme instance.
    Exposed for testing eval side-effects. *)

val session : t -> Session.t
(** [session t] returns the server's session state.
    Exposed for testing checkpoint operations. *)

val connect_line : t -> string
(** [connect_line t] returns the [BILK CONNECT <port> <key>] line
    suitable for parsing by {!Ssh_connect.parse_connect_line}.
    If {!config.insecure} is [true], the key is ["insecure"]. *)

val validate_config : config -> (unit, string) result
(** [validate_config config] checks that the configuration is valid.
    Rejects [insecure=true] with a non-loopback bind address. *)

(** {1 Authentication} *)

val authenticate_client : t -> bool
(** [authenticate_client t] runs the mutual authentication handshake
    with the connected client.  Returns [true] if the client proved
    knowledge of the shared key.  In insecure mode, returns [true]
    immediately without sending any messages. *)

val crypto_key : t -> Repl_crypto.key option
(** [crypto_key t] returns the server's encryption key, or [None] in
    insecure mode.  Exposed for testing the handshake. *)
