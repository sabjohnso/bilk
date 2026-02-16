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

val run : t -> unit
(** [run t] starts the server's TCP listener and eval loop. Blocks
    until shutdown or signal. *)

(** {1 Message handling (exposed for testing)} *)

val handle_eval : t -> string -> unit
(** [handle_eval t expr] evaluates [expr] in the server's instance,
    capturing output.  Sends {!Repl_protocol.Output},
    {!Repl_protocol.Result}, or {!Repl_protocol.Error} to the
    connected client. *)

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

val session_token : t -> string
(** [session_token t] returns the server's session token.
    Exposed for testing resume verification. *)

val instance : t -> Instance.t
(** [instance t] returns the server's Scheme instance.
    Exposed for testing eval side-effects. *)
