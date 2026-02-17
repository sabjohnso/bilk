(** REPL client — smart client with local UI.

    Connects to a remote REPL server and runs the full line editor
    locally (paredit, highlighting, completion menu). Only evaluation
    and identifier completion require server round-trips; everything
    else is handled with zero latency. *)

(** {1 Configuration} *)

(** Client configuration. *)
type config = {
  host : string;
  (** Server hostname or IP address. *)

  port : int;
  (** Server TCP port. *)

  theme : string option;
  (** Color theme: ["dark"], ["light"], ["none"], or a file path.
      [None] uses the default dark theme. *)

  history_file : string option;
  (** Path to command history file. [None] disables persistence. *)

  paredit : bool;
  (** Enable paredit mode for structural editing. *)

  key : string option;
  (** Session key for authenticated encryption.
      [None] connects without encryption (insecure mode). *)
}

(** {1 Connection} *)

(** Opaque connection to a remote REPL server. *)
type connection

val connection_of_fd : Unix.file_descr -> connection
(** [connection_of_fd fd] wraps a file descriptor (e.g. from a
    socketpair) as a connection. Useful for testing. *)

val close_connection : connection -> unit
(** [close_connection conn] closes the underlying file descriptor.
    Silently ignores errors if already closed. *)

(** {1 Protocol operations} *)

val send_msg : connection -> Repl_protocol.client_msg -> unit
(** [send_msg conn msg] serializes and sends a client message. *)

val recv_msg : connection -> Repl_protocol.server_msg
(** [recv_msg conn] reads and deserializes one server message.
    Blocks until a complete frame is available.  Raises
    [End_of_file] if the connection is closed.
    @raise Repl_protocol.Protocol_error on malformed data. *)

val eval_remote :
  connection ->
  on_output:(string -> unit) ->
  on_read:(string -> string) ->
  string ->
  [ `Result of string | `Error of string | `Disconnected ]
(** [eval_remote conn ~on_output ~on_read expr] sends [Eval expr] and
    reads responses until [Result] or [Error]. Calls [on_output] for
    streaming [Output] messages and [on_read] for [Read_request]
    prompts. Returns [`Disconnected] on connection loss. *)

val request_completions : connection -> string -> string list
(** [request_completions conn prefix] sends [Complete prefix] and
    reads until [Completions]. Returns the candidate list, or [[]]
    on error or disconnect. *)

(** {1 Interactive client} *)

val print_client_help : unit -> unit
(** Print a formatted list of all commands (client-local and
    server-side) to stdout. *)

val authenticate : connection -> Repl_crypto.key -> unit
(** [authenticate conn key] performs the mutual authentication
    handshake with the server.  Verifies that the server knows the
    shared key, and proves knowledge of it in return.
    @raise Repl_protocol.Protocol_error if authentication fails. *)

val sanitize_read_prompt : string -> string
(** [sanitize_read_prompt raw] prepends ["[remote] "] to the prompt,
    strips control characters and ANSI escape sequences, and truncates
    to a reasonable length.  This prevents a malicious server from
    social-engineering the user via crafted [Read_request] prompts. *)

val validate_theme_name : string -> bool
(** [validate_theme_name name] returns [true] if [name] is a safe
    theme name.  Built-in names (["dark"], ["light"], ["none"], ["off"])
    are always valid.  Custom names must be non-empty and contain no
    path separators, [..], or null bytes. *)

val connect : config -> unit
(** [connect config] connects to the server and runs the full
    interactive REPL with local line editing, paredit, highlighting,
    and completion.  Client-local commands ([,quit], [,help],
    [,paredit], [,theme], [,clear]) are handled without a server
    round-trip; all other comma commands and Scheme input are sent
    to the server.  Blocks until EOF or disconnect. *)
