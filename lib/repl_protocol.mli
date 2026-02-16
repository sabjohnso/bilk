(** Binary protocol for remote REPL communication.

    Length-prefixed frames with a 1-byte tag. Frame format:
    [\[u32 payload_length\]\[u8 tag\]\[payload...\]]

    Client and server message types are disjoint — tags 0x01-0x04
    for client messages, 0x81-0x84 for server messages. *)

(** Messages sent from client to server. *)
type client_msg =
  | Key_input of string   (** Raw keystroke bytes. *)
  | Resize of int * int   (** Terminal rows, cols. *)
  | Ping                  (** Keep-alive. *)
  | Disconnect            (** Clean disconnect. *)

(** Messages sent from server to client. *)
type server_msg =
  | Output of string      (** Raw ANSI terminal output. *)
  | Pong                  (** Keep-alive response. *)
  | Scrollback of string  (** Full replay on reconnect. *)
  | Server_exit           (** Server shutting down. *)

(** {1 Serialization} *)

val write_client_msg : Buffer.t -> client_msg -> unit
(** Write a client message as a framed packet into [buf]. *)

val write_server_msg : Buffer.t -> server_msg -> unit
(** Write a server message as a framed packet into [buf]. *)

(** {1 Deserialization} *)

exception Protocol_error of string

val read_client_msg : string -> int -> client_msg * int
(** [read_client_msg data offset] decodes one client message starting
    at [offset]. Returns [(msg, next_offset)].
    @raise Protocol_error on malformed data.
    @raise Protocol_error if not enough data. *)

val read_server_msg : string -> int -> server_msg * int
(** [read_server_msg data offset] decodes one server message starting
    at [offset]. Returns [(msg, next_offset)].
    @raise Protocol_error on malformed data. *)

val frame_available : string -> int -> bool
(** [frame_available data offset] returns [true] if a complete frame
    is available at [offset] in [data]. *)
