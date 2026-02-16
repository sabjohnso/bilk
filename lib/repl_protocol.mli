(** Binary protocol for remote REPL communication.

    Length-prefixed frames with a 1-byte tag. Frame format:
    [\[u32 payload_length\]\[u8 tag\]\[payload...\]]

    Client and server message types are disjoint — tags 0x01-0x06
    for client messages, 0x81-0x88 for server messages. *)

(** Messages sent from client to server. *)
type client_msg =
  | Eval of string         (** Complete expression to evaluate. *)
  | Complete of string     (** Prefix to complete. *)
  | Interrupt              (** Interrupt current evaluation. *)
  | Input of string        (** Response to a read request. *)
  | Resume of string       (** Session token for resumption. *)
  | Disconnect             (** Clean disconnect. *)

(** Server readiness status. *)
type status = Ready | Busy

(** Messages sent from server to client. *)
type server_msg =
  | Output of string         (** Terminal output (prompts, print, etc). *)
  | Result of string         (** Evaluation result value. *)
  | Error of string          (** Evaluation error message. *)
  | Completions of string list  (** Completion candidates. *)
  | Read_request of string   (** Prompt for user input (read). *)
  | Status of status         (** Server readiness status. *)
  | Session_ok               (** Session resume accepted. *)
  | Session_deny             (** Session resume denied. *)

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

val max_frame_size : int
(** Maximum allowed frame payload size (16 MiB).
    Frames exceeding this are rejected with {!Protocol_error}. *)

val frame_available : string -> int -> bool
(** [frame_available data offset] returns [true] if a complete frame
    is available at [offset] in [data].
    @raise Protocol_error if the payload length exceeds {!max_frame_size}. *)
