(** Debug Adapter Protocol (DAP) wire format and message types.

    Provides types and functions for reading/writing DAP messages over
    a byte stream using the Content-Length framing protocol.  Messages
    are JSON-encoded and classified as requests, responses, or events. *)

(** {1 Exceptions} *)

exception Dap_error of string
(** Raised on malformed DAP messages or wire-format errors. *)

(** {1 Types} *)

(** A DAP request from the client. *)
type request = {
  seq : int;
  (** Sequence number of the request. *)
  command : string;
  (** The command to execute. *)
  arguments : Yojson.Safe.t;
  (** Command arguments, or [`Null] if absent. *)
}

(** A DAP response to a request. *)
type response = {
  seq : int;
  (** Sequence number of the response. *)
  request_seq : int;
  (** Sequence number of the request this responds to. *)
  success : bool;
  (** Whether the request succeeded. *)
  command : string;
  (** The command that was requested. *)
  message : string option;
  (** Error message when [success] is [false]. *)
  body : Yojson.Safe.t;
  (** Response body, or [`Null] if absent. *)
}

(** A DAP event sent to the client. *)
type event = {
  seq : int;
  (** Sequence number of the event. *)
  event_name : string;
  (** The event type name. *)
  body : Yojson.Safe.t;
  (** Event body, or [`Null] if absent. *)
}

(** A parsed DAP message. *)
type message =
  | Request of request
  | Response of response
  | Event of event

(** {1 Wire format} *)

val read_message : in_channel -> message
(** [read_message ic] reads a DAP message from [ic] using
    Content-Length framing.
    @raise Dap_error on malformed framing or JSON. *)

val write_response : out_channel -> int ref -> response -> unit
(** [write_response oc seq_ref resp] writes [resp] as a DAP response to [oc],
    using [seq_ref] for the outgoing sequence number (incremented). *)

val write_event : out_channel -> int ref -> event -> unit
(** [write_event oc seq_ref evt] writes [evt] as a DAP event to [oc],
    using [seq_ref] for the outgoing sequence number (incremented). *)

(** {1 JSON helpers} *)

val request_of_json : Yojson.Safe.t -> request
(** [request_of_json json] parses a DAP request from JSON.
    @raise Dap_error if required fields are missing. *)

val response_to_json : response -> Yojson.Safe.t
(** [response_to_json resp] converts a DAP response to JSON. *)

val event_to_json : event -> Yojson.Safe.t
(** [event_to_json evt] converts a DAP event to JSON. *)

val message_of_json : Yojson.Safe.t -> message
(** [message_of_json json] dispatches on the ["type"] field.
    @raise Dap_error on unknown or missing type. *)

val make_response : int ref -> request -> bool -> Yojson.Safe.t -> response
(** [make_response seq_ref req success body] creates a response to [req],
    allocating a fresh sequence number from [seq_ref]. *)

val make_event : int ref -> string -> Yojson.Safe.t -> event
(** [make_event seq_ref name body] creates an event with a fresh
    sequence number from [seq_ref]. *)
