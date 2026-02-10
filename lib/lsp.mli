(** LSP — Language Server Protocol JSON-RPC 2.0 wire format.

    Provides types and functions for reading/writing LSP messages over
    a byte stream using the Content-Length framing protocol (same wire
    format as DAP, different JSON envelope).  Uses {!Yojson.Safe}. *)

(** {1 Exceptions} *)

exception Lsp_error of string
(** Raised on malformed LSP messages or wire-format errors. *)

(** {1 Types} *)

(** A JSON-RPC request (has [id]). *)
type request = {
  id : Yojson.Safe.t;
  (** Request identifier (int or string per JSON-RPC 2.0). *)
  method_ : string;
  (** The method name. *)
  params : Yojson.Safe.t;
  (** Method parameters, or [`Null] if absent. *)
}

(** A JSON-RPC notification (no [id]). *)
type notification = {
  method_ : string;
  (** The method name. *)
  params : Yojson.Safe.t;
  (** Method parameters, or [`Null] if absent. *)
}

(** A JSON-RPC response. *)
type response = {
  id : Yojson.Safe.t;
  (** The request identifier this responds to. *)
  result : Yojson.Safe.t;
  (** Result value, or [`Null] on error. *)
  error : response_error option;
  (** Error object, or [None] on success. *)
}

(** A JSON-RPC error object. *)
and response_error = {
  code : int;
  (** Error code. *)
  message : string;
  (** Error message. *)
  data : Yojson.Safe.t;
  (** Additional error data, or [`Null]. *)
}

(** A parsed LSP message. *)
type message =
  | Request of request
  | Notification of notification
  | Response of response

(** {1 Wire format} *)

val read_message : in_channel -> message
(** [read_message ic] reads an LSP message from [ic] using
    Content-Length framing.
    @raise Lsp_error on malformed framing or JSON. *)

val write_response : out_channel -> response -> unit
(** [write_response oc resp] writes [resp] as an LSP response to [oc]. *)

val write_notification : out_channel -> string -> Yojson.Safe.t -> unit
(** [write_notification oc method_ params] writes a JSON-RPC notification
    to [oc] with the given method name and parameters. *)

(** {1 Response construction} *)

val make_response : Yojson.Safe.t -> Yojson.Safe.t -> response
(** [make_response id result] creates a successful response. *)

val make_error_response : Yojson.Safe.t -> int -> string -> response
(** [make_error_response id code message] creates an error response. *)

(** {1 JSON conversion} *)

val message_of_json : Yojson.Safe.t -> message
(** [message_of_json json] dispatches on presence of [id] and [method]
    fields to classify as Request, Notification, or Response.
    @raise Lsp_error on missing fields or non-object input. *)

val response_to_json : response -> Yojson.Safe.t
(** [response_to_json resp] converts a response to JSON. *)

(** {1 JSON helpers} *)

val get_string : string -> (string * Yojson.Safe.t) list -> string
(** [get_string key assoc] extracts a string field from a JSON association
    list.
    @raise Lsp_error if the field is missing or not a string. *)

val get_int : string -> (string * Yojson.Safe.t) list -> int
(** [get_int key assoc] extracts an integer field from a JSON association
    list.
    @raise Lsp_error if the field is missing or not an integer. *)

val get_opt : string -> (string * Yojson.Safe.t) list -> Yojson.Safe.t
(** [get_opt key assoc] extracts a field from a JSON association list,
    returning [`Null] if absent. *)

(** {1 Standard error codes} *)

val method_not_found : int
(** JSON-RPC error code for unknown methods (-32601). *)

val internal_error : int
(** JSON-RPC error code for internal errors (-32603). *)
