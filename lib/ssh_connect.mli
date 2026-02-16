(** SSH connect-line parser for [bilk connect].

    Parses the [BILK CONNECT <port> <base64-key>] line emitted by
    [bilk serve] on stdout, and validates [user@host] targets.
    All functions are pure — no I/O or process management. *)

(** {1 Types} *)

(** Information extracted from a [BILK CONNECT] line. *)
type connect_info = {
  port : int;
  (** TCP port the server is listening on. *)

  key : string;
  (** Base64-encoded session key (or ["insecure"]). *)
}

(** Errors that can occur when scanning SSH output. *)
type connect_error =
  | No_bilk_on_remote of string
      (** No [BILK CONNECT] line found — bilk may not be installed. *)
  | Ssh_failed of string
      (** SSH process produced an error. *)
  | Invalid_connect_line of string
      (** A [BILK CONNECT] line was found but could not be parsed. *)

(** {1 Parsers} *)

val parse_connect_line : string -> connect_info option
(** [parse_connect_line s] parses a line of the form
    ["BILK CONNECT <port> <key>"].  Returns [None] if the line
    does not match the expected format. *)

val parse_target : string -> (string * string) option
(** [parse_target s] parses ["user@host"] into [(user, host)].
    Returns [None] if [s] does not contain exactly one ['@']. *)

val scan_for_connect : string list -> (connect_info, connect_error) result
(** [scan_for_connect lines] scans a list of SSH stdout lines for
    the first valid [BILK CONNECT] line.  Returns [Error] if none
    is found or if the line is malformed. *)
