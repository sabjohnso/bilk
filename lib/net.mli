(** Networking operations for TCP, UDP, and DNS.

    TCP connections produce standard {!Port.t} values so existing I/O
    procedures work transparently.  Listeners and UDP sockets are wrapped
    in {!Datum.Opaque} values. *)

(** {1 TCP} *)

val tcp_connect : string -> int -> Port.t * Port.t
(** [tcp_connect host port] connects to [host:port] via TCP and returns
    [(input_port, output_port)].
    Raises [Failure] on connection failure. *)

val tcp_listen : int -> int -> Datum.t
(** [tcp_listen port backlog] creates a TCP listener on the given [port]
    with the specified [backlog].  Pass [0] for an ephemeral port.
    Returns an {!Datum.Opaque} wrapping a [tcp_listener]. *)

val tcp_accept : Datum.t -> Port.t * Port.t
(** [tcp_accept listener] accepts a connection from [listener] and returns
    [(input_port, output_port)].
    Raises [Failure] if [listener] is not a tcp-listener or is closed. *)

val tcp_listener_port : Datum.t -> int
(** [tcp_listener_port listener] returns the port number [listener] is
    bound to. *)

val tcp_close : Datum.t -> unit
(** [tcp_close listener] closes the listener socket.  Closing an
    already-closed listener is a no-op. *)

val is_tcp_listener : Datum.t -> bool
(** [is_tcp_listener v] is [true] if [v] is a tcp-listener opaque value. *)

(** {1 UDP} *)

val udp_open : unit -> Datum.t
(** [udp_open ()] creates a UDP socket.  Returns an {!Datum.Opaque}
    wrapping a [udp_socket]. *)

val udp_bind : Datum.t -> string -> int -> unit
(** [udp_bind socket address port] binds the UDP socket to
    [address:port]. *)

val udp_send_to : Datum.t -> bytes -> string -> int -> int
(** [udp_send_to socket data address port] sends [data] to
    [address:port] and returns the number of bytes sent. *)

val udp_receive : Datum.t -> int -> bytes * string * int
(** [udp_receive socket max_len] receives up to [max_len] bytes.
    Returns [(data, sender_address, sender_port)]. *)

val udp_close : Datum.t -> unit
(** [udp_close socket] closes the UDP socket.  Closing an already-closed
    socket is a no-op. *)

val is_udp_socket : Datum.t -> bool
(** [is_udp_socket v] is [true] if [v] is a udp-socket opaque value. *)

val udp_local_address : Datum.t -> string * int
(** [udp_local_address socket] returns the local [(address, port)] the
    UDP socket is bound to. *)

(** {1 DNS} *)

val net_resolve : string -> string
(** [net_resolve hostname] resolves [hostname] and returns the first
    address as a string.
    Raises [Failure] if resolution fails. *)

val net_resolve_all : string -> string list
(** [net_resolve_all hostname] resolves [hostname] and returns all unique
    addresses as strings. *)
