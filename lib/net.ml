(* --- Opaque type names --- *)

let tcp_listener_type = "tcp-listener"
let udp_socket_type = "udp-socket"

(* --- Internal types --- *)

type tcp_listener = {
  fd : Unix.file_descr;
  mutable closed : bool;
  bound_port : int;
}

type udp_socket = {
  fd : Unix.file_descr;
  mutable closed : bool;
}

(* --- Opaque wrapping/unwrapping --- *)

let wrap_tcp_listener l =
  Datum.Opaque {
    opaque_type_name = tcp_listener_type;
    opaque_data = Obj.repr l;
    opaque_open = true;
  }

let unwrap_tcp_listener context = function
  | Datum.Opaque o when o.opaque_type_name = tcp_listener_type ->
    (Obj.obj o.opaque_data : tcp_listener)
  | _ -> failwith (context ^ ": expected tcp-listener")

let wrap_udp_socket s =
  Datum.Opaque {
    opaque_type_name = udp_socket_type;
    opaque_data = Obj.repr s;
    opaque_open = true;
  }

let unwrap_udp_socket context = function
  | Datum.Opaque o when o.opaque_type_name = udp_socket_type ->
    (Obj.obj o.opaque_data : udp_socket)
  | _ -> failwith (context ^ ": expected udp-socket")

(* --- Address helpers --- *)

let string_of_sockaddr = function
  | Unix.ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr
  | Unix.ADDR_UNIX s -> s

let port_of_sockaddr = function
  | Unix.ADDR_INET (_, p) -> p
  | Unix.ADDR_UNIX _ -> 0

(* --- TCP --- *)

let tcp_connect host port =
  let addrs = Unix.getaddrinfo host (string_of_int port)
      [Unix.AI_SOCKTYPE Unix.SOCK_STREAM] in
  match addrs with
  | [] -> failwith (Printf.sprintf "tcp-connect: cannot resolve %s:%d" host port)
  | ai :: _ ->
    let fd = Unix.socket ai.ai_family ai.ai_socktype ai.ai_protocol in
    (try Unix.connect fd ai.ai_addr with
     | Unix.Unix_error (code, _, _) ->
       Unix.close fd;
       failwith (Printf.sprintf "tcp-connect: %s: %s:%d"
                   (Unix.error_message code) host port));
    let fd_in = Unix.dup fd in
    let label = Printf.sprintf "<tcp:%s:%d>" host port in
    let in_chan = Unix.in_channel_of_descr fd_in in
    let out_chan = Unix.out_channel_of_descr fd in
    let inp = Port.of_in_channel ~file:label in_chan in
    let outp = Port.of_out_channel ~file:label out_chan in
    (inp, outp)

let tcp_listen port backlog =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  (try
     Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
     Unix.listen fd backlog
   with Unix.Unix_error (code, _, _) ->
     Unix.close fd;
     failwith (Printf.sprintf "tcp-listen: %s: port %d"
                 (Unix.error_message code) port));
  let actual_port = port_of_sockaddr (Unix.getsockname fd) in
  wrap_tcp_listener { fd; closed = false; bound_port = actual_port }

let tcp_accept listener =
  let l = unwrap_tcp_listener "tcp-accept" listener in
  if l.closed then failwith "tcp-accept: listener is closed";
  let client_fd, client_addr = Unix.accept l.fd in
  let label = Printf.sprintf "<tcp:%s:%d>"
      (string_of_sockaddr client_addr) (port_of_sockaddr client_addr) in
  let fd_in = Unix.dup client_fd in
  let in_chan = Unix.in_channel_of_descr fd_in in
  let out_chan = Unix.out_channel_of_descr client_fd in
  let inp = Port.of_in_channel ~file:label in_chan in
  let outp = Port.of_out_channel ~file:label out_chan in
  (inp, outp)

let tcp_listener_port listener =
  let l = unwrap_tcp_listener "tcp-listener-port" listener in
  l.bound_port

let tcp_close listener =
  let l = unwrap_tcp_listener "tcp-close" listener in
  if not l.closed then begin
    l.closed <- true;
    (match listener with
     | Datum.Opaque o -> o.opaque_open <- false
     | _ -> ());
    Unix.close l.fd
  end

let is_tcp_listener = function
  | Datum.Opaque o -> o.opaque_type_name = tcp_listener_type
  | _ -> false

(* --- UDP --- *)

let udp_open () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  wrap_udp_socket { fd; closed = false }

let udp_bind sock address port =
  let s = unwrap_udp_socket "udp-bind!" sock in
  if s.closed then failwith "udp-bind!: socket is closed";
  let addr = Unix.inet_addr_of_string address in
  Unix.bind s.fd (Unix.ADDR_INET (addr, port))

let udp_send_to sock data address port =
  let s = unwrap_udp_socket "udp-send-to" sock in
  if s.closed then failwith "udp-send-to: socket is closed";
  let addr = Unix.inet_addr_of_string address in
  let dest = Unix.ADDR_INET (addr, port) in
  Unix.sendto s.fd data 0 (Bytes.length data) [] dest

let udp_receive sock max_len =
  let s = unwrap_udp_socket "udp-receive" sock in
  if s.closed then failwith "udp-receive: socket is closed";
  let buf = Bytes.create max_len in
  let n, sender = Unix.recvfrom s.fd buf 0 max_len [] in
  let data = Bytes.sub buf 0 n in
  let addr = string_of_sockaddr sender in
  let port = port_of_sockaddr sender in
  (data, addr, port)

let udp_close sock =
  let s = unwrap_udp_socket "udp-close" sock in
  if not s.closed then begin
    s.closed <- true;
    (match sock with
     | Datum.Opaque o -> o.opaque_open <- false
     | _ -> ());
    Unix.close s.fd
  end

let is_udp_socket = function
  | Datum.Opaque o -> o.opaque_type_name = udp_socket_type
  | _ -> false

let udp_local_address sock =
  let s = unwrap_udp_socket "udp-local-address" sock in
  let sa = Unix.getsockname s.fd in
  (string_of_sockaddr sa, port_of_sockaddr sa)

(* --- DNS --- *)

let net_resolve hostname =
  let addrs = Unix.getaddrinfo hostname ""
      [Unix.AI_SOCKTYPE Unix.SOCK_STREAM] in
  match addrs with
  | [] -> failwith (Printf.sprintf "net-resolve: cannot resolve %s" hostname)
  | ai :: _ ->
    string_of_sockaddr ai.ai_addr

let net_resolve_all hostname =
  let addrs = Unix.getaddrinfo hostname ""
      [Unix.AI_SOCKTYPE Unix.SOCK_STREAM] in
  let seen = Hashtbl.create 8 in
  List.filter_map (fun ai ->
    let s = string_of_sockaddr ai.Unix.ai_addr in
    if Hashtbl.mem seen s then None
    else begin Hashtbl.replace seen s (); Some s end
  ) addrs
