type config = {
  port : int;
  scrollback_size : int;
  auto_checkpoint : bool;
  name : string;
}

type t = {
  config : config;
  scrollback : Scrollback.t;
  mutable client_fd : Unix.file_descr option;
  mutable alive : bool;
  mutable inst : Instance.t option;
  mutable session : Session.t option;
  mutable last_ping : float;
  mutable last_pong : float;
}

let create config =
  { config;
    scrollback = Scrollback.create ~max_bytes:config.scrollback_size;
    client_fd = None;
    alive = true;
    inst = None;
    session = None;
    last_ping = 0.0;
    last_pong = 0.0;
  }

let accept_client t fd =
  t.client_fd <- Some fd;
  t.last_pong <- Unix.gettimeofday ()

let append_output t s =
  Scrollback.append t.scrollback s

let get_scrollback t =
  Scrollback.contents t.scrollback

let clear_scrollback t =
  Scrollback.clear t.scrollback

let is_alive t = t.alive

let send_to_client t msg =
  match t.client_fd with
  | None -> ()
  | Some fd ->
    let buf = Buffer.create 256 in
    Repl_protocol.write_server_msg buf msg;
    let data = Buffer.contents buf in
    (try
       ignore (Unix.write_substring fd data 0 (String.length data))
     with Unix.Unix_error _ ->
       t.client_fd <- None)

let shutdown t =
  t.alive <- false;
  Option.iter (fun fd ->
    (try Unix.close fd with Unix.Unix_error _ -> ())
  ) t.client_fd;
  t.client_fd <- None

let relay_output t s =
  append_output t s;
  send_to_client t (Repl_protocol.Output s)

let auto_checkpoint_on_disconnect t =
  if t.config.auto_checkpoint then begin
    match t.inst, t.session with
    | Some inst, Some session ->
      let name = Printf.sprintf "%s-disconnect-%d"
          t.config.name (int_of_float (Unix.gettimeofday ())) in
      (try Session.checkpoint session name inst
       with Session.Session_error _ -> ())
    | _ -> ()
  end

let disconnect_client t =
  Option.iter (fun fd ->
    (try Unix.close fd with Unix.Unix_error _ -> ())
  ) t.client_fd;
  auto_checkpoint_on_disconnect t;
  t.client_fd <- None

let handle_client_msg t msg =
  match msg with
  | Repl_protocol.Disconnect ->
    disconnect_client t
  | Repl_protocol.Eval _s ->
    ()
  | Repl_protocol.Complete _s ->
    ()
  | Repl_protocol.Interrupt ->
    ()
  | Repl_protocol.Input _s ->
    ()
  | Repl_protocol.Resume _s ->
    ()

let ping_interval = 30.0
let pong_timeout = 60.0

let check_keepalive t =
  let now = Unix.gettimeofday () in
  if t.client_fd <> None && now -. t.last_ping > ping_interval then begin
    send_to_client t (Repl_protocol.Status Repl_protocol.Ready);
    t.last_ping <- now
  end;
  if t.client_fd <> None && t.last_pong > 0.0
     && now -. t.last_pong > pong_timeout then begin
    disconnect_client t
  end

let run t =
  let server_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server_sock Unix.SO_REUSEADDR true;
  Unix.bind server_sock (Unix.ADDR_INET (Unix.inet_addr_any, t.config.port));
  Unix.listen server_sock 1;
  let inst = Instance.create () in
  let session = Session.create () in
  t.inst <- Some inst;
  t.session <- Some session;
  let handle_signal _ =
    t.alive <- false
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_signal);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_signal);
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (pipe_read, pipe_write) = Unix.pipe () in
  let read_buf = Bytes.create 4096 in
  (try
     while t.alive do
       let fds_to_watch =
         match t.client_fd with
         | Some fd -> [fd; pipe_read]
         | None -> [server_sock]
       in
       let (ready, _, _) =
         try Unix.select fds_to_watch [] [] 1.0
         with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
       in
       List.iter (fun fd ->
         if fd == server_sock then begin
           let (client_fd, _addr) = Unix.accept server_sock in
           t.client_fd <- Some client_fd;
           t.last_pong <- Unix.gettimeofday ();
           let sb = get_scrollback t in
           if sb <> "" then
             send_to_client t (Repl_protocol.Output sb)
         end else if Some fd = t.client_fd then begin
           let n = try Unix.read fd read_buf 0 4096
             with Unix.Unix_error _ -> 0
           in
           if n = 0 then
             disconnect_client t
           else begin
             let data = Bytes.sub_string read_buf 0 n in
             let off = ref 0 in
             (try
                while Repl_protocol.frame_available data !off do
                  let (msg, next) = Repl_protocol.read_client_msg data !off in
                  handle_client_msg t msg;
                  (match msg with
                   | Repl_protocol.Eval s ->
                     (try
                        ignore (Unix.write_substring pipe_write s 0
                                  (String.length s))
                      with Unix.Unix_error _ -> ())
                   | _ -> ());
                  off := next
                done
              with Repl_protocol.Protocol_error _ ->
                disconnect_client t)
           end
         end else if fd == pipe_read then begin
           let n = try Unix.read fd read_buf 0 4096
             with Unix.Unix_error _ -> 0
           in
           if n > 0 then begin
             let s = Bytes.sub_string read_buf 0 n in
             relay_output t s
           end
         end
       ) ready;
       check_keepalive t
     done
   with e ->
     (try Unix.close pipe_read with Unix.Unix_error _ -> ());
     (try Unix.close pipe_write with Unix.Unix_error _ -> ());
     (try Unix.close server_sock with Unix.Unix_error _ -> ());
     raise e);
  (try Unix.close pipe_read with Unix.Unix_error _ -> ());
  (try Unix.close pipe_write with Unix.Unix_error _ -> ());
  (try Unix.close server_sock with Unix.Unix_error _ -> ())
