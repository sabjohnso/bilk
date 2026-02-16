(* --- Configuration --- *)

type config = {
  port : int;
  auto_checkpoint : bool;
  name : string;
  bind_address : Unix.inet_addr;
  session_timeout : int;
  insecure : bool;
}

(* --- State --- *)

type t = {
  config : config;
  inst : Instance.t;
  session : Session.t;
  token : string;
  crypto_key : Repl_crypto.key option;
  interrupt : bool ref;
  mutable client_fd : Unix.file_descr option;
  mutable alive : bool;
  recv_buf : Buffer.t;
}

(* --- Token generation --- *)

let generate_token () =
  Mirage_crypto_rng_unix.use_default ();
  let raw = Mirage_crypto_rng.generate 16 in
  let buf = Buffer.create 32 in
  String.iter (fun c ->
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))
  ) raw;
  Buffer.contents buf

(* --- Constructor --- *)

let create config =
  let crypto_key =
    if config.insecure then None
    else Some (Repl_crypto.generate_key ())
  in
  { config;
    inst = Instance.create ();
    session = Session.create ();
    token = generate_token ();
    crypto_key;
    interrupt = ref false;
    client_fd = None;
    alive = true;
    recv_buf = Buffer.create 4096;
  }

(* --- Accessors --- *)

let is_alive t = t.alive
let session_token t = t.token
let instance t = t.inst

(* --- Connect line and validation --- *)

let connect_line t =
  let key_str = match t.crypto_key with
    | None -> "insecure"
    | Some k -> Repl_crypto.key_to_base64 k
  in
  Printf.sprintf "BILK CONNECT %d %s" t.config.port key_str

let is_loopback addr =
  let s = Unix.string_of_inet_addr addr in
  s = "127.0.0.1" || s = "::1"

let validate_config config =
  if config.insecure && not (is_loopback config.bind_address) then
    Error "insecure mode requires loopback bind address"
  else
    Ok ()

(* --- Client communication --- *)

let accept_client t fd =
  t.client_fd <- Some fd

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

(* --- Auto-checkpoint on disconnect --- *)

let auto_checkpoint_on_disconnect t =
  if t.config.auto_checkpoint then begin
    let name = Printf.sprintf "%s-disconnect-%d"
        t.config.name (int_of_float (Unix.gettimeofday ())) in
    (try Session.checkpoint t.session name t.inst
     with Session.Session_error _ -> ())
  end

let disconnect_client t =
  Option.iter (fun fd ->
    (try Unix.close fd with Unix.Unix_error _ -> ())
  ) t.client_fd;
  auto_checkpoint_on_disconnect t;
  t.client_fd <- None

(* --- Eval handler --- *)

let handle_eval t expr =
  let inst = t.inst in
  (* Save original ports *)
  let saved_output = !(inst.current_output) in
  let saved_error = !(inst.current_error) in
  (* Redirect to capture ports *)
  let capture_out = Port.open_output_string () in
  let capture_err = Port.open_output_string () in
  inst.current_output := capture_out;
  inst.current_error := capture_err;
  (* Install interrupt check via on_call *)
  let saved_on_call = !(inst.on_call) in
  t.interrupt := false;
  inst.on_call := Some (fun _loc _proc _args ->
    if !(t.interrupt) then begin
      t.interrupt := false;
      raise (Vm.Runtime_error "interrupted")
    end);
  (* Evaluate all expressions in the string *)
  let port = Port.of_string expr in
  let last_result = ref Datum.Void in
  let error_msg = ref None in
  let rec eval_loop () =
    try
      let syntax = Reader.read_syntax inst.readtable port in
      match syntax with
      | { Syntax.datum = Syntax.Eof; _ } -> ()
      | _ ->
        begin try
          last_result := Instance.eval_syntax inst syntax;
          Port.flush capture_out;
          Port.flush capture_err
        with
        | Reader.Read_error (_loc, msg) -> error_msg := Some msg
        | Compiler.Compile_error (_loc, msg) -> error_msg := Some msg
        | Vm.Runtime_error msg -> error_msg := Some msg
        | Fasl.Fasl_error msg -> error_msg := Some msg
        | Failure msg -> error_msg := Some msg
        end;
        if !error_msg = None then eval_loop ()
    with
    | Reader.Read_error (_loc, msg) -> error_msg := Some msg
  in
  eval_loop ();
  (* Restore ports and on_call *)
  inst.current_output := saved_output;
  inst.current_error := saved_error;
  inst.on_call := saved_on_call;
  (* Send captured output *)
  let out_text = Port.get_output_string capture_out in
  let err_text = Port.get_output_string capture_err in
  if out_text <> "" then
    send_to_client t (Repl_protocol.Output out_text);
  if err_text <> "" then
    send_to_client t (Repl_protocol.Output err_text);
  (* Send result or error *)
  match !error_msg with
  | Some msg ->
    send_to_client t (Repl_protocol.Error msg)
  | None ->
    let result_str = match !last_result with
      | Datum.Void -> ""
      | v -> Datum.to_string v
    in
    send_to_client t (Repl_protocol.Result result_str)

(* --- Complete handler --- *)

let handle_complete t prefix =
  let inst = t.inst in
  let syms = Symbol.all inst.Instance.symbols in
  let bound = List.filter_map (fun sym ->
    match Env.lookup inst.Instance.global_env sym with
    | Some _ -> Some (Symbol.name sym)
    | None -> None
  ) syms in
  let syntax_names = Expander.binding_names inst.Instance.syn_env in
  let all = bound @ syntax_names in
  let candidates = List.sort_uniq String.compare all in
  let matches = Completion.find_matches prefix candidates in
  send_to_client t (Repl_protocol.Completions matches)

(* --- Interrupt handler --- *)

let handle_interrupt t =
  t.interrupt := true

(* --- Resume handler --- *)

let handle_resume t token =
  if Eqaf.equal token t.token then
    send_to_client t Repl_protocol.Session_ok
  else
    send_to_client t Repl_protocol.Session_deny

(* --- Message dispatch --- *)

let handle_client_msg t msg =
  match msg with
  | Repl_protocol.Disconnect ->
    disconnect_client t
  | Repl_protocol.Eval s ->
    handle_eval t s
  | Repl_protocol.Complete s ->
    handle_complete t s
  | Repl_protocol.Interrupt ->
    handle_interrupt t
  | Repl_protocol.Input _s ->
    () (* Input forwarding deferred to a later phase *)
  | Repl_protocol.Resume s ->
    handle_resume t s

(* --- Keepalive --- *)

let ping_interval = 30.0
let pong_timeout = 60.0

let check_keepalive t last_ping last_pong =
  let now = Unix.gettimeofday () in
  if t.client_fd <> None && now -. !last_ping > ping_interval then begin
    send_to_client t (Repl_protocol.Status Repl_protocol.Ready);
    last_ping := now
  end;
  if t.client_fd <> None && !last_pong > 0.0
     && now -. !last_pong > pong_timeout then
    disconnect_client t

(* --- Main server loop --- *)

let run t =
  let server_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server_sock Unix.SO_REUSEADDR true;
  Unix.bind server_sock (Unix.ADDR_INET (t.config.bind_address, t.config.port));
  Unix.listen server_sock 1;
  let handle_signal _ = t.alive <- false in
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_signal);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_signal);
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let read_buf = Bytes.create 4096 in
  let last_ping = ref 0.0 in
  let last_pong = ref 0.0 in
  (try
     while t.alive do
       let fds_to_watch =
         match t.client_fd with
         | Some fd -> [fd]
         | None -> [server_sock]
       in
       let (ready, _, _) =
         try Unix.select fds_to_watch [] [] 1.0
         with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
       in
       List.iter (fun fd ->
         if fd == server_sock then begin
           let (client_fd, _addr) = Unix.accept server_sock in
           accept_client t client_fd;
           last_pong := Unix.gettimeofday ()
         end else if Some fd = t.client_fd then begin
           let n = try Unix.read fd read_buf 0 4096
             with Unix.Unix_error _ -> 0
           in
           if n = 0 then
             disconnect_client t
           else begin
             Buffer.add_subbytes t.recv_buf read_buf 0 n;
             let data = Buffer.contents t.recv_buf in
             let off = ref 0 in
             (try
                while Repl_protocol.frame_available data !off do
                  let (msg, next) = Repl_protocol.read_client_msg data !off in
                  handle_client_msg t msg;
                  off := next
                done
              with Repl_protocol.Protocol_error _ ->
                disconnect_client t);
             let remaining = String.length data - !off in
             Buffer.clear t.recv_buf;
             if remaining > 0 then
               Buffer.add_string t.recv_buf
                 (String.sub data !off remaining)
           end
         end
       ) ready;
       check_keepalive t last_ping last_pong
     done
   with e ->
     (try Unix.close server_sock with Unix.Unix_error _ -> ());
     raise e);
  (try Unix.close server_sock with Unix.Unix_error _ -> ())
