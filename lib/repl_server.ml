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
  mutable inst : Instance.t;
  mutable session : Session.t;
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
let session t = t.session

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

let crypto_key t = t.crypto_key

(* --- Client communication --- *)

let accept_client t fd =
  Option.iter (fun old_fd ->
    (try Unix.close old_fd with Unix.Unix_error _ -> ())
  ) t.client_fd;
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

(* --- Authentication handshake --- *)

let send_server_msg_to_fd fd msg =
  let buf = Buffer.create 256 in
  Repl_protocol.write_server_msg buf msg;
  let data = Buffer.contents buf in
  ignore (Unix.write_substring fd data 0 (String.length data))

let read_client_msg_from_fd fd =
  let buf = Buffer.create 256 in
  let tmp = Bytes.create 4096 in
  let rec loop () =
    let data = Buffer.contents buf in
    if Repl_protocol.frame_available data 0 then begin
      let (msg, _) = Repl_protocol.read_client_msg data 0 in
      Some msg
    end else begin
      let n = try Unix.read fd tmp 0 4096
        with Unix.Unix_error _ -> 0 in
      if n = 0 then None
      else begin
        Buffer.add_subbytes buf tmp 0 n;
        loop ()
      end
    end
  in
  loop ()

let authenticate_client t =
  match t.crypto_key, t.client_fd with
  | None, _ -> true  (* insecure mode *)
  | _, None -> false  (* no client connected *)
  | Some key, Some fd ->
    (try
       (* Step 1: send challenge *)
       let server_nonce = Repl_crypto.auth_challenge () in
       send_server_msg_to_fd fd (Repl_protocol.Auth_challenge server_nonce);
       (* Step 2: read client response *)
       match read_client_msg_from_fd fd with
       | Some (Repl_protocol.Auth_response (client_hmac, client_nonce)) ->
         if Repl_crypto.verify_auth key server_nonce client_hmac then begin
           (* Step 3: prove server identity back *)
           let server_hmac = Repl_crypto.auth_response key client_nonce in
           send_server_msg_to_fd fd (Repl_protocol.Auth_ok server_hmac);
           true
         end else begin
           send_server_msg_to_fd fd Repl_protocol.Auth_deny;
           false
         end
       | _ ->
         send_server_msg_to_fd fd Repl_protocol.Auth_deny;
         false
     with Unix.Unix_error _ | Repl_protocol.Protocol_error _ -> false)

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

let graceful_shutdown t =
  auto_checkpoint_on_disconnect t;
  shutdown t

(* --- Path safety for ,load --- *)

let safe_resolve_path ~allowed path =
  let full =
    if Filename.is_relative path
    then Filename.concat (Sys.getcwd ()) path
    else path
  in
  match (try Some (Unix.realpath full)
         with Unix.Unix_error _ -> None) with
  | None -> Error (Printf.sprintf "path not found: %s" path)
  | Some resolved ->
    let within dir =
      let n = String.length dir in
      String.length resolved >= n
      && String.sub resolved 0 n = dir
      && (String.length resolved = n || resolved.[n] = '/')
    in
    if List.exists within allowed then Ok resolved
    else Error "path is outside allowed directories"

(* --- Server-side comma command dispatch --- *)

let handle_server_command t line =
  let word, arg =
    let s = String.sub line 1 (String.length line - 1) in
    match String.index_opt s ' ' with
    | Some i ->
      (String.sub s 0 i,
       String.trim (String.sub s (i + 1) (String.length s - i - 1)))
    | None -> (s, "")
  in
  match word with
  | "checkpoint" ->
    if arg = "" then
      send_to_client t (Repl_protocol.Error "Usage: ,checkpoint <name>")
    else begin
      (try
         Session.checkpoint t.session arg t.inst;
         send_to_client t
           (Repl_protocol.Output (Printf.sprintf "Checkpoint saved: %s\n" arg));
         send_to_client t (Repl_protocol.Result "")
       with
       | Checkpoint.Checkpoint_error msg ->
         send_to_client t (Repl_protocol.Error msg)
       | Session.Session_error msg ->
         send_to_client t (Repl_protocol.Error msg))
    end
  | "revert" ->
    if arg = "" then
      send_to_client t (Repl_protocol.Error "Usage: ,revert <name>")
    else begin
      (try
         t.inst <- Session.revert t.session arg;
         send_to_client t
           (Repl_protocol.Output (Printf.sprintf "Reverted to: %s\n" arg));
         send_to_client t (Repl_protocol.Result "")
       with Session.Session_error msg ->
         send_to_client t (Repl_protocol.Error msg))
    end
  | "checkpoints" ->
    let cps = Session.list_checkpoints t.session in
    if cps = [] then
      send_to_client t (Repl_protocol.Output "No checkpoints.\n")
    else begin
      let text = String.concat "\n"
          (List.map (fun name -> "  " ^ name) cps) ^ "\n" in
      send_to_client t (Repl_protocol.Output text)
    end;
    send_to_client t (Repl_protocol.Result "")
  | "save-session" ->
    if arg = "" then
      send_to_client t (Repl_protocol.Error "Usage: ,save-session <name>")
    else begin
      (try
         let home = Search_path.bilk_home () in
         Session_store.save ~home t.session arg;
         let nc = List.length (Session.list_checkpoints t.session) in
         send_to_client t
           (Repl_protocol.Output
              (Printf.sprintf "Session saved: %s (%d checkpoint%s)\n"
                 arg nc (if nc = 1 then "" else "s")));
         send_to_client t (Repl_protocol.Result "")
       with
       | Session_store.Store_error msg ->
         send_to_client t (Repl_protocol.Error msg)
       | Session.Session_error msg ->
         send_to_client t (Repl_protocol.Error msg)
       | Sys_error msg ->
         send_to_client t (Repl_protocol.Error msg))
    end
  | "load-session" ->
    if arg = "" then
      send_to_client t (Repl_protocol.Error "Usage: ,load-session <name>")
    else begin
      (try
         let home = Search_path.bilk_home () in
         let loaded = Session_store.load ~home arg in
         let cps = Session.list_checkpoints loaded in
         let nc = List.length cps in
         t.session <- loaded;
         (match List.rev cps with
          | latest :: _ ->
            t.inst <- Session.revert t.session latest
          | [] -> ());
         send_to_client t
           (Repl_protocol.Output
              (Printf.sprintf "Session loaded: %s (%d checkpoint%s)\n"
                 arg nc (if nc = 1 then "" else "s")));
         send_to_client t (Repl_protocol.Result "")
       with
       | Session_store.Store_error msg ->
         send_to_client t (Repl_protocol.Error msg)
       | Session.Session_error msg ->
         send_to_client t (Repl_protocol.Error msg)
       | Sys_error msg ->
         send_to_client t (Repl_protocol.Error msg))
    end
  | "sessions" ->
    let home = Search_path.bilk_home () in
    let names = Session_store.list ~home () in
    if names = [] then
      send_to_client t (Repl_protocol.Output "No saved sessions.\n")
    else begin
      let text = String.concat "\n"
          (List.map (fun name -> "  " ^ name) names) ^ "\n" in
      send_to_client t (Repl_protocol.Output text)
    end;
    send_to_client t (Repl_protocol.Result "")
  | "reload" ->
    if arg = "" then
      send_to_client t (Repl_protocol.Error "Usage: ,reload (library name)")
    else begin
      (try
         let port = Port.of_string arg in
         let syntax = Reader.read_syntax t.inst.Instance.readtable port in
         let lib_name = Library.parse_library_name syntax in
         Instance.reload_library t.inst lib_name;
         send_to_client t
           (Repl_protocol.Output
              (Printf.sprintf "Reloaded %s\n" (Library.name_to_string lib_name)));
         send_to_client t (Repl_protocol.Result "")
       with
       | Reader.Read_error (_, msg) ->
         send_to_client t (Repl_protocol.Error msg)
       | Compiler.Compile_error (_, msg) ->
         send_to_client t (Repl_protocol.Error msg)
       | Vm.Runtime_error msg ->
         send_to_client t (Repl_protocol.Error msg)
       | Failure msg ->
         send_to_client t (Repl_protocol.Error msg))
    end
  | "exports" ->
    if arg = "" then
      send_to_client t (Repl_protocol.Error "Usage: ,exports (library name)")
    else begin
      (try
         let port = Port.of_string arg in
         let syntax = Reader.read_syntax t.inst.Instance.readtable port in
         let lib_name = Library.parse_library_name syntax in
         match Instance.ensure_library t.inst lib_name with
         | None ->
           send_to_client t
             (Repl_protocol.Error
                (Printf.sprintf "Library not found: %s"
                   (Library.name_to_string lib_name)))
         | Some lib ->
           let (rt, syn) = Library.export_names lib in
           let buf = Buffer.create 256 in
           let rt_sorted = List.sort String.compare rt in
           let syn_sorted = List.sort String.compare syn in
           if syn_sorted <> [] then begin
             Buffer.add_string buf "Syntax:\n";
             List.iter (fun name ->
               Buffer.add_string buf (Printf.sprintf "  %s\n" name)
             ) syn_sorted
           end;
           if rt_sorted <> [] then begin
             Buffer.add_string buf "Runtime:\n";
             List.iter (fun name ->
               Buffer.add_string buf (Printf.sprintf "  %s\n" name)
             ) rt_sorted
           end;
           send_to_client t (Repl_protocol.Output (Buffer.contents buf));
           send_to_client t (Repl_protocol.Result "")
       with
       | Reader.Read_error (_, msg) ->
         send_to_client t (Repl_protocol.Error msg)
       | Compiler.Compile_error (_, msg) ->
         send_to_client t (Repl_protocol.Error msg)
       | Failure msg ->
         send_to_client t (Repl_protocol.Error msg))
    end
  | "deps" ->
    if arg = "" then
      send_to_client t (Repl_protocol.Error "Usage: ,deps (library name)")
    else begin
      (try
         let port = Port.of_string arg in
         let syntax = Reader.read_syntax t.inst.Instance.readtable port in
         let lib_name = Library.parse_library_name syntax in
         let builtins = Build.builtin_library_names t.inst in
         let nodes = Dep_graph.build_graph ~builtins
             ~search_paths:!(t.inst.Instance.search_paths)
             t.inst.readtable [lib_name] in
         send_to_client t
           (Repl_protocol.Output (Dep_graph.format_tree nodes lib_name));
         send_to_client t (Repl_protocol.Result "")
       with
       | Reader.Read_error (_, msg) ->
         send_to_client t (Repl_protocol.Error msg)
       | Compiler.Compile_error (_, msg) ->
         send_to_client t (Repl_protocol.Error msg)
       | Dep_graph.Resolve_error (name, msg) ->
         send_to_client t
           (Repl_protocol.Error
              (Printf.sprintf "resolving %s: %s"
                 (Library.name_to_string name) msg))
       | Failure msg ->
         send_to_client t (Repl_protocol.Error msg))
    end
  | "load" ->
    if arg = "" then
      send_to_client t (Repl_protocol.Error "Usage: ,load <file>")
    else begin
      let allowed =
        [Sys.getcwd ()] @ !(t.inst.Instance.search_paths) in
      let allowed_real = List.filter_map (fun d ->
        try Some (Unix.realpath d)
        with Unix.Unix_error _ -> None
      ) allowed in
      match safe_resolve_path ~allowed:allowed_real arg with
      | Error msg ->
        send_to_client t (Repl_protocol.Error msg)
      | Ok resolved ->
        (try
           let port = Port.of_file resolved in
           ignore (Instance.eval_port t.inst port);
           send_to_client t
             (Repl_protocol.Output (Printf.sprintf "Loaded %s\n" arg));
           send_to_client t (Repl_protocol.Result "")
         with
         | Reader.Read_error (_, msg) ->
           send_to_client t (Repl_protocol.Error msg)
         | Compiler.Compile_error (_, msg) ->
           send_to_client t (Repl_protocol.Error msg)
         | Vm.Runtime_error msg ->
           send_to_client t (Repl_protocol.Error msg)
         | Fasl.Fasl_error msg ->
           send_to_client t (Repl_protocol.Error msg)
         | Failure msg ->
           send_to_client t (Repl_protocol.Error msg)
         | Sys_error msg ->
           send_to_client t (Repl_protocol.Error msg))
    end
  | "env" ->
    let inst = t.inst in
    let syms = Symbol.all inst.Instance.symbols in
    let bound = List.filter_map (fun sym ->
      match Env.lookup inst.Instance.global_env sym with
      | Some _ -> Some (Symbol.name sym)
      | None -> None
    ) syms in
    let sorted = List.sort String.compare bound in
    let text = String.concat " " sorted ^ "\n" in
    send_to_client t (Repl_protocol.Output text);
    send_to_client t (Repl_protocol.Result "")
  | "libs" ->
    let libs = Library.list_all t.inst.Instance.libraries in
    let names = List.map
        (fun (l : Library.t) -> Library.name_to_string l.name) libs in
    let sorted = List.sort String.compare names in
    let text = String.concat "\n" sorted ^ "\n" in
    send_to_client t (Repl_protocol.Output text);
    send_to_client t (Repl_protocol.Result "")
  | "help" ->
    let text =
      "Server commands:\n\
       \  ,checkpoint <name>   Save a named checkpoint\n\
       \  ,revert <name>       Revert to a checkpoint\n\
       \  ,checkpoints         List all checkpoints\n\
       \  ,save-session <name> Save session by name\n\
       \  ,load-session <name> Load session by name\n\
       \  ,sessions            List saved sessions\n\
       \  ,env                 List global bindings\n\
       \  ,libs                List loaded libraries\n\
       \  ,exports <lib>       List library exports\n\
       \  ,deps <lib>          Show library dependencies\n\
       \  ,reload <lib>        Reload a library\n\
       \  ,load <file>         Load a Scheme file\n\
       Client commands:\n\
       \  ,quit                Disconnect and exit\n\
       \  ,help                Show this help\n\
       \  ,paredit             Toggle paredit mode\n\
       \  ,theme <name>        Switch color theme\n\
       \  ,clear               Clear screen\n"
    in
    send_to_client t (Repl_protocol.Output text);
    send_to_client t (Repl_protocol.Result "")
  | _ ->
    send_to_client t
      (Repl_protocol.Error
         (Printf.sprintf "Unknown command: ,%s" word))

(* --- Eval input/output bounds --- *)

let max_eval_input = 1_048_576     (* 1 MiB *)
let max_eval_output = 16_777_216   (* 16 MiB *)

let truncate_output s =
  if String.length s > max_eval_output then
    String.sub s 0 max_eval_output ^ "\n[output truncated]"
  else s

(* --- Eval handler --- *)

let handle_eval t expr =
  if String.length expr > max_eval_input then
    send_to_client t (Repl_protocol.Error "input too large")
  else
  let trimmed = String.trim expr in
  if String.length trimmed > 0 && trimmed.[0] = ',' then begin
    handle_server_command t trimmed;
  end else begin
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
  (* Send captured output (truncated if over limit) *)
  let out_text = truncate_output (Port.get_output_string capture_out) in
  let err_text = truncate_output (Port.get_output_string capture_err) in
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
      | v -> truncate_output (Datum.to_string v)
    in
    send_to_client t (Repl_protocol.Result result_str)
  end

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
  | Repl_protocol.Auth_response _ ->
    () (* Auth_response outside handshake is ignored *)

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
  let handle_signal _ =
    t.alive <- false
  in
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
           let auth_ok = authenticate_client t in
           if not auth_ok then
             disconnect_client t
           else
             last_pong := Unix.gettimeofday ()
         end else if Some fd = t.client_fd then begin
           let n = try Unix.read fd read_buf 0 4096
             with Unix.Unix_error _ -> 0
           in
           if n = 0 then
             disconnect_client t
           else begin
             Buffer.add_subbytes t.recv_buf read_buf 0 n;
             if Buffer.length t.recv_buf > Repl_protocol.max_frame_size + 4 then
               disconnect_client t
             else begin
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
         end
       ) ready;
       check_keepalive t last_ping last_pong
     done
   with e ->
     (try Unix.close server_sock with Unix.Unix_error _ -> ());
     raise e);
  (* Deferred from signal handler — checkpoint in normal code flow *)
  auto_checkpoint_on_disconnect t;
  (try Unix.close server_sock with Unix.Unix_error _ -> ())
