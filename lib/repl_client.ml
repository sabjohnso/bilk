(* --- Configuration --- *)

type config = {
  host : string;
  port : int;
  theme : string option;
  history_file : string option;
  paredit : bool;
  key : string option;
}

(* --- Connection --- *)

type connection = {
  fd : Unix.file_descr;
  recv_buf : Buffer.t;
}

let connection_of_fd fd =
  { fd; recv_buf = Buffer.create 4096 }

let close_connection conn =
  (try Unix.close conn.fd with Unix.Unix_error _ -> ())

(* --- Low-level protocol I/O --- *)

let send_msg conn msg =
  let buf = Buffer.create 256 in
  Repl_protocol.write_client_msg buf msg;
  let data = Buffer.contents buf in
  let _ = Unix.write_substring conn.fd data 0 (String.length data) in
  ()

let recv_msg conn =
  (* Try to decode a complete frame from the buffer, reading more if needed *)
  let rec try_decode () =
    let data = Buffer.contents conn.recv_buf in
    if Repl_protocol.frame_available data 0 then begin
      let (msg, next) = Repl_protocol.read_server_msg data 0 in
      let remaining = String.length data - next in
      Buffer.clear conn.recv_buf;
      if remaining > 0 then
        Buffer.add_string conn.recv_buf (String.sub data next remaining);
      msg
    end else begin
      let tmp = Bytes.create 4096 in
      let n = Unix.read conn.fd tmp 0 4096 in
      if n = 0 then raise End_of_file;
      Buffer.add_subbytes conn.recv_buf tmp 0 n;
      if Buffer.length conn.recv_buf > Repl_protocol.max_frame_size + 4 then
        raise (Repl_protocol.Protocol_error "recv buffer overflow");
      try_decode ()
    end
  in
  try_decode ()

(* --- Authentication handshake --- *)

let authenticate conn key =
  (* Step 1: read Auth_challenge from server *)
  let msg = recv_msg conn in
  let server_nonce = match msg with
    | Repl_protocol.Auth_challenge n -> n
    | _ -> raise (Repl_protocol.Protocol_error "expected Auth_challenge")
  in
  (* Step 2: prove we know the key *)
  let client_hmac = Repl_crypto.auth_response key server_nonce in
  let client_nonce = Repl_crypto.auth_challenge () in
  let buf = Buffer.create 256 in
  Repl_protocol.write_client_msg buf
    (Repl_protocol.Auth_response (client_hmac, client_nonce));
  let data = Buffer.contents buf in
  let _ = Unix.write_substring conn.fd data 0 (String.length data) in
  (* Step 3: verify server's proof *)
  let resp = recv_msg conn in
  match resp with
  | Repl_protocol.Auth_ok server_hmac ->
    if not (Repl_crypto.verify_auth key client_nonce server_hmac) then
      raise (Repl_protocol.Protocol_error "server authentication failed")
  | Repl_protocol.Auth_deny ->
    raise (Repl_protocol.Protocol_error "authentication rejected by server")
  | _ ->
    raise (Repl_protocol.Protocol_error "unexpected message during auth")

(* --- eval_remote --- *)

let eval_remote conn ~on_output ~on_read expr =
  try
    send_msg conn (Repl_protocol.Eval expr);
    let rec loop () =
      let msg = recv_msg conn in
      match msg with
      | Repl_protocol.Result s -> `Result s
      | Repl_protocol.Error s -> `Error s
      | Repl_protocol.Output s -> on_output s; loop ()
      | Repl_protocol.Read_request prompt ->
        let response = on_read prompt in
        send_msg conn (Repl_protocol.Input response);
        loop ()
      | Repl_protocol.Status _
      | Repl_protocol.Completions _
      | Repl_protocol.Session_ok
      | Repl_protocol.Session_deny
      | Repl_protocol.Auth_challenge _
      | Repl_protocol.Auth_ok _
      | Repl_protocol.Auth_deny -> loop ()
    in
    loop ()
  with
  | End_of_file | Unix.Unix_error _ | Repl_protocol.Protocol_error _ ->
    `Disconnected

(* --- request_completions --- *)

let request_completions conn prefix =
  try
    send_msg conn (Repl_protocol.Complete prefix);
    let rec loop () =
      let msg = recv_msg conn in
      match msg with
      | Repl_protocol.Completions lst -> lst
      | Repl_protocol.Output _
      | Repl_protocol.Status _
      | Repl_protocol.Session_ok
      | Repl_protocol.Session_deny
      | Repl_protocol.Auth_challenge _
      | Repl_protocol.Auth_ok _
      | Repl_protocol.Auth_deny -> loop ()
      | Repl_protocol.Result _
      | Repl_protocol.Error _
      | Repl_protocol.Read_request _ -> []
    in
    loop ()
  with
  | End_of_file | Unix.Unix_error _ | Repl_protocol.Protocol_error _ -> []

(* --- Local completeness check (no server needed) --- *)

let is_unterminated msg =
  let prefix = "unterminated" in
  let len = String.length prefix in
  String.length msg >= len && String.sub msg 0 len = prefix

let is_complete_local rt text =
  let port = Port.of_string text in
  let rec check () =
    try
      match Reader.read_syntax rt port with
      | { Syntax.datum = Syntax.Eof; _ } -> true
      | _ -> check ()
    with Reader.Read_error (_, msg) ->
      if is_unterminated msg then false else true
  in
  check ()

(* --- Theme resolution --- *)

let resolve_theme name =
  match name with
  | "dark" -> Some Highlight.dark_theme
  | "light" -> Some Highlight.light_theme
  | "none" | "off" -> None
  | path ->
    if Sys.file_exists path then
      Some (Highlight.load_theme path)
    else begin
      Printf.eprintf "Theme not found: %s\n%!" path;
      None
    end

(* --- Completion logic (local path + remote identifiers) --- *)

let apply_matches ~width:_ text cursor start matches =
  match matches with
  | [] -> Line_editor.No_completions
  | _ ->
    let cp = Completion.common_prefix matches in
    let before = String.sub text 0 start in
    let after = String.sub text cursor (String.length text - cursor) in
    Line_editor.Menu {
      text = before ^ cp ^ after;
      cursor = start + String.length cp;
      candidates = matches;
      start;
    }

let make_complete_fn conn rt =
  fun text cursor ~width ->
    match Completion_context.detect rt text cursor with
    | Completion_context.No_context ->
      Line_editor.No_completions
    | Completion_context.String_literal (content, start) ->
      let matches = Completion.complete_path content in
      apply_matches ~width text cursor start matches
    | Completion_context.Repl_command_arg (cmd, arg, start) ->
      let matches = match cmd with
        | ",load" | ",save-session" | ",load-session" ->
          Completion.complete_path arg
        | ",theme" ->
          Completion.find_matches arg ["dark"; "light"; "none"]
        | _ -> []
      in
      apply_matches ~width text cursor start matches
    | Completion_context.Import_library (_parts, prefix, start) ->
      let candidates = request_completions conn prefix in
      apply_matches ~width text cursor start candidates
    | Completion_context.Identifier (prefix, start) ->
      let candidates = request_completions conn prefix in
      apply_matches ~width text cursor start candidates

(* --- Client-local help --- *)

let print_client_help () =
  print_string
    "Client commands (handled locally):\n\
     \  ,quit ,q       Disconnect and exit\n\
     \  ,help ,h       Show this help\n\
     \  ,paredit       Toggle paredit mode\n\
     \  ,theme <name>  Switch color theme (dark, light, none, or path)\n\
     \  ,clear         Clear screen\n\
     \n\
     Server commands (executed remotely):\n\
     \  ,checkpoint <name>    Save a named checkpoint\n\
     \  ,revert <name>        Revert to a checkpoint\n\
     \  ,checkpoints          List all checkpoints\n\
     \  ,save-session <file>  Save session to file\n\
     \  ,load-session <file>  Load session from file\n\
     \  ,env                  List global bindings\n\
     \  ,libs                 List loaded libraries\n\
     \  ,exports <lib>        List library exports\n\
     \  ,deps <lib>           Show library dependencies\n\
     \  ,reload <lib>         Reload a library\n\
     \  ,load <file>          Load a Scheme file\n";
  flush stdout

(* --- Full interactive client --- *)

let connect config =
  let addr = Unix.inet_addr_of_string
      (try config.host
       with _ ->
         let entry = Unix.gethostbyname config.host in
         Unix.string_of_inet_addr entry.Unix.h_addr_list.(0))
  in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (try
     Unix.connect sock (Unix.ADDR_INET (addr, config.port))
   with e ->
     Unix.close sock;
     raise e);
  let conn = connection_of_fd sock in
  (match config.key with
   | Some key_str ->
     (match Repl_crypto.key_of_base64 key_str with
      | Some key -> authenticate conn key
      | None ->
        close_connection conn;
        failwith "invalid key")
   | None -> ());
  let rt = Readtable.default in
  let theme = match config.theme with
    | None -> Some Highlight.dark_theme
    | Some name -> resolve_theme name
  in
  let paredit_ref = ref config.paredit in
  let theme_ref = ref theme in
  let highlight_fn text cursor =
    match !theme_ref with
    | None -> text
    | Some t -> Highlight.highlight_line t rt text cursor
  in
  let complete_fn = make_complete_fn conn rt in
  let editor = Line_editor.create {
    prompt = "\x1b[1mbilk>\x1b[0m ";
    continuation_prompt = "  ... ";
    history_file = config.history_file;
    max_history = 1000;
    is_complete = Some (fun text -> is_complete_local rt text);
    highlight = Some highlight_fn;
    paredit = Some paredit_ref;
    readtable = Some rt;
    complete = Some complete_fn;
    on_idle = None;
  } in
  (* Install SIGPIPE handler to avoid crashes on broken connections *)
  let prev_sigpipe = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  let cleanup () =
    Sys.set_signal Sys.sigpipe prev_sigpipe;
    Line_editor.destroy editor;
    close_connection conn
  in
  let on_output s =
    let _ = Unix.write_substring Unix.stdout s 0 (String.length s) in
    ()
  in
  let on_read _prompt =
    (* Simple line read for (read) requests *)
    let buf = Buffer.create 64 in
    let rec read_line () =
      let c = Bytes.create 1 in
      let n = try Unix.read Unix.stdin c 0 1 with Unix.Unix_error _ -> 0 in
      if n = 0 then Buffer.contents buf
      else begin
        let ch = Bytes.get c 0 in
        if ch = '\n' then Buffer.contents buf
        else begin Buffer.add_char buf ch; read_line () end
      end
    in
    read_line ()
  in
  let send_to_server trimmed =
    (* Install SIGINT handler to send Interrupt during eval *)
    let prev_sigint = Sys.signal Sys.sigint
        (Sys.Signal_handle (fun _ ->
           (try send_msg conn Repl_protocol.Interrupt with _ -> ())))
    in
    let result = eval_remote conn ~on_output ~on_read trimmed in
    Sys.set_signal Sys.sigint prev_sigint;
    match result with
    | `Result s ->
      if s <> "" then print_endline s;
      true
    | `Error s ->
      Printf.eprintf "%s\n%!" s;
      true
    | `Disconnected ->
      Printf.eprintf "Connection lost.\n%!";
      false
  in
  let rec loop () =
    match Line_editor.read_input editor with
    | Line_editor.Interrupted ->
      print_endline "Interrupted.";
      loop ()
    | Line_editor.Eof ->
      (try send_msg conn (Repl_protocol.Disconnect) with _ -> ())
    | Line_editor.Input input ->
      let trimmed = String.trim input in
      if trimmed = "" then
        loop ()
      else begin
        Line_editor.history_add editor input;
        match Repl_command.classify trimmed with
        | Repl_command.Client_local Quit ->
          (try send_msg conn (Repl_protocol.Disconnect) with _ -> ())
        | Repl_command.Client_local Help ->
          print_client_help ();
          loop ()
        | Repl_command.Client_local Paredit ->
          paredit_ref := not !paredit_ref;
          if !paredit_ref then
            Printf.printf "Paredit mode enabled.\n%!"
          else
            Printf.printf "Paredit mode disabled.\n%!";
          loop ()
        | Repl_command.Client_local (Theme name) ->
          let new_theme = resolve_theme name in
          theme_ref := new_theme;
          (match new_theme with
           | Some t -> Printf.printf "Switched to theme: %s\n%!" t.Highlight.name
           | None -> Printf.printf "Highlighting disabled.\n%!");
          loop ()
        | Repl_command.Client_local Clear ->
          Printf.printf "\x1b[2J\x1b[H%!";
          loop ()
        | Repl_command.Server_side | Repl_command.Scheme_input ->
          if send_to_server trimmed then loop ()
      end
  in
  (try loop ()
   with _ -> ());
  cleanup ()
