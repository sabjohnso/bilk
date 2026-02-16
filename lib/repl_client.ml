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
      try_decode ()
    end
  in
  try_decode ()

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
      | Repl_protocol.Session_deny -> loop ()
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
      | Repl_protocol.Session_deny -> loop ()
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
  let rt = Readtable.default in
  let theme = match config.theme with
    | None -> Some Highlight.dark_theme
    | Some name -> resolve_theme name
  in
  let paredit_ref = ref config.paredit in
  let highlight_fn text cursor =
    match theme with
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
          loop ()
        | `Error s ->
          Printf.eprintf "%s\n%!" s;
          loop ()
        | `Disconnected ->
          Printf.eprintf "Connection lost.\n%!"
      end
  in
  (try loop ()
   with _ -> ());
  cleanup ()
