open Bilk

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Helper: read all available server messages from fd *)
let read_server_msgs fd =
  let tmp = Bytes.create 8192 in
  let n = Unix.read fd tmp 0 8192 in
  if n = 0 then []
  else begin
    let data = Bytes.sub_string tmp 0 n in
    let msgs = ref [] in
    let off = ref 0 in
    while Repl_protocol.frame_available data !off do
      let (msg, next) = Repl_protocol.read_server_msg data !off in
      msgs := msg :: !msgs;
      off := next
    done;
    List.rev !msgs
  end

(* Helper: create a server with a socketpair client *)
let make_server () =
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  (server, client_fd, server_fd)

(* --- Eval: simple result --- *)

let test_eval_result () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(+ 1 2)";
  let msgs = read_server_msgs client_fd in
  (* Should contain a Result "3" *)
  let has_result = List.exists (function
    | Repl_protocol.Result s -> s = "3"
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "result is 3" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Eval: error --- *)

let test_eval_error () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(error \"boom\")";
  let msgs = read_server_msgs client_fd in
  let has_error = List.exists (function
    | Repl_protocol.Error s -> String.length s > 0
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "error returned" true has_error;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Eval: captures display output --- *)

let test_eval_captures_output () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(display \"hello\")";
  let msgs = read_server_msgs client_fd in
  let output = List.filter_map (function
    | Repl_protocol.Output s -> Some s
    | _ -> None
  ) msgs in
  let combined = String.concat "" output in
  Alcotest.(check bool) "output contains hello" true
    (String.length combined > 0 && String.sub combined 0 5 = "hello");
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Eval: void result sends empty string --- *)

let test_eval_void_result () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(define x 42)";
  let msgs = read_server_msgs client_fd in
  let has_result = List.exists (function
    | Repl_protocol.Result _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "result sent" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Eval: defines persist across evals --- *)

let test_eval_persists_state () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(define x 42)";
  (* Drain the first batch *)
  ignore (read_server_msgs client_fd);
  Repl_server.handle_eval server "x";
  let msgs = read_server_msgs client_fd in
  let has_result = List.exists (function
    | Repl_protocol.Result s -> s = "42"
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "x is 42" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Complete: returns matching symbols --- *)

let test_complete_matches () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_complete server "car";
  let msgs = read_server_msgs client_fd in
  let completions = List.filter_map (function
    | Repl_protocol.Completions lst -> Some lst
    | _ -> None
  ) msgs in
  Alcotest.(check bool) "got completions" true (completions <> []);
  let lst = List.hd completions in
  let has_car = List.mem "car" lst in
  Alcotest.(check bool) "car in completions" true has_car;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Complete: empty prefix returns all --- *)

let test_complete_empty_prefix () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_complete server "";
  let msgs = read_server_msgs client_fd in
  let completions = List.filter_map (function
    | Repl_protocol.Completions lst -> Some lst
    | _ -> None
  ) msgs in
  Alcotest.(check bool) "got completions" true (completions <> []);
  let lst = List.hd completions in
  (* Empty prefix should return many candidates *)
  Alcotest.(check bool) "many candidates" true (List.length lst > 10);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Complete: no matches --- *)

let test_complete_no_matches () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_complete server "zzzzz-nonexistent";
  let msgs = read_server_msgs client_fd in
  let completions = List.filter_map (function
    | Repl_protocol.Completions lst -> Some lst
    | _ -> None
  ) msgs in
  Alcotest.(check bool) "got completions" true (completions <> []);
  let lst = List.hd completions in
  Alcotest.(check (list string)) "empty list" [] lst;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Resume: correct token --- *)

let test_resume_ok () =
  let (server, client_fd, _server_fd) = make_server () in
  let token = Repl_server.session_token server in
  Repl_server.handle_resume server token;
  let msgs = read_server_msgs client_fd in
  let has_ok = List.exists (function
    | Repl_protocol.Session_ok -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "session ok" true has_ok;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Resume: wrong token --- *)

let test_resume_deny () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_resume server "wrong-token";
  let msgs = read_server_msgs client_fd in
  let has_deny = List.exists (function
    | Repl_protocol.Session_deny -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "session deny" true has_deny;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Disconnect keeps server alive --- *)

let test_disconnect_keeps_alive () =
  let (server, client_fd, _server_fd) = make_server () in
  Unix.close client_fd;
  Alcotest.(check bool) "still alive" true (Repl_server.is_alive server);
  Repl_server.shutdown server

(* --- Interrupt sets flag --- *)

let test_interrupt_flag () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_interrupt server;
  Alcotest.(check bool) "still alive after interrupt" true
    (Repl_server.is_alive server);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Instance accessor --- *)

let test_instance_accessor () =
  let (server, client_fd, _server_fd) = make_server () in
  let inst = Repl_server.instance server in
  let syms = Symbol.all inst.Instance.symbols in
  Alcotest.(check bool) "has symbols" true (List.length syms > 0);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Session token is non-empty --- *)

let test_session_token_nonempty () =
  let (server, client_fd, _server_fd) = make_server () in
  let token = Repl_server.session_token server in
  Alcotest.(check bool) "non-empty token" true (String.length token > 0);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- connect_line: non-insecure format --- *)

let test_connect_line_format () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = false;
  } in
  let server = Repl_server.create config in
  let line = Repl_server.connect_line server in
  (* Should start with "BILK CONNECT 7890 " *)
  Alcotest.(check bool) "starts with prefix" true
    (String.length line > 18 &&
     String.sub line 0 18 = "BILK CONNECT 7890 ");
  (* Key should not be "insecure" *)
  let parts = String.split_on_char ' ' line in
  let key = List.nth parts 3 in
  Alcotest.(check bool) "key not insecure" true (key <> "insecure");
  Repl_server.shutdown server

(* --- connect_line: insecure --- *)

let test_connect_line_insecure () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let line = Repl_server.connect_line server in
  Alcotest.(check string) "insecure connect line"
    "BILK CONNECT 7890 insecure" line;
  Repl_server.shutdown server

(* --- connect_line: parseable roundtrip --- *)

let test_connect_line_parseable () =
  let config = {
    Repl_server.port = 4567;
    auto_checkpoint = false;
    name = "rt";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = false;
  } in
  let server = Repl_server.create config in
  let line = Repl_server.connect_line server in
  let parsed = Ssh_connect.parse_connect_line line in
  (match parsed with
   | Some info ->
     Alcotest.(check int) "port roundtrips" 4567 info.port;
     Alcotest.(check bool) "key non-empty" true
       (String.length info.key > 0)
   | None ->
     Alcotest.fail "connect_line should be parseable");
  Repl_server.shutdown server

(* --- validate_config --- *)

let test_validate_insecure_loopback_ok () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  Alcotest.(check bool) "loopback insecure ok" true
    (Repl_server.validate_config config = Ok ())

let test_validate_insecure_any_rejected () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_any;
    session_timeout = 86400;
    insecure = true;
  } in
  Alcotest.(check bool) "any insecure rejected" true
    (match Repl_server.validate_config config with
     | Error _ -> true
     | Ok () -> false)

let test_validate_secure_any_ok () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_any;
    session_timeout = 86400;
    insecure = false;
  } in
  Alcotest.(check bool) "any secure ok" true
    (Repl_server.validate_config config = Ok ())

let test_validate_insecure_specific_ip_rejected () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_of_string "192.168.1.1";
    session_timeout = 86400;
    insecure = true;
  } in
  Alcotest.(check bool) "specific ip insecure rejected" true
    (match Repl_server.validate_config config with
     | Error _ -> true
     | Ok () -> false)

(* --- Graceful shutdown: auto-checkpoint when enabled --- *)

let test_graceful_shutdown_auto_checkpoint () =
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = true;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  Repl_server.graceful_shutdown server;
  let cps = Session.list_checkpoints (Repl_server.session server) in
  Alcotest.(check bool) "has auto-checkpoint" true (List.length cps > 0);
  (try Unix.close client_fd with Unix.Unix_error _ -> ())

(* --- Graceful shutdown: no checkpoint when disabled --- *)

let test_graceful_shutdown_no_checkpoint () =
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  Repl_server.graceful_shutdown server;
  let cps = Session.list_checkpoints (Repl_server.session server) in
  Alcotest.(check (list string)) "no checkpoints" [] cps;
  (try Unix.close client_fd with Unix.Unix_error _ -> ())

(* --- Server commands: ,checkpoint creates checkpoint --- *)

let test_server_cmd_checkpoint () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",checkpoint foo";
  let msgs = read_server_msgs client_fd in
  let has_output = List.exists (function
    | Repl_protocol.Output s -> String.length s > 0
    | Repl_protocol.Result _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "got response" true has_output;
  (* No Error message *)
  let has_error = List.exists (function
    | Repl_protocol.Error _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "no error" false has_error;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: checkpoint + revert restores state --- *)

let test_server_cmd_checkpoint_revert () =
  let (server, client_fd, _server_fd) = make_server () in
  (* Define x=1 *)
  Repl_server.handle_eval server "(define x 1)";
  ignore (read_server_msgs client_fd);
  (* Checkpoint *)
  Repl_server.handle_eval server ",checkpoint snap";
  ignore (read_server_msgs client_fd);
  (* Define x=2 *)
  Repl_server.handle_eval server "(define x 2)";
  ignore (read_server_msgs client_fd);
  (* Verify x=2 *)
  Repl_server.handle_eval server "x";
  let msgs = read_server_msgs client_fd in
  let has_2 = List.exists (function
    | Repl_protocol.Result "2" -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "x is 2" true has_2;
  (* Revert *)
  Repl_server.handle_eval server ",revert snap";
  ignore (read_server_msgs client_fd);
  (* Verify x=1 *)
  Repl_server.handle_eval server "x";
  let msgs2 = read_server_msgs client_fd in
  let has_1 = List.exists (function
    | Repl_protocol.Result "1" -> true
    | _ -> false
  ) msgs2 in
  Alcotest.(check bool) "x is 1 after revert" true has_1;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: ,checkpoints lists names --- *)

let test_server_cmd_checkpoints () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",checkpoint alpha";
  ignore (read_server_msgs client_fd);
  Repl_server.handle_eval server ",checkpoints";
  let msgs = read_server_msgs client_fd in
  let output = List.filter_map (function
    | Repl_protocol.Output s -> Some s
    | _ -> None
  ) msgs in
  let combined = String.concat "" output in
  Alcotest.(check bool) "lists alpha" true
    (String.length combined > 0
     && try let _ = String.split_on_char '\n' combined
            |> List.exists (fun l -> String.trim l = "alpha")
        in true with _ -> false);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: ,env produces output --- *)

let test_server_cmd_env () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",env";
  let msgs = read_server_msgs client_fd in
  let output = List.filter_map (function
    | Repl_protocol.Output s -> Some s
    | _ -> None
  ) msgs in
  let combined = String.concat "" output in
  Alcotest.(check bool) "env has car" true
    (let contains s sub =
       let slen = String.length s and sublen = String.length sub in
       let rec check i = i + sublen <= slen
         && (String.sub s i sublen = sub || check (i + 1)) in
       check 0
     in contains combined "car");
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: ,libs produces output --- *)

let test_server_cmd_libs () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",libs";
  let msgs = read_server_msgs client_fd in
  let has_output = List.exists (function
    | Repl_protocol.Output _ -> true
    | Repl_protocol.Result _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "got output" true has_output;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: ,help produces output --- *)

let test_server_cmd_help () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",help";
  let msgs = read_server_msgs client_fd in
  let output = List.filter_map (function
    | Repl_protocol.Output s -> Some s
    | _ -> None
  ) msgs in
  let combined = String.concat "" output in
  Alcotest.(check bool) "help has content" true (String.length combined > 0);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: unknown command → Error --- *)

let test_server_cmd_unknown () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",xyzzy-nonsense";
  let msgs = read_server_msgs client_fd in
  let has_error = List.exists (function
    | Repl_protocol.Error s ->
      (let slen = String.length s and sublen = 7 in
       let rec check i = i + sublen <= slen
         && (String.sub s i sublen = "Unknown" || check (i + 1)) in
       check 0)
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "unknown error" true has_error;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: regular (+ 1 2) still works --- *)

let test_server_cmd_scheme_passthrough () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(+ 1 2)";
  let msgs = read_server_msgs client_fd in
  let has_result = List.exists (function
    | Repl_protocol.Result "3" -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "result is 3" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

let () =
  Alcotest.run "Repl_server"
    [ ("eval",
       [ Alcotest.test_case "simple result" `Quick test_eval_result
       ; Alcotest.test_case "error" `Quick test_eval_error
       ; Alcotest.test_case "captures output" `Quick test_eval_captures_output
       ; Alcotest.test_case "void result" `Quick test_eval_void_result
       ; Alcotest.test_case "persists state" `Quick test_eval_persists_state
       ])
    ; ("complete",
       [ Alcotest.test_case "matches" `Quick test_complete_matches
       ; Alcotest.test_case "empty prefix" `Quick test_complete_empty_prefix
       ; Alcotest.test_case "no matches" `Quick test_complete_no_matches
       ])
    ; ("session",
       [ Alcotest.test_case "resume ok" `Quick test_resume_ok
       ; Alcotest.test_case "resume deny" `Quick test_resume_deny
       ; Alcotest.test_case "token non-empty" `Quick test_session_token_nonempty
       ])
    ; ("lifecycle",
       [ Alcotest.test_case "disconnect keeps alive" `Quick
           test_disconnect_keeps_alive
       ; Alcotest.test_case "interrupt flag" `Quick test_interrupt_flag
       ; Alcotest.test_case "instance accessor" `Quick test_instance_accessor
       ])
    ; ("connect_line",
       [ Alcotest.test_case "format" `Quick test_connect_line_format
       ; Alcotest.test_case "insecure" `Quick test_connect_line_insecure
       ; Alcotest.test_case "parseable" `Quick test_connect_line_parseable
       ])
    ; ("validate_config",
       [ Alcotest.test_case "insecure loopback ok" `Quick
           test_validate_insecure_loopback_ok
       ; Alcotest.test_case "insecure any rejected" `Quick
           test_validate_insecure_any_rejected
       ; Alcotest.test_case "secure any ok" `Quick
           test_validate_secure_any_ok
       ; Alcotest.test_case "insecure specific ip rejected" `Quick
           test_validate_insecure_specific_ip_rejected
       ])
    ; ("graceful_shutdown",
       [ Alcotest.test_case "auto-checkpoint on graceful" `Quick
           test_graceful_shutdown_auto_checkpoint
       ; Alcotest.test_case "no checkpoint when disabled" `Quick
           test_graceful_shutdown_no_checkpoint
       ])
    ; ("server_commands",
       [ Alcotest.test_case ",checkpoint creates" `Quick
           test_server_cmd_checkpoint
       ; Alcotest.test_case ",checkpoint + ,revert" `Quick
           test_server_cmd_checkpoint_revert
       ; Alcotest.test_case ",checkpoints lists" `Quick
           test_server_cmd_checkpoints
       ; Alcotest.test_case ",env" `Quick test_server_cmd_env
       ; Alcotest.test_case ",libs" `Quick test_server_cmd_libs
       ; Alcotest.test_case ",help" `Quick test_server_cmd_help
       ; Alcotest.test_case "unknown command" `Quick test_server_cmd_unknown
       ; Alcotest.test_case "scheme passthrough" `Quick
           test_server_cmd_scheme_passthrough
       ])
    ]
