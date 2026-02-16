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
    ]
