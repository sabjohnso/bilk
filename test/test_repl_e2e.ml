open Bilk

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Helper: create server + socketpair, return (server, client_fd, server_fd)
   The client_fd is the "client side" of the socketpair — used for reading
   server messages as raw bytes (same pattern as test_repl_server.ml). *)
let make_e2e () =
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = true;
    name = "e2e-test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  (server, client_fd, server_fd)

(* Helper: read all available server messages from fd (non-blocking style) *)
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

let contains s sub =
  let slen = String.length s and sublen = String.length sub in
  let rec check i = i + sublen <= slen
    && (String.sub s i sublen = sub || check (i + 1)) in
  check 0

(* --- E2E 1: Eval round-trip --- *)

let test_eval_roundtrip () =
  let (server, client_fd, _server_fd) = make_e2e () in
  Repl_server.handle_eval server "(+ 1 2)";
  let msgs = read_server_msgs client_fd in
  let has_result = List.exists (function
    | Repl_protocol.Result "3" -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "eval (+ 1 2) = 3" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- E2E 2: Complete round-trip --- *)

let test_complete_roundtrip () =
  let (server, client_fd, _server_fd) = make_e2e () in
  Repl_server.handle_complete server "car";
  let msgs = read_server_msgs client_fd in
  let has_car = List.exists (function
    | Repl_protocol.Completions lst -> List.mem "car" lst
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "completions contain car" true has_car;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- E2E 3: Server command round-trip --- *)

let test_server_command_roundtrip () =
  let (server, client_fd, _server_fd) = make_e2e () in
  Repl_server.handle_eval server ",checkpoint test-cp";
  let msgs = read_server_msgs client_fd in
  let has_output = List.exists (function
    | Repl_protocol.Output s -> contains s "Checkpoint"
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "checkpoint output" true has_output;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- E2E 4: Checkpoint + revert --- *)

let test_checkpoint_revert () =
  let (server, client_fd, _server_fd) = make_e2e () in
  (* Define x=1 *)
  Repl_server.handle_eval server "(define x 1)";
  ignore (read_server_msgs client_fd);
  (* Checkpoint *)
  Repl_server.handle_eval server ",checkpoint snap";
  ignore (read_server_msgs client_fd);
  (* Define x=2 *)
  Repl_server.handle_eval server "(define x 2)";
  ignore (read_server_msgs client_fd);
  (* Revert *)
  Repl_server.handle_eval server ",revert snap";
  ignore (read_server_msgs client_fd);
  (* Eval x *)
  Repl_server.handle_eval server "x";
  let msgs = read_server_msgs client_fd in
  let has_1 = List.exists (function
    | Repl_protocol.Result "1" -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "x = 1 after revert" true has_1;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- E2E 5: Disconnect + resume --- *)

let test_disconnect_resume () =
  let (server, client_fd1, server_fd1) = make_e2e () in
  let token = Repl_server.session_token server in
  (* Use the first connection *)
  Repl_server.handle_eval server "(define y 42)";
  ignore (read_server_msgs client_fd1);
  (* Disconnect first connection *)
  Repl_server.handle_client_msg server Repl_protocol.Disconnect;
  (try Unix.close client_fd1 with Unix.Unix_error _ -> ());
  (try Unix.close server_fd1 with Unix.Unix_error _ -> ());
  (* Reconnect with a new socketpair *)
  let (client_fd2, server_fd2) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd2;
  (* Resume with correct token *)
  Repl_server.handle_resume server token;
  let msgs = read_server_msgs client_fd2 in
  let has_ok = List.exists (function
    | Repl_protocol.Session_ok -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "session ok" true has_ok;
  Repl_server.shutdown server;
  Unix.close client_fd2

(* --- E2E 6: State persists across disconnect --- *)

let test_state_persists () =
  let (server, client_fd1, server_fd1) = make_e2e () in
  (* Define y=99 *)
  Repl_server.handle_eval server "(define y 99)";
  ignore (read_server_msgs client_fd1);
  (* Disconnect *)
  Repl_server.handle_client_msg server Repl_protocol.Disconnect;
  (try Unix.close client_fd1 with Unix.Unix_error _ -> ());
  (try Unix.close server_fd1 with Unix.Unix_error _ -> ());
  (* Reconnect *)
  let (client_fd2, server_fd2) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd2;
  (* Eval y — state should be preserved *)
  Repl_server.handle_eval server "y";
  let msgs = read_server_msgs client_fd2 in
  let has_99 = List.exists (function
    | Repl_protocol.Result "99" -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "y = 99 after reconnect" true has_99;
  Repl_server.shutdown server;
  Unix.close client_fd2

let () =
  Alcotest.run "Repl_e2e"
    [ ("eval",
       [ Alcotest.test_case "eval round-trip" `Quick test_eval_roundtrip
       ; Alcotest.test_case "complete round-trip" `Quick
           test_complete_roundtrip
       ])
    ; ("commands",
       [ Alcotest.test_case "server command" `Quick
           test_server_command_roundtrip
       ; Alcotest.test_case "checkpoint + revert" `Quick
           test_checkpoint_revert
       ])
    ; ("session",
       [ Alcotest.test_case "disconnect + resume" `Quick
           test_disconnect_resume
       ; Alcotest.test_case "state persists" `Quick test_state_persists
       ])
    ]
