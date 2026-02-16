open Bilk

(* Helper: write server messages to a fd *)
let write_server_msg fd msg =
  let buf = Buffer.create 256 in
  Repl_protocol.write_server_msg buf msg;
  let data = Buffer.contents buf in
  ignore (Unix.write_substring fd data 0 (String.length data))

(* Helper: read a client message from a fd *)
let read_client_msg_from_fd fd =
  let tmp = Bytes.create 4096 in
  let n = Unix.read fd tmp 0 4096 in
  let data = Bytes.sub_string tmp 0 n in
  let (msg, _) = Repl_protocol.read_client_msg data 0 in
  msg

(* Helper: create a socketpair and return (client_conn, server_fd) *)
let make_pair () =
  let (cfd, sfd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (Repl_client.connection_of_fd cfd, sfd)

(* --- send_msg / recv_msg round-trip --- *)

let test_send_recv_roundtrip () =
  let (conn, server_fd) = make_pair () in
  Repl_client.send_msg conn (Repl_protocol.Eval "(+ 1 2)");
  let msg = read_client_msg_from_fd server_fd in
  Alcotest.(check bool) "eval message" true
    (match msg with Repl_protocol.Eval s -> s = "(+ 1 2)" | _ -> false);
  write_server_msg server_fd (Repl_protocol.Result "3");
  let resp = Repl_client.recv_msg conn in
  Alcotest.(check bool) "result message" true
    (match resp with Repl_protocol.Result s -> s = "3" | _ -> false);
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: simple result --- *)

let test_eval_result () =
  let (conn, server_fd) = make_pair () in
  (* Pre-write the server response before calling eval_remote *)
  write_server_msg server_fd (Repl_protocol.Result "42");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "(+ 40 2)" in
  Alcotest.(check bool) "result is 42" true
    (result = `Result "42");
  (* Verify the client sent Eval *)
  let msg = read_client_msg_from_fd server_fd in
  Alcotest.(check bool) "sent eval" true
    (match msg with Repl_protocol.Eval s -> s = "(+ 40 2)" | _ -> false);
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: output then result --- *)

let test_eval_output_then_result () =
  let (conn, server_fd) = make_pair () in
  let output_buf = Buffer.create 64 in
  write_server_msg server_fd (Repl_protocol.Output "hello\n");
  write_server_msg server_fd (Repl_protocol.Result "42");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun s -> Buffer.add_string output_buf s)
    ~on_read:(fun _ -> "")
    "(begin (display \"hello\") (newline) 42)" in
  Alcotest.(check string) "output captured" "hello\n"
    (Buffer.contents output_buf);
  Alcotest.(check bool) "result is 42" true
    (result = `Result "42");
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: error --- *)

let test_eval_error () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Error "unbound variable: x");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "x" in
  Alcotest.(check bool) "error returned" true
    (result = `Error "unbound variable: x");
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: read request --- *)

let test_eval_read_request () =
  let (conn, server_fd) = make_pair () in
  (* Pre-write Read_request. After eval_remote sends Input, we need
     the Result to be readable. Use a helper process approach:
     write Read_request, then after reading Input back, write Result.
     Since we can't do that without threads, test the simpler case:
     pre-write Read_request + Result, and verify Input was sent. *)
  write_server_msg server_fd (Repl_protocol.Read_request "Enter: ");
  (* We also pre-write the Result that follows.  eval_remote will:
     1. send Eval, 2. read Read_request, 3. call on_read, 4. send Input,
     5. read Result.  Since the socket buffer has both messages, this works. *)
  write_server_msg server_fd (Repl_protocol.Result "done");
  let read_called = ref false in
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun prompt ->
      Alcotest.(check string) "prompt" "Enter: " prompt;
      read_called := true;
      "user-input")
    "(read)" in
  Alcotest.(check bool) "on_read called" true !read_called;
  Alcotest.(check bool) "result is done" true
    (result = `Result "done");
  (* Verify that Eval and Input were sent *)
  let tmp = Bytes.create 4096 in
  let n = Unix.read server_fd tmp 0 4096 in
  let data = Bytes.sub_string tmp 0 n in
  let (msg1, off1) = Repl_protocol.read_client_msg data 0 in
  Alcotest.(check bool) "sent eval" true
    (match msg1 with Repl_protocol.Eval _ -> true | _ -> false);
  let (msg2, _) = Repl_protocol.read_client_msg data off1 in
  Alcotest.(check bool) "sent input" true
    (match msg2 with Repl_protocol.Input s -> s = "user-input" | _ -> false);
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: disconnect --- *)

let test_eval_disconnect () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (conn, server_fd) = make_pair () in
  Unix.close server_fd;
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "(+ 1 2)" in
  Alcotest.(check bool) "disconnected" true
    (result = `Disconnected);
  Repl_client.close_connection conn

(* --- eval_remote: skips Status messages --- *)

let test_eval_skips_status () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Status Repl_protocol.Busy);
  write_server_msg server_fd (Repl_protocol.Status Repl_protocol.Ready);
  write_server_msg server_fd (Repl_protocol.Result "ok");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "expr" in
  Alcotest.(check bool) "result is ok" true
    (result = `Result "ok");
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- request_completions: normal --- *)

let test_completions () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd
    (Repl_protocol.Completions ["car"; "cdr"; "cons"]);
  let result = Repl_client.request_completions conn "c" in
  Alcotest.(check (list string)) "completions" ["car"; "cdr"; "cons"] result;
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- request_completions: empty --- *)

let test_completions_empty () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Completions []);
  let result = Repl_client.request_completions conn "zzz" in
  Alcotest.(check (list string)) "empty completions" [] result;
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- request_completions: skips Status before Completions --- *)

let test_completions_skips_status () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Status Repl_protocol.Busy);
  write_server_msg server_fd
    (Repl_protocol.Completions ["define"; "display"]);
  let result = Repl_client.request_completions conn "d" in
  Alcotest.(check (list string)) "completions" ["define"; "display"] result;
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- request_completions: disconnect returns [] --- *)

let test_completions_disconnect () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (conn, server_fd) = make_pair () in
  Unix.close server_fd;
  let result = Repl_client.request_completions conn "c" in
  Alcotest.(check (list string)) "empty on disconnect" [] result;
  Repl_client.close_connection conn

(* --- config: all fields --- *)

let test_config_fields () =
  let config : Repl_client.config = {
    host = "example.com";
    port = 7890;
    theme = Some "dark";
    history_file = Some "/tmp/history";
    paredit = true;
    key = Some "secret-key";
  } in
  Alcotest.(check string) "host" "example.com" config.host;
  Alcotest.(check int) "port" 7890 config.port;
  Alcotest.(check (option string)) "theme" (Some "dark") config.theme;
  Alcotest.(check (option string)) "history_file"
    (Some "/tmp/history") config.history_file;
  Alcotest.(check bool) "paredit" true config.paredit;
  Alcotest.(check (option string)) "key" (Some "secret-key") config.key

(* --- Command classification integration --- *)

let test_classify_quit_integration () =
  Alcotest.(check bool) ",quit is Client_local Quit" true
    (Repl_command.classify ",quit" = Client_local Quit)

let test_classify_checkpoint_integration () =
  Alcotest.(check bool) ",checkpoint is Server_side" true
    (Repl_command.classify ",checkpoint x" = Server_side)

let test_classify_scheme_integration () =
  Alcotest.(check bool) "(+ 1 2) is Scheme_input" true
    (Repl_command.classify "(+ 1 2)" = Scheme_input)

let () =
  Alcotest.run "Repl_client"
    [ ("send_recv",
       [ Alcotest.test_case "roundtrip" `Quick test_send_recv_roundtrip
       ])
    ; ("eval_remote",
       [ Alcotest.test_case "simple result" `Quick test_eval_result
       ; Alcotest.test_case "output then result" `Quick
           test_eval_output_then_result
       ; Alcotest.test_case "error" `Quick test_eval_error
       ; Alcotest.test_case "read request" `Quick test_eval_read_request
       ; Alcotest.test_case "disconnect" `Quick test_eval_disconnect
       ; Alcotest.test_case "skips status" `Quick test_eval_skips_status
       ])
    ; ("request_completions",
       [ Alcotest.test_case "normal" `Quick test_completions
       ; Alcotest.test_case "empty" `Quick test_completions_empty
       ; Alcotest.test_case "skips status" `Quick test_completions_skips_status
       ; Alcotest.test_case "disconnect" `Quick test_completions_disconnect
       ])
    ; ("config",
       [ Alcotest.test_case "fields" `Quick test_config_fields
       ])
    ; ("command_classify",
       [ Alcotest.test_case ",quit integration" `Quick
           test_classify_quit_integration
       ; Alcotest.test_case ",checkpoint integration" `Quick
           test_classify_checkpoint_integration
       ; Alcotest.test_case "scheme integration" `Quick
           test_classify_scheme_integration
       ])
    ]
