open Bilk

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Test the REPL server's core components using socketpairs
   instead of TCP connections *)

(* --- Server survives client disconnect --- *)

let test_server_survives_disconnect () =
  let config = {
    Repl_server.port = 0;
    scrollback_size = 1024;
    auto_checkpoint = false;
    name = "test";
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  Unix.close client_fd;
  let alive = Repl_server.is_alive server in
  Alcotest.(check bool) "server still alive" true alive;
  Repl_server.shutdown server

(* --- Scrollback replay on reconnect --- *)

let test_scrollback_replay () =
  let config = {
    Repl_server.port = 0;
    scrollback_size = 4096;
    auto_checkpoint = false;
    name = "test";
  } in
  let server = Repl_server.create config in
  Repl_server.append_output server "hello world\n";
  let sb = Repl_server.get_scrollback server in
  Alcotest.(check string) "scrollback" "hello world\n" sb;
  Repl_server.shutdown server

(* --- Protocol message relay --- *)

let test_protocol_write_read () =
  let buf = Buffer.create 64 in
  Repl_protocol.write_server_msg buf (Output "test output");
  let data = Buffer.contents buf in
  let (msg, _consumed) = Repl_protocol.read_server_msg data 0 in
  Alcotest.(check bool) "output matches" true
    (match msg with Repl_protocol.Output s -> s = "test output" | _ -> false)

(* --- Keepalive / ping-pong --- *)

let test_keepalive_state () =
  let config = {
    Repl_server.port = 0;
    scrollback_size = 1024;
    auto_checkpoint = false;
    name = "test";
  } in
  let server = Repl_server.create config in
  let (_, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  Alcotest.(check bool) "alive after accept" true
    (Repl_server.is_alive server);
  Repl_server.shutdown server

(* --- Scrollback accumulation across multiple appends --- *)

let test_scrollback_accumulation () =
  let config = {
    Repl_server.port = 0;
    scrollback_size = 4096;
    auto_checkpoint = false;
    name = "test";
  } in
  let server = Repl_server.create config in
  Repl_server.append_output server "line 1\n";
  Repl_server.append_output server "line 2\n";
  Repl_server.append_output server "line 3\n";
  let sb = Repl_server.get_scrollback server in
  Alcotest.(check string) "accumulated" "line 1\nline 2\nline 3\n" sb;
  Repl_server.shutdown server

(* --- clear_scrollback empties the buffer --- *)

let test_clear_scrollback () =
  let config = {
    Repl_server.port = 0;
    scrollback_size = 4096;
    auto_checkpoint = false;
    name = "test";
  } in
  let server = Repl_server.create config in
  Repl_server.append_output server "line 1\n";
  Repl_server.append_output server "line 2\n";
  Repl_server.clear_scrollback server;
  let sb = Repl_server.get_scrollback server in
  Alcotest.(check string) "empty after clear" "" sb;
  Repl_server.shutdown server

let () =
  Alcotest.run "Repl_server"
    [ ("lifecycle",
       [ Alcotest.test_case "survives disconnect" `Quick
           test_server_survives_disconnect
       ; Alcotest.test_case "scrollback replay" `Quick
           test_scrollback_replay
       ; Alcotest.test_case "keepalive state" `Quick
           test_keepalive_state
       ; Alcotest.test_case "scrollback accumulation" `Quick
           test_scrollback_accumulation
       ; Alcotest.test_case "clear scrollback" `Quick
           test_clear_scrollback
       ])
    ; ("protocol",
       [ Alcotest.test_case "write/read" `Quick
           test_protocol_write_read
       ])
    ]
