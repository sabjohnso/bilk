open Bilk

(* Test the REPL client's protocol-level behavior using socketpairs *)

(* --- Protocol-level tests --- *)

let test_client_sends_key_input () =
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (* Client sends a key input message *)
  let buf = Buffer.create 64 in
  Repl_protocol.write_client_msg buf (Key_input "abc");
  let data = Buffer.contents buf in
  let _ = Unix.write_substring client_fd data 0 (String.length data) in
  (* Read from server side *)
  let recv = Bytes.create 256 in
  let n = Unix.read server_fd recv 0 256 in
  let recv_str = Bytes.sub_string recv 0 n in
  let (msg, _) = Repl_protocol.read_client_msg recv_str 0 in
  Alcotest.(check bool) "key input" true
    (match msg with Repl_protocol.Key_input s -> s = "abc" | _ -> false);
  Unix.close client_fd;
  Unix.close server_fd

let test_client_sends_resize () =
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let buf = Buffer.create 64 in
  Repl_protocol.write_client_msg buf (Resize (24, 80));
  let data = Buffer.contents buf in
  let _ = Unix.write_substring client_fd data 0 (String.length data) in
  let recv = Bytes.create 256 in
  let n = Unix.read server_fd recv 0 256 in
  let recv_str = Bytes.sub_string recv 0 n in
  let (msg, _) = Repl_protocol.read_client_msg recv_str 0 in
  Alcotest.(check bool) "resize" true
    (match msg with Repl_protocol.Resize (r, c) -> r = 24 && c = 80 | _ -> false);
  Unix.close client_fd;
  Unix.close server_fd

let test_client_receives_output () =
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (* Server sends output *)
  let buf = Buffer.create 64 in
  Repl_protocol.write_server_msg buf (Output "bilk> ");
  let data = Buffer.contents buf in
  let _ = Unix.write_substring server_fd data 0 (String.length data) in
  (* Client reads *)
  let recv = Bytes.create 256 in
  let n = Unix.read client_fd recv 0 256 in
  let recv_str = Bytes.sub_string recv 0 n in
  let (msg, _) = Repl_protocol.read_server_msg recv_str 0 in
  Alcotest.(check bool) "output" true
    (match msg with Repl_protocol.Output s -> s = "bilk> " | _ -> false);
  Unix.close client_fd;
  Unix.close server_fd

let test_client_receives_scrollback () =
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let buf = Buffer.create 64 in
  Repl_protocol.write_server_msg buf (Scrollback "previous output\n");
  let data = Buffer.contents buf in
  let _ = Unix.write_substring server_fd data 0 (String.length data) in
  let recv = Bytes.create 256 in
  let n = Unix.read client_fd recv 0 256 in
  let recv_str = Bytes.sub_string recv 0 n in
  let (msg, _) = Repl_protocol.read_server_msg recv_str 0 in
  Alcotest.(check bool) "scrollback" true
    (match msg with Repl_protocol.Scrollback s -> s = "previous output\n" | _ -> false);
  Unix.close client_fd;
  Unix.close server_fd

let test_client_config () =
  let config : Repl_client.config = { host = "localhost"; port = 9999 } in
  Alcotest.(check string) "host" "localhost" config.host;
  Alcotest.(check int) "port" 9999 config.port

let () =
  Alcotest.run "Repl_client"
    [ ("protocol",
       [ Alcotest.test_case "send key input" `Quick test_client_sends_key_input
       ; Alcotest.test_case "send resize" `Quick test_client_sends_resize
       ; Alcotest.test_case "receive output" `Quick test_client_receives_output
       ; Alcotest.test_case "receive scrollback" `Quick test_client_receives_scrollback
       ])
    ; ("config",
       [ Alcotest.test_case "fields" `Quick test_client_config
       ])
    ]
