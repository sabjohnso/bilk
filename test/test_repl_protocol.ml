open Bilk

(* --- Alcotest testable for messages --- *)

let client_msg_testable =
  Alcotest.testable
    (fun fmt msg ->
       match msg with
       | Repl_protocol.Key_input s ->
         Format.fprintf fmt "Key_input(%S)" s
       | Repl_protocol.Resize (r, c) ->
         Format.fprintf fmt "Resize(%d, %d)" r c
       | Repl_protocol.Ping ->
         Format.fprintf fmt "Ping"
       | Repl_protocol.Disconnect ->
         Format.fprintf fmt "Disconnect")
    (=)

let server_msg_testable =
  Alcotest.testable
    (fun fmt msg ->
       match msg with
       | Repl_protocol.Output s ->
         Format.fprintf fmt "Output(%S)" s
       | Repl_protocol.Pong ->
         Format.fprintf fmt "Pong"
       | Repl_protocol.Scrollback s ->
         Format.fprintf fmt "Scrollback(%S)" s
       | Repl_protocol.Server_exit ->
         Format.fprintf fmt "Server_exit")
    (=)

(* --- Round-trip tests --- *)

let roundtrip_client msg =
  let buf = Buffer.create 64 in
  Repl_protocol.write_client_msg buf msg;
  let bytes = Buffer.contents buf in
  let (decoded, consumed) = Repl_protocol.read_client_msg bytes 0 in
  Alcotest.check client_msg_testable "round-trip" msg decoded;
  Alcotest.(check int) "consumed all bytes"
    (String.length bytes) consumed

let roundtrip_server msg =
  let buf = Buffer.create 64 in
  Repl_protocol.write_server_msg buf msg;
  let bytes = Buffer.contents buf in
  let (decoded, consumed) = Repl_protocol.read_server_msg bytes 0 in
  Alcotest.check server_msg_testable "round-trip" msg decoded;
  Alcotest.(check int) "consumed all bytes"
    (String.length bytes) consumed

let test_rt_key_input () =
  roundtrip_client (Repl_protocol.Key_input "hello")

let test_rt_key_input_empty () =
  roundtrip_client (Repl_protocol.Key_input "")

let test_rt_resize () =
  roundtrip_client (Repl_protocol.Resize (24, 80))

let test_rt_ping () =
  roundtrip_client Repl_protocol.Ping

let test_rt_disconnect () =
  roundtrip_client Repl_protocol.Disconnect

let test_rt_output () =
  roundtrip_server (Repl_protocol.Output "bilk> ")

let test_rt_pong () =
  roundtrip_server Repl_protocol.Pong

let test_rt_scrollback () =
  roundtrip_server (Repl_protocol.Scrollback "previous output\n")

let test_rt_server_exit () =
  roundtrip_server Repl_protocol.Server_exit

(* --- Frame length correctness --- *)

let test_frame_length () =
  let buf = Buffer.create 64 in
  Repl_protocol.write_client_msg buf (Key_input "abc");
  let bytes = Buffer.contents buf in
  (* Frame: u32 payload_length + u8 tag + payload *)
  let payload_len =
    (Char.code bytes.[0] lsl 24) lor
    (Char.code bytes.[1] lsl 16) lor
    (Char.code bytes.[2] lsl 8) lor
    (Char.code bytes.[3])
  in
  Alcotest.(check int) "payload length"
    (String.length bytes - 4)  (* total - u32 header *)
    payload_len

(* --- Properties --- *)

let prop_client_roundtrip =
  QCheck2.Test.make ~count:200
    ~name:"client message round-trip"
    QCheck2.Gen.(oneof [
      map (fun s -> Repl_protocol.Key_input s)
        (string_size (int_range 0 100));
      map2 (fun r c -> Repl_protocol.Resize (r, c))
        (int_range 1 500) (int_range 1 500);
      return Repl_protocol.Ping;
      return Repl_protocol.Disconnect;
    ])
    (fun msg ->
       let buf = Buffer.create 64 in
       Repl_protocol.write_client_msg buf msg;
       let bytes = Buffer.contents buf in
       let (decoded, consumed) = Repl_protocol.read_client_msg bytes 0 in
       decoded = msg && consumed = String.length bytes)

let prop_server_roundtrip =
  QCheck2.Test.make ~count:200
    ~name:"server message round-trip"
    QCheck2.Gen.(oneof [
      map (fun s -> Repl_protocol.Output s)
        (string_size (int_range 0 100));
      return Repl_protocol.Pong;
      map (fun s -> Repl_protocol.Scrollback s)
        (string_size (int_range 0 500));
      return Repl_protocol.Server_exit;
    ])
    (fun msg ->
       let buf = Buffer.create 64 in
       Repl_protocol.write_server_msg buf msg;
       let bytes = Buffer.contents buf in
       let (decoded, consumed) = Repl_protocol.read_server_msg bytes 0 in
       decoded = msg && consumed = String.length bytes)

let () =
  Alcotest.run "Repl_protocol"
    [ ("client_roundtrip",
       [ Alcotest.test_case "key input" `Quick test_rt_key_input
       ; Alcotest.test_case "key empty" `Quick test_rt_key_input_empty
       ; Alcotest.test_case "resize" `Quick test_rt_resize
       ; Alcotest.test_case "ping" `Quick test_rt_ping
       ; Alcotest.test_case "disconnect" `Quick test_rt_disconnect
       ])
    ; ("server_roundtrip",
       [ Alcotest.test_case "output" `Quick test_rt_output
       ; Alcotest.test_case "pong" `Quick test_rt_pong
       ; Alcotest.test_case "scrollback" `Quick test_rt_scrollback
       ; Alcotest.test_case "server_exit" `Quick test_rt_server_exit
       ])
    ; ("frame",
       [ Alcotest.test_case "length" `Quick test_frame_length
       ])
    ; ("properties",
       List.map QCheck_alcotest.to_alcotest
         [ prop_client_roundtrip
         ; prop_server_roundtrip
         ])
    ]
