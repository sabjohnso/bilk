open Bilk

(* --- Alcotest testable for messages --- *)

let status_to_string = function
  | Repl_protocol.Ready -> "Ready"
  | Repl_protocol.Busy -> "Busy"

let client_msg_testable =
  Alcotest.testable
    (fun fmt msg ->
       match msg with
       | Repl_protocol.Eval s ->
         Format.fprintf fmt "Eval(%S)" s
       | Repl_protocol.Complete s ->
         Format.fprintf fmt "Complete(%S)" s
       | Repl_protocol.Interrupt ->
         Format.fprintf fmt "Interrupt"
       | Repl_protocol.Input s ->
         Format.fprintf fmt "Input(%S)" s
       | Repl_protocol.Resume s ->
         Format.fprintf fmt "Resume(%S)" s
       | Repl_protocol.Disconnect ->
         Format.fprintf fmt "Disconnect")
    (=)

let server_msg_testable =
  Alcotest.testable
    (fun fmt msg ->
       match msg with
       | Repl_protocol.Output s ->
         Format.fprintf fmt "Output(%S)" s
       | Repl_protocol.Result s ->
         Format.fprintf fmt "Result(%S)" s
       | Repl_protocol.Error s ->
         Format.fprintf fmt "Error(%S)" s
       | Repl_protocol.Completions lst ->
         Format.fprintf fmt "Completions(%a)"
           Format.(pp_print_list ~pp_sep:(fun f () -> pp_print_string f ", ")
                     pp_print_string) lst
       | Repl_protocol.Read_request s ->
         Format.fprintf fmt "Read_request(%S)" s
       | Repl_protocol.Status st ->
         Format.fprintf fmt "Status(%s)" (status_to_string st)
       | Repl_protocol.Session_ok ->
         Format.fprintf fmt "Session_ok"
       | Repl_protocol.Session_deny ->
         Format.fprintf fmt "Session_deny")
    (=)

(* --- Round-trip helpers --- *)

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

(* --- Client round-trip tests --- *)

let test_rt_eval () =
  roundtrip_client (Repl_protocol.Eval "(+ 1 2)")

let test_rt_eval_empty () =
  roundtrip_client (Repl_protocol.Eval "")

let test_rt_complete () =
  roundtrip_client (Repl_protocol.Complete "string-")

let test_rt_interrupt () =
  roundtrip_client Repl_protocol.Interrupt

let test_rt_input () =
  roundtrip_client (Repl_protocol.Input "user response")

let test_rt_resume () =
  roundtrip_client (Repl_protocol.Resume (String.make 32 '\xab'))

let test_rt_disconnect () =
  roundtrip_client Repl_protocol.Disconnect

(* --- Server round-trip tests --- *)

let test_rt_output () =
  roundtrip_server (Repl_protocol.Output "bilk> ")

let test_rt_result () =
  roundtrip_server (Repl_protocol.Result "3")

let test_rt_error () =
  roundtrip_server (Repl_protocol.Error "unbound variable: foo")

let test_rt_completions () =
  roundtrip_server (Repl_protocol.Completions
                      ["string-append"; "string-copy"; "string-length"])

let test_rt_completions_empty () =
  roundtrip_server (Repl_protocol.Completions [])

let test_rt_read_request () =
  roundtrip_server (Repl_protocol.Read_request "Enter value: ")

let test_rt_status_ready () =
  roundtrip_server (Repl_protocol.Status Repl_protocol.Ready)

let test_rt_status_busy () =
  roundtrip_server (Repl_protocol.Status Repl_protocol.Busy)

let test_rt_session_ok () =
  roundtrip_server Repl_protocol.Session_ok

let test_rt_session_deny () =
  roundtrip_server Repl_protocol.Session_deny

(* --- Frame length correctness --- *)

let test_frame_length () =
  let buf = Buffer.create 64 in
  Repl_protocol.write_client_msg buf (Eval "(+ 1 2)");
  let bytes = Buffer.contents buf in
  let payload_len =
    (Char.code bytes.[0] lsl 24) lor
    (Char.code bytes.[1] lsl 16) lor
    (Char.code bytes.[2] lsl 8) lor
    (Char.code bytes.[3])
  in
  Alcotest.(check int) "payload length"
    (String.length bytes - 4)
    payload_len

(* --- Properties --- *)

let prop_client_roundtrip =
  QCheck2.Test.make ~count:200
    ~name:"client message round-trip"
    QCheck2.Gen.(oneof [
      map (fun s -> Repl_protocol.Eval s)
        (string_size (int_range 0 100));
      map (fun s -> Repl_protocol.Complete s)
        (string_size (int_range 0 50));
      return Repl_protocol.Interrupt;
      map (fun s -> Repl_protocol.Input s)
        (string_size (int_range 0 100));
      map (fun s -> Repl_protocol.Resume s)
        (string_size (int_range 0 64));
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
      map (fun s -> Repl_protocol.Result s)
        (string_size (int_range 0 100));
      map (fun s -> Repl_protocol.Error s)
        (string_size (int_range 0 100));
      map (fun lst -> Repl_protocol.Completions lst)
        (list_size (int_range 0 20)
           (string_size (int_range 0 30)));
      map (fun s -> Repl_protocol.Read_request s)
        (string_size (int_range 0 100));
      map (fun b -> Repl_protocol.Status
             (if b then Repl_protocol.Ready else Repl_protocol.Busy))
        bool;
      return Repl_protocol.Session_ok;
      return Repl_protocol.Session_deny;
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
       [ Alcotest.test_case "eval" `Quick test_rt_eval
       ; Alcotest.test_case "eval empty" `Quick test_rt_eval_empty
       ; Alcotest.test_case "complete" `Quick test_rt_complete
       ; Alcotest.test_case "interrupt" `Quick test_rt_interrupt
       ; Alcotest.test_case "input" `Quick test_rt_input
       ; Alcotest.test_case "resume" `Quick test_rt_resume
       ; Alcotest.test_case "disconnect" `Quick test_rt_disconnect
       ])
    ; ("server_roundtrip",
       [ Alcotest.test_case "output" `Quick test_rt_output
       ; Alcotest.test_case "result" `Quick test_rt_result
       ; Alcotest.test_case "error" `Quick test_rt_error
       ; Alcotest.test_case "completions" `Quick test_rt_completions
       ; Alcotest.test_case "completions empty" `Quick test_rt_completions_empty
       ; Alcotest.test_case "read_request" `Quick test_rt_read_request
       ; Alcotest.test_case "status ready" `Quick test_rt_status_ready
       ; Alcotest.test_case "status busy" `Quick test_rt_status_busy
       ; Alcotest.test_case "session_ok" `Quick test_rt_session_ok
       ; Alcotest.test_case "session_deny" `Quick test_rt_session_deny
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
