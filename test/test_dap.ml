open Wile

let test_parse_valid_request () =
  let json = `Assoc [
    ("seq", `Int 1);
    ("type", `String "request");
    ("command", `String "initialize");
    ("arguments", `Assoc [("clientID", `String "test")])
  ] in
  let req = Dap.request_of_json json in
  Alcotest.(check int) "seq" 1 req.seq;
  Alcotest.(check string) "command" "initialize" req.command;
  Alcotest.(check bool) "args not null" true (req.arguments <> `Null)

let test_parse_request_missing_command () =
  let json = `Assoc [
    ("seq", `Int 1);
    ("type", `String "request")
  ] in
  Alcotest.check_raises "missing command" (Dap.Dap_error "missing or invalid field: command")
    (fun () -> ignore (Dap.request_of_json json))

let test_response_to_json_roundtrip () =
  let resp : Dap.response = {
    seq = 1; request_seq = 1; success = true;
    command = "initialize"; message = None;
    body = `Assoc [("supportsConfigurationDoneRequest", `Bool true)]
  } in
  let json = Dap.response_to_json resp in
  match json with
  | `Assoc assoc ->
    Alcotest.(check string) "type" "response"
      (match List.assoc "type" assoc with `String s -> s | _ -> "");
    Alcotest.(check bool) "success"  true
      (match List.assoc "success" assoc with `Bool b -> b | _ -> false);
    Alcotest.(check int) "request_seq" 1
      (match List.assoc "request_seq" assoc with `Int n -> n | _ -> -1)
  | _ -> Alcotest.fail "expected Assoc"

let test_event_to_json_roundtrip () =
  let evt : Dap.event = {
    seq = 2; event_name = "stopped";
    body = `Assoc [("reason", `String "breakpoint")]
  } in
  let json = Dap.event_to_json evt in
  match json with
  | `Assoc assoc ->
    Alcotest.(check string) "type" "event"
      (match List.assoc "type" assoc with `String s -> s | _ -> "");
    Alcotest.(check string) "event" "stopped"
      (match List.assoc "event" assoc with `String s -> s | _ -> "")
  | _ -> Alcotest.fail "expected Assoc"

let test_message_of_json_request () =
  let json = `Assoc [
    ("seq", `Int 1);
    ("type", `String "request");
    ("command", `String "launch")
  ] in
  match Dap.message_of_json json with
  | Dap.Request req ->
    Alcotest.(check string) "command" "launch" req.command
  | _ -> Alcotest.fail "expected Request"

let test_message_of_json_event () =
  let json = `Assoc [
    ("seq", `Int 1);
    ("type", `String "event");
    ("event", `String "initialized")
  ] in
  match Dap.message_of_json json with
  | Dap.Event evt ->
    Alcotest.(check string) "event" "initialized" evt.event_name
  | _ -> Alcotest.fail "expected Event"

let test_write_read_roundtrip () =
  let r_in, w_out = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r_in in
  let oc = Unix.out_channel_of_descr w_out in
  let seq_ref = ref 1 in
  let resp : Dap.response = {
    seq = 0; request_seq = 1; success = true;
    command = "test"; message = None; body = `Null
  } in
  Dap.write_response oc seq_ref resp;
  let msg = Dap.read_message ic in
  close_in ic;
  close_out_noerr oc;
  match msg with
  | Dap.Response r ->
    Alcotest.(check string) "command" "test" r.command;
    Alcotest.(check bool) "success" true r.success
  | _ -> Alcotest.fail "expected Response"

let test_malformed_content_length () =
  let r_in, w_out = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r_in in
  let oc = Unix.out_channel_of_descr w_out in
  output_string oc "Bad-Header: 42\r\n\r\n";
  flush oc;
  close_out oc;
  Alcotest.check_raises "bad header"
    (Dap.Dap_error "expected Content-Length header, got: Bad-Header: 42")
    (fun () -> ignore (Dap.read_message ic));
  close_in_noerr ic

let test_request_null_arguments () =
  let json = `Assoc [
    ("seq", `Int 5);
    ("type", `String "request");
    ("command", `String "disconnect")
  ] in
  let req = Dap.request_of_json json in
  Alcotest.(check int) "seq" 5 req.seq;
  Alcotest.(check string) "args null" "null"
    (Yojson.Safe.to_string req.arguments)

let test_response_with_error_message () =
  let resp : Dap.response = {
    seq = 3; request_seq = 2; success = false;
    command = "evaluate"; message = Some "evaluation failed";
    body = `Null
  } in
  let json = Dap.response_to_json resp in
  match json with
  | `Assoc assoc ->
    Alcotest.(check bool) "has message" true
      (List.mem_assoc "message" assoc);
    Alcotest.(check string) "message value" "evaluation failed"
      (match List.assoc "message" assoc with `String s -> s | _ -> "")
  | _ -> Alcotest.fail "expected Assoc"

let test_multiple_sequential_messages () =
  let r_in, w_out = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r_in in
  let oc = Unix.out_channel_of_descr w_out in
  let seq_ref = ref 1 in
  let evt1 = Dap.make_event seq_ref "initialized" `Null in
  let evt2 = Dap.make_event seq_ref "stopped"
    (`Assoc [("reason", `String "entry")]) in
  Dap.write_event oc seq_ref evt1;
  Dap.write_event oc seq_ref evt2;
  close_out oc;
  let msg1 = Dap.read_message ic in
  let msg2 = Dap.read_message ic in
  close_in_noerr ic;
  (match msg1 with
   | Dap.Event e -> Alcotest.(check string) "evt1" "initialized" e.event_name
   | _ -> Alcotest.fail "expected Event 1");
  (match msg2 with
   | Dap.Event e -> Alcotest.(check string) "evt2" "stopped" e.event_name
   | _ -> Alcotest.fail "expected Event 2")

let () =
  Alcotest.run "Dap" [
    "parse", [
      Alcotest.test_case "valid request" `Quick test_parse_valid_request;
      Alcotest.test_case "missing command" `Quick test_parse_request_missing_command;
      Alcotest.test_case "null arguments" `Quick test_request_null_arguments;
      Alcotest.test_case "message_of_json request" `Quick test_message_of_json_request;
      Alcotest.test_case "message_of_json event" `Quick test_message_of_json_event;
    ];
    "serialize", [
      Alcotest.test_case "response roundtrip" `Quick test_response_to_json_roundtrip;
      Alcotest.test_case "event roundtrip" `Quick test_event_to_json_roundtrip;
      Alcotest.test_case "response error message" `Quick test_response_with_error_message;
    ];
    "wire", [
      Alcotest.test_case "write/read roundtrip" `Quick test_write_read_roundtrip;
      Alcotest.test_case "malformed header" `Quick test_malformed_content_length;
      Alcotest.test_case "multiple messages" `Quick test_multiple_sequential_messages;
    ];
  ]
