open Wile

(* --- Parsing tests --- *)

let test_parse_request_int_id () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "initialize");
    ("params", `Assoc [("clientInfo", `String "test")])
  ] in
  match Lsp.message_of_json json with
  | Lsp.Request req ->
    Alcotest.(check int) "id" 1
      (match req.id with `Int n -> n | _ -> -1);
    Alcotest.(check string) "method" "initialize" req.method_;
    Alcotest.(check bool) "params not null" true (req.params <> `Null)
  | _ -> Alcotest.fail "expected Request"

let test_parse_request_string_id () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `String "abc-123");
    ("method", `String "textDocument/hover");
    ("params", `Null)
  ] in
  match Lsp.message_of_json json with
  | Lsp.Request req ->
    Alcotest.(check string) "id" "abc-123"
      (match req.id with `String s -> s | _ -> "");
    Alcotest.(check string) "method" "textDocument/hover" req.method_
  | _ -> Alcotest.fail "expected Request"

let test_parse_notification () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "initialized");
    ("params", `Assoc [])
  ] in
  match Lsp.message_of_json json with
  | Lsp.Notification n ->
    Alcotest.(check string) "method" "initialized" n.method_
  | _ -> Alcotest.fail "expected Notification"

let test_parse_notification_no_params () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "exit")
  ] in
  match Lsp.message_of_json json with
  | Lsp.Notification n ->
    Alcotest.(check string) "method" "exit" n.method_;
    Alcotest.(check string) "params null" "null"
      (Yojson.Safe.to_string n.params)
  | _ -> Alcotest.fail "expected Notification"

let test_parse_response_success () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("result", `Assoc [("capabilities", `Assoc [])])
  ] in
  match Lsp.message_of_json json with
  | Lsp.Response resp ->
    Alcotest.(check int) "id" 1
      (match resp.id with `Int n -> n | _ -> -1);
    Alcotest.(check bool) "no error" true (resp.error = None);
    Alcotest.(check bool) "has result" true (resp.result <> `Null)
  | _ -> Alcotest.fail "expected Response"

let test_parse_response_error () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 2);
    ("error", `Assoc [
      ("code", `Int (-32601));
      ("message", `String "Method not found")
    ])
  ] in
  match Lsp.message_of_json json with
  | Lsp.Response resp ->
    (match resp.error with
     | Some err ->
       Alcotest.(check int) "code" (-32601) err.code;
       Alcotest.(check string) "message" "Method not found" err.message
     | None -> Alcotest.fail "expected error")
  | _ -> Alcotest.fail "expected Response"

let test_parse_missing_method_and_id () =
  let json = `Assoc [("jsonrpc", `String "2.0")] in
  Alcotest.check_raises "missing method and id"
    (Lsp.Lsp_error "message must have 'method' or 'id' field")
    (fun () -> ignore (Lsp.message_of_json json))

let test_parse_non_object () =
  Alcotest.check_raises "non-object"
    (Lsp.Lsp_error "message must be a JSON object")
    (fun () -> ignore (Lsp.message_of_json (`String "hello")))

(* --- Serialization tests --- *)

let test_response_to_json_success () =
  let resp = Lsp.make_response (`Int 1) (`Assoc [("x", `Int 42)]) in
  let json = Lsp.response_to_json resp in
  match json with
  | `Assoc assoc ->
    Alcotest.(check string) "jsonrpc" "2.0"
      (match List.assoc "jsonrpc" assoc with `String s -> s | _ -> "");
    Alcotest.(check int) "id" 1
      (match List.assoc "id" assoc with `Int n -> n | _ -> -1);
    Alcotest.(check bool) "has result" true (List.mem_assoc "result" assoc);
    Alcotest.(check bool) "no error" false (List.mem_assoc "error" assoc)
  | _ -> Alcotest.fail "expected Assoc"

let test_response_to_json_error () =
  let resp = Lsp.make_error_response (`Int 5) (-32601) "not found" in
  let json = Lsp.response_to_json resp in
  match json with
  | `Assoc assoc ->
    Alcotest.(check bool) "has error" true (List.mem_assoc "error" assoc);
    Alcotest.(check bool) "no result" false (List.mem_assoc "result" assoc);
    (match List.assoc "error" assoc with
     | `Assoc err ->
       Alcotest.(check int) "code" (-32601)
         (match List.assoc "code" err with `Int n -> n | _ -> 0);
       Alcotest.(check string) "message" "not found"
         (match List.assoc "message" err with `String s -> s | _ -> "")
     | _ -> Alcotest.fail "error not an object")
  | _ -> Alcotest.fail "expected Assoc"

(* --- Wire format tests --- *)

let test_write_read_response_roundtrip () =
  let r_in, w_out = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r_in in
  let oc = Unix.out_channel_of_descr w_out in
  let resp = Lsp.make_response (`Int 1) (`String "ok") in
  Lsp.write_response oc resp;
  close_out oc;
  let msg = Lsp.read_message ic in
  close_in_noerr ic;
  match msg with
  | Lsp.Response r ->
    Alcotest.(check int) "id" 1
      (match r.id with `Int n -> n | _ -> -1);
    Alcotest.(check string) "result" "\"ok\""
      (Yojson.Safe.to_string r.result)
  | _ -> Alcotest.fail "expected Response"

let test_write_read_notification_roundtrip () =
  let r_in, w_out = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r_in in
  let oc = Unix.out_channel_of_descr w_out in
  Lsp.write_notification oc "textDocument/publishDiagnostics"
    (`Assoc [("uri", `String "file:///test.scm");
             ("diagnostics", `List [])]);
  close_out oc;
  let msg = Lsp.read_message ic in
  close_in_noerr ic;
  match msg with
  | Lsp.Notification n ->
    Alcotest.(check string) "method" "textDocument/publishDiagnostics"
      n.method_
  | _ -> Alcotest.fail "expected Notification"

let test_malformed_content_length () =
  let r_in, w_out = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r_in in
  let oc = Unix.out_channel_of_descr w_out in
  output_string oc "Bad-Header: 42\r\n\r\n";
  flush oc;
  close_out oc;
  Alcotest.check_raises "bad header"
    (Lsp.Lsp_error "expected Content-Length header, got: Bad-Header: 42")
    (fun () -> ignore (Lsp.read_message ic));
  close_in_noerr ic

let test_multiple_messages () =
  let r_in, w_out = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r_in in
  let oc = Unix.out_channel_of_descr w_out in
  Lsp.write_notification oc "initialized" (`Assoc []);
  let resp = Lsp.make_response (`Int 1) `Null in
  Lsp.write_response oc resp;
  close_out oc;
  let msg1 = Lsp.read_message ic in
  let msg2 = Lsp.read_message ic in
  close_in_noerr ic;
  (match msg1 with
   | Lsp.Notification n ->
     Alcotest.(check string) "msg1" "initialized" n.method_
   | _ -> Alcotest.fail "expected Notification");
  (match msg2 with
   | Lsp.Response r ->
     Alcotest.(check int) "msg2 id" 1
       (match r.id with `Int n -> n | _ -> -1)
   | _ -> Alcotest.fail "expected Response")

(* --- JSON helper tests --- *)

let test_get_string_missing () =
  Alcotest.check_raises "missing field"
    (Lsp.Lsp_error "missing or invalid field: foo")
    (fun () -> ignore (Lsp.get_string "foo" []))

let test_get_int_missing () =
  Alcotest.check_raises "missing field"
    (Lsp.Lsp_error "missing or invalid field: bar")
    (fun () -> ignore (Lsp.get_int "bar" []))

let test_error_codes () =
  Alcotest.(check int) "method_not_found" (-32601) Lsp.method_not_found;
  Alcotest.(check int) "internal_error" (-32603) Lsp.internal_error

let () =
  Alcotest.run "Lsp" [
    "parse", [
      Alcotest.test_case "request int id" `Quick test_parse_request_int_id;
      Alcotest.test_case "request string id" `Quick test_parse_request_string_id;
      Alcotest.test_case "notification" `Quick test_parse_notification;
      Alcotest.test_case "notification no params" `Quick test_parse_notification_no_params;
      Alcotest.test_case "response success" `Quick test_parse_response_success;
      Alcotest.test_case "response error" `Quick test_parse_response_error;
      Alcotest.test_case "missing method and id" `Quick test_parse_missing_method_and_id;
      Alcotest.test_case "non-object" `Quick test_parse_non_object;
    ];
    "serialize", [
      Alcotest.test_case "response success" `Quick test_response_to_json_success;
      Alcotest.test_case "response error" `Quick test_response_to_json_error;
    ];
    "wire", [
      Alcotest.test_case "response roundtrip" `Quick test_write_read_response_roundtrip;
      Alcotest.test_case "notification roundtrip" `Quick test_write_read_notification_roundtrip;
      Alcotest.test_case "malformed header" `Quick test_malformed_content_length;
      Alcotest.test_case "multiple messages" `Quick test_multiple_messages;
    ];
    "helpers", [
      Alcotest.test_case "get_string missing" `Quick test_get_string_missing;
      Alcotest.test_case "get_int missing" `Quick test_get_int_missing;
      Alcotest.test_case "error codes" `Quick test_error_codes;
    ];
  ]
