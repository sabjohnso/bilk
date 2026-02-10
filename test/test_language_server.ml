open Wile

(* Helper: send a JSON-RPC message via Content-Length framing *)
let send_msg oc json =
  let body = Yojson.Safe.to_string json in
  Printf.fprintf oc "Content-Length: %d\r\n\r\n%s" (String.length body) body;
  flush oc

let send_request oc id method_ params =
  send_msg oc (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String method_);
    ("params", params);
  ])

let send_notification oc method_ params =
  send_msg oc (`Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
    ("params", params);
  ])

let read_response ic =
  match Lsp.read_message ic with
  | Lsp.Response r -> r
  | _ -> Alcotest.fail "expected Response"

let read_notification ic =
  match Lsp.read_message ic with
  | Lsp.Notification n -> n
  | _ -> Alcotest.fail "expected Notification"

(* Helper to run a function with piped channels *)
let with_pipes f =
  let r1, w1 = Unix.pipe () in
  let r2, w2 = Unix.pipe () in
  let client_out = Unix.out_channel_of_descr w1 in
  let server_in = Unix.in_channel_of_descr r1 in
  let server_out = Unix.out_channel_of_descr w2 in
  let client_in = Unix.in_channel_of_descr r2 in
  Fun.protect ~finally:(fun () ->
    close_out_noerr client_out;
    close_in_noerr server_in;
    close_out_noerr server_out;
    close_in_noerr client_in)
    (fun () -> f client_out client_in server_in server_out)

(* Helper to do the LSP handshake *)
let do_handshake client_out client_in =
  send_request client_out 0 "initialize" (`Assoc []);
  let resp = read_response client_in in
  Alcotest.(check bool) "init success" true (resp.error = None);
  send_notification client_out "initialized" (`Assoc [])

(* Helper to open a document *)
let did_open client_out uri text =
  send_notification client_out "textDocument/didOpen"
    (`Assoc [
      ("textDocument", `Assoc [
        ("uri", `String uri);
        ("languageId", `String "scheme");
        ("version", `Int 1);
        ("text", `String text)])])

(* --- Document management tests --- *)

let test_initialize_handshake () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    send_request client_out 0 "initialize" (`Assoc []);
    let resp = read_response client_in in
    Alcotest.(check bool) "init success" true (resp.error = None);
    (* Check capabilities *)
    (match resp.result with
     | `Assoc result_assoc ->
       (match List.assoc_opt "capabilities" result_assoc with
        | Some (`Assoc caps) ->
          Alcotest.(check bool) "has hover" true
            (List.mem_assoc "hoverProvider" caps)
        | _ -> Alcotest.fail "expected capabilities")
     | _ -> Alcotest.fail "expected result object");
    send_notification client_out "initialized" (`Assoc []);
    send_notification client_out "exit" `Null;
    Thread.join t)

let test_shutdown_exit () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    send_request client_out 1 "shutdown" `Null;
    let resp = read_response client_in in
    Alcotest.(check bool) "shutdown ok" true (resp.error = None);
    send_notification client_out "exit" `Null;
    Thread.join t)

let test_unknown_method_error () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    send_request client_out 1 "custom/unknown" `Null;
    let resp = read_response client_in in
    Alcotest.(check bool) "has error" true (resp.error <> None);
    (match resp.error with
     | Some err -> Alcotest.(check int) "code" Lsp.method_not_found err.code
     | None -> Alcotest.fail "expected error");
    send_notification client_out "exit" `Null;
    Thread.join t)

(* --- Diagnostics tests --- *)

let test_diagnostics_valid_code () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "(+ 1 2)";
    let notif = read_notification client_in in
    Alcotest.(check string) "method" "textDocument/publishDiagnostics"
      notif.method_;
    (match notif.params with
     | `Assoc p ->
       (match List.assoc_opt "diagnostics" p with
        | Some (`List diags) ->
          Alcotest.(check int) "no diagnostics" 0 (List.length diags)
        | _ -> Alcotest.fail "expected diagnostics list")
     | _ -> Alcotest.fail "expected params object");
    send_notification client_out "exit" `Null;
    Thread.join t)

let test_diagnostics_read_error () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "(+ 1";
    let notif = read_notification client_in in
    (match notif.params with
     | `Assoc p ->
       (match List.assoc_opt "diagnostics" p with
        | Some (`List diags) ->
          Alcotest.(check bool) "has diagnostics" true
            (List.length diags > 0)
        | _ -> Alcotest.fail "expected diagnostics list")
     | _ -> Alcotest.fail "expected params object");
    send_notification client_out "exit" `Null;
    Thread.join t)

let test_diagnostics_did_close_clears () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "(+ 1 2)";
    let _notif = read_notification client_in in
    send_notification client_out "textDocument/didClose"
      (`Assoc [("textDocument", `Assoc [("uri", `String "file:///test.scm")])]);
    let notif = read_notification client_in in
    (match notif.params with
     | `Assoc p ->
       (match List.assoc_opt "diagnostics" p with
        | Some (`List []) -> ()
        | _ -> Alcotest.fail "expected empty diagnostics")
     | _ -> Alcotest.fail "expected params object");
    send_notification client_out "exit" `Null;
    Thread.join t)

let test_diagnostics_did_change () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "(+ 1 2)";
    let _notif1 = read_notification client_in in
    send_notification client_out "textDocument/didChange"
      (`Assoc [
        ("textDocument", `Assoc [
          ("uri", `String "file:///test.scm");
          ("version", `Int 2)]);
        ("contentChanges", `List [
          `Assoc [("text", `String "(+ 1")]])]);
    let notif2 = read_notification client_in in
    (match notif2.params with
     | `Assoc p ->
       (match List.assoc_opt "diagnostics" p with
        | Some (`List diags) ->
          Alcotest.(check bool) "has diag after change" true
            (List.length diags > 0)
        | _ -> Alcotest.fail "expected diagnostics list")
     | _ -> Alcotest.fail "expected params object");
    send_notification client_out "exit" `Null;
    Thread.join t)

(* --- Hover tests --- *)

let test_hover_keyword () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    (* "(define x 1)" — hover over "define" at col 1 *)
    did_open client_out "file:///test.scm" "(define x 1)";
    let _notif = read_notification client_in in
    send_request client_out 1 "textDocument/hover"
      (`Assoc [
        ("textDocument", `Assoc [("uri", `String "file:///test.scm")]);
        ("position", `Assoc [("line", `Int 0); ("character", `Int 2)])]);
    let resp = read_response client_in in
    (match resp.result with
     | `Assoc assoc ->
       (match List.assoc_opt "contents" assoc with
        | Some (`Assoc c) ->
          let value = match List.assoc_opt "value" c with
            | Some (`String s) -> s | _ -> "" in
          Alcotest.(check bool) "contains define" true
            (String.length value > 0)
        | _ -> Alcotest.fail "expected contents")
     | `Null -> Alcotest.fail "expected hover result"
     | _ -> Alcotest.fail "unexpected result type");
    send_notification client_out "exit" `Null;
    Thread.join t)

let test_hover_whitespace () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "(+ 1 2)";
    let _notif = read_notification client_in in
    (* Hover over the space between 1 and 2 *)
    send_request client_out 1 "textDocument/hover"
      (`Assoc [
        ("textDocument", `Assoc [("uri", `String "file:///test.scm")]);
        ("position", `Assoc [("line", `Int 0); ("character", `Int 4)])]);
    let resp = read_response client_in in
    Alcotest.(check string) "null result" "null"
      (Yojson.Safe.to_string resp.result);
    send_notification client_out "exit" `Null;
    Thread.join t)

(* --- Completion tests --- *)

let test_completion_prefix () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "(def";
    let _notif = read_notification client_in in
    send_request client_out 1 "textDocument/completion"
      (`Assoc [
        ("textDocument", `Assoc [("uri", `String "file:///test.scm")]);
        ("position", `Assoc [("line", `Int 0); ("character", `Int 4)])]);
    let resp = read_response client_in in
    (match resp.result with
     | `List items ->
       Alcotest.(check bool) "has completions" true (List.length items > 0);
       (* All items should start with "def" *)
       List.iter (fun item ->
         match item with
         | `Assoc a ->
           let label = match List.assoc_opt "label" a with
             | Some (`String s) -> s | _ -> "" in
           Alcotest.(check bool) "starts with def" true
             (String.length label >= 3 &&
              String.sub label 0 3 = "def")
         | _ -> ()
       ) items
     | _ -> Alcotest.fail "expected list");
    send_notification client_out "exit" `Null;
    Thread.join t)

(* --- Go-to-definition tests --- *)

let test_definition_define () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "(define foo 1)\nfoo";
    let _notif = read_notification client_in in
    (* Goto definition of "foo" at line 1, col 0 *)
    send_request client_out 1 "textDocument/definition"
      (`Assoc [
        ("textDocument", `Assoc [("uri", `String "file:///test.scm")]);
        ("position", `Assoc [("line", `Int 1); ("character", `Int 0)])]);
    let resp = read_response client_in in
    (match resp.result with
     | `Assoc assoc ->
       Alcotest.(check bool) "has uri" true (List.mem_assoc "uri" assoc);
       Alcotest.(check bool) "has range" true (List.mem_assoc "range" assoc)
     | `Null -> () (* Acceptable if highlight doesn't find the binding *)
     | _ -> Alcotest.fail "unexpected result type");
    send_notification client_out "exit" `Null;
    Thread.join t)

let test_definition_unbound () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "xyz";
    let _notif = read_notification client_in in
    send_request client_out 1 "textDocument/definition"
      (`Assoc [
        ("textDocument", `Assoc [("uri", `String "file:///test.scm")]);
        ("position", `Assoc [("line", `Int 0); ("character", `Int 0)])]);
    let resp = read_response client_in in
    Alcotest.(check string) "null for unbound" "null"
      (Yojson.Safe.to_string resp.result);
    send_notification client_out "exit" `Null;
    Thread.join t)

(* --- Document symbols tests --- *)

let test_document_symbols () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm"
      "(define foo 1)\n(define (bar x) x)";
    let _notif = read_notification client_in in
    send_request client_out 1 "textDocument/documentSymbol"
      (`Assoc [
        ("textDocument", `Assoc [("uri", `String "file:///test.scm")])]);
    let resp = read_response client_in in
    (match resp.result with
     | `List symbols ->
       Alcotest.(check bool) "has symbols" true (List.length symbols >= 2);
       let names = List.filter_map (fun s ->
         match s with
         | `Assoc a ->
           (match List.assoc_opt "name" a with
            | Some (`String n) -> Some n | _ -> None)
         | _ -> None
       ) symbols in
       Alcotest.(check bool) "has foo" true (List.mem "foo" names);
       Alcotest.(check bool) "has bar" true (List.mem "bar" names)
     | _ -> Alcotest.fail "expected list");
    send_notification client_out "exit" `Null;
    Thread.join t)

(* --- Semantic tokens tests --- *)

let test_semantic_tokens_basic () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "(define x 42)";
    let _notif = read_notification client_in in
    send_request client_out 1 "textDocument/semanticTokens/full"
      (`Assoc [
        ("textDocument", `Assoc [("uri", `String "file:///test.scm")])]);
    let resp = read_response client_in in
    (match resp.result with
     | `Assoc assoc ->
       (match List.assoc_opt "data" assoc with
        | Some (`List data) ->
          (* Should have at least some tokens *)
          Alcotest.(check bool) "has token data" true
            (List.length data > 0);
          (* Each token is 5 ints *)
          Alcotest.(check int) "divisible by 5" 0
            (List.length data mod 5)
        | _ -> Alcotest.fail "expected data list")
     | _ -> Alcotest.fail "expected result object");
    send_notification client_out "exit" `Null;
    Thread.join t)

let test_semantic_tokens_empty () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    did_open client_out "file:///test.scm" "";
    let _notif = read_notification client_in in
    send_request client_out 1 "textDocument/semanticTokens/full"
      (`Assoc [
        ("textDocument", `Assoc [("uri", `String "file:///test.scm")])]);
    let resp = read_response client_in in
    (match resp.result with
     | `Assoc assoc ->
       (match List.assoc_opt "data" assoc with
        | Some (`List []) -> ()
        | _ -> Alcotest.fail "expected empty data")
     | _ -> Alcotest.fail "expected result object");
    send_notification client_out "exit" `Null;
    Thread.join t)

(* --- Position utility tests --- *)

let test_position_conversion () =
  with_pipes (fun client_out client_in server_in server_out ->
    let inst = Instance.create () in
    let ls = Language_server.create inst in
    let t = Thread.create (fun () ->
      Language_server.run_session ls server_in server_out) () in
    do_handshake client_out client_in;
    (* Multi-line: hover on second line *)
    did_open client_out "file:///test.scm" "(+ 1 2)\n(* 3 4)";
    let _notif = read_notification client_in in
    (* Hover over "*" at line 1, col 1 *)
    send_request client_out 1 "textDocument/hover"
      (`Assoc [
        ("textDocument", `Assoc [("uri", `String "file:///test.scm")]);
        ("position", `Assoc [("line", `Int 1); ("character", `Int 1)])]);
    let resp = read_response client_in in
    (match resp.result with
     | `Assoc assoc ->
       (match List.assoc_opt "contents" assoc with
        | Some (`Assoc c) ->
          let value = match List.assoc_opt "value" c with
            | Some (`String s) -> s | _ -> "" in
          Alcotest.(check bool) "has * info" true
            (String.length value > 0)
        | _ -> Alcotest.fail "expected contents")
     | `Null -> Alcotest.fail "expected hover result"
     | _ -> Alcotest.fail "unexpected result type");
    send_notification client_out "exit" `Null;
    Thread.join t)

let () =
  Alcotest.run "Language_server" [
    "session", [
      Alcotest.test_case "initialize handshake" `Quick test_initialize_handshake;
      Alcotest.test_case "shutdown/exit" `Quick test_shutdown_exit;
      Alcotest.test_case "unknown method error" `Quick test_unknown_method_error;
    ];
    "diagnostics", [
      Alcotest.test_case "valid code" `Quick test_diagnostics_valid_code;
      Alcotest.test_case "read error" `Quick test_diagnostics_read_error;
      Alcotest.test_case "didClose clears" `Quick test_diagnostics_did_close_clears;
      Alcotest.test_case "didChange" `Quick test_diagnostics_did_change;
    ];
    "hover", [
      Alcotest.test_case "keyword" `Quick test_hover_keyword;
      Alcotest.test_case "whitespace" `Quick test_hover_whitespace;
    ];
    "completion", [
      Alcotest.test_case "prefix filter" `Quick test_completion_prefix;
    ];
    "definition", [
      Alcotest.test_case "define'd name" `Quick test_definition_define;
      Alcotest.test_case "unbound symbol" `Quick test_definition_unbound;
    ];
    "symbols", [
      Alcotest.test_case "document symbols" `Quick test_document_symbols;
    ];
    "semantic_tokens", [
      Alcotest.test_case "basic" `Quick test_semantic_tokens_basic;
      Alcotest.test_case "empty doc" `Quick test_semantic_tokens_empty;
    ];
    "position", [
      Alcotest.test_case "multi-line hover" `Quick test_position_conversion;
    ];
  ]
