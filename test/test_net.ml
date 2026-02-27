open Bilk

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let eval_multi inst s =
  let port = Port.of_string s in
  Instance.eval_port inst port

(* ------------------------------------------------------------------ *)
(* TCP connect + listen/accept round-trip                             *)
(* ------------------------------------------------------------------ *)

let test_tcp_round_trip () =
  (* Use a dedicated instance for the threaded test *)
  let inst = Instance.create () in
  ignore (eval_multi inst "(import (bilk net))");
  (* Set up a listener on ephemeral port *)
  ignore (eval_multi inst "(define srv (tcp-listen 0))");
  let bound_port = match eval_multi inst "(tcp-listener-port srv)" with
    | Datum.Fixnum n -> n
    | _ -> Alcotest.fail "expected fixnum for bound port"
  in
  Alcotest.(check bool) "bound port > 0" true (bound_port > 0);
  (* Accept in a thread *)
  let server_result = ref Datum.Void in
  let server_thread = Thread.create (fun () ->
    ignore (eval_multi inst "(define sc (tcp-accept srv))");
    let data = eval_multi inst "(read-line (car sc))" in
    server_result := data;
    ignore (eval_multi inst {|(write-string "pong\n" (cdr sc))|});
    ignore (eval_multi inst "(flush-output-port (cdr sc))");
    ignore (eval_multi inst "(close-input-port (car sc))");
    ignore (eval_multi inst "(close-output-port (cdr sc))")
  ) () in
  (* Client connects *)
  let client_inst = Instance.create () in
  ignore (eval_multi client_inst "(import (bilk net))");
  ignore (eval_multi client_inst
    (Printf.sprintf {|(define cc (tcp-connect "127.0.0.1" %d))|} bound_port));
  ignore (eval_multi client_inst {|(write-string "ping\n" (cdr cc))|});
  ignore (eval_multi client_inst "(flush-output-port (cdr cc))");
  let client_data = eval_multi client_inst "(read-line (car cc))" in
  ignore (eval_multi client_inst "(close-input-port (car cc))");
  ignore (eval_multi client_inst "(close-output-port (cdr cc))");
  Thread.join server_thread;
  (* Verify round-trip *)
  Alcotest.check datum_testable "server received ping"
    (Datum.Str (Bytes.of_string "ping")) !server_result;
  Alcotest.check datum_testable "client received pong"
    (Datum.Str (Bytes.of_string "pong")) client_data;
  (* Close listener *)
  ignore (eval_multi inst "(tcp-close srv)")

(* ------------------------------------------------------------------ *)
(* TCP listener predicates                                            *)
(* ------------------------------------------------------------------ *)

let test_tcp_listener_predicates () =
  let inst = Instance.create () in
  ignore (eval_multi inst "(import (bilk net))");
  Alcotest.check datum_testable "tcp-listener? #t for listener"
    (Datum.Bool true) (eval_multi inst
      "(define l (tcp-listen 0)) (tcp-listener? l)");
  ignore (eval_multi inst "(tcp-close l)");
  Alcotest.check datum_testable "tcp-listener? #f for fixnum"
    (Datum.Bool false) (eval_multi inst "(tcp-listener? 42)");
  Alcotest.check datum_testable "tcp-listener? #f for string"
    (Datum.Bool false) (eval_multi inst {|(tcp-listener? "hello")|})

(* ------------------------------------------------------------------ *)
(* UDP round-trip                                                     *)
(* ------------------------------------------------------------------ *)

let test_udp_round_trip () =
  let inst = Instance.create () in
  ignore (eval_multi inst "(import (bilk net))");
  (* Open two UDP sockets *)
  ignore (eval_multi inst "(define s1 (udp-open))");
  ignore (eval_multi inst "(define s2 (udp-open))");
  (* Bind receiver to ephemeral port *)
  ignore (eval_multi inst {|(udp-bind! s1 "127.0.0.1" 0)|});
  (* Get bound port via udp-local-address *)
  let recv_port = match eval_multi inst
    "(cdr (udp-local-address s1))" with
    | Datum.Fixnum n -> n
    | other -> Alcotest.fail
        (Printf.sprintf "expected fixnum port, got %s" (Datum.to_string other))
  in
  Alcotest.(check bool) "recv port > 0" true (recv_port > 0);
  (* Send from s2 to s1 *)
  ignore (eval_multi inst
    (Printf.sprintf {|(udp-send-to s2 (string->utf8 "hello") "127.0.0.1" %d)|}
       recv_port));
  (* Receive on s1: returns (bytevector . (address . port)) *)
  ignore (eval_multi inst "(define pkt (udp-receive s1 1024))");
  let bv = eval_multi inst "(car pkt)" in
  Alcotest.check datum_testable "received data"
    (Datum.Bytevector (Bytes.of_string "hello")) bv;
  (* Predicates *)
  Alcotest.check datum_testable "udp-socket? s1"
    (Datum.Bool true) (eval_multi inst "(udp-socket? s1)");
  Alcotest.check datum_testable "udp-socket? #f for fixnum"
    (Datum.Bool false) (eval_multi inst "(udp-socket? 42)");
  (* Close *)
  ignore (eval_multi inst "(udp-close s1)");
  ignore (eval_multi inst "(udp-close s2)")

(* ------------------------------------------------------------------ *)
(* DNS resolution                                                     *)
(* ------------------------------------------------------------------ *)

let test_dns_resolve_localhost () =
  let inst = Instance.create () in
  ignore (eval_multi inst "(import (bilk net))");
  let result = eval_multi inst {|(net-resolve "localhost")|} in
  let s = match result with
    | Datum.Str bs -> Bytes.to_string bs
    | other -> Alcotest.fail
        (Printf.sprintf "expected string, got %s" (Datum.to_string other))
  in
  Alcotest.(check bool) "localhost resolves to 127.0.0.1 or ::1"
    true (s = "127.0.0.1" || s = "::1");
  (* net-resolve-all returns a list *)
  let all = eval_multi inst {|(net-resolve-all "localhost")|} in
  let is_list = match Datum.to_list all with
    | Some (_ :: _) -> true
    | _ -> false
  in
  Alcotest.(check bool) "net-resolve-all returns non-empty list" true is_list

(* ------------------------------------------------------------------ *)
(* Error cases                                                        *)
(* ------------------------------------------------------------------ *)

let test_tcp_connect_refused () =
  let inst = Instance.create () in
  ignore (eval_multi inst "(import (bilk net))");
  let raised = ref false in
  (try
     ignore (eval_multi inst {|(tcp-connect "127.0.0.1" 1)|})
   with
   | Vm.Runtime_error _ -> raised := true);
  Alcotest.(check bool) "connect to port 1 raises error" true !raised

let test_tcp_double_close () =
  let inst = Instance.create () in
  ignore (eval_multi inst "(import (bilk net))");
  ignore (eval_multi inst "(define l (tcp-listen 0))");
  ignore (eval_multi inst "(tcp-close l)");
  (* Double close should not raise *)
  ignore (eval_multi inst "(tcp-close l)")

let test_type_mismatch () =
  let inst = Instance.create () in
  ignore (eval_multi inst "(import (bilk net))");
  let raised = ref false in
  (try ignore (eval_multi inst "(tcp-accept 42)")
   with Vm.Runtime_error _ -> raised := true);
  Alcotest.(check bool) "tcp-accept on non-listener raises" true !raised

(* ------------------------------------------------------------------ *)
(* Test runner                                                        *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Net"
    [ ("TCP",
       [ Alcotest.test_case "round-trip" `Quick test_tcp_round_trip
       ; Alcotest.test_case "predicates" `Quick test_tcp_listener_predicates
       ; Alcotest.test_case "connect refused" `Quick test_tcp_connect_refused
       ; Alcotest.test_case "double close" `Quick test_tcp_double_close
       ; Alcotest.test_case "type mismatch" `Quick test_type_mismatch
       ])
    ; ("UDP",
       [ Alcotest.test_case "round-trip" `Quick test_udp_round_trip
       ])
    ; ("DNS",
       [ Alcotest.test_case "resolve localhost" `Quick test_dns_resolve_localhost
       ])
    ]
