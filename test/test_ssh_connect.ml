open Bilk

(* --- parse_connect_line --- *)

let test_parse_valid () =
  let line = "BILK CONNECT 7890 abc123def" in
  match Ssh_connect.parse_connect_line line with
  | Some info ->
    Alcotest.(check int) "port" 7890 info.port;
    Alcotest.(check string) "key" "abc123def" info.key
  | None ->
    Alcotest.fail "expected Some"

let test_parse_wrong_prefix () =
  let line = "SSH CONNECT 7890 abc123" in
  Alcotest.(check bool) "wrong prefix" true
    (Ssh_connect.parse_connect_line line = None)

let test_parse_missing_key () =
  let line = "BILK CONNECT 7890" in
  Alcotest.(check bool) "missing key" true
    (Ssh_connect.parse_connect_line line = None)

let test_parse_non_numeric_port () =
  let line = "BILK CONNECT abc key123" in
  Alcotest.(check bool) "non-numeric port" true
    (Ssh_connect.parse_connect_line line = None)

let test_parse_port_zero () =
  let line = "BILK CONNECT 0 key123" in
  Alcotest.(check bool) "port zero" true
    (Ssh_connect.parse_connect_line line = None)

let test_parse_port_negative () =
  let line = "BILK CONNECT -1 key123" in
  Alcotest.(check bool) "negative port" true
    (Ssh_connect.parse_connect_line line = None)

let test_parse_port_too_high () =
  let line = "BILK CONNECT 65536 key123" in
  Alcotest.(check bool) "port > 65535" true
    (Ssh_connect.parse_connect_line line = None)

let test_parse_port_max_valid () =
  let line = "BILK CONNECT 65535 key123" in
  match Ssh_connect.parse_connect_line line with
  | Some info -> Alcotest.(check int) "port" 65535 info.port
  | None -> Alcotest.fail "expected Some for port 65535"

let test_parse_port_min_valid () =
  let line = "BILK CONNECT 1 key123" in
  match Ssh_connect.parse_connect_line line with
  | Some info -> Alcotest.(check int) "port" 1 info.port
  | None -> Alcotest.fail "expected Some for port 1"

let test_parse_extra_whitespace () =
  let line = "BILK CONNECT  7890  abc123" in
  match Ssh_connect.parse_connect_line line with
  | Some info ->
    Alcotest.(check int) "port" 7890 info.port;
    Alcotest.(check string) "key" "abc123" info.key
  | None ->
    Alcotest.fail "expected Some (extra whitespace)"

(* --- parse_target --- *)

let test_target_valid () =
  match Ssh_connect.parse_target "user@host.example.com" with
  | Some (user, host) ->
    Alcotest.(check string) "user" "user" user;
    Alcotest.(check string) "host" "host.example.com" host
  | None ->
    Alcotest.fail "expected Some"

let test_target_no_at () =
  Alcotest.(check bool) "no @" true
    (Ssh_connect.parse_target "hostname" = None)

let test_target_multiple_at () =
  Alcotest.(check bool) "multiple @" true
    (Ssh_connect.parse_target "a@b@c" = None)

(* --- scan_for_connect --- *)

let test_scan_found () =
  let lines = [
    "Loading bilk...";
    "BILK CONNECT 9999 mykey";
    "Server ready.";
  ] in
  match Ssh_connect.scan_for_connect lines with
  | Ok info ->
    Alcotest.(check int) "port" 9999 info.port;
    Alcotest.(check string) "key" "mykey" info.key
  | Error _ ->
    Alcotest.fail "expected Ok"

let test_scan_not_found () =
  let lines = [
    "bash: bilk: command not found";
  ] in
  match Ssh_connect.scan_for_connect lines with
  | Error (Ssh_connect.No_bilk_on_remote _) -> ()
  | _ -> Alcotest.fail "expected No_bilk_on_remote"

let test_scan_empty_lines () =
  match Ssh_connect.scan_for_connect [] with
  | Error (Ssh_connect.No_bilk_on_remote _) -> ()
  | _ -> Alcotest.fail "expected No_bilk_on_remote"

(* --- QCheck2 property: roundtrip --- *)

let test_roundtrip_property =
  let open QCheck2 in
  Test.make ~name:"parse_connect_line roundtrip"
    ~count:100
    (Gen.pair
       (Gen.int_range 1 65535)
       (Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 20)))
    (fun (port, key) ->
       let line = Printf.sprintf "BILK CONNECT %d %s" port key in
       match Ssh_connect.parse_connect_line line with
       | Some info -> info.port = port && info.key = key
       | None -> false)

(* --- build_serve_args --- *)

let test_default_config () =
  let config = {
    Ssh_connect.port = 7890;
    auto_checkpoint = false;
    name = "default";
    session_timeout = 86400;
  } in
  let args = Ssh_connect.build_serve_args config in
  Alcotest.(check bool) "has --port" true (List.mem "--port" args);
  Alcotest.(check bool) "has 7890" true (List.mem "7890" args)

let test_auto_checkpoint_flag () =
  let config = {
    Ssh_connect.port = 7890;
    auto_checkpoint = true;
    name = "default";
    session_timeout = 86400;
  } in
  let args = Ssh_connect.build_serve_args config in
  Alcotest.(check bool) "has --auto-checkpoint" true
    (List.mem "--auto-checkpoint" args)

let test_name_flag () =
  let config = {
    Ssh_connect.port = 7890;
    auto_checkpoint = false;
    name = "myrepl";
    session_timeout = 86400;
  } in
  let args = Ssh_connect.build_serve_args config in
  Alcotest.(check bool) "has --name" true (List.mem "--name" args);
  Alcotest.(check bool) "has myrepl" true (List.mem "myrepl" args)

let test_session_timeout_flag () =
  let config = {
    Ssh_connect.port = 7890;
    auto_checkpoint = false;
    name = "default";
    session_timeout = 3600;
  } in
  let args = Ssh_connect.build_serve_args config in
  Alcotest.(check bool) "has --session-timeout" true
    (List.mem "--session-timeout" args);
  Alcotest.(check bool) "has 3600" true (List.mem "3600" args)

let prop_no_insecure_or_bind =
  let open QCheck2 in
  Test.make ~name:"build_serve_args never contains --insecure or --bind"
    ~count:200
    (Gen.quad
       (Gen.int_range 1 65535)
       Gen.bool
       (Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 20))
       (Gen.int_range 1 604800))
    (fun (port, auto_checkpoint, name, session_timeout) ->
       let config = { Ssh_connect.port; auto_checkpoint; name; session_timeout } in
       let args = Ssh_connect.build_serve_args config in
       not (List.mem "--insecure" args) && not (List.mem "--bind" args))

(* --- Security: adversarial inputs --- *)

let test_parse_flag_injection_port () =
  (* Port field containing shell metacharacters *)
  let line = "BILK CONNECT --port=7890 key" in
  Alcotest.(check bool) "flag in port rejected" true
    (Ssh_connect.parse_connect_line line = None)

let test_parse_flag_injection_key () =
  (* Key field with flag-like content — should still parse (key is opaque) *)
  let line = "BILK CONNECT 7890 --insecure" in
  match Ssh_connect.parse_connect_line line with
  | Some info ->
    Alcotest.(check int) "port" 7890 info.port;
    Alcotest.(check string) "key" "--insecure" info.key
  | None -> Alcotest.fail "key --insecure should be accepted as opaque string"

let test_parse_extra_fields_rejected () =
  let line = "BILK CONNECT 7890 key extra" in
  Alcotest.(check bool) "extra fields rejected" true
    (Ssh_connect.parse_connect_line line = None)

let test_target_empty_user () =
  Alcotest.(check bool) "empty user rejected" true
    (Ssh_connect.parse_target "@host" = None)

let test_target_empty_host () =
  Alcotest.(check bool) "empty host rejected" true
    (Ssh_connect.parse_target "user@" = None)

let test_scan_invalid_port_in_connect () =
  let lines = ["BILK CONNECT 99999 mykey"] in
  match Ssh_connect.scan_for_connect lines with
  | Error (Ssh_connect.Invalid_connect_line _) -> ()
  | _ -> Alcotest.fail "expected Invalid_connect_line for bad port"

let () =
  Alcotest.run "Ssh_connect"
    [ ("parse_connect_line",
       [ Alcotest.test_case "valid" `Quick test_parse_valid
       ; Alcotest.test_case "wrong prefix" `Quick test_parse_wrong_prefix
       ; Alcotest.test_case "missing key" `Quick test_parse_missing_key
       ; Alcotest.test_case "non-numeric port" `Quick test_parse_non_numeric_port
       ; Alcotest.test_case "port zero" `Quick test_parse_port_zero
       ; Alcotest.test_case "negative port" `Quick test_parse_port_negative
       ; Alcotest.test_case "port > 65535" `Quick test_parse_port_too_high
       ; Alcotest.test_case "port 65535" `Quick test_parse_port_max_valid
       ; Alcotest.test_case "port 1" `Quick test_parse_port_min_valid
       ; Alcotest.test_case "extra whitespace" `Quick test_parse_extra_whitespace
       ])
    ; ("parse_target",
       [ Alcotest.test_case "valid" `Quick test_target_valid
       ; Alcotest.test_case "no @" `Quick test_target_no_at
       ; Alcotest.test_case "multiple @" `Quick test_target_multiple_at
       ])
    ; ("scan_for_connect",
       [ Alcotest.test_case "found" `Quick test_scan_found
       ; Alcotest.test_case "not found" `Quick test_scan_not_found
       ; Alcotest.test_case "empty lines" `Quick test_scan_empty_lines
       ])
    ; ("property",
       [ QCheck_alcotest.to_alcotest test_roundtrip_property
       ])
    ; ("adversarial",
       [ Alcotest.test_case "flag injection port" `Quick
           test_parse_flag_injection_port
       ; Alcotest.test_case "flag-like key accepted" `Quick
           test_parse_flag_injection_key
       ; Alcotest.test_case "extra fields rejected" `Quick
           test_parse_extra_fields_rejected
       ; Alcotest.test_case "empty user rejected" `Quick
           test_target_empty_user
       ; Alcotest.test_case "empty host rejected" `Quick
           test_target_empty_host
       ; Alcotest.test_case "invalid port in scan" `Quick
           test_scan_invalid_port_in_connect
       ])
    ; ("build_serve_args",
       [ Alcotest.test_case "default config" `Quick test_default_config
       ; Alcotest.test_case "auto-checkpoint" `Quick test_auto_checkpoint_flag
       ; Alcotest.test_case "name" `Quick test_name_flag
       ; Alcotest.test_case "session-timeout" `Quick test_session_timeout_flag
       ; QCheck_alcotest.to_alcotest prop_no_insecure_or_bind
       ])
    ]
