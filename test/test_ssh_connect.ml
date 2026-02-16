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

let () =
  Alcotest.run "Ssh_connect"
    [ ("parse_connect_line",
       [ Alcotest.test_case "valid" `Quick test_parse_valid
       ; Alcotest.test_case "wrong prefix" `Quick test_parse_wrong_prefix
       ; Alcotest.test_case "missing key" `Quick test_parse_missing_key
       ; Alcotest.test_case "non-numeric port" `Quick test_parse_non_numeric_port
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
    ]
