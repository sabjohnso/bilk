open Wile

let loc_testable =
  Alcotest.testable Loc.pp Loc.equal

let check_loc = Alcotest.check loc_testable

let test_port_empty () =
  let p = Port.of_string "" in
  Alcotest.(check (option char)) "peek empty" None (Port.peek_char p);
  Alcotest.(check (option char)) "read empty" None (Port.read_char p)

let test_port_single_char () =
  let p = Port.of_string "x" in
  Alcotest.(check (option char)) "peek" (Some 'x') (Port.peek_char p);
  Alcotest.(check (option char)) "read" (Some 'x') (Port.read_char p);
  Alcotest.(check (option char)) "peek after" None (Port.peek_char p);
  Alcotest.(check (option char)) "read after" None (Port.read_char p)

let test_port_peek_idempotent () =
  let p = Port.of_string "abc" in
  Alcotest.(check (option char)) "peek 1" (Some 'a') (Port.peek_char p);
  Alcotest.(check (option char)) "peek 2" (Some 'a') (Port.peek_char p);
  Alcotest.(check (option char)) "peek 3" (Some 'a') (Port.peek_char p);
  ignore (Port.read_char p);
  Alcotest.(check (option char)) "peek after read" (Some 'b') (Port.peek_char p)

let test_port_location_basic () =
  let p = Port.of_string "ab" in
  check_loc "initial" (Loc.make "<string>" 1 1) (Port.current_loc p);
  ignore (Port.read_char p);
  check_loc "after a" (Loc.make "<string>" 1 2) (Port.current_loc p);
  ignore (Port.read_char p);
  check_loc "after b" (Loc.make "<string>" 1 3) (Port.current_loc p)

let test_port_location_newline () =
  let p = Port.of_string "a\nb" in
  check_loc "initial" (Loc.make "<string>" 1 1) (Port.current_loc p);
  ignore (Port.read_char p);  (* a *)
  check_loc "after a" (Loc.make "<string>" 1 2) (Port.current_loc p);
  ignore (Port.read_char p);  (* \n *)
  check_loc "after newline" (Loc.make "<string>" 2 1) (Port.current_loc p);
  ignore (Port.read_char p);  (* b *)
  check_loc "after b" (Loc.make "<string>" 2 2) (Port.current_loc p)

let test_port_location_crlf () =
  let p = Port.of_string "a\r\nb" in
  check_loc "initial" (Loc.make "<string>" 1 1) (Port.current_loc p);
  ignore (Port.read_char p);  (* a *)
  check_loc "after a" (Loc.make "<string>" 1 2) (Port.current_loc p);
  ignore (Port.read_char p);  (* \r\n treated as one *)
  check_loc "after crlf" (Loc.make "<string>" 2 1) (Port.current_loc p);
  ignore (Port.read_char p);  (* b *)
  check_loc "after b" (Loc.make "<string>" 2 2) (Port.current_loc p)

let test_port_location_cr_alone () =
  let p = Port.of_string "a\rb" in
  check_loc "initial" (Loc.make "<string>" 1 1) (Port.current_loc p);
  ignore (Port.read_char p);  (* a *)
  check_loc "after a" (Loc.make "<string>" 1 2) (Port.current_loc p);
  ignore (Port.read_char p);  (* \r alone *)
  check_loc "after cr" (Loc.make "<string>" 2 1) (Port.current_loc p);
  ignore (Port.read_char p);  (* b *)
  check_loc "after b" (Loc.make "<string>" 2 2) (Port.current_loc p)

let test_port_custom_file () =
  let p = Port.of_string ~file:"test.scm" "x" in
  check_loc "custom file" (Loc.make "test.scm" 1 1) (Port.current_loc p)

let test_port_multiple_lines () =
  let p = Port.of_string "ab\ncd\nef" in
  (* Read through to end, checking locations at key points *)
  ignore (Port.read_char p); ignore (Port.read_char p); (* ab *)
  ignore (Port.read_char p); (* \n *)
  check_loc "start of line 2" (Loc.make "<string>" 2 1) (Port.current_loc p);
  ignore (Port.read_char p); ignore (Port.read_char p); (* cd *)
  ignore (Port.read_char p); (* \n *)
  check_loc "start of line 3" (Loc.make "<string>" 3 1) (Port.current_loc p)

(* QCheck tests *)
let qcheck_roundtrip =
  QCheck2.Test.make ~name:"read all chars reconstructs string"
    ~count:100
    QCheck2.Gen.string
    (fun s ->
      let p = Port.of_string s in
      let buf = Buffer.create (String.length s) in
      let rec loop () =
        match Port.read_char p with
        | None -> ()
        | Some c -> Buffer.add_char buf c; loop ()
      in
      loop ();
      (* Note: \r\n becomes just \r in output since read_char consumes both *)
      let expected =
        let b = Buffer.create (String.length s) in
        let i = ref 0 in
        while !i < String.length s do
          let c = s.[!i] in
          if c = '\r' && !i + 1 < String.length s && s.[!i + 1] = '\n' then begin
            Buffer.add_char b '\r';
            i := !i + 2
          end else begin
            Buffer.add_char b c;
            i := !i + 1
          end
        done;
        Buffer.contents b
      in
      Buffer.contents buf = expected)

let test_of_file_basic () =
  let tmp = Filename.temp_file "wile_test" ".scm" in
  let () =
    let oc = open_out tmp in
    output_string oc "(+ 1 2)";
    close_out oc
  in
  let p = Port.of_file tmp in
  check_loc "file name" (Loc.make tmp 1 1) (Port.current_loc p);
  Alcotest.(check (option char)) "first char" (Some '(') (Port.peek_char p);
  Sys.remove tmp

let test_of_file_not_found () =
  Alcotest.check_raises "sys_error"
    (Sys_error "/tmp/nonexistent_wile_test.scm: No such file or directory")
    (fun () -> ignore (Port.of_file "/tmp/nonexistent_wile_test.scm"))

let test_of_file_loc () =
  let tmp = Filename.temp_file "wile_test" ".scm" in
  let () =
    let oc = open_out tmp in
    output_string oc "a\nb";
    close_out oc
  in
  let p = Port.of_file tmp in
  ignore (Port.read_char p);  (* a *)
  ignore (Port.read_char p);  (* \n *)
  check_loc "line 2" (Loc.make tmp 2 1) (Port.current_loc p);
  Sys.remove tmp

let () =
  Alcotest.run "Port"
    [ ("Port",
       [ Alcotest.test_case "empty" `Quick test_port_empty
       ; Alcotest.test_case "single char" `Quick test_port_single_char
       ; Alcotest.test_case "peek idempotent" `Quick test_port_peek_idempotent
       ; Alcotest.test_case "location basic" `Quick test_port_location_basic
       ; Alcotest.test_case "location newline" `Quick test_port_location_newline
       ; Alcotest.test_case "location crlf" `Quick test_port_location_crlf
       ; Alcotest.test_case "location cr alone" `Quick test_port_location_cr_alone
       ; Alcotest.test_case "custom file" `Quick test_port_custom_file
       ; Alcotest.test_case "multiple lines" `Quick test_port_multiple_lines
       ; QCheck_alcotest.to_alcotest qcheck_roundtrip
       ])
    ; ("File",
       [ Alcotest.test_case "of_file basic" `Quick test_of_file_basic
       ; Alcotest.test_case "of_file not found" `Quick test_of_file_not_found
       ; Alcotest.test_case "of_file loc" `Quick test_of_file_loc
       ])
    ]
