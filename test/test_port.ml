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

(* --- Output port tests --- *)

let test_output_string_basic () =
  let p = Port.open_output_string () in
  Port.write_char p 'a';
  Port.write_char p 'b';
  Port.write_char p 'c';
  Alcotest.(check string) "abc" "abc" (Port.get_output_string p)

let test_output_string_write_string () =
  let p = Port.open_output_string () in
  Port.write_string p "hello";
  Port.write_string p " world";
  Alcotest.(check string) "hello world" "hello world" (Port.get_output_string p)

let test_output_string_write_uchar () =
  let p = Port.open_output_string () in
  Port.write_uchar p (Uchar.of_int 0x03BB);  (* lambda *)
  Alcotest.(check string) "lambda" "\xCE\xBB" (Port.get_output_string p)

let test_output_string_close () =
  let p = Port.open_output_string () in
  Port.write_string p "test";
  Alcotest.(check bool) "open" true (Port.is_open p);
  Port.close p;
  Alcotest.(check bool) "closed" false (Port.is_open p)

let test_output_channel () =
  let tmp = Filename.temp_file "wile_test" ".out" in
  let p = Port.open_output_file tmp in
  Port.write_string p "hello file";
  Port.close p;
  let content = In_channel.with_open_bin tmp In_channel.input_all in
  Alcotest.(check string) "file content" "hello file" content;
  Sys.remove tmp

let test_read_line_basic () =
  let p = Port.of_string "hello\nworld" in
  Alcotest.(check (option string)) "line 1" (Some "hello") (Port.read_line p);
  Alcotest.(check (option string)) "line 2" (Some "world") (Port.read_line p);
  Alcotest.(check (option string)) "eof" None (Port.read_line p)

let test_read_line_newline () =
  let p = Port.of_string "abc\n\n" in
  Alcotest.(check (option string)) "line" (Some "abc") (Port.read_line p);
  Alcotest.(check (option string)) "empty after newline" (Some "") (Port.read_line p);
  Alcotest.(check (option string)) "eof" None (Port.read_line p)

let test_read_line_eof () =
  let p = Port.of_string "" in
  Alcotest.(check (option string)) "eof" None (Port.read_line p)

let test_read_u8 () =
  let p = Port.of_string "\x41\x42\x43" in
  Alcotest.(check (option int)) "A" (Some 0x41) (Port.read_u8 p);
  Alcotest.(check (option int)) "B" (Some 0x42) (Port.read_u8 p);
  Alcotest.(check (option int)) "C" (Some 0x43) (Port.read_u8 p);
  Alcotest.(check (option int)) "eof" None (Port.read_u8 p)

let test_peek_u8 () =
  let p = Port.of_string "\xFF" in
  Alcotest.(check (option int)) "peek" (Some 0xFF) (Port.peek_u8 p);
  Alcotest.(check (option int)) "peek again" (Some 0xFF) (Port.peek_u8 p);
  ignore (Port.read_u8 p);
  Alcotest.(check (option int)) "eof" None (Port.peek_u8 p)

let test_is_input_output () =
  let ip = Port.of_string "x" in
  let op = Port.open_output_string () in
  Alcotest.(check bool) "input is input" true (Port.is_input ip);
  Alcotest.(check bool) "input not output" false (Port.is_output ip);
  Alcotest.(check bool) "output is output" true (Port.is_output op);
  Alcotest.(check bool) "output not input" false (Port.is_input op)

let test_close_prevents_io () =
  let p = Port.of_string "abc" in
  Port.close p;
  Alcotest.check_raises "read after close"
    (Failure "read-char: port is closed")
    (fun () -> ignore (Port.read_char p))

let test_write_to_input_fails () =
  let p = Port.of_string "abc" in
  Alcotest.check_raises "write to input"
    (Failure "write-string: not an output port")
    (fun () -> Port.write_string p "x")

let test_read_from_output_fails () =
  let p = Port.open_output_string () in
  Alcotest.check_raises "read from output"
    (Failure "peek-char: not an input port")
    (fun () -> ignore (Port.peek_char p))

let test_write_u8 () =
  let p = Port.open_output_string () in
  Port.write_u8 p 0x41;
  Port.write_u8 p 0x42;
  Alcotest.(check string) "AB" "AB" (Port.get_output_string p)

let test_write_bytes () =
  let p = Port.open_output_string () in
  let data = Bytes.of_string "hello world" in
  Port.write_bytes p data 6 5;
  Alcotest.(check string) "world" "world" (Port.get_output_string p)

let test_file_name () =
  let p = Port.of_string ~file:"test.scm" "x" in
  Alcotest.(check string) "file name" "test.scm" (Port.file_name p);
  let op = Port.open_output_string () in
  Alcotest.(check string) "string output" "<string>" (Port.file_name op)

let test_open_input_file () =
  let tmp = Filename.temp_file "wile_test" ".scm" in
  let () =
    let oc = open_out tmp in
    output_string oc "abc";
    close_out oc
  in
  let p = Port.open_input_file tmp in
  Alcotest.(check (option char)) "first char" (Some 'a') (Port.read_char p);
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
    ; ("Output",
       [ Alcotest.test_case "string basic" `Quick test_output_string_basic
       ; Alcotest.test_case "write_string" `Quick test_output_string_write_string
       ; Alcotest.test_case "write_uchar" `Quick test_output_string_write_uchar
       ; Alcotest.test_case "close" `Quick test_output_string_close
       ; Alcotest.test_case "channel" `Quick test_output_channel
       ; Alcotest.test_case "write_u8" `Quick test_write_u8
       ; Alcotest.test_case "write_bytes" `Quick test_write_bytes
       ])
    ; ("Read ops",
       [ Alcotest.test_case "read_line basic" `Quick test_read_line_basic
       ; Alcotest.test_case "read_line newline" `Quick test_read_line_newline
       ; Alcotest.test_case "read_line eof" `Quick test_read_line_eof
       ; Alcotest.test_case "read_u8" `Quick test_read_u8
       ; Alcotest.test_case "peek_u8" `Quick test_peek_u8
       ])
    ; ("Predicates",
       [ Alcotest.test_case "is_input/is_output" `Quick test_is_input_output
       ; Alcotest.test_case "close prevents io" `Quick test_close_prevents_io
       ; Alcotest.test_case "write to input" `Quick test_write_to_input_fails
       ; Alcotest.test_case "read from output" `Quick test_read_from_output_fails
       ; Alcotest.test_case "file_name" `Quick test_file_name
       ; Alcotest.test_case "open_input_file" `Quick test_open_input_file
       ])
    ]
