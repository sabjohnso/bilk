open Wile

let loc_testable =
  Alcotest.testable Loc.pp Loc.equal

let check_loc = Alcotest.check loc_testable

let test_loc_make () =
  let loc = Loc.make "test.scm" 10 5 in
  Alcotest.(check string) "file" "test.scm" loc.file;
  Alcotest.(check int) "line" 10 loc.line;
  Alcotest.(check int) "col" 5 loc.col

let test_loc_none () =
  Alcotest.(check string) "file" "<unknown>" Loc.none.file;
  Alcotest.(check int) "line" 0 Loc.none.line;
  Alcotest.(check int) "col" 0 Loc.none.col

let test_loc_to_string () =
  let loc = Loc.make "foo.scm" 3 7 in
  Alcotest.(check string) "to_string" "foo.scm:3:7" (Loc.to_string loc);
  Alcotest.(check string) "none" "<unknown>:0:0" (Loc.to_string Loc.none)

let test_loc_equal () =
  let a = Loc.make "a.scm" 1 1 in
  let b = Loc.make "a.scm" 1 1 in
  let c = Loc.make "b.scm" 1 1 in
  let d = Loc.make "a.scm" 2 1 in
  let e = Loc.make "a.scm" 1 2 in
  check_loc "same" a b;
  Alcotest.(check bool) "diff file" false (Loc.equal a c);
  Alcotest.(check bool) "diff line" false (Loc.equal a d);
  Alcotest.(check bool) "diff col" false (Loc.equal a e)

let () =
  Alcotest.run "Loc"
    [ ("Loc",
       [ Alcotest.test_case "make" `Quick test_loc_make
       ; Alcotest.test_case "none" `Quick test_loc_none
       ; Alcotest.test_case "to_string" `Quick test_loc_to_string
       ; Alcotest.test_case "equal" `Quick test_loc_equal
       ])
    ]
