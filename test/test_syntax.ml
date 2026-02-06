open Wile

let syntax_testable =
  Alcotest.testable Syntax.pp Syntax.equal

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let check_syntax = Alcotest.check syntax_testable
let check_datum = Alcotest.check datum_testable

let loc1 = Loc.make "test.scm" 1 1
let loc2 = Loc.make "test.scm" 2 5

let test_syntax_make () =
  let s = Syntax.make loc1 (Syntax.Fixnum 42) in
  Alcotest.(check bool) "loc matches" true (Loc.equal s.loc loc1);
  match s.datum with
  | Syntax.Fixnum 42 -> ()
  | _ -> Alcotest.fail "expected Fixnum 42"

let test_syntax_to_datum_atoms () =
  check_datum "bool true" (Datum.Bool true)
    (Syntax.to_datum (Syntax.make loc1 (Syntax.Bool true)));
  check_datum "bool false" (Datum.Bool false)
    (Syntax.to_datum (Syntax.make loc1 (Syntax.Bool false)));
  check_datum "fixnum" (Datum.Fixnum 42)
    (Syntax.to_datum (Syntax.make loc1 (Syntax.Fixnum 42)));
  check_datum "flonum" (Datum.Flonum 3.14)
    (Syntax.to_datum (Syntax.make loc1 (Syntax.Flonum 3.14)));
  check_datum "char" (Datum.Char (Uchar.of_int 65))
    (Syntax.to_datum (Syntax.make loc1 (Syntax.Char (Uchar.of_int 65))));
  check_datum "string" (Datum.Str "hello")
    (Syntax.to_datum (Syntax.make loc1 (Syntax.Str "hello")));
  check_datum "symbol" (Datum.Symbol "foo")
    (Syntax.to_datum (Syntax.make loc1 (Syntax.Symbol "foo")));
  check_datum "nil" Datum.Nil
    (Syntax.to_datum (Syntax.make loc1 Syntax.Nil));
  check_datum "eof" Datum.Eof
    (Syntax.to_datum (Syntax.make loc1 Syntax.Eof));
  check_datum "bytevector" (Datum.Bytevector (Bytes.of_string "\x01\x02"))
    (Syntax.to_datum (Syntax.make loc1 (Syntax.Bytevector (Bytes.of_string "\x01\x02"))))

let test_syntax_to_datum_pair () =
  let car = Syntax.make loc1 (Syntax.Fixnum 1) in
  let cdr = Syntax.make loc1 (Syntax.Fixnum 2) in
  let pair = Syntax.make loc1 (Syntax.Pair (car, cdr)) in
  check_datum "pair" (Datum.Pair (Datum.Fixnum 1, Datum.Fixnum 2))
    (Syntax.to_datum pair)

let test_syntax_to_datum_vector () =
  let elts = [|
    Syntax.make loc1 (Syntax.Fixnum 1);
    Syntax.make loc1 (Syntax.Symbol "x")
  |] in
  let vec = Syntax.make loc1 (Syntax.Vector elts) in
  check_datum "vector" (Datum.Vector [| Datum.Fixnum 1; Datum.Symbol "x" |])
    (Syntax.to_datum vec)

let test_syntax_to_datum_nested () =
  (* (1 (2 3)) *)
  let inner_cdr = Syntax.make loc1 (Syntax.Pair (
    Syntax.make loc1 (Syntax.Fixnum 3),
    Syntax.make loc1 Syntax.Nil
  )) in
  let inner = Syntax.make loc1 (Syntax.Pair (
    Syntax.make loc1 (Syntax.Fixnum 2),
    inner_cdr
  )) in
  let outer_cdr = Syntax.make loc1 (Syntax.Pair (
    inner,
    Syntax.make loc1 Syntax.Nil
  )) in
  let outer = Syntax.make loc1 (Syntax.Pair (
    Syntax.make loc1 (Syntax.Fixnum 1),
    outer_cdr
  )) in
  let expected = Datum.Pair (
    Datum.Fixnum 1,
    Datum.Pair (
      Datum.Pair (Datum.Fixnum 2, Datum.Pair (Datum.Fixnum 3, Datum.Nil)),
      Datum.Nil
    )
  ) in
  check_datum "nested" expected (Syntax.to_datum outer)

let test_syntax_equal_datum () =
  let a = Syntax.make loc1 (Syntax.Fixnum 42) in
  let b = Syntax.make loc2 (Syntax.Fixnum 42) in
  let c = Syntax.make loc1 (Syntax.Fixnum 99) in
  Alcotest.(check bool) "same datum diff loc" true (Syntax.equal_datum a b);
  Alcotest.(check bool) "diff datum" false (Syntax.equal_datum a c)

let test_syntax_equal () =
  let a = Syntax.make loc1 (Syntax.Fixnum 42) in
  let b = Syntax.make loc1 (Syntax.Fixnum 42) in
  let c = Syntax.make loc2 (Syntax.Fixnum 42) in
  check_syntax "same" a b;
  Alcotest.(check bool) "diff loc" false (Syntax.equal a c)

let test_syntax_pp () =
  let s = Syntax.make loc1 (Syntax.Symbol "hello") in
  Alcotest.(check string) "pp" "hello" (Syntax.to_string s);
  let pair = Syntax.make loc1 (Syntax.Pair (
    Syntax.make loc1 (Syntax.Fixnum 1),
    Syntax.make loc1 (Syntax.Fixnum 2)
  )) in
  Alcotest.(check string) "pp pair" "(1 . 2)" (Syntax.to_string pair)

let () =
  Alcotest.run "Syntax"
    [ ("Syntax",
       [ Alcotest.test_case "make" `Quick test_syntax_make
       ; Alcotest.test_case "to_datum atoms" `Quick test_syntax_to_datum_atoms
       ; Alcotest.test_case "to_datum pair" `Quick test_syntax_to_datum_pair
       ; Alcotest.test_case "to_datum vector" `Quick test_syntax_to_datum_vector
       ; Alcotest.test_case "to_datum nested" `Quick test_syntax_to_datum_nested
       ; Alcotest.test_case "equal_datum" `Quick test_syntax_equal_datum
       ; Alcotest.test_case "equal" `Quick test_syntax_equal
       ; Alcotest.test_case "pp" `Quick test_syntax_pp
       ])
    ]
