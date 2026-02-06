open Wile

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let check_datum = Alcotest.check datum_testable

let test_datum_bool () =
  check_datum "true" (Datum.Bool true) (Datum.Bool true);
  check_datum "false" (Datum.Bool false) (Datum.Bool false);
  Alcotest.(check bool) "true != false" false
    (Datum.equal (Datum.Bool true) (Datum.Bool false))

let test_datum_fixnum () =
  check_datum "zero" (Datum.Fixnum 0) (Datum.Fixnum 0);
  check_datum "42" (Datum.Fixnum 42) (Datum.Fixnum 42);
  Alcotest.(check bool) "0 != 1" false
    (Datum.equal (Datum.Fixnum 0) (Datum.Fixnum 1))

let test_datum_symbol () =
  check_datum "foo" (Datum.Symbol "foo") (Datum.Symbol "foo");
  Alcotest.(check bool) "foo != bar" false
    (Datum.equal (Datum.Symbol "foo") (Datum.Symbol "bar"))

let test_datum_nil_eof () =
  check_datum "nil" Datum.Nil Datum.Nil;
  check_datum "eof" Datum.Eof Datum.Eof;
  Alcotest.(check bool) "nil != eof" false
    (Datum.equal Datum.Nil Datum.Eof)

let test_datum_pair () =
  let p = Datum.Pair (Datum.Fixnum 1, Datum.Fixnum 2) in
  check_datum "pair" p p;
  let q = Datum.Pair (Datum.Fixnum 1, Datum.Fixnum 3) in
  Alcotest.(check bool) "diff cdr" false (Datum.equal p q)

let test_datum_string () =
  check_datum "str" (Datum.Str "hello") (Datum.Str "hello");
  Alcotest.(check bool) "diff str" false
    (Datum.equal (Datum.Str "a") (Datum.Str "b"))

let test_datum_char () =
  let a = Datum.Char (Uchar.of_int 0x41) in
  let b = Datum.Char (Uchar.of_int 0x42) in
  check_datum "char A" a a;
  Alcotest.(check bool) "A != B" false (Datum.equal a b)

let test_datum_vector () =
  let v1 = Datum.Vector [| Datum.Fixnum 1; Datum.Fixnum 2 |] in
  let v2 = Datum.Vector [| Datum.Fixnum 1; Datum.Fixnum 2 |] in
  let v3 = Datum.Vector [| Datum.Fixnum 1; Datum.Fixnum 3 |] in
  check_datum "vec eq" v1 v2;
  Alcotest.(check bool) "vec neq" false (Datum.equal v1 v3)

let test_datum_bytevector () =
  let b1 = Datum.Bytevector (Bytes.of_string "\x01\x02") in
  let b2 = Datum.Bytevector (Bytes.of_string "\x01\x02") in
  let b3 = Datum.Bytevector (Bytes.of_string "\x01\x03") in
  check_datum "bv eq" b1 b2;
  Alcotest.(check bool) "bv neq" false (Datum.equal b1 b3)

let test_datum_flonum () =
  check_datum "3.14" (Datum.Flonum 3.14) (Datum.Flonum 3.14);
  check_datum "nan = nan" (Datum.Flonum Float.nan) (Datum.Flonum Float.nan);
  Alcotest.(check bool) "1.0 != 2.0" false
    (Datum.equal (Datum.Flonum 1.0) (Datum.Flonum 2.0))

let test_datum_cross_type () =
  Alcotest.(check bool) "bool != fixnum" false
    (Datum.equal (Datum.Bool true) (Datum.Fixnum 1));
  Alcotest.(check bool) "nil != symbol" false
    (Datum.equal Datum.Nil (Datum.Symbol "nil"));
  Alcotest.(check bool) "fixnum != flonum" false
    (Datum.equal (Datum.Fixnum 1) (Datum.Flonum 1.0))

let test_datum_void () =
  let datum_testable = Alcotest.testable Datum.pp Datum.equal in
  Alcotest.check datum_testable "void = void" Datum.Void Datum.Void;
  Alcotest.(check bool) "void != nil" false
    (Datum.equal Datum.Void Datum.Nil);
  Alcotest.(check string) "void pp" "#<void>" (Datum.to_string Datum.Void)

let test_datum_primitive () =
  let p = Datum.Primitive { prim_name = "add"; prim_fn = (fun _ -> Datum.Void) } in
  Alcotest.(check string) "primitive pp" "#<primitive add>" (Datum.to_string p);
  let p2 = Datum.Primitive { prim_name = "add"; prim_fn = (fun _ -> Datum.Nil) } in
  Alcotest.(check bool) "same name = equal" true (Datum.equal p p2);
  let p3 = Datum.Primitive { prim_name = "sub"; prim_fn = (fun _ -> Datum.Void) } in
  Alcotest.(check bool) "diff name = not equal" false (Datum.equal p p3)

let test_datum_closure () =
  let dummy_code : Datum.code = {
    instructions = [| Opcode.Halt |];
    constants = [||];
    symbols = [||];
    children = [||];
    params = [||];
    variadic = false;
    name = "test";
  } in
  let c1 = Datum.Closure { clos_name = "f"; clos_code = dummy_code; clos_env = [] } in
  let c2 = Datum.Closure { clos_name = "f"; clos_code = dummy_code; clos_env = [] } in
  Alcotest.(check string) "closure pp" "#<closure f>" (Datum.to_string c1);
  Alcotest.(check bool) "closures never equal" false (Datum.equal c1 c2)

let test_datum_pp () =
  Alcotest.(check string) "#t" "#t" (Datum.to_string (Bool true));
  Alcotest.(check string) "#f" "#f" (Datum.to_string (Bool false));
  Alcotest.(check string) "42" "42" (Datum.to_string (Fixnum 42));
  Alcotest.(check string) "3.14" "3.14" (Datum.to_string (Flonum 3.14));
  Alcotest.(check string) "+inf.0" "+inf.0" (Datum.to_string (Flonum infinity));
  Alcotest.(check string) "-inf.0" "-inf.0" (Datum.to_string (Flonum neg_infinity));
  Alcotest.(check string) "+nan.0" "+nan.0" (Datum.to_string (Flonum Float.nan));
  Alcotest.(check string) "sym" "foo" (Datum.to_string (Symbol "foo"));
  Alcotest.(check string) "nil" "()" (Datum.to_string Nil);
  Alcotest.(check string) "eof" "#<eof>" (Datum.to_string Eof);
  Alcotest.(check string) "pair" "(1 . 2)"
    (Datum.to_string (Pair (Fixnum 1, Fixnum 2)));
  Alcotest.(check string) "list" "(1 2 3)"
    (Datum.to_string (Pair (Fixnum 1, Pair (Fixnum 2, Pair (Fixnum 3, Nil)))));
  Alcotest.(check string) "vector" "#(1 2)"
    (Datum.to_string (Vector [| Fixnum 1; Fixnum 2 |]));
  Alcotest.(check string) "string" "\"hello\""
    (Datum.to_string (Str "hello"));
  Alcotest.(check string) "bytevector" "#u8(1 2 3)"
    (Datum.to_string (Bytevector (Bytes.of_string "\x01\x02\x03")))

let () =
  Alcotest.run "Datum"
    [ ("Datum",
       [ Alcotest.test_case "bool" `Quick test_datum_bool
       ; Alcotest.test_case "fixnum" `Quick test_datum_fixnum
       ; Alcotest.test_case "flonum" `Quick test_datum_flonum
       ; Alcotest.test_case "symbol" `Quick test_datum_symbol
       ; Alcotest.test_case "nil and eof" `Quick test_datum_nil_eof
       ; Alcotest.test_case "pair" `Quick test_datum_pair
       ; Alcotest.test_case "string" `Quick test_datum_string
       ; Alcotest.test_case "char" `Quick test_datum_char
       ; Alcotest.test_case "vector" `Quick test_datum_vector
       ; Alcotest.test_case "bytevector" `Quick test_datum_bytevector
       ; Alcotest.test_case "cross-type inequality" `Quick test_datum_cross_type
       ; Alcotest.test_case "pp" `Quick test_datum_pp
       ; Alcotest.test_case "void" `Quick test_datum_void
       ; Alcotest.test_case "primitive" `Quick test_datum_primitive
       ; Alcotest.test_case "closure" `Quick test_datum_closure
       ])
    ]
