open Wile

(* -- Helpers -- *)

let symbol_testable =
  Alcotest.testable Symbol.pp Symbol.equal

let check_symbol = Alcotest.check symbol_testable

(* -- Unit tests -- *)

let test_intern_same_id () =
  let tbl = Symbol.create_table () in
  let s1 = Symbol.intern tbl "foo" in
  let s2 = Symbol.intern tbl "foo" in
  Alcotest.(check int) "same id" (Symbol.id s1) (Symbol.id s2);
  check_symbol "same symbol" s1 s2

let test_intern_different_ids () =
  let tbl = Symbol.create_table () in
  let s1 = Symbol.intern tbl "foo" in
  let s2 = Symbol.intern tbl "bar" in
  Alcotest.(check bool) "different ids" true (Symbol.id s1 <> Symbol.id s2);
  Alcotest.(check bool) "not equal" false (Symbol.equal s1 s2)

let test_name_roundtrip () =
  let tbl = Symbol.create_table () in
  let s = Symbol.intern tbl "hello" in
  Alcotest.(check string) "name" "hello" (Symbol.name s)

let test_equal_compare () =
  let tbl = Symbol.create_table () in
  let s1 = Symbol.intern tbl "a" in
  let s2 = Symbol.intern tbl "a" in
  let s3 = Symbol.intern tbl "b" in
  Alcotest.(check bool) "equal same" true (Symbol.equal s1 s2);
  Alcotest.(check bool) "not equal diff" false (Symbol.equal s1 s3);
  Alcotest.(check int) "compare same = 0" 0 (Symbol.compare s1 s2);
  Alcotest.(check bool) "compare diff != 0" true (Symbol.compare s1 s3 <> 0)

let test_to_string () =
  let tbl = Symbol.create_table () in
  let s = Symbol.intern tbl "lambda" in
  Alcotest.(check string) "to_string" "lambda" (Symbol.to_string s)

let test_hash () =
  let tbl = Symbol.create_table () in
  let s = Symbol.intern tbl "x" in
  Alcotest.(check int) "hash = id" (Symbol.id s) (Symbol.hash s)

(* -- QCheck tests -- *)

let gen_symbol_name =
  QCheck2.Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 1 20))

let qcheck_name_roundtrip =
  QCheck2.Test.make
    ~name:"name (intern tbl s) = s"
    ~count:200
    gen_symbol_name
    (fun s ->
      let tbl = Symbol.create_table () in
      String.equal s (Symbol.name (Symbol.intern tbl s)))

let qcheck_intern_idempotent =
  QCheck2.Test.make
    ~name:"intern tbl s returns same id on repeated calls"
    ~count:200
    gen_symbol_name
    (fun s ->
      let tbl = Symbol.create_table () in
      let s1 = Symbol.intern tbl s in
      let s2 = Symbol.intern tbl s in
      Symbol.id s1 = Symbol.id s2)

let () =
  Alcotest.run "Symbol"
    [ ("Symbol",
       [ Alcotest.test_case "intern same id" `Quick test_intern_same_id
       ; Alcotest.test_case "intern different ids" `Quick test_intern_different_ids
       ; Alcotest.test_case "name round-trip" `Quick test_name_roundtrip
       ; Alcotest.test_case "equal/compare" `Quick test_equal_compare
       ; Alcotest.test_case "to_string" `Quick test_to_string
       ; Alcotest.test_case "hash = id" `Quick test_hash
       ; QCheck_alcotest.to_alcotest qcheck_name_roundtrip
       ; QCheck_alcotest.to_alcotest qcheck_intern_idempotent
       ])
    ]
