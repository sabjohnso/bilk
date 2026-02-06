open Wile

(* -- Unit tests -- *)

let test_create () =
  let inst = Instance.create () in
  ignore (inst : Instance.t)

let test_intern () =
  let inst = Instance.create () in
  let s1 = Instance.intern inst "foo" in
  let s2 = Instance.intern inst "foo" in
  Alcotest.(check bool) "same symbol" true (Symbol.equal s1 s2);
  Alcotest.(check string) "name" "foo" (Symbol.name s1)

let test_independent_tables () =
  let i1 = Instance.create () in
  let i2 = Instance.create () in
  let s1 = Instance.intern i1 "x" in
  let s2 = Instance.intern i2 "x" in
  Alcotest.(check string) "same name" (Symbol.name s1) (Symbol.name s2);
  (* Different tables — ids are independent, both start at 0 *)
  Alcotest.(check int) "id from i1" 0 (Symbol.id s1);
  Alcotest.(check int) "id from i2" 0 (Symbol.id s2)

let test_default_readtable () =
  let inst = Instance.create () in
  (* Default readtable should be Readtable.default — check a known property *)
  Alcotest.(check bool) "space is whitespace" true
    (Readtable.is_whitespace inst.readtable ' ');
  Alcotest.(check bool) "( is delimiter" true
    (Readtable.is_delimiter inst.readtable '(')

let test_custom_readtable () =
  let rt = Readtable.with_fold_case true Readtable.default in
  let inst = Instance.create ~readtable:rt () in
  Alcotest.(check bool) "fold_case on" true
    (Readtable.fold_case inst.readtable)

let () =
  Alcotest.run "Instance"
    [ ("Instance",
       [ Alcotest.test_case "create" `Quick test_create
       ; Alcotest.test_case "intern" `Quick test_intern
       ; Alcotest.test_case "independent tables" `Quick test_independent_tables
       ; Alcotest.test_case "default readtable" `Quick test_default_readtable
       ; Alcotest.test_case "custom readtable" `Quick test_custom_readtable
       ])
    ]
