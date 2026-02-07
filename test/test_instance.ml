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
  (* Different tables — ids are independent, both get the same id for "x" *)
  Alcotest.(check int) "same id in both tables" (Symbol.id s1) (Symbol.id s2)

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

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let check_datum = Alcotest.check datum_testable

let test_eval_string () =
  let inst = Instance.create () in
  check_datum "eval fixnum" (Datum.Fixnum 42) (Instance.eval_string inst "42");
  check_datum "eval add" (Datum.Fixnum 3) (Instance.eval_string inst "(+ 1 2)")

let test_eval_syntax () =
  let inst = Instance.create () in
  let port = Port.of_string "42" in
  let expr = Reader.read_syntax inst.readtable port in
  check_datum "eval syntax" (Datum.Fixnum 42) (Instance.eval_syntax inst expr)

let test_primitives_registered () =
  let inst = Instance.create () in
  let plus_sym = Instance.intern inst "+" in
  let v = Env.lookup inst.global_env plus_sym in
  Alcotest.(check bool) "plus is bound" true (Option.is_some v);
  match v with
  | Some (Datum.Primitive p) ->
    Alcotest.(check string) "plus name" "+" p.prim_name
  | _ -> Alcotest.fail "expected primitive"

let test_scheme_base_registered () =
  let inst = Instance.create () in
  match Library.lookup inst.libraries ["scheme"; "base"] with
  | Some lib ->
    Alcotest.(check bool) "has +" true (Hashtbl.mem lib.exports "+");
    Alcotest.(check bool) "has car" true (Hashtbl.mem lib.exports "car");
    Alcotest.(check bool) "has define" true (Hashtbl.mem lib.syntax_exports "define")
  | None -> Alcotest.fail "expected (scheme base) library"

let test_scheme_char_registered () =
  let inst = Instance.create () in
  match Library.lookup inst.libraries ["scheme"; "char"] with
  | Some _ -> ()
  | None -> Alcotest.fail "expected (scheme char) library"

let test_features () =
  let inst = Instance.create () in
  Alcotest.(check bool) "has r7rs" true (List.mem "r7rs" inst.features);
  Alcotest.(check bool) "has wile" true (List.mem "wile" inst.features)

let test_eval_port_multiple () =
  let inst = Instance.create () in
  let port = Port.of_string "(define x 10) (define y 20) (+ x y)" in
  check_datum "last result" (Datum.Fixnum 30) (Instance.eval_port inst port)

let test_eval_port_empty () =
  let inst = Instance.create () in
  let port = Port.of_string "" in
  check_datum "empty port" Datum.Void (Instance.eval_port inst port)

let test_eval_port_define_use () =
  let inst = Instance.create () in
  let port = Port.of_string "(define (square x) (* x x)) (square 7)" in
  check_datum "define+use" (Datum.Fixnum 49) (Instance.eval_port inst port)

let test_eval_port_import () =
  let inst = Instance.create () in
  let port = Port.of_string "(import (scheme base)) (+ 1 2)" in
  check_datum "import+expr" (Datum.Fixnum 3) (Instance.eval_port inst port)

let () =
  Alcotest.run "Instance"
    [ ("Instance",
       [ Alcotest.test_case "create" `Quick test_create
       ; Alcotest.test_case "intern" `Quick test_intern
       ; Alcotest.test_case "independent tables" `Quick test_independent_tables
       ; Alcotest.test_case "default readtable" `Quick test_default_readtable
       ; Alcotest.test_case "custom readtable" `Quick test_custom_readtable
       ; Alcotest.test_case "eval_string" `Quick test_eval_string
       ; Alcotest.test_case "eval_syntax" `Quick test_eval_syntax
       ; Alcotest.test_case "primitives registered" `Quick test_primitives_registered
       ])
    ; ("Libraries",
       [ Alcotest.test_case "(scheme base) registered" `Quick test_scheme_base_registered
       ; Alcotest.test_case "(scheme char) registered" `Quick test_scheme_char_registered
       ; Alcotest.test_case "features" `Quick test_features
       ])
    ; ("eval_port",
       [ Alcotest.test_case "multiple expressions" `Quick test_eval_port_multiple
       ; Alcotest.test_case "empty port" `Quick test_eval_port_empty
       ; Alcotest.test_case "define + use" `Quick test_eval_port_define_use
       ; Alcotest.test_case "import + expr" `Quick test_eval_port_import
       ])
    ]
