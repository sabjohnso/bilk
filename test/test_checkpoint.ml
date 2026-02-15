open Bilk

(* --- Helpers --- *)

let eval inst s = Instance.eval_string inst s

let datum_testable : Datum.t Alcotest.testable =
  Alcotest.testable Datum.pp Datum.equal

let round_trip inst =
  let data = Checkpoint.snapshot inst in
  Checkpoint.restore data

(* --- Phase 1: Data-Only Round-Trip --- *)

let test_empty_instance_round_trip () =
  let inst = Instance.create () in
  let inst2 = round_trip inst in
  (* Primitives survive: + must still work *)
  let result = eval inst2 "(+ 1 2)" in
  Alcotest.check datum_testable "primitive + works" (Datum.Fixnum 3) result

let test_user_fixnum_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define x 42)");
  let inst2 = round_trip inst in
  let result = eval inst2 "x" in
  Alcotest.check datum_testable "fixnum binding" (Datum.Fixnum 42) result

let test_user_string_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define greeting \"hello\")");
  let inst2 = round_trip inst in
  let result = eval inst2 "greeting" in
  Alcotest.check datum_testable "string binding"
    (Datum.Str (Bytes.of_string "hello")) result

let test_user_list_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define xs '(1 2 3))");
  let inst2 = round_trip inst in
  let result = eval inst2 "(car xs)" in
  Alcotest.check datum_testable "car of list" (Datum.Fixnum 1) result;
  let result2 = eval inst2 "(length xs)" in
  Alcotest.check datum_testable "length of list" (Datum.Fixnum 3) result2

let test_multiple_bindings_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define a 1)");
  ignore (eval inst "(define b #t)");
  ignore (eval inst "(define c '())");
  ignore (eval inst "(define d #\\x)");
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "fixnum" (Datum.Fixnum 1) (eval inst2 "a");
  Alcotest.check datum_testable "bool" (Datum.Bool true) (eval inst2 "b");
  Alcotest.check datum_testable "nil" Datum.Nil (eval inst2 "c");
  Alcotest.check datum_testable "char" (Datum.Char (Uchar.of_int (Char.code 'x'))) (eval inst2 "d")

let test_vector_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define v (vector 10 20 30))");
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "vector-ref"
    (Datum.Fixnum 20) (eval inst2 "(vector-ref v 1)")

let test_numeric_tower_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define big (expt 2 100))");
  ignore (eval inst "(define rat 1/3)");
  ignore (eval inst "(define flo 3.14)");
  let inst2 = round_trip inst in
  (* bignum *)
  Alcotest.check datum_testable "bignum"
    (eval inst "big") (eval inst2 "big");
  (* rational *)
  Alcotest.check datum_testable "rational"
    (eval inst "rat") (eval inst2 "rat");
  (* flonum *)
  Alcotest.check datum_testable "flonum"
    (Datum.Flonum 3.14) (eval inst2 "flo")

(* --- Phase 2: Closure Round-Trip --- *)

let test_simple_closure_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define (square x) (* x x))");
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "closure call"
    (Datum.Fixnum 49) (eval inst2 "(square 7)")

let test_recursive_closure_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))");
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "factorial 5"
    (Datum.Fixnum 120) (eval inst2 "(fact 5)")

let test_closure_captures_value () =
  let inst = Instance.create () in
  ignore (eval inst "(define base 10)");
  ignore (eval inst "(define (add-base x) (+ base x))");
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "closure captures global"
    (Datum.Fixnum 15) (eval inst2 "(add-base 5)")

let test_shared_frame_closures () =
  let inst = Instance.create () in
  ignore (eval inst {|
    (define counter
      (let ((n 0))
        (list
          (lambda () (set! n (+ n 1)) n)
          (lambda () n))))
  |});
  let inst2 = round_trip inst in
  (* Call the incrementer *)
  Alcotest.check datum_testable "first increment"
    (Datum.Fixnum 1) (eval inst2 "((car counter))");
  Alcotest.check datum_testable "second increment"
    (Datum.Fixnum 2) (eval inst2 "((car counter))");
  (* Reader should see the same value *)
  Alcotest.check datum_testable "shared frame"
    (Datum.Fixnum 2) (eval inst2 "((cadr counter))")

let test_lambda_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define f (lambda (x y) (+ x y)))");
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "lambda call"
    (Datum.Fixnum 7) (eval inst2 "(f 3 4)")

let test_higher_order_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define (make-adder n) (lambda (x) (+ n x)))");
  ignore (eval inst "(define add5 (make-adder 5))");
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "higher-order closure"
    (Datum.Fixnum 15) (eval inst2 "(add5 10)")

(* --- Phase 3: Compound Types Round-Trip --- *)

let test_hash_table_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define ht (make-hash-table))");
  ignore (eval inst {|(hash-table-set! ht "x" 1)|});
  ignore (eval inst {|(hash-table-set! ht "y" 2)|});
  (* Verify original works *)
  Alcotest.check datum_testable "before: hash-table?"
    (Datum.Bool true) (eval inst "(hash-table? ht)");
  Alcotest.check datum_testable "before: size"
    (Datum.Fixnum 2) (eval inst "(hash-table-size ht)");
  let inst2 = round_trip inst in
  (* Check hash-table? first *)
  Alcotest.check datum_testable "after: hash-table?"
    (Datum.Bool true) (eval inst2 "(hash-table? ht)");
  Alcotest.check datum_testable "after: size"
    (Datum.Fixnum 2) (eval inst2 "(hash-table-size ht)");
  Alcotest.check datum_testable "hash-table-ref x"
    (Datum.Fixnum 1) (eval inst2 {|(hash-table-ref ht "x")|});
  Alcotest.check datum_testable "hash-table-ref y"
    (Datum.Fixnum 2) (eval inst2 {|(hash-table-ref ht "y")|})

let test_promise_forced_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define p (delay 42))");
  ignore (eval inst "(force p)");
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "forced promise"
    (Datum.Fixnum 42) (eval inst2 "(force p)")

let test_promise_unforced_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define p (delay (+ 1 2)))");
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "unforced promise"
    (Datum.Fixnum 3) (eval inst2 "(force p)")

let test_skipped_port_becomes_void () =
  let inst = Instance.create () in
  ignore (eval inst "(define p (current-input-port))");
  let inst2 = round_trip inst in
  (* Port becomes Void after round-trip *)
  Alcotest.check datum_testable "port becomes void"
    Datum.Void (eval inst2 "p")

(* --- Phase 4: Metadata Round-Trip --- *)

let test_gensym_counter_preserved () =
  let inst = Instance.create () in
  (* define-syntax triggers gensym usage *)
  ignore (eval inst "(define-syntax my-or (syntax-rules () ((my-or a b) (let ((t a)) (if t t b)))))");
  ignore (eval inst "(my-or #f 42)");
  let counter_before = !(inst.Instance.gensym_counter) in
  let inst2 = round_trip inst in
  Alcotest.(check int) "gensym counter preserved"
    counter_before !(inst2.Instance.gensym_counter)

let test_user_macro_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(define-syntax my-when (syntax-rules () ((my-when test body ...) (if test (begin body ...)))))");
  ignore (eval inst "(define result 0)");
  let inst2 = round_trip inst in
  ignore (eval inst2 "(my-when #t (set! result 42))");
  Alcotest.check datum_testable "macro works after restore"
    (Datum.Fixnum 42) (eval inst2 "result")

let test_library_survives_round_trip () =
  let inst = Instance.create () in
  ignore (eval inst "(import (scheme char))");
  (* char-alphabetic? comes from (scheme char) *)
  let inst2 = round_trip inst in
  Alcotest.check datum_testable "library proc works"
    (Datum.Bool true) (eval inst2 "(char-alphabetic? #\\a)")

(* --- Test suite --- *)

let () =
  Alcotest.run "Checkpoint" [
    "phase1_data_round_trip", [
      Alcotest.test_case "empty instance" `Quick test_empty_instance_round_trip;
      Alcotest.test_case "user fixnum" `Quick test_user_fixnum_round_trip;
      Alcotest.test_case "user string" `Quick test_user_string_round_trip;
      Alcotest.test_case "user list" `Quick test_user_list_round_trip;
      Alcotest.test_case "multiple bindings" `Quick test_multiple_bindings_round_trip;
      Alcotest.test_case "vector" `Quick test_vector_round_trip;
      Alcotest.test_case "numeric tower" `Quick test_numeric_tower_round_trip;
    ];
    "phase2_closure_round_trip", [
      Alcotest.test_case "simple closure" `Quick test_simple_closure_round_trip;
      Alcotest.test_case "recursive closure" `Quick test_recursive_closure_round_trip;
      Alcotest.test_case "closure captures global" `Quick test_closure_captures_value;
      Alcotest.test_case "shared frame closures" `Quick test_shared_frame_closures;
      Alcotest.test_case "lambda" `Quick test_lambda_round_trip;
      Alcotest.test_case "higher-order closure" `Quick test_higher_order_round_trip;
    ];
    "phase3_compound_types", [
      Alcotest.test_case "hash table" `Quick test_hash_table_round_trip;
      Alcotest.test_case "forced promise" `Quick test_promise_forced_round_trip;
      Alcotest.test_case "unforced promise" `Quick test_promise_unforced_round_trip;
      Alcotest.test_case "port becomes void" `Quick test_skipped_port_becomes_void;
    ];
    "phase4_metadata", [
      Alcotest.test_case "gensym counter" `Quick test_gensym_counter_preserved;
      Alcotest.test_case "user macro" `Quick test_user_macro_round_trip;
      Alcotest.test_case "library survives" `Quick test_library_survives_round_trip;
    ];
  ]
