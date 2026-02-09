open Wile

(* -- Helpers -- *)

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let check_datum = Alcotest.check datum_testable

(* -- Instance lifecycle -- *)

let test_create () =
  let ih = Wile_c_api.create_instance () in
  Alcotest.(check bool) "handle > 0" true (ih > 0);
  Wile_c_api.destroy_instance ih

let test_create_multiple () =
  let ih1 = Wile_c_api.create_instance () in
  let ih2 = Wile_c_api.create_instance () in
  Alcotest.(check bool) "different handles" true (ih1 <> ih2);
  Wile_c_api.destroy_instance ih1;
  Wile_c_api.destroy_instance ih2

let test_destroy_invalid () =
  (* Should not crash *)
  Wile_c_api.destroy_instance 99999

(* -- Error handling -- *)

let test_error_no_error () =
  let ih = Wile_c_api.create_instance () in
  Alcotest.(check string) "no error" "" (Wile_c_api.error_message ih);
  Wile_c_api.destroy_instance ih

let test_error_after_eval_error () =
  let ih = Wile_c_api.create_instance () in
  let r = Wile_c_api.eval_string ih "(undefined-var)" in
  Alcotest.(check int) "returns 0" 0 r;
  let msg = Wile_c_api.error_message ih in
  Alcotest.(check bool) "has error" true (String.length msg > 0);
  Wile_c_api.destroy_instance ih

let test_error_cleared_on_success () =
  let ih = Wile_c_api.create_instance () in
  ignore (Wile_c_api.eval_string ih "(undefined-var)");
  let r = Wile_c_api.eval_string ih "42" in
  Alcotest.(check bool) "success" true (r > 0);
  Alcotest.(check string) "error cleared" "" (Wile_c_api.error_message ih);
  Wile_c_api.destroy_instance ih

(* -- Evaluation -- *)

let test_eval_string_fixnum () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.eval_string ih "42" in
  Alcotest.(check bool) "handle > 0" true (vh > 0);
  check_datum "fixnum 42"
    (Datum.Fixnum 42)
    (Option.get (Wile_c_api.resolve ih vh));
  Wile_c_api.destroy_instance ih

let test_eval_string_expr () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.eval_string ih "(+ 1 2)" in
  check_datum "1+2=3"
    (Datum.Fixnum 3)
    (Option.get (Wile_c_api.resolve ih vh));
  Wile_c_api.destroy_instance ih

let test_eval_string_error () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.eval_string ih "(car 42)" in
  Alcotest.(check int) "error returns 0" 0 vh;
  Wile_c_api.destroy_instance ih

(* -- Value constructors -- *)

let test_make_nil () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_nil ih in
  check_datum "nil" Datum.Nil (Option.get (Wile_c_api.resolve ih vh));
  Wile_c_api.destroy_instance ih

let test_make_void () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_void ih in
  check_datum "void" Datum.Void (Option.get (Wile_c_api.resolve ih vh));
  Wile_c_api.destroy_instance ih

let test_make_bool () =
  let ih = Wile_c_api.create_instance () in
  let t = Wile_c_api.make_bool ih 1 in
  let f = Wile_c_api.make_bool ih 0 in
  check_datum "#t" (Datum.Bool true) (Option.get (Wile_c_api.resolve ih t));
  check_datum "#f" (Datum.Bool false) (Option.get (Wile_c_api.resolve ih f));
  Wile_c_api.destroy_instance ih

let test_make_fixnum () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_fixnum ih 99 in
  check_datum "99" (Datum.Fixnum 99) (Option.get (Wile_c_api.resolve ih vh));
  Wile_c_api.destroy_instance ih

let test_make_flonum () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_flonum ih 3.14 in
  check_datum "3.14" (Datum.Flonum 3.14) (Option.get (Wile_c_api.resolve ih vh));
  Wile_c_api.destroy_instance ih

let test_make_string () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_string ih "hello" in
  check_datum "hello"
    (Datum.Str (Bytes.of_string "hello"))
    (Option.get (Wile_c_api.resolve ih vh));
  Wile_c_api.destroy_instance ih

let test_make_symbol () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_symbol ih "foo" in
  check_datum "foo" (Datum.Symbol "foo") (Option.get (Wile_c_api.resolve ih vh));
  Wile_c_api.destroy_instance ih

let test_make_cons () =
  let ih = Wile_c_api.create_instance () in
  let a = Wile_c_api.make_fixnum ih 1 in
  let d = Wile_c_api.make_fixnum ih 2 in
  let p = Wile_c_api.make_cons ih a d in
  Alcotest.(check bool) "pair handle > 0" true (p > 0);
  (match Wile_c_api.resolve ih p with
   | Some (Datum.Pair { car; cdr }) ->
     check_datum "car" (Datum.Fixnum 1) car;
     check_datum "cdr" (Datum.Fixnum 2) cdr
   | _ -> Alcotest.fail "expected pair");
  Wile_c_api.destroy_instance ih

let test_make_vector () =
  let ih = Wile_c_api.create_instance () in
  let e0 = Wile_c_api.make_fixnum ih 10 in
  let e1 = Wile_c_api.make_fixnum ih 20 in
  let v = Wile_c_api.make_vector ih [| e0; e1 |] in
  (match Wile_c_api.resolve ih v with
   | Some (Datum.Vector arr) ->
     Alcotest.(check int) "length" 2 (Array.length arr);
     check_datum "v[0]" (Datum.Fixnum 10) arr.(0);
     check_datum "v[1]" (Datum.Fixnum 20) arr.(1)
   | _ -> Alcotest.fail "expected vector");
  Wile_c_api.destroy_instance ih

let test_make_list () =
  let ih = Wile_c_api.create_instance () in
  let e0 = Wile_c_api.make_fixnum ih 1 in
  let e1 = Wile_c_api.make_fixnum ih 2 in
  let e2 = Wile_c_api.make_fixnum ih 3 in
  let l = Wile_c_api.make_list ih [| e0; e1; e2 |] in
  check_datum "(1 2 3)"
    (Datum.list_of [Datum.Fixnum 1; Datum.Fixnum 2; Datum.Fixnum 3])
    (Option.get (Wile_c_api.resolve ih l));
  Wile_c_api.destroy_instance ih

(* -- Type predicates -- *)

let test_is_nil () =
  let ih = Wile_c_api.create_instance () in
  let n = Wile_c_api.make_nil ih in
  let f = Wile_c_api.make_fixnum ih 1 in
  Alcotest.(check int) "nil is nil" 1 (Wile_c_api.is_nil ih n);
  Alcotest.(check int) "fixnum not nil" 0 (Wile_c_api.is_nil ih f);
  Wile_c_api.destroy_instance ih

let test_is_bool () =
  let ih = Wile_c_api.create_instance () in
  let b = Wile_c_api.make_bool ih 1 in
  let n = Wile_c_api.make_fixnum ih 1 in
  Alcotest.(check int) "bool is bool" 1 (Wile_c_api.is_bool ih b);
  Alcotest.(check int) "fixnum not bool" 0 (Wile_c_api.is_bool ih n);
  Wile_c_api.destroy_instance ih

let test_is_fixnum () =
  let ih = Wile_c_api.create_instance () in
  let f = Wile_c_api.make_fixnum ih 1 in
  let s = Wile_c_api.make_string ih "x" in
  Alcotest.(check int) "fixnum is fixnum" 1 (Wile_c_api.is_fixnum ih f);
  Alcotest.(check int) "string not fixnum" 0 (Wile_c_api.is_fixnum ih s);
  Wile_c_api.destroy_instance ih

let test_is_flonum () =
  let ih = Wile_c_api.create_instance () in
  let f = Wile_c_api.make_flonum ih 1.0 in
  let n = Wile_c_api.make_fixnum ih 1 in
  Alcotest.(check int) "flonum is flonum" 1 (Wile_c_api.is_flonum ih f);
  Alcotest.(check int) "fixnum not flonum" 0 (Wile_c_api.is_flonum ih n);
  Wile_c_api.destroy_instance ih

let test_is_string () =
  let ih = Wile_c_api.create_instance () in
  let s = Wile_c_api.make_string ih "x" in
  let n = Wile_c_api.make_fixnum ih 1 in
  Alcotest.(check int) "string is string" 1 (Wile_c_api.is_string ih s);
  Alcotest.(check int) "fixnum not string" 0 (Wile_c_api.is_string ih n);
  Wile_c_api.destroy_instance ih

let test_is_symbol () =
  let ih = Wile_c_api.create_instance () in
  let s = Wile_c_api.make_symbol ih "x" in
  let n = Wile_c_api.make_fixnum ih 1 in
  Alcotest.(check int) "symbol is symbol" 1 (Wile_c_api.is_symbol ih s);
  Alcotest.(check int) "fixnum not symbol" 0 (Wile_c_api.is_symbol ih n);
  Wile_c_api.destroy_instance ih

let test_is_pair () =
  let ih = Wile_c_api.create_instance () in
  let a = Wile_c_api.make_fixnum ih 1 in
  let d = Wile_c_api.make_fixnum ih 2 in
  let p = Wile_c_api.make_cons ih a d in
  Alcotest.(check int) "pair is pair" 1 (Wile_c_api.is_pair ih p);
  Alcotest.(check int) "fixnum not pair" 0 (Wile_c_api.is_pair ih a);
  Wile_c_api.destroy_instance ih

let test_is_vector () =
  let ih = Wile_c_api.create_instance () in
  let e = Wile_c_api.make_fixnum ih 1 in
  let v = Wile_c_api.make_vector ih [| e |] in
  Alcotest.(check int) "vector is vector" 1 (Wile_c_api.is_vector ih v);
  Alcotest.(check int) "fixnum not vector" 0 (Wile_c_api.is_vector ih e);
  Wile_c_api.destroy_instance ih

let test_is_true () =
  let ih = Wile_c_api.create_instance () in
  let t = Wile_c_api.make_bool ih 1 in
  let f = Wile_c_api.make_bool ih 0 in
  let n = Wile_c_api.make_fixnum ih 0 in
  Alcotest.(check int) "#t is true" 1 (Wile_c_api.is_true ih t);
  Alcotest.(check int) "#f is false" 0 (Wile_c_api.is_true ih f);
  Alcotest.(check int) "0 is true" 1 (Wile_c_api.is_true ih n);
  Wile_c_api.destroy_instance ih

(* -- Value extractors -- *)

let test_get_bool () =
  let ih = Wile_c_api.create_instance () in
  let t = Wile_c_api.make_bool ih 1 in
  let f = Wile_c_api.make_bool ih 0 in
  Alcotest.(check int) "#t" 1 (Wile_c_api.get_bool ih t);
  Alcotest.(check int) "#f" 0 (Wile_c_api.get_bool ih f);
  Wile_c_api.destroy_instance ih

let test_get_bool_type_error () =
  let ih = Wile_c_api.create_instance () in
  let n = Wile_c_api.make_fixnum ih 1 in
  let _ = Wile_c_api.get_bool ih n in
  let msg = Wile_c_api.error_message ih in
  Alcotest.(check string) "type error" "not a boolean" msg;
  Wile_c_api.destroy_instance ih

let test_get_fixnum () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_fixnum ih 42 in
  Alcotest.(check int) "42" 42 (Wile_c_api.get_fixnum ih vh);
  Wile_c_api.destroy_instance ih

let test_get_fixnum_type_error () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_string ih "x" in
  let _ = Wile_c_api.get_fixnum ih vh in
  let msg = Wile_c_api.error_message ih in
  Alcotest.(check string) "type error" "not a fixnum" msg;
  Wile_c_api.destroy_instance ih

let test_get_flonum () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_flonum ih 2.5 in
  Alcotest.(check (float 0.0)) "2.5" 2.5 (Wile_c_api.get_flonum ih vh);
  Wile_c_api.destroy_instance ih

let test_get_string () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_string ih "hello" in
  Alcotest.(check string) "hello" "hello" (Wile_c_api.get_string ih vh);
  Wile_c_api.destroy_instance ih

let test_get_symbol_name () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_symbol ih "bar" in
  Alcotest.(check string) "bar" "bar" (Wile_c_api.get_symbol_name ih vh);
  Wile_c_api.destroy_instance ih

(* -- car/cdr -- *)

let test_car_cdr () =
  let ih = Wile_c_api.create_instance () in
  let a = Wile_c_api.make_fixnum ih 10 in
  let d = Wile_c_api.make_fixnum ih 20 in
  let p = Wile_c_api.make_cons ih a d in
  let car_h = Wile_c_api.do_car ih p in
  let cdr_h = Wile_c_api.do_cdr ih p in
  check_datum "car" (Datum.Fixnum 10) (Option.get (Wile_c_api.resolve ih car_h));
  check_datum "cdr" (Datum.Fixnum 20) (Option.get (Wile_c_api.resolve ih cdr_h));
  Wile_c_api.destroy_instance ih

let test_car_type_error () =
  let ih = Wile_c_api.create_instance () in
  let n = Wile_c_api.make_fixnum ih 1 in
  let r = Wile_c_api.do_car ih n in
  Alcotest.(check int) "returns 0" 0 r;
  let msg = Wile_c_api.error_message ih in
  Alcotest.(check string) "type error" "not a pair" msg;
  Wile_c_api.destroy_instance ih

(* -- vector_length/vector_ref -- *)

let test_vector_length () =
  let ih = Wile_c_api.create_instance () in
  let e0 = Wile_c_api.make_fixnum ih 1 in
  let e1 = Wile_c_api.make_fixnum ih 2 in
  let e2 = Wile_c_api.make_fixnum ih 3 in
  let v = Wile_c_api.make_vector ih [| e0; e1; e2 |] in
  Alcotest.(check int) "length 3" 3 (Wile_c_api.do_vector_length ih v);
  Wile_c_api.destroy_instance ih

let test_vector_ref () =
  let ih = Wile_c_api.create_instance () in
  let e0 = Wile_c_api.make_fixnum ih 10 in
  let e1 = Wile_c_api.make_fixnum ih 20 in
  let v = Wile_c_api.make_vector ih [| e0; e1 |] in
  let r0 = Wile_c_api.do_vector_ref ih v 0 in
  let r1 = Wile_c_api.do_vector_ref ih v 1 in
  check_datum "v[0]" (Datum.Fixnum 10) (Option.get (Wile_c_api.resolve ih r0));
  check_datum "v[1]" (Datum.Fixnum 20) (Option.get (Wile_c_api.resolve ih r1));
  Wile_c_api.destroy_instance ih

let test_vector_ref_out_of_range () =
  let ih = Wile_c_api.create_instance () in
  let e0 = Wile_c_api.make_fixnum ih 1 in
  let v = Wile_c_api.make_vector ih [| e0 |] in
  let r = Wile_c_api.do_vector_ref ih v 5 in
  Alcotest.(check int) "returns 0" 0 r;
  let msg = Wile_c_api.error_message ih in
  Alcotest.(check string) "out of range" "vector index out of range" msg;
  Wile_c_api.destroy_instance ih

(* -- Display and write -- *)

let test_display_string () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_string ih "hello" in
  Alcotest.(check string) "display" "hello" (Wile_c_api.display_to_string ih vh);
  Wile_c_api.destroy_instance ih

let test_write_string () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_string ih "hello" in
  Alcotest.(check string) "write" "\"hello\"" (Wile_c_api.write_to_string ih vh);
  Wile_c_api.destroy_instance ih

(* -- Lookup and call -- *)

let test_lookup_found () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.do_lookup ih "+" in
  Alcotest.(check bool) "found +" true (vh > 0);
  (match Wile_c_api.resolve ih vh with
   | Some (Datum.Primitive p) ->
     Alcotest.(check string) "name" "+" p.prim_name
   | _ -> Alcotest.fail "expected primitive");
  Wile_c_api.destroy_instance ih

let test_lookup_not_found () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.do_lookup ih "no-such-thing" in
  Alcotest.(check int) "not found" 0 vh;
  Wile_c_api.destroy_instance ih

let test_call_simple () =
  let ih = Wile_c_api.create_instance () in
  let plus = Wile_c_api.do_lookup ih "+" in
  let a = Wile_c_api.make_fixnum ih 3 in
  let b = Wile_c_api.make_fixnum ih 4 in
  let r = Wile_c_api.do_call ih plus [| a; b |] in
  check_datum "3+4=7"
    (Datum.Fixnum 7)
    (Option.get (Wile_c_api.resolve ih r));
  Wile_c_api.destroy_instance ih

let test_call_error () =
  let ih = Wile_c_api.create_instance () in
  let n = Wile_c_api.make_fixnum ih 42 in
  let r = Wile_c_api.do_call ih n [| |] in
  Alcotest.(check int) "error" 0 r;
  Wile_c_api.destroy_instance ih

(* -- Release -- *)

let test_release () =
  let ih = Wile_c_api.create_instance () in
  let vh = Wile_c_api.make_fixnum ih 42 in
  Alcotest.(check bool) "before release" true
    (Option.is_some (Wile_c_api.resolve ih vh));
  Wile_c_api.release ih vh;
  Alcotest.(check bool) "after release" true
    (Option.is_none (Wile_c_api.resolve ih vh));
  Wile_c_api.destroy_instance ih

let test_release_invalid () =
  let ih = Wile_c_api.create_instance () in
  (* Should not crash *)
  Wile_c_api.release ih 99999;
  Wile_c_api.destroy_instance ih

(* -- Multiple instances -- *)

let test_multiple_instances () =
  let ih1 = Wile_c_api.create_instance () in
  let ih2 = Wile_c_api.create_instance () in
  (* Define x in instance 1 only *)
  ignore (Wile_c_api.eval_string ih1 "(define x 42)");
  let r1 = Wile_c_api.do_lookup ih1 "x" in
  let r2 = Wile_c_api.do_lookup ih2 "x" in
  Alcotest.(check bool) "found in inst1" true (r1 > 0);
  Alcotest.(check int) "not in inst2" 0 r2;
  Wile_c_api.destroy_instance ih1;
  Wile_c_api.destroy_instance ih2

let () =
  Alcotest.run "Wile_c_api"
    [ ("Instance lifecycle",
       [ Alcotest.test_case "create" `Quick test_create
       ; Alcotest.test_case "create multiple" `Quick test_create_multiple
       ; Alcotest.test_case "destroy invalid" `Quick test_destroy_invalid
       ])
    ; ("Error handling",
       [ Alcotest.test_case "no error" `Quick test_error_no_error
       ; Alcotest.test_case "error after eval" `Quick test_error_after_eval_error
       ; Alcotest.test_case "error cleared on success" `Quick test_error_cleared_on_success
       ])
    ; ("Evaluation",
       [ Alcotest.test_case "eval fixnum" `Quick test_eval_string_fixnum
       ; Alcotest.test_case "eval expr" `Quick test_eval_string_expr
       ; Alcotest.test_case "eval error" `Quick test_eval_string_error
       ])
    ; ("Constructors",
       [ Alcotest.test_case "nil" `Quick test_make_nil
       ; Alcotest.test_case "void" `Quick test_make_void
       ; Alcotest.test_case "bool" `Quick test_make_bool
       ; Alcotest.test_case "fixnum" `Quick test_make_fixnum
       ; Alcotest.test_case "flonum" `Quick test_make_flonum
       ; Alcotest.test_case "string" `Quick test_make_string
       ; Alcotest.test_case "symbol" `Quick test_make_symbol
       ; Alcotest.test_case "cons" `Quick test_make_cons
       ; Alcotest.test_case "vector" `Quick test_make_vector
       ; Alcotest.test_case "list" `Quick test_make_list
       ])
    ; ("Predicates",
       [ Alcotest.test_case "is_nil" `Quick test_is_nil
       ; Alcotest.test_case "is_bool" `Quick test_is_bool
       ; Alcotest.test_case "is_fixnum" `Quick test_is_fixnum
       ; Alcotest.test_case "is_flonum" `Quick test_is_flonum
       ; Alcotest.test_case "is_string" `Quick test_is_string
       ; Alcotest.test_case "is_symbol" `Quick test_is_symbol
       ; Alcotest.test_case "is_pair" `Quick test_is_pair
       ; Alcotest.test_case "is_vector" `Quick test_is_vector
       ; Alcotest.test_case "is_true" `Quick test_is_true
       ])
    ; ("Extractors",
       [ Alcotest.test_case "get_bool" `Quick test_get_bool
       ; Alcotest.test_case "get_bool type error" `Quick test_get_bool_type_error
       ; Alcotest.test_case "get_fixnum" `Quick test_get_fixnum
       ; Alcotest.test_case "get_fixnum type error" `Quick test_get_fixnum_type_error
       ; Alcotest.test_case "get_flonum" `Quick test_get_flonum
       ; Alcotest.test_case "get_string" `Quick test_get_string
       ; Alcotest.test_case "get_symbol_name" `Quick test_get_symbol_name
       ])
    ; ("Pair operations",
       [ Alcotest.test_case "car/cdr" `Quick test_car_cdr
       ; Alcotest.test_case "car type error" `Quick test_car_type_error
       ])
    ; ("Vector operations",
       [ Alcotest.test_case "vector_length" `Quick test_vector_length
       ; Alcotest.test_case "vector_ref" `Quick test_vector_ref
       ; Alcotest.test_case "vector_ref out of range" `Quick test_vector_ref_out_of_range
       ])
    ; ("Display/Write",
       [ Alcotest.test_case "display" `Quick test_display_string
       ; Alcotest.test_case "write" `Quick test_write_string
       ])
    ; ("Lookup/Call",
       [ Alcotest.test_case "lookup found" `Quick test_lookup_found
       ; Alcotest.test_case "lookup not found" `Quick test_lookup_not_found
       ; Alcotest.test_case "call simple" `Quick test_call_simple
       ; Alcotest.test_case "call error" `Quick test_call_error
       ])
    ; ("Memory",
       [ Alcotest.test_case "release" `Quick test_release
       ; Alcotest.test_case "release invalid" `Quick test_release_invalid
       ])
    ; ("Multiple instances",
       [ Alcotest.test_case "independent state" `Quick test_multiple_instances
       ])
    ]
