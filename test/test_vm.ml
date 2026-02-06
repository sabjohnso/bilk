open Wile

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let check_datum = Alcotest.check datum_testable

(* Helper: evaluate a string in a fresh instance *)
let eval s =
  let inst = Instance.create () in
  Instance.eval_string inst s

(* Helper: evaluate multiple strings sequentially in same instance, return last result *)
let eval_seq strs =
  let inst = Instance.create () in
  let result = ref Datum.Void in
  List.iter (fun s -> result := Instance.eval_string inst s) strs;
  !result

(* --- Self-evaluating literals --- *)

let test_fixnum () =
  check_datum "42" (Datum.Fixnum 42) (eval "42")

let test_negative_fixnum () =
  check_datum "-7" (Datum.Fixnum (-7)) (eval "-7")

let test_bool_true () =
  check_datum "#t" (Datum.Bool true) (eval "#t")

let test_bool_false () =
  check_datum "#f" (Datum.Bool false) (eval "#f")

let test_string () =
  check_datum "hello" (Datum.Str "hello") (eval "\"hello\"")

let test_flonum () =
  check_datum "3.14" (Datum.Flonum 3.14) (eval "3.14")

(* --- Quote --- *)

let test_quote_list () =
  let result = eval "'(1 2 3)" in
  let expected = Datum.Pair (Datum.Fixnum 1,
    Datum.Pair (Datum.Fixnum 2,
      Datum.Pair (Datum.Fixnum 3, Datum.Nil))) in
  check_datum "quoted list" expected result

let test_quote_symbol () =
  check_datum "quoted symbol" (Datum.Symbol "foo") (eval "'foo")

(* --- Variables --- *)

let test_define_and_ref () =
  let result = eval_seq ["(define x 42)"; "x"] in
  check_datum "define then ref" (Datum.Fixnum 42) result

let test_unbound_variable () =
  Alcotest.check_raises "unbound"
    (Vm.Runtime_error "unbound variable: nonexistent")
    (fun () -> ignore (eval "nonexistent"))

(* --- Arithmetic --- *)

let test_add () =
  check_datum "(+ 1 2)" (Datum.Fixnum 3) (eval "(+ 1 2)")

let test_add_variadic () =
  check_datum "(+ 1 2 3 4)" (Datum.Fixnum 10) (eval "(+ 1 2 3 4)")

let test_add_zero_args () =
  check_datum "(+)" (Datum.Fixnum 0) (eval "(+)")

let test_sub () =
  check_datum "(- 10 3)" (Datum.Fixnum 7) (eval "(- 10 3)")

let test_sub_negate () =
  check_datum "(- 5)" (Datum.Fixnum (-5)) (eval "(- 5)")

let test_mul () =
  check_datum "(* 3 4)" (Datum.Fixnum 12) (eval "(* 3 4)")

let test_nested_calls () =
  check_datum "(+ (* 2 3) 4)" (Datum.Fixnum 10) (eval "(+ (* 2 3) 4)")

(* --- Conditionals --- *)

let test_if_true () =
  check_datum "if true" (Datum.Fixnum 1) (eval "(if #t 1 2)")

let test_if_false () =
  check_datum "if false" (Datum.Fixnum 2) (eval "(if #f 1 2)")

let test_if_truthy () =
  (* Per R7RS, only #f is false *)
  check_datum "if 0" (Datum.Fixnum 1) (eval "(if 0 1 2)")

let test_if_no_alternate () =
  check_datum "if no alt" Datum.Void (eval "(if #f 1)")

(* --- Lambda --- *)

let test_lambda_identity () =
  check_datum "identity" (Datum.Fixnum 42) (eval "((lambda (x) x) 42)")

let test_lambda_closure () =
  (* make-adder pattern *)
  let result = eval_seq [
    "(define (make-adder n) (lambda (x) (+ x n)))";
    "(define add5 (make-adder 5))";
    "(add5 10)"
  ] in
  check_datum "closure captures env" (Datum.Fixnum 15) result

(* --- Define --- *)

let test_define_shorthand () =
  let result = eval_seq [
    "(define (f x) (+ x 1))";
    "(f 5)"
  ] in
  check_datum "define shorthand" (Datum.Fixnum 6) result

(* --- Set! --- *)

let test_set_bang () =
  let result = eval_seq [
    "(define x 1)";
    "(set! x 2)";
    "x"
  ] in
  check_datum "set!" (Datum.Fixnum 2) result

(* --- Begin --- *)

let test_begin () =
  check_datum "begin" (Datum.Fixnum 3) (eval "(begin 1 2 3)")

(* --- List primitives --- *)

let test_cons () =
  check_datum "cons" (Datum.Pair (Datum.Fixnum 1, Datum.Fixnum 2)) (eval "(cons 1 2)")

let test_car () =
  check_datum "car" (Datum.Fixnum 1) (eval "(car '(1 2 3))")

let test_cdr () =
  let expected = Datum.Pair (Datum.Fixnum 2,
    Datum.Pair (Datum.Fixnum 3, Datum.Nil)) in
  check_datum "cdr" expected (eval "(cdr '(1 2 3))")

let test_null () =
  check_datum "null? nil" (Datum.Bool true) (eval "(null? '())");
  check_datum "null? pair" (Datum.Bool false) (eval "(null? '(1))")

let test_pair_pred () =
  check_datum "pair? pair" (Datum.Bool true) (eval "(pair? '(1 2))");
  check_datum "pair? nil" (Datum.Bool false) (eval "(pair? '())")

let test_not_prim () =
  check_datum "not #f" (Datum.Bool true) (eval "(not #f)");
  check_datum "not #t" (Datum.Bool false) (eval "(not #t)");
  check_datum "not 0" (Datum.Bool false) (eval "(not 0)")

(* --- Comparison --- *)

let test_less_than () =
  check_datum "< true" (Datum.Bool true) (eval "(< 1 2)");
  check_datum "< false" (Datum.Bool false) (eval "(< 2 1)")

let test_num_equal () =
  check_datum "= true" (Datum.Bool true) (eval "(= 5 5)");
  check_datum "= false" (Datum.Bool false) (eval "(= 5 6)")

let test_greater_than () =
  check_datum "> true" (Datum.Bool true) (eval "(> 3 1)");
  check_datum "> false" (Datum.Bool false) (eval "(> 1 3)")

(* --- Tail recursion --- *)

let test_tail_recursion () =
  (* A loop that counts down from a large number — should not stack overflow *)
  let result = eval_seq [
    "(define (loop n) (if (= n 0) 0 (loop (- n 1))))";
    "(loop 100000)"
  ] in
  check_datum "tail recursion" (Datum.Fixnum 0) result

let () =
  Alcotest.run "VM"
    [ ("self-evaluating",
       [ Alcotest.test_case "fixnum" `Quick test_fixnum
       ; Alcotest.test_case "negative fixnum" `Quick test_negative_fixnum
       ; Alcotest.test_case "bool true" `Quick test_bool_true
       ; Alcotest.test_case "bool false" `Quick test_bool_false
       ; Alcotest.test_case "string" `Quick test_string
       ; Alcotest.test_case "flonum" `Quick test_flonum
       ])
    ; ("quote",
       [ Alcotest.test_case "quoted list" `Quick test_quote_list
       ; Alcotest.test_case "quoted symbol" `Quick test_quote_symbol
       ])
    ; ("variables",
       [ Alcotest.test_case "define and ref" `Quick test_define_and_ref
       ; Alcotest.test_case "unbound variable" `Quick test_unbound_variable
       ])
    ; ("arithmetic",
       [ Alcotest.test_case "add" `Quick test_add
       ; Alcotest.test_case "add variadic" `Quick test_add_variadic
       ; Alcotest.test_case "add zero args" `Quick test_add_zero_args
       ; Alcotest.test_case "sub" `Quick test_sub
       ; Alcotest.test_case "sub negate" `Quick test_sub_negate
       ; Alcotest.test_case "mul" `Quick test_mul
       ; Alcotest.test_case "nested calls" `Quick test_nested_calls
       ])
    ; ("conditionals",
       [ Alcotest.test_case "if true" `Quick test_if_true
       ; Alcotest.test_case "if false" `Quick test_if_false
       ; Alcotest.test_case "if truthy" `Quick test_if_truthy
       ; Alcotest.test_case "if no alternate" `Quick test_if_no_alternate
       ])
    ; ("lambda",
       [ Alcotest.test_case "identity" `Quick test_lambda_identity
       ; Alcotest.test_case "closure captures env" `Quick test_lambda_closure
       ])
    ; ("define",
       [ Alcotest.test_case "define shorthand" `Quick test_define_shorthand
       ])
    ; ("set!",
       [ Alcotest.test_case "set!" `Quick test_set_bang
       ])
    ; ("begin",
       [ Alcotest.test_case "begin" `Quick test_begin
       ])
    ; ("list primitives",
       [ Alcotest.test_case "cons" `Quick test_cons
       ; Alcotest.test_case "car" `Quick test_car
       ; Alcotest.test_case "cdr" `Quick test_cdr
       ; Alcotest.test_case "null?" `Quick test_null
       ; Alcotest.test_case "pair?" `Quick test_pair_pred
       ; Alcotest.test_case "not" `Quick test_not_prim
       ])
    ; ("comparison",
       [ Alcotest.test_case "<" `Quick test_less_than
       ; Alcotest.test_case "=" `Quick test_num_equal
       ; Alcotest.test_case ">" `Quick test_greater_than
       ])
    ; ("tail recursion",
       [ Alcotest.test_case "tail recursion" `Quick test_tail_recursion
       ])
    ]
