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

(* --- and/or --- *)

let test_and_empty () =
  check_datum "(and)" (Datum.Bool true) (eval "(and)")

let test_and_single () =
  check_datum "(and 1)" (Datum.Fixnum 1) (eval "(and 1)")

let test_and_two () =
  check_datum "(and 1 2)" (Datum.Fixnum 2) (eval "(and 1 2)")

let test_and_short_circuit () =
  check_datum "(and #f 2)" (Datum.Bool false) (eval "(and #f 2)")

let test_and_middle_false () =
  check_datum "(and 1 #f 3)" (Datum.Bool false) (eval "(and 1 #f 3)")

let test_and_all_true () =
  check_datum "(and 1 2 3)" (Datum.Fixnum 3) (eval "(and 1 2 3)")

let test_or_empty () =
  check_datum "(or)" (Datum.Bool false) (eval "(or)")

let test_or_single () =
  check_datum "(or 1)" (Datum.Fixnum 1) (eval "(or 1)")

let test_or_false_then_value () =
  check_datum "(or #f 2)" (Datum.Fixnum 2) (eval "(or #f 2)")

let test_or_short_circuit () =
  check_datum "(or 1 2)" (Datum.Fixnum 1) (eval "(or 1 2)")

let test_or_all_false () =
  check_datum "(or #f #f #f)" (Datum.Bool false) (eval "(or #f #f #f)")

let test_or_last_true () =
  check_datum "(or #f #f 3)" (Datum.Fixnum 3) (eval "(or #f #f 3)")

(* --- when/unless --- *)

let test_when_true () =
  check_datum "when true" (Datum.Fixnum 42) (eval "(when #t 42)")

let test_when_false () =
  check_datum "when false" Datum.Void (eval "(when #f 42)")

let test_when_multi_body () =
  check_datum "when multi" (Datum.Fixnum 3) (eval "(when #t 1 2 3)")

let test_unless_false () =
  check_datum "unless false" (Datum.Fixnum 42) (eval "(unless #f 42)")

let test_unless_true () =
  check_datum "unless true" Datum.Void (eval "(unless #t 42)")

(* --- let --- *)

let test_let_simple () =
  check_datum "let simple" (Datum.Fixnum 1) (eval "(let ((x 1)) x)")

let test_let_two_bindings () =
  check_datum "let two" (Datum.Fixnum 3) (eval "(let ((x 1) (y 2)) (+ x y))")

let test_let_shadow () =
  check_datum "let shadow" (Datum.Fixnum 2) (eval "(let ((x 1)) (let ((x 2)) x))")

(* --- let* --- *)

let test_let_star () =
  check_datum "let* seq" (Datum.Fixnum 2) (eval "(let* ((x 1) (y (+ x 1))) y)")

let test_let_star_empty () =
  check_datum "let* empty" (Datum.Fixnum 42) (eval "(let* () 42)")

(* --- letrec / letrec* --- *)

let test_letrec_factorial () =
  check_datum "letrec factorial" (Datum.Fixnum 120)
    (eval "(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))")

let test_letrec_star_seq () =
  check_datum "letrec* seq" (Datum.Fixnum 2)
    (eval "(letrec* ((a 1) (b (+ a 1))) b)")

let test_letrec_mutual () =
  let result = eval
    "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) \
              (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) \
       (even? 10))" in
  check_datum "mutual recursion" (Datum.Bool true) result

(* --- Named let --- *)

let test_named_let () =
  check_datum "named let sum" (Datum.Fixnum 10)
    (eval "(let loop ((i 0) (s 0)) (if (= i 5) s (loop (+ i 1) (+ s i))))")

let test_named_let_large () =
  check_datum "named let large" (Datum.Fixnum 0)
    (eval "(let loop ((n 100000)) (if (= n 0) 0 (loop (- n 1))))")

(* --- Internal define --- *)

let test_internal_define () =
  check_datum "internal define" (Datum.Fixnum 11)
    (eval "((lambda (x) (define a (+ x 1)) a) 10)")

let test_internal_define_fn () =
  check_datum "internal define fn" (Datum.Fixnum 6)
    (eval "((lambda () (define (f x) (+ x 1)) (f 5)))")

let test_internal_define_mutual () =
  let result = eval
    "((lambda () \
        (define (even? n) (if (= n 0) #t (odd? (- n 1)))) \
        (define (odd? n) (if (= n 0) #f (even? (- n 1)))) \
        (even? 6)))" in
  check_datum "internal define mutual" (Datum.Bool true) result

(* --- cond --- *)

let test_cond_true () =
  check_datum "cond true" (Datum.Fixnum 1) (eval "(cond (#t 1))")

let test_cond_second () =
  check_datum "cond second" (Datum.Fixnum 2) (eval "(cond (#f 1) (#t 2))")

let test_cond_else () =
  check_datum "cond else" (Datum.Fixnum 2) (eval "(cond (#f 1) (else 2))")

let test_cond_no_match () =
  check_datum "cond no match" Datum.Void (eval "(cond (#f 1))")

let test_cond_expr () =
  check_datum "cond expr" (Datum.Symbol "yes") (eval "(cond ((< 1 2) 'yes))")

let test_cond_multi_body () =
  check_datum "cond multi body" (Datum.Fixnum 3) (eval "(cond (#t 1 2 3))")

let test_cond_test_only () =
  (* (cond (42)) → 42, the test value is returned *)
  check_datum "cond test only" (Datum.Fixnum 42) (eval "(cond (42))")

(* --- case --- *)

let test_case_match () =
  check_datum "case match" (Datum.Symbol "b")
    (eval "(case 2 ((1) 'a) ((2) 'b) (else 'c))")

let test_case_multi_datum () =
  check_datum "case multi datum" (Datum.Symbol "hi")
    (eval "(case 3 ((1 2) 'lo) ((3 4) 'hi))")

let test_case_else () =
  check_datum "case else" (Datum.Symbol "other")
    (eval "(case 99 ((1) 'a) (else 'other))")

let test_case_no_match () =
  check_datum "case no match" Datum.Void
    (eval "(case 99 ((1) 'a))")

(* --- do --- *)

let test_do_simple () =
  check_datum "do simple" (Datum.Fixnum 5)
    (eval "(do ((i 0 (+ i 1))) ((= i 5) i))")

let test_do_two_vars () =
  check_datum "do two vars" (Datum.Fixnum 10)
    (eval "(do ((i 0 (+ i 1)) (s 0 (+ s i))) ((= i 5) s))")

let test_do_no_result () =
  check_datum "do no result" Datum.Void
    (eval "(do ((i 0 (+ i 1))) ((= i 3)))")

(* --- Equivalence primitives --- *)

let test_eqv_fixnum () =
  check_datum "eqv? same int" (Datum.Bool true) (eval "(eqv? 1 1)");
  check_datum "eqv? diff int" (Datum.Bool false) (eval "(eqv? 1 2)")

let test_eqv_symbol () =
  check_datum "eqv? same sym" (Datum.Bool true) (eval "(eqv? 'a 'a)");
  check_datum "eqv? diff sym" (Datum.Bool false) (eval "(eqv? 'a 'b)")

let test_eqv_string () =
  check_datum "eqv? strings" (Datum.Bool false) (eval "(eqv? \"a\" \"a\")")

let test_eqv_bool () =
  check_datum "eqv? #t #t" (Datum.Bool true) (eval "(eqv? #t #t)");
  check_datum "eqv? #t #f" (Datum.Bool false) (eval "(eqv? #t #f)")

let test_eqv_nil () =
  check_datum "eqv? nil nil" (Datum.Bool true) (eval "(eqv? '() '())")

let test_eq () =
  check_datum "eq? #t #t" (Datum.Bool true) (eval "(eq? #t #t)");
  check_datum "eq? 1 1" (Datum.Bool true) (eval "(eq? 1 1)")

let test_prim_list () =
  let expected = Datum.Pair (Datum.Fixnum 1,
    Datum.Pair (Datum.Fixnum 2,
      Datum.Pair (Datum.Fixnum 3, Datum.Nil))) in
  check_datum "list 1 2 3" expected (eval "(list 1 2 3)");
  check_datum "list empty" Datum.Nil (eval "(list)")

let test_le () =
  check_datum "<= true" (Datum.Bool true) (eval "(<= 1 2)");
  check_datum "<= equal" (Datum.Bool true) (eval "(<= 2 2)");
  check_datum "<= false" (Datum.Bool false) (eval "(<= 3 2)")

let test_ge () =
  check_datum ">= true" (Datum.Bool true) (eval "(>= 2 1)");
  check_datum ">= equal" (Datum.Bool true) (eval "(>= 2 2)");
  check_datum ">= false" (Datum.Bool false) (eval "(>= 1 2)")

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
    ; ("and",
       [ Alcotest.test_case "(and)" `Quick test_and_empty
       ; Alcotest.test_case "(and 1)" `Quick test_and_single
       ; Alcotest.test_case "(and 1 2)" `Quick test_and_two
       ; Alcotest.test_case "(and #f 2)" `Quick test_and_short_circuit
       ; Alcotest.test_case "(and 1 #f 3)" `Quick test_and_middle_false
       ; Alcotest.test_case "(and 1 2 3)" `Quick test_and_all_true
       ])
    ; ("or",
       [ Alcotest.test_case "(or)" `Quick test_or_empty
       ; Alcotest.test_case "(or 1)" `Quick test_or_single
       ; Alcotest.test_case "(or #f 2)" `Quick test_or_false_then_value
       ; Alcotest.test_case "(or 1 2)" `Quick test_or_short_circuit
       ; Alcotest.test_case "(or #f #f #f)" `Quick test_or_all_false
       ; Alcotest.test_case "(or #f #f 3)" `Quick test_or_last_true
       ])
    ; ("when/unless",
       [ Alcotest.test_case "when true" `Quick test_when_true
       ; Alcotest.test_case "when false" `Quick test_when_false
       ; Alcotest.test_case "when multi body" `Quick test_when_multi_body
       ; Alcotest.test_case "unless false" `Quick test_unless_false
       ; Alcotest.test_case "unless true" `Quick test_unless_true
       ])
    ; ("let",
       [ Alcotest.test_case "let simple" `Quick test_let_simple
       ; Alcotest.test_case "let two bindings" `Quick test_let_two_bindings
       ; Alcotest.test_case "let shadow" `Quick test_let_shadow
       ])
    ; ("let*",
       [ Alcotest.test_case "let*" `Quick test_let_star
       ; Alcotest.test_case "let* empty" `Quick test_let_star_empty
       ])
    ; ("letrec",
       [ Alcotest.test_case "letrec factorial" `Quick test_letrec_factorial
       ; Alcotest.test_case "letrec* sequential" `Quick test_letrec_star_seq
       ; Alcotest.test_case "letrec mutual" `Quick test_letrec_mutual
       ])
    ; ("named let",
       [ Alcotest.test_case "named let sum" `Quick test_named_let
       ; Alcotest.test_case "named let large" `Quick test_named_let_large
       ])
    ; ("internal define",
       [ Alcotest.test_case "internal define" `Quick test_internal_define
       ; Alcotest.test_case "internal define fn" `Quick test_internal_define_fn
       ; Alcotest.test_case "internal define mutual" `Quick test_internal_define_mutual
       ])
    ; ("cond",
       [ Alcotest.test_case "cond true" `Quick test_cond_true
       ; Alcotest.test_case "cond second" `Quick test_cond_second
       ; Alcotest.test_case "cond else" `Quick test_cond_else
       ; Alcotest.test_case "cond no match" `Quick test_cond_no_match
       ; Alcotest.test_case "cond expr" `Quick test_cond_expr
       ; Alcotest.test_case "cond multi body" `Quick test_cond_multi_body
       ; Alcotest.test_case "cond test only" `Quick test_cond_test_only
       ])
    ; ("case",
       [ Alcotest.test_case "case match" `Quick test_case_match
       ; Alcotest.test_case "case multi datum" `Quick test_case_multi_datum
       ; Alcotest.test_case "case else" `Quick test_case_else
       ; Alcotest.test_case "case no match" `Quick test_case_no_match
       ])
    ; ("do",
       [ Alcotest.test_case "do simple" `Quick test_do_simple
       ; Alcotest.test_case "do two vars" `Quick test_do_two_vars
       ; Alcotest.test_case "do no result" `Quick test_do_no_result
       ])
    ; ("equivalence",
       [ Alcotest.test_case "eqv? fixnum" `Quick test_eqv_fixnum
       ; Alcotest.test_case "eqv? symbol" `Quick test_eqv_symbol
       ; Alcotest.test_case "eqv? string" `Quick test_eqv_string
       ; Alcotest.test_case "eqv? bool" `Quick test_eqv_bool
       ; Alcotest.test_case "eqv? nil" `Quick test_eqv_nil
       ; Alcotest.test_case "eq?" `Quick test_eq
       ; Alcotest.test_case "list" `Quick test_prim_list
       ; Alcotest.test_case "<=" `Quick test_le
       ; Alcotest.test_case ">=" `Quick test_ge
       ])
    ; ("tail recursion",
       [ Alcotest.test_case "tail recursion" `Quick test_tail_recursion
       ])
    ]
