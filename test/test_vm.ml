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
  check_datum "hello" (Datum.Str (Bytes.of_string "hello")) (eval "\"hello\"")

let test_flonum () =
  check_datum "3.14" (Datum.Flonum 3.14) (eval "3.14")

(* --- Quote --- *)

let test_quote_list () =
  let result = eval "'(1 2 3)" in
  let expected = Datum.Pair { car = Datum.Fixnum 1; cdr =
    Datum.Pair { car = Datum.Fixnum 2; cdr =
      Datum.Pair { car = Datum.Fixnum 3; cdr = Datum.Nil } } } in
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
  check_datum "cons" (Datum.Pair { car = Datum.Fixnum 1; cdr = Datum.Fixnum 2 }) (eval "(cons 1 2)")

let test_car () =
  check_datum "car" (Datum.Fixnum 1) (eval "(car '(1 2 3))")

let test_cdr () =
  let expected = Datum.Pair { car = Datum.Fixnum 2; cdr =
    Datum.Pair { car = Datum.Fixnum 3; cdr = Datum.Nil } } in
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
  let expected = Datum.Pair { car = Datum.Fixnum 1; cdr =
    Datum.Pair { car = Datum.Fixnum 2; cdr =
      Datum.Pair { car = Datum.Fixnum 3; cdr = Datum.Nil } } } in
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

(* --- apply --- *)

let test_apply_basic () =
  check_datum "apply basic" (Datum.Fixnum 6) (eval "(apply + '(1 2 3))")

let test_apply_spread () =
  check_datum "apply spread" (Datum.Fixnum 6) (eval "(apply + 1 2 '(3))")

let test_apply_cons () =
  check_datum "apply cons" (Datum.Pair { car = Datum.Fixnum 1; cdr = Datum.Fixnum 2 })
    (eval "(apply cons '(1 2))")

let test_apply_empty () =
  check_datum "apply empty" Datum.Nil (eval "(apply list '())")

let test_apply_multi_spread () =
  let expected = Datum.Pair { car = Datum.Fixnum 1; cdr =
    Datum.Pair { car = Datum.Fixnum 2; cdr =
      Datum.Pair { car = Datum.Fixnum 3; cdr =
        Datum.Pair { car = Datum.Fixnum 4; cdr = Datum.Nil } } } } in
  check_datum "apply multi spread" expected (eval "(apply list 1 2 '(3 4))")

let test_apply_lambda () =
  check_datum "apply lambda" (Datum.Fixnum 7)
    (eval "(apply (lambda (x y) (+ x y)) '(3 4))")

let test_apply_first_class () =
  check_datum "apply first class" (Datum.Fixnum 3)
    (eval "(let ((a apply)) (a + '(1 2)))")

let test_apply_error () =
  Alcotest.check_raises "apply non-list"
    (Vm.Runtime_error "apply: last argument must be a list")
    (fun () -> ignore (eval "(apply + 1)"))

(* --- call/cc --- *)

let test_callcc_unused () =
  check_datum "unused k" (Datum.Fixnum 42)
    (eval "(call/cc (lambda (k) 42))")

let test_callcc_invoke () =
  check_datum "invoke k" (Datum.Fixnum 42)
    (eval "(call/cc (lambda (k) (k 42)))")

let test_callcc_abort () =
  check_datum "k aborts" (Datum.Fixnum 42)
    (eval "(call/cc (lambda (k) (k 42) 99))")

let test_callcc_expr_context () =
  check_datum "expr context" (Datum.Fixnum 11)
    (eval "(+ 1 (call/cc (lambda (k) (k 10))))")

let test_callcc_saved () =
  check_datum "saved cont" (Datum.Fixnum 42)
    (eval "(let ((saved #f)) \
            (let ((result (call/cc (lambda (k) (set! saved k) 1)))) \
              (if (= result 1) (saved 42) result)))")

let test_callcc_long_name () =
  check_datum "long name" (Datum.Fixnum 42)
    (eval "(call-with-current-continuation (lambda (k) (k 42)))")

let test_callcc_first_class () =
  check_datum "first class" (Datum.Fixnum 42)
    (eval "(let ((cc call/cc)) (cc (lambda (k) (k 42))))")

let test_callcc_multi_shot () =
  (* Multi-shot: continuation invoked 3 times *)
  check_datum "multi shot" (Datum.Fixnum 15)
    (eval "(let ((k-saved #f)) \
            (let ((x (call/cc (lambda (k) (set! k-saved k) 0)))) \
              (if (< x 15) (k-saved (+ x 5)) x)))")

let test_callcc_nested () =
  check_datum "nested call/cc" (Datum.Fixnum 42)
    (eval "(call/cc (lambda (outer) (call/cc (lambda (inner) (outer 42)))))")

let test_callcc_tail () =
  check_datum "tail call/cc" (Datum.Fixnum 42)
    (eval "((lambda () (call/cc (lambda (k) (k 42)))))")

let test_callcc_closure () =
  (* Continuation captured inside a closure, invoked from outside *)
  check_datum "closure captures k" (Datum.Fixnum 99)
    (eval "(let ((escape #f)) \
            (let ((result (call/cc (lambda (k) (set! escape k) 0)))) \
              (if (= result 0) (escape 99) result)))")

(* --- values / call-with-values --- *)

let test_cwv_basic () =
  check_datum "cwv basic" (Datum.Fixnum 3)
    (eval "(call-with-values (lambda () (values 1 2)) +)")

let test_cwv_single () =
  check_datum "cwv single" (Datum.Fixnum 42)
    (eval "(call-with-values (lambda () 42) (lambda (x) x))")

let test_cwv_triple () =
  let expected = Datum.Pair { car = Datum.Fixnum 1; cdr =
    Datum.Pair { car = Datum.Fixnum 2; cdr =
      Datum.Pair { car = Datum.Fixnum 3; cdr = Datum.Nil } } } in
  check_datum "cwv triple" expected
    (eval "(call-with-values (lambda () (values 1 2 3)) list)")

let test_values_single () =
  check_datum "values single" (Datum.Fixnum 42) (eval "(values 42)")

let test_cwv_no_values () =
  check_datum "cwv no values" (Datum.Fixnum 99)
    (eval "(call-with-values (lambda () (values)) (lambda () 99))")

let test_cwv_nested () =
  check_datum "cwv nested" (Datum.Fixnum 3)
    (eval "(call-with-values \
            (lambda () (values 1 2)) \
            (lambda (a b) (+ a b)))")

(* --- dynamic-wind --- *)

let test_dw_basic () =
  (* before, thunk, after all called in order; result is thunk's value *)
  check_datum "dw basic" (Datum.Fixnum 42)
    (eval "(let ((log '())) \
            (let ((result \
              (dynamic-wind \
                (lambda () (set! log (cons 'before log))) \
                (lambda () 42) \
                (lambda () (set! log (cons 'after log)))))) \
              result))")

let test_dw_order () =
  (* verify before/thunk/after execution order via side effects *)
  check_datum "dw order"
    (Datum.Pair { car = Datum.Fixnum 3; cdr =
      Datum.Pair { car = Datum.Fixnum 2; cdr =
        Datum.Pair { car = Datum.Fixnum 1; cdr = Datum.Nil } } })
    (eval "(let ((log '())) \
            (dynamic-wind \
              (lambda () (set! log (cons 1 log))) \
              (lambda () (set! log (cons 2 log))) \
              (lambda () (set! log (cons 3 log)))) \
            log)")

let test_dw_escape () =
  (* escaping via continuation calls after *)
  check_datum "dw escape"
    (Datum.Pair { car = Datum.Symbol "after"; cdr =
      Datum.Pair { car = Datum.Symbol "before"; cdr = Datum.Nil } })
    (eval "(let ((log '())) \
            (call/cc (lambda (k) \
              (dynamic-wind \
                (lambda () (set! log (cons 'before log))) \
                (lambda () (k 'escaped)) \
                (lambda () (set! log (cons 'after log)))))) \
            log)")

let test_dw_reenter () =
  (* re-entering via saved continuation calls before again *)
  check_datum "dw reenter" (Datum.Fixnum 2)
    (eval "(let ((k-saved #f) (count 0)) \
            (dynamic-wind \
              (lambda () (set! count (+ count 1))) \
              (lambda () (call/cc (lambda (k) (set! k-saved k)))) \
              (lambda () #f)) \
            (if (< count 2) (k-saved #f) count))")

let test_dw_nested () =
  (* nested dynamic-wind: outer-before, inner-before, inner-after, outer-after *)
  check_datum "dw nested"
    (Datum.Pair { car = Datum.Symbol "a1"; cdr =
      Datum.Pair { car = Datum.Symbol "a2"; cdr =
        Datum.Pair { car = Datum.Symbol "b2"; cdr =
          Datum.Pair { car = Datum.Symbol "b1"; cdr = Datum.Nil } } } })
    (eval "(let ((log '())) \
            (dynamic-wind \
              (lambda () (set! log (cons 'b1 log))) \
              (lambda () \
                (dynamic-wind \
                  (lambda () (set! log (cons 'b2 log))) \
                  (lambda () #f) \
                  (lambda () (set! log (cons 'a2 log))))) \
              (lambda () (set! log (cons 'a1 log)))) \
            log)")

let test_dw_escape_nested () =
  (* escaping from nested dynamic-wind calls both afters in right order *)
  check_datum "dw escape nested"
    (Datum.Pair { car = Datum.Symbol "a1"; cdr =
      Datum.Pair { car = Datum.Symbol "a2"; cdr =
        Datum.Pair { car = Datum.Symbol "b2"; cdr =
          Datum.Pair { car = Datum.Symbol "b1"; cdr = Datum.Nil } } } })
    (eval "(let ((log '())) \
            (call/cc (lambda (k) \
              (dynamic-wind \
                (lambda () (set! log (cons 'b1 log))) \
                (lambda () \
                  (dynamic-wind \
                    (lambda () (set! log (cons 'b2 log))) \
                    (lambda () (k 'done)) \
                    (lambda () (set! log (cons 'a2 log))))) \
                (lambda () (set! log (cons 'a1 log)))))) \
            log)")

let test_dw_first_class () =
  (* dynamic-wind as first-class value *)
  check_datum "dw first class" (Datum.Fixnum 99)
    (eval "(let ((dw dynamic-wind)) \
            (dw (lambda () #f) (lambda () 99) (lambda () #f)))")

let test_dw_thunk_result () =
  (* thunk's return value is dynamic-wind's result *)
  check_datum "dw thunk result" (Datum.Pair { car = Datum.Fixnum 1; cdr = Datum.Fixnum 2 })
    (eval "(dynamic-wind \
            (lambda () #f) \
            (lambda () (cons 1 2)) \
            (lambda () #f))")

(* --- numeric enhancements --- *)

let test_division () =
  check_datum "/ exact" (Datum.Fixnum 3) (eval "(/ 6 2)");
  check_datum "/ inexact" (Datum.Flonum 2.5) (eval "(/ 5 2)");
  check_datum "/ float" (Datum.Flonum 2.0) (eval "(/ 6.0 3.0)");
  check_datum "/ reciprocal" (Datum.Flonum 0.5) (eval "(/ 2)")

let test_abs () =
  check_datum "abs pos" (Datum.Fixnum 5) (eval "(abs 5)");
  check_datum "abs neg" (Datum.Fixnum 5) (eval "(abs -5)");
  check_datum "abs float" (Datum.Flonum 3.14) (eval "(abs -3.14)")

let test_min_max () =
  check_datum "min" (Datum.Fixnum 1) (eval "(min 3 1 2)");
  check_datum "max" (Datum.Fixnum 3) (eval "(max 1 3 2)");
  check_datum "min single" (Datum.Fixnum 42) (eval "(min 42)")

let test_quotient_remainder_modulo () =
  check_datum "quotient" (Datum.Fixnum 3) (eval "(quotient 10 3)");
  check_datum "remainder" (Datum.Fixnum 1) (eval "(remainder 10 3)");
  check_datum "remainder neg" (Datum.Fixnum (-1)) (eval "(remainder -10 3)");
  check_datum "modulo" (Datum.Fixnum 1) (eval "(modulo 10 3)");
  check_datum "modulo neg" (Datum.Fixnum 2) (eval "(modulo -10 3)")

let test_floor_ceil_trunc_round () =
  check_datum "floor 2.7" (Datum.Flonum 2.0) (eval "(floor 2.7)");
  check_datum "floor -2.3" (Datum.Flonum (-3.0)) (eval "(floor -2.3)");
  check_datum "ceiling 2.3" (Datum.Flonum 3.0) (eval "(ceiling 2.3)");
  check_datum "truncate 2.7" (Datum.Flonum 2.0) (eval "(truncate 2.7)");
  check_datum "truncate -2.7" (Datum.Flonum (-2.0)) (eval "(truncate -2.7)");
  check_datum "round 2.5" (Datum.Flonum 2.0) (eval "(round 2.5)");
  check_datum "round 3.5" (Datum.Flonum 4.0) (eval "(round 3.5)");
  check_datum "floor int" (Datum.Fixnum 3) (eval "(floor 3)")

let test_gcd_lcm () =
  check_datum "gcd" (Datum.Fixnum 4) (eval "(gcd 8 12)");
  check_datum "gcd 0" (Datum.Fixnum 0) (eval "(gcd)");
  check_datum "gcd three" (Datum.Fixnum 3) (eval "(gcd 9 12 15)");
  check_datum "lcm" (Datum.Fixnum 24) (eval "(lcm 8 12)");
  check_datum "lcm empty" (Datum.Fixnum 1) (eval "(lcm)")

let test_exact_inexact () =
  check_datum "exact->inexact" (Datum.Flonum 42.0) (eval "(exact->inexact 42)");
  check_datum "inexact->exact" (Datum.Fixnum 3) (eval "(inexact->exact 3.0)");
  check_datum "inexact" (Datum.Flonum 42.0) (eval "(inexact 42)");
  check_datum "exact" (Datum.Fixnum 3) (eval "(exact 3.0)")

let test_expt_sqrt () =
  check_datum "expt" (Datum.Fixnum 8) (eval "(expt 2 3)");
  check_datum "expt 0" (Datum.Fixnum 1) (eval "(expt 5 0)");
  check_datum "sqrt 4" (Datum.Fixnum 2) (eval "(sqrt 4)");
  check_datum "sqrt 2" (Datum.Flonum (sqrt 2.0)) (eval "(sqrt 2)")

let test_chain_compare () =
  check_datum "< chain true" (Datum.Bool true) (eval "(< 1 2 3)");
  check_datum "< chain false" (Datum.Bool false) (eval "(< 1 3 2)");
  check_datum "= chain true" (Datum.Bool true) (eval "(= 2 2 2)");
  check_datum "<= chain" (Datum.Bool true) (eval "(<= 1 1 2)");
  check_datum ">= chain" (Datum.Bool true) (eval "(>= 3 2 2)")

let test_number_string () =
  check_datum "num->str" (Datum.Str (Bytes.of_string "42")) (eval "(number->string 42)");
  check_datum "num->str hex" (Datum.Str (Bytes.of_string "ff")) (eval "(number->string 255 16)");
  check_datum "num->str bin" (Datum.Str (Bytes.of_string "101")) (eval "(number->string 5 2)");
  check_datum "str->num" (Datum.Fixnum 42) (eval "(string->number \"42\")");
  check_datum "str->num float" (Datum.Flonum 3.14) (eval "(string->number \"3.14\")");
  check_datum "str->num fail" (Datum.Bool false) (eval "(string->number \"abc\")");
  check_datum "str->num hex" (Datum.Fixnum 255) (eval "(string->number \"ff\" 16)")

(* --- equal? --- *)

let test_equal_basic () =
  check_datum "equal? same int" (Datum.Bool true) (eval "(equal? 1 1)");
  check_datum "equal? diff int" (Datum.Bool false) (eval "(equal? 1 2)");
  check_datum "equal? strings" (Datum.Bool true) (eval "(equal? \"abc\" \"abc\")");
  check_datum "equal? nested" (Datum.Bool true)
    (eval "(equal? '(1 (2 3)) '(1 (2 3)))");
  check_datum "equal? diff struct" (Datum.Bool false)
    (eval "(equal? '(1 2) '(1 3))");
  check_datum "equal? vectors" (Datum.Bool true)
    (eval "(equal? '#(1 2 3) '#(1 2 3))");
  check_datum "equal? diff types" (Datum.Bool false)
    (eval "(equal? 1 \"1\")")

(* --- type predicates --- *)

let test_type_boolean () =
  check_datum "boolean? #t" (Datum.Bool true) (eval "(boolean? #t)");
  check_datum "boolean? #f" (Datum.Bool true) (eval "(boolean? #f)");
  check_datum "boolean? 0" (Datum.Bool false) (eval "(boolean? 0)")

let test_type_boolean_eq () =
  check_datum "boolean=? same" (Datum.Bool true) (eval "(boolean=? #t #t)");
  check_datum "boolean=? diff" (Datum.Bool false) (eval "(boolean=? #t #f)");
  check_datum "boolean=? three" (Datum.Bool true) (eval "(boolean=? #f #f #f)")

let test_type_number () =
  check_datum "number? int" (Datum.Bool true) (eval "(number? 42)");
  check_datum "number? float" (Datum.Bool true) (eval "(number? 3.14)");
  check_datum "number? sym" (Datum.Bool false) (eval "(number? 'x)");
  check_datum "complex? int" (Datum.Bool true) (eval "(complex? 42)");
  check_datum "real? int" (Datum.Bool true) (eval "(real? 42)");
  check_datum "rational? int" (Datum.Bool true) (eval "(rational? 42)")

let test_type_integer () =
  check_datum "integer? int" (Datum.Bool true) (eval "(integer? 42)");
  check_datum "integer? float" (Datum.Bool false) (eval "(integer? 3.14)");
  check_datum "integer? exact float" (Datum.Bool true) (eval "(integer? 3.0)")

let test_type_exact () =
  check_datum "exact? int" (Datum.Bool true) (eval "(exact? 42)");
  check_datum "exact? float" (Datum.Bool false) (eval "(exact? 3.14)");
  check_datum "inexact? int" (Datum.Bool false) (eval "(inexact? 42)");
  check_datum "inexact? float" (Datum.Bool true) (eval "(inexact? 3.14)");
  check_datum "exact-integer?" (Datum.Bool true) (eval "(exact-integer? 42)");
  check_datum "exact-integer? float" (Datum.Bool false) (eval "(exact-integer? 3.14)")

let test_type_numeric_preds () =
  check_datum "zero? 0" (Datum.Bool true) (eval "(zero? 0)");
  check_datum "zero? 1" (Datum.Bool false) (eval "(zero? 1)");
  check_datum "zero? 0.0" (Datum.Bool true) (eval "(zero? 0.0)");
  check_datum "positive? 1" (Datum.Bool true) (eval "(positive? 1)");
  check_datum "positive? -1" (Datum.Bool false) (eval "(positive? -1)");
  check_datum "negative? -1" (Datum.Bool true) (eval "(negative? -1)");
  check_datum "negative? 1" (Datum.Bool false) (eval "(negative? 1)");
  check_datum "odd? 3" (Datum.Bool true) (eval "(odd? 3)");
  check_datum "odd? 2" (Datum.Bool false) (eval "(odd? 2)");
  check_datum "even? 4" (Datum.Bool true) (eval "(even? 4)");
  check_datum "even? 3" (Datum.Bool false) (eval "(even? 3)")

let test_type_symbol () =
  check_datum "symbol? sym" (Datum.Bool true) (eval "(symbol? 'foo)");
  check_datum "symbol? str" (Datum.Bool false) (eval "(symbol? \"foo\")");
  check_datum "symbol=? same" (Datum.Bool true) (eval "(symbol=? 'a 'a)");
  check_datum "symbol=? diff" (Datum.Bool false) (eval "(symbol=? 'a 'b)")

let test_symbol_string () =
  check_datum "sym->str" (Datum.Str (Bytes.of_string "hello"))
    (eval "(symbol->string 'hello)");
  check_datum "str->sym" (Datum.Symbol "hello")
    (eval "(string->symbol \"hello\")");
  check_datum "round-trip" (Datum.Bool true)
    (eval "(equal? 'hello (string->symbol (symbol->string 'hello)))")

let test_type_predicates () =
  check_datum "char?" (Datum.Bool true) (eval "(char? #\\a)");
  check_datum "char? int" (Datum.Bool false) (eval "(char? 42)");
  check_datum "string?" (Datum.Bool true) (eval "(string? \"hi\")");
  check_datum "string? int" (Datum.Bool false) (eval "(string? 42)");
  check_datum "vector?" (Datum.Bool true) (eval "(vector? '#(1))");
  check_datum "vector? int" (Datum.Bool false) (eval "(vector? 42)");
  check_datum "bytevector?" (Datum.Bool true) (eval "(bytevector? #u8(1))");
  check_datum "bytevector? int" (Datum.Bool false) (eval "(bytevector? 42)");
  check_datum "procedure? lambda" (Datum.Bool true) (eval "(procedure? car)");
  check_datum "procedure? int" (Datum.Bool false) (eval "(procedure? 42)");
  check_datum "procedure? closure" (Datum.Bool true) (eval "(procedure? (lambda () 1))");
  check_datum "list? proper" (Datum.Bool true) (eval "(list? '(1 2 3))");
  check_datum "list? nil" (Datum.Bool true) (eval "(list? '())");
  check_datum "list? dotted" (Datum.Bool false) (eval "(list? (cons 1 2))");
  check_datum "list? non" (Datum.Bool false) (eval "(list? 42)");
  check_datum "eof-object?" (Datum.Bool true) (eval "(eof-object? (eof-object))");
  check_datum "eof-object? int" (Datum.Bool false) (eval "(eof-object? 42)")

(* --- pair & list primitives --- *)

let test_set_car_cdr () =
  check_datum "set-car!" (Datum.Pair { car = Datum.Fixnum 99; cdr = Datum.Fixnum 2 })
    (eval "(let ((p (cons 1 2))) (set-car! p 99) p)");
  check_datum "set-cdr!" (Datum.Pair { car = Datum.Fixnum 1; cdr = Datum.Fixnum 99 })
    (eval "(let ((p (cons 1 2))) (set-cdr! p 99) p)")

let test_cxxr () =
  check_datum "caar" (Datum.Fixnum 1)
    (eval "(caar '((1 2) 3))");
  check_datum "cadr" (Datum.Fixnum 2)
    (eval "(cadr '(1 2 3))");
  check_datum "cdar" (Datum.Fixnum 2)
    (eval "(cdar '((1 . 2) 3))");
  check_datum "cddr" (Datum.Pair { car = Datum.Fixnum 3; cdr = Datum.Nil })
    (eval "(cddr '(1 2 3))")

let test_make_list () =
  check_datum "make-list" (Datum.Fixnum 3) (eval "(length (make-list 3))");
  check_datum "make-list fill" (Datum.Bool true)
    (eval "(equal? '(#t #t #t) (make-list 3 #t))");
  check_datum "make-list 0" Datum.Nil (eval "(make-list 0)")

let test_length () =
  check_datum "length 0" (Datum.Fixnum 0) (eval "(length '())");
  check_datum "length 3" (Datum.Fixnum 3) (eval "(length '(1 2 3))")

let test_append () =
  check_datum "append 2" (Datum.Bool true)
    (eval "(equal? '(1 2 3 4) (append '(1 2) '(3 4)))");
  check_datum "append 3" (Datum.Bool true)
    (eval "(equal? '(1 2 3) (append '(1) '(2) '(3)))");
  check_datum "append empty" Datum.Nil (eval "(append)");
  check_datum "append one" (Datum.Bool true)
    (eval "(equal? '(1 2) (append '(1 2)))");
  check_datum "append last non-list" (Datum.Pair { car = Datum.Fixnum 1; cdr = Datum.Fixnum 2 })
    (eval "(append '(1) 2)")

let test_reverse () =
  check_datum "reverse" (Datum.Bool true)
    (eval "(equal? '(3 2 1) (reverse '(1 2 3)))");
  check_datum "reverse empty" Datum.Nil (eval "(reverse '())")

let test_list_tail_ref_set () =
  check_datum "list-tail" (Datum.Bool true)
    (eval "(equal? '(3 4) (list-tail '(1 2 3 4) 2))");
  check_datum "list-ref" (Datum.Fixnum 3)
    (eval "(list-ref '(1 2 3 4) 2)");
  check_datum "list-set!" (Datum.Bool true)
    (eval "(let ((l (list 1 2 3))) (list-set! l 1 99) (equal? '(1 99 3) l))")

let test_list_copy () =
  check_datum "list-copy" (Datum.Bool true)
    (eval "(let ((a '(1 2 3))) \
             (let ((b (list-copy a))) \
               (equal? a b)))");
  check_datum "list-copy independent" (Datum.Bool true)
    (eval "(let ((a (list 1 2 3))) \
             (let ((b (list-copy a))) \
               (set-car! b 99) \
               (equal? '(1 2 3) a)))")

let test_memq_member () =
  check_datum "memq found" (Datum.Bool true)
    (eval "(equal? '(2 3) (memq 2 '(1 2 3)))");
  check_datum "memq not found" (Datum.Bool false)
    (eval "(memq 4 '(1 2 3))");
  check_datum "memv found" (Datum.Bool true)
    (eval "(equal? '(2 3) (memv 2 '(1 2 3)))");
  check_datum "member found" (Datum.Bool true)
    (eval "(equal? '((2) 3) (member '(2) '(1 (2) 3)))");
  check_datum "member not found" (Datum.Bool false)
    (eval "(member '(4) '(1 (2) 3))")

let test_assq_assoc () =
  check_datum "assq found" (Datum.Bool true)
    (eval "(equal? '(b 2) (assq 'b '((a 1) (b 2) (c 3))))");
  check_datum "assq not found" (Datum.Bool false)
    (eval "(assq 'd '((a 1) (b 2)))");
  check_datum "assv found" (Datum.Bool true)
    (eval "(equal? '(2 b) (assv 2 '((1 a) (2 b) (3 c))))");
  check_datum "assoc found" (Datum.Bool true)
    (eval "(equal? '((2) b) (assoc '(2) '((1 a) ((2) b) (3 c))))");
  check_datum "assoc not found" (Datum.Bool false)
    (eval "(assoc '(4) '((1 a) (2 b)))")

(* --- character primitives --- *)

let test_char_compare () =
  check_datum "char=?" (Datum.Bool true) (eval "(char=? #\\a #\\a)");
  check_datum "char=? diff" (Datum.Bool false) (eval "(char=? #\\a #\\b)");
  check_datum "char<?" (Datum.Bool true) (eval "(char<? #\\a #\\b)");
  check_datum "char>?" (Datum.Bool true) (eval "(char>? #\\b #\\a)");
  check_datum "char<=?" (Datum.Bool true) (eval "(char<=? #\\a #\\a)");
  check_datum "char>=?" (Datum.Bool true) (eval "(char>=? #\\b #\\a)")

let test_char_ci_compare () =
  check_datum "char-ci=?" (Datum.Bool true) (eval "(char-ci=? #\\A #\\a)");
  check_datum "char-ci=? diff" (Datum.Bool false) (eval "(char-ci=? #\\a #\\b)");
  check_datum "char-ci<?" (Datum.Bool true) (eval "(char-ci<? #\\A #\\b)");
  check_datum "char-ci>?" (Datum.Bool true) (eval "(char-ci>? #\\B #\\a)")

let test_char_integer () =
  check_datum "char->integer" (Datum.Fixnum 65) (eval "(char->integer #\\A)");
  check_datum "integer->char" (Datum.Char (Uchar.of_int 65)) (eval "(integer->char 65)");
  check_datum "round-trip" (Datum.Bool true)
    (eval "(char=? #\\A (integer->char (char->integer #\\A)))")

let test_char_case () =
  check_datum "upcase" (Datum.Char (Uchar.of_int 0x41)) (eval "(char-upcase #\\a)");
  check_datum "downcase" (Datum.Char (Uchar.of_int 0x61)) (eval "(char-downcase #\\A)");
  check_datum "foldcase" (Datum.Char (Uchar.of_int 0x61)) (eval "(char-foldcase #\\A)")

let test_char_classification () =
  check_datum "alphabetic" (Datum.Bool true) (eval "(char-alphabetic? #\\a)");
  check_datum "alphabetic non" (Datum.Bool false) (eval "(char-alphabetic? #\\1)");
  check_datum "numeric" (Datum.Bool true) (eval "(char-numeric? #\\5)");
  check_datum "numeric non" (Datum.Bool false) (eval "(char-numeric? #\\a)");
  check_datum "whitespace space" (Datum.Bool true) (eval "(char-whitespace? #\\space)");
  check_datum "whitespace tab" (Datum.Bool true) (eval "(char-whitespace? #\\tab)");
  check_datum "whitespace non" (Datum.Bool false) (eval "(char-whitespace? #\\a)");
  check_datum "upper-case" (Datum.Bool true) (eval "(char-upper-case? #\\A)");
  check_datum "upper-case non" (Datum.Bool false) (eval "(char-upper-case? #\\a)");
  check_datum "lower-case" (Datum.Bool true) (eval "(char-lower-case? #\\a)");
  check_datum "lower-case non" (Datum.Bool false) (eval "(char-lower-case? #\\A)")

let test_digit_value () =
  check_datum "digit-value 5" (Datum.Fixnum 5) (eval "(digit-value #\\5)");
  check_datum "digit-value 0" (Datum.Fixnum 0) (eval "(digit-value #\\0)");
  check_datum "digit-value non" (Datum.Bool false) (eval "(digit-value #\\a)")

(* --- string primitives --- *)

let test_string_construct () =
  check_datum "make-string" (Datum.Fixnum 3) (eval "(string-length (make-string 3))");
  check_datum "make-string fill" (Datum.Bool true)
    (eval "(string=? \"aaa\" (make-string 3 #\\a))");
  check_datum "string from chars" (Datum.Bool true)
    (eval "(string=? \"abc\" (string #\\a #\\b #\\c))")

let test_string_ref_set () =
  check_datum "string-ref" (Datum.Char (Uchar.of_int 0x62))
    (eval "(string-ref \"abc\" 1)");
  check_datum "string-set!" (Datum.Bool true)
    (eval "(let ((s (string-copy \"abc\"))) (string-set! s 1 #\\x) (string=? \"axc\" s))")

let test_string_compare () =
  check_datum "string=?" (Datum.Bool true) (eval "(string=? \"abc\" \"abc\")");
  check_datum "string=? diff" (Datum.Bool false) (eval "(string=? \"abc\" \"abd\")");
  check_datum "string<?" (Datum.Bool true) (eval "(string<? \"abc\" \"abd\")");
  check_datum "string>?" (Datum.Bool true) (eval "(string>? \"abd\" \"abc\")");
  check_datum "string<=?" (Datum.Bool true) (eval "(string<=? \"abc\" \"abc\")");
  check_datum "string>=?" (Datum.Bool true) (eval "(string>=? \"abc\" \"abc\")")

let test_string_ci_compare () =
  check_datum "string-ci=?" (Datum.Bool true) (eval "(string-ci=? \"ABC\" \"abc\")");
  check_datum "string-ci<?" (Datum.Bool true) (eval "(string-ci<? \"ABC\" \"abd\")")

let test_substring () =
  check_datum "substring" (Datum.Bool true)
    (eval "(string=? \"bc\" (substring \"abcd\" 1 3))")

let test_string_append () =
  check_datum "string-append" (Datum.Bool true)
    (eval "(string=? \"abcdef\" (string-append \"ab\" \"cd\" \"ef\"))");
  check_datum "string-append empty" (Datum.Bool true)
    (eval "(string=? \"\" (string-append))")

let test_string_list_conv () =
  check_datum "string->list" (Datum.Bool true)
    (eval "(equal? '(#\\a #\\b #\\c) (string->list \"abc\"))");
  check_datum "list->string" (Datum.Bool true)
    (eval "(string=? \"abc\" (list->string '(#\\a #\\b #\\c)))")

let test_string_copy_fill () =
  check_datum "string-copy" (Datum.Bool true)
    (eval "(let ((a \"abc\")) \
             (let ((b (string-copy a))) \
               (string=? a b)))");
  check_datum "string-copy independent" (Datum.Bool true)
    (eval "(let ((a (string-copy \"abc\"))) \
             (let ((b (string-copy a))) \
               (string-set! b 0 #\\x) \
               (string=? \"abc\" a)))");
  check_datum "string-copy!" (Datum.Bool true)
    (eval "(let ((s (string-copy \"abcde\"))) \
             (string-copy! s 1 \"xy\") \
             (string=? \"axyde\" s))");
  check_datum "string-fill!" (Datum.Bool true)
    (eval "(let ((s (string-copy \"abc\"))) \
             (string-fill! s #\\x) \
             (string=? \"xxx\" s))")

let test_string_case () =
  check_datum "string-upcase" (Datum.Bool true)
    (eval "(string=? \"ABC\" (string-upcase \"abc\"))");
  check_datum "string-downcase" (Datum.Bool true)
    (eval "(string=? \"abc\" (string-downcase \"ABC\"))");
  check_datum "string-foldcase" (Datum.Bool true)
    (eval "(string=? \"abc\" (string-foldcase \"ABC\"))")

(* --- vector primitives --- *)

let test_vector_construct () =
  check_datum "make-vector" (Datum.Fixnum 3) (eval "(vector-length (make-vector 3))");
  check_datum "make-vector fill" (Datum.Bool true)
    (eval "(equal? '#(7 7 7) (make-vector 3 7))");
  check_datum "vector" (Datum.Bool true)
    (eval "(equal? '#(1 2 3) (vector 1 2 3))")

let test_vector_ref_set () =
  check_datum "vector-ref" (Datum.Fixnum 2) (eval "(vector-ref '#(1 2 3) 1)");
  check_datum "vector-set!" (Datum.Bool true)
    (eval "(let ((v (vector 1 2 3))) (vector-set! v 1 99) (equal? '#(1 99 3) v))")

let test_vector_conversions () =
  check_datum "vector->list" (Datum.Bool true)
    (eval "(equal? '(1 2 3) (vector->list '#(1 2 3)))");
  check_datum "list->vector" (Datum.Bool true)
    (eval "(equal? '#(1 2 3) (list->vector '(1 2 3)))");
  check_datum "vector->string" (Datum.Bool true)
    (eval "(string=? \"abc\" (vector->string (vector #\\a #\\b #\\c)))");
  check_datum "string->vector" (Datum.Bool true)
    (eval "(equal? (vector #\\a #\\b #\\c) (string->vector \"abc\"))")

let test_vector_copy_append () =
  check_datum "vector-copy" (Datum.Bool true)
    (eval "(let ((a (vector 1 2 3))) \
             (let ((b (vector-copy a))) \
               (vector-set! b 0 99) \
               (equal? '#(1 2 3) a)))");
  check_datum "vector-copy!" (Datum.Bool true)
    (eval "(let ((v (vector 1 2 3 4 5))) \
             (vector-copy! v 1 (vector 10 20)) \
             (equal? '#(1 10 20 4 5) v))");
  check_datum "vector-append" (Datum.Bool true)
    (eval "(equal? '#(1 2 3 4) (vector-append '#(1 2) '#(3 4)))");
  check_datum "vector-fill!" (Datum.Bool true)
    (eval "(let ((v (vector 1 2 3))) \
             (vector-fill! v 0) \
             (equal? '#(0 0 0) v))")

(* --- bytevector primitives --- *)

let test_bytevector_construct () =
  check_datum "make-bytevector" (Datum.Fixnum 3) (eval "(bytevector-length (make-bytevector 3))");
  check_datum "make-bytevector fill" (Datum.Bool true)
    (eval "(equal? (bytevector 7 7 7) (make-bytevector 3 7))");
  check_datum "bytevector" (Datum.Fixnum 3) (eval "(bytevector-length (bytevector 1 2 3))")

let test_bytevector_ref_set () =
  check_datum "bytevector-u8-ref" (Datum.Fixnum 2) (eval "(bytevector-u8-ref (bytevector 1 2 3) 1)");
  check_datum "bytevector-u8-set!" (Datum.Bool true)
    (eval "(let ((bv (bytevector 1 2 3))) \
             (bytevector-u8-set! bv 1 99) \
             (equal? (bytevector 1 99 3) bv))")

let test_bytevector_copy_append () =
  check_datum "bytevector-copy" (Datum.Bool true)
    (eval "(let ((a (bytevector 1 2 3))) \
             (let ((b (bytevector-copy a))) \
               (bytevector-u8-set! b 0 99) \
               (equal? (bytevector 1 2 3) a)))");
  check_datum "bytevector-append" (Datum.Bool true)
    (eval "(equal? (bytevector 1 2 3 4) (bytevector-append (bytevector 1 2) (bytevector 3 4)))")

let test_utf8_string () =
  check_datum "utf8->string" (Datum.Bool true)
    (eval "(string=? \"abc\" (utf8->string (bytevector 97 98 99)))");
  check_datum "string->utf8" (Datum.Bool true)
    (eval "(equal? (bytevector 97 98 99) (string->utf8 \"abc\"))")

(* --- exception primitives --- *)

let test_error_objects () =
  check_datum "error-object?" (Datum.Bool true)
    (eval "(error-object? (%make-error \"test\" 1 2))");
  check_datum "error-object? non" (Datum.Bool false)
    (eval "(error-object? 42)");
  check_datum "error-object-message" (Datum.Bool true)
    (eval "(string=? \"test\" (error-object-message (%make-error \"test\")))");
  check_datum "error-object-irritants" (Datum.Bool true)
    (eval "(equal? '(1 2) (error-object-irritants (%make-error \"test\" 1 2)))")

let test_raise_handle () =
  check_datum "basic raise/handle" (Datum.Fixnum 42)
    (eval "(call/cc (lambda (k) \
             (with-exception-handler \
               (lambda (e) (k 42)) \
               (lambda () (raise 'boom)))))");
  check_datum "handler gets value" (Datum.Symbol "boom")
    (eval "(call/cc (lambda (k) \
             (with-exception-handler \
               (lambda (e) (k e)) \
               (lambda () (raise 'boom)))))")

let test_nested_handlers () =
  check_datum "nested handlers" (Datum.Symbol "inner")
    (eval "(call/cc (lambda (k) \
             (with-exception-handler \
               (lambda (e) (k 'outer)) \
               (lambda () \
                 (call/cc (lambda (k2) \
                   (with-exception-handler \
                     (lambda (e) (k2 'inner)) \
                     (lambda () (raise 'boom)))))))))")

let test_raise_continuable () =
  check_datum "raise-continuable" (Datum.Fixnum 99)
    (eval "(with-exception-handler \
             (lambda (e) 99) \
             (lambda () (raise-continuable 'boom)))")

let test_error_raise () =
  check_datum "error raises" (Datum.Bool true)
    (eval "(call/cc (lambda (k) \
             (with-exception-handler \
               (lambda (e) (k (error-object? e))) \
               (lambda () (error \"test error\" 1 2)))))");
  check_datum "error message" (Datum.Bool true)
    (eval "(call/cc (lambda (k) \
             (with-exception-handler \
               (lambda (e) (k (string=? \"test error\" (error-object-message e)))) \
               (lambda () (error \"test error\")))))")

let test_exception_dw_interaction () =
  (* handler can escape with call/cc through dynamic-wind *)
  check_datum "exception + dynamic-wind" (Datum.Fixnum 42)
    (eval "(call/cc (lambda (k) \
             (with-exception-handler \
               (lambda (e) (k 42)) \
               (lambda () \
                 (dynamic-wind \
                   (lambda () #f) \
                   (lambda () (raise 'boom)) \
                   (lambda () #f))))))")

let test_unhandled_error () =
  Alcotest.check_raises "unhandled raise" (Vm.Runtime_error "unhandled exception: boom")
    (fun () -> ignore (eval "(raise 'boom)"))

(* --- higher-order + write --- *)

let test_map () =
  check_datum "map basic" (Datum.Bool true)
    (eval "(equal? '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)))");
  check_datum "map empty" Datum.Nil
    (eval "(map (lambda (x) x) '())")

let test_for_each () =
  check_datum "for-each" (Datum.Fixnum 6)
    (eval "(let ((sum 0)) \
             (for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3)) \
             sum)")

let test_string_map () =
  check_datum "string-map" (Datum.Bool true)
    (eval "(string=? \"ABC\" (string-map char-upcase \"abc\"))")

let test_string_for_each () =
  check_datum "string-for-each" (Datum.Fixnum 3)
    (eval "(let ((count 0)) \
             (string-for-each (lambda (c) (set! count (+ count 1))) \"abc\") \
             count)")

let test_vector_map () =
  check_datum "vector-map" (Datum.Bool true)
    (eval "(equal? '#(2 4 6) (vector-map (lambda (x) (* x 2)) '#(1 2 3)))")

let test_vector_for_each () =
  check_datum "vector-for-each" (Datum.Fixnum 6)
    (eval "(let ((sum 0)) \
             (vector-for-each (lambda (x) (set! sum (+ sum x))) '#(1 2 3)) \
             sum)")

let test_write () =
  (* write puts quotes on strings, display doesn't *)
  check_datum "write returns void" Datum.Void
    (eval "(write 42)")

(* --- define-syntax / syntax-rules --- *)

let test_macro_constant () =
  check_datum "constant macro" (Datum.Fixnum 5)
    (eval_seq [
      "(define-syntax five (syntax-rules () ((five) 5)))";
      "(five)"])

let test_macro_pattern_var () =
  check_datum "pattern var" (Datum.Fixnum 6)
    (eval_seq [
      "(define-syntax inc (syntax-rules () ((inc x) (+ x 1))))";
      "(inc 5)"])

let test_macro_multi_rule () =
  check_datum "multi rule" (Datum.Fixnum 1)
    (eval_seq [
      "(define-syntax my-if (syntax-rules () \
         ((my-if #t t f) t) \
         ((my-if #f t f) f) \
         ((my-if test t f) (if test t f))))";
      "(my-if #t 1 2)"])

let test_macro_ellipsis () =
  check_datum "ellipsis" (Datum.Bool true)
    (eval_seq [
      "(define-syntax my-list (syntax-rules () ((my-list x ...) (list x ...))))";
      "(equal? '(1 2 3) (my-list 1 2 3))"])

let test_macro_ellipsis_zero () =
  check_datum "ellipsis zero" Datum.Nil
    (eval_seq [
      "(define-syntax my-list (syntax-rules () ((my-list x ...) (list x ...))))";
      "(my-list)"])

let test_macro_recursive () =
  check_datum "recursive macro" (Datum.Fixnum 3)
    (eval_seq [
      "(define-syntax my-begin (syntax-rules () \
         ((my-begin e) e) \
         ((my-begin e rest ...) (begin e (my-begin rest ...)))))";
      "(my-begin 1 2 3)"])

let test_macro_hygiene () =
  (* swap! macro introduces tmp — shouldn't capture user's tmp *)
  check_datum "hygiene swap" (Datum.Bool true)
    (eval_seq [
      "(define-syntax swap! (syntax-rules () \
         ((swap! a b) (let ((tmp a)) (set! a b) (set! b tmp)))))";
      "(let ((x 1) (y 2) (tmp 99)) \
         (swap! x y) \
         (and (= x 2) (= y 1) (= tmp 99)))"])

let test_macro_persistent () =
  check_datum "persistent" (Datum.Fixnum 10)
    (eval_seq [
      "(define-syntax double (syntax-rules () ((double x) (+ x x))))";
      "(double 5)"])

let test_macro_nested () =
  check_datum "nested macro" (Datum.Fixnum 7)
    (eval_seq [
      "(define-syntax inc (syntax-rules () ((inc x) (+ x 1))))";
      "(define-syntax double (syntax-rules () ((double x) (+ x x))))";
      "(inc (double 3))"])

let test_macro_no_match () =
  Alcotest.check_raises "no matching pattern"
    (Compiler.Compile_error (Loc.none, ""))
    (fun () ->
      try ignore (eval_seq [
        "(define-syntax my-add (syntax-rules () ((my-add a b) (+ a b))))";
        "(my-add 1)"])
      with Compiler.Compile_error (_, _) ->
        raise (Compiler.Compile_error (Loc.none, "")))

let test_macro_literal () =
  check_datum "literal keyword" (Datum.Fixnum 10)
    (eval_seq [
      "(define-syntax my-arrow (syntax-rules (=>) \
         ((my-arrow x => f) (f x))))";
      "(my-arrow 5 => (lambda (n) (* n 2)))"])

let test_macro_underscore () =
  check_datum "underscore wildcard" (Datum.Fixnum 42)
    (eval_seq [
      "(define-syntax always-42 (syntax-rules () ((always-42 _ _) 42)))";
      "(always-42 hello world)"])

let test_macro_vector_ellipsis () =
  check_datum "vector ellipsis" (Datum.Bool true)
    (eval_seq [
      "(define-syntax vec-list (syntax-rules () \
         ((vec-list #(x ...)) (list x ...))))";
      "(equal? '(1 2 3) (vec-list #(1 2 3)))"])

let test_macro_underscore_literal () =
  (* When _ is in the literals list, it should match literally, not as wildcard *)
  check_datum "underscore as literal" (Datum.Fixnum 1)
    (eval_seq [
      "(define _ 'placeholder)";
      "(define-syntax test-ul (syntax-rules (_) \
         ((test-ul _) 1) \
         ((test-ul x) 2)))";
      "(test-ul _)"]);
  check_datum "non-underscore falls through" (Datum.Fixnum 2)
    (eval_seq [
      "(define _ 'placeholder)";
      "(define-syntax test-ul (syntax-rules (_) \
         ((test-ul _) 1) \
         ((test-ul x) 2)))";
      "(test-ul hello)"])

let test_macro_vector_tmpl_ellipsis () =
  (* Vector template with ellipsis: #(x ...) in template position *)
  check_datum "vector template ellipsis" (Datum.Bool true)
    (eval_seq [
      "(define-syntax list->vec (syntax-rules () \
         ((list->vec x ...) #(x ...))))";
      "(equal? #(1 2 3) (list->vec 1 2 3))"]);
  check_datum "vector template ellipsis zero" (Datum.Bool true)
    (eval_seq [
      "(define-syntax list->vec (syntax-rules () \
         ((list->vec x ...) #(x ...))))";
      "(equal? #() (list->vec))"])

let test_guard_single_eval () =
  (* Guard test expressions should be evaluated exactly once *)
  check_datum "guard test single eval" (Datum.Fixnum 1)
    (eval_seq [
      "(define counter 0)";
      "(guard (exn \
         ((begin (set! counter (+ counter 1)) exn))) \
       (raise 42))";
      "counter"])

let test_guard_multi_values () =
  (* Guard body should propagate multiple return values *)
  check_datum "guard multi values" (Datum.Bool true)
    (eval "(equal? '(1 2 3) \
             (call-with-values \
               (lambda () (guard (exn (else 'error)) (values 1 2 3))) \
               list))")

let test_guard_reraise_dyn_env () =
  (* Re-raise should happen in the original raise's dynamic environment.
     The dynamic-wind after thunk should fire exactly once on exit. *)
  check_datum "guard reraise dyn env" (Datum.Bool true)
    (eval_seq [
      "(define log '())";
      "(guard (exn (#t 'caught)) \
         (guard (exn (#f)) \
           (dynamic-wind \
             (lambda () (set! log (cons 'in log))) \
             (lambda () (raise 'boom)) \
             (lambda () (set! log (cons 'out log))))))";
      "(equal? '(out in out in) log)"])

(* --- let-syntax / letrec-syntax --- *)

let test_let_syntax_basic () =
  check_datum "let-syntax" (Datum.Fixnum 10)
    (eval "(let-syntax ((double (syntax-rules () ((double x) (+ x x))))) \
             (double 5))")

let test_let_syntax_scope () =
  (* let-syntax binding not visible outside *)
  check_datum "let-syntax scope" (Datum.Fixnum 3)
    (eval "(begin \
             (let-syntax ((inc (syntax-rules () ((inc x) (+ x 1))))) \
               (inc 2)))")

let test_let_syntax_shadow () =
  check_datum "let-syntax shadows" (Datum.Fixnum 100)
    (eval_seq [
      "(define-syntax double (syntax-rules () ((double x) (+ x x))))";
      "(let-syntax ((double (syntax-rules () ((double x) (* x x))))) \
         (double 10))";
    ])

let test_letrec_syntax_self_ref () =
  check_datum "letrec-syntax self-ref" (Datum.Fixnum 6)
    (eval "(letrec-syntax \
             ((my-or (syntax-rules () \
               ((my-or) #f) \
               ((my-or e) e) \
               ((my-or e rest ...) (let ((t e)) (if t t (my-or rest ...))))))) \
             (my-or #f #f 6))")

let test_let_syntax_multi () =
  check_datum "let-syntax multi" (Datum.Fixnum 8)
    (eval "(let-syntax \
             ((inc (syntax-rules () ((inc x) (+ x 1)))) \
              (double (syntax-rules () ((double x) (+ x x))))) \
             (double (inc 3)))")

let test_let_syntax_hygiene () =
  (* Test that let-syntax introduces a scope boundary *)
  check_datum "let-syntax in nested let" (Datum.Fixnum 10)
    (eval "(let ((x 5)) \
             (let-syntax ((double (syntax-rules () ((double e) (+ e e))))) \
               (double x)))")

(* --- quasiquote --- *)

let test_quasiquote_simple () =
  check_datum "qq simple" (Datum.Bool true)
    (eval "(equal? '(1 2 3) `(1 2 3))")

let test_quasiquote_unquote () =
  check_datum "qq unquote" (Datum.Bool true)
    (eval "(let ((x 5)) (equal? '(1 5 3) `(1 ,x 3)))")

let test_quasiquote_splicing () =
  check_datum "qq splicing" (Datum.Bool true)
    (eval "(let ((xs '(2 3))) (equal? '(1 2 3 4) `(1 ,@xs 4)))")

let test_quasiquote_nested () =
  check_datum "qq nested" (Datum.Bool true)
    (eval "(let ((x 1)) (equal? `(a ,(+ x 1) c) '(a 2 c)))")

let test_quasiquote_no_unquote () =
  check_datum "qq no unquote" (Datum.Bool true)
    (eval "(equal? '(a b c) `(a b c))")

let test_quasiquote_expr () =
  check_datum "qq expr" (Datum.Bool true)
    (eval "(equal? '(1 4 3) `(1 ,(+ 2 2) 3))")

let test_quasiquote_nested_splicing () =
  (* Nested quasiquote preserves unquote-splicing at depth > 0 *)
  check_datum "qq nested splicing" (Datum.Bool true)
    (eval "(let ((x '(1 2))) \
             (equal? `(a `(b ,@x c) d) \
                     '(a (quasiquote (b (unquote-splicing x) c)) d)))")

(* --- guard --- *)

let test_guard_basic () =
  check_datum "guard basic" (Datum.Fixnum 42)
    (eval "(guard (exn ((= exn 42) exn)) (raise 42))")

let test_guard_else () =
  check_datum "guard else" (Datum.Symbol "caught")
    (eval "(guard (exn (else 'caught)) (raise 'boom))")

let test_guard_no_match () =
  (* guard with no matching clause re-raises *)
  check_datum "guard no match reraise" (Datum.Symbol "outer")
    (eval "(call/cc (lambda (k) \
             (with-exception-handler \
               (lambda (e) (k 'outer)) \
               (lambda () \
                 (guard (exn ((equal? exn 99) 'inner)) \
                   (raise 'boom))))))")

let test_guard_error_object () =
  check_datum "guard error object" (Datum.Bool true)
    (eval "(guard (exn ((error-object? exn) \
                        (string=? \"test\" (error-object-message exn)))) \
             (error \"test\" 1 2))")

let test_guard_body () =
  check_datum "guard body" (Datum.Fixnum 10)
    (eval "(guard (exn (else 0)) (+ 3 7))")

let test_guard_multi_clause () =
  check_datum "guard multi clause" (Datum.Symbol "second")
    (eval "(guard (exn \
              ((= exn 1) 'first) \
              ((= exn 2) 'second) \
              ((= exn 3) 'third)) \
             (raise 2))")

let test_guard_test_only_clause () =
  (* (guard (exn (exn)) ...) — test-only clause returns the test value *)
  check_datum "guard test-only" (Datum.Fixnum 42)
    (eval "(guard (exn (exn)) (raise 42))")

let test_guard_arrow_clause () =
  (* => clause: test value is passed to proc.
     (and (error-object? exn) exn) returns exn when true. *)
  check_datum "guard arrow" (Datum.Str (Bytes.of_string "oops"))
    (eval "(guard (exn ((and (error-object? exn) exn) => error-object-message)) \
             (error \"oops\"))")

(* --- define-record-type --- *)

let test_record_basic () =
  check_datum "record basic" (Datum.Fixnum 1)
    (eval_seq [
      "(define-record-type <point> (make-point x y) point? (x point-x) (y point-y))";
      "(point-x (make-point 1 2))"])

let test_record_predicate () =
  check_datum "record pred true" (Datum.Bool true)
    (eval_seq [
      "(define-record-type <point> (make-point x y) point? (x point-x) (y point-y))";
      "(point? (make-point 1 2))"]);
  check_datum "record pred false" (Datum.Bool false)
    (eval_seq [
      "(define-record-type <point> (make-point x y) point? (x point-x) (y point-y))";
      "(point? 42)"])

let test_record_accessor () =
  check_datum "record accessor" (Datum.Fixnum 2)
    (eval_seq [
      "(define-record-type <point> (make-point x y) point? (x point-x) (y point-y))";
      "(point-y (make-point 1 2))"])

let test_record_mutator () =
  check_datum "record mutator" (Datum.Fixnum 99)
    (eval_seq [
      "(define-record-type <point> (make-point x y) point? \
         (x point-x set-point-x!) (y point-y))";
      "(let ((p (make-point 1 2))) \
         (set-point-x! p 99) \
         (point-x p))"])

let test_record_distinct_types () =
  check_datum "distinct types" (Datum.Bool false)
    (eval_seq [
      "(define-record-type <point> (make-point x y) point? (x point-x) (y point-y))";
      "(define-record-type <pair2> (make-pair2 a b) pair2? (a pair2-a) (b pair2-b))";
      "(point? (make-pair2 1 2))"])

let test_record_r7rs_example () =
  (* R7RS example: <pare> with kons/pare?/kar/kdr/set-kar! *)
  check_datum "R7RS pare" (Datum.Fixnum 3)
    (eval_seq [
      "(define-record-type <pare> (kons x y) pare? \
         (x kar set-kar!) (y kdr))";
      "(let ((p (kons 1 2))) \
         (set-kar! p 3) \
         (kar p))"])

let test_record_partial_ctor () =
  (* Constructor with subset of fields — uninitialized field *)
  check_datum "partial ctor" (Datum.Fixnum 1)
    (eval_seq [
      "(define-record-type <node> (make-leaf val) node? \
         (val node-val) (left node-left) (right node-right))";
      "(node-val (make-leaf 1))"])

let test_record_type_name () =
  (* R7RS §5.5: <name> is bound to a representation of the record type *)
  check_datum "record type name bound" (Datum.Bool true)
    (eval_seq [
      "(define-record-type <point> (make-point x y) point? (x point-x) (y point-y))";
      "(symbol? <point>)"])

(* --- syntax-error --- *)

let test_syntax_error () =
  Alcotest.check_raises "syntax-error"
    (Compiler.Compile_error (Loc.none, ""))
    (fun () ->
      try ignore (eval "(syntax-error \"bad form\")")
      with Compiler.Compile_error (_, _) ->
        raise (Compiler.Compile_error (Loc.none, "")))

let test_syntax_error_in_template () =
  Alcotest.check_raises "syntax-error in template"
    (Compiler.Compile_error (Loc.none, ""))
    (fun () ->
      try ignore (eval_seq [
        "(define-syntax must-be-pair (syntax-rules () \
           ((must-be-pair (a b)) (list a b)) \
           ((must-be-pair x) (syntax-error \"expected pair\"))))";
        "(must-be-pair 42)"])
      with Compiler.Compile_error (_, _) ->
        raise (Compiler.Compile_error (Loc.none, "")))

(* --- internal define-syntax --- *)

let test_internal_define_syntax () =
  check_datum "internal define-syntax" (Datum.Fixnum 10)
    (eval "(let ((x 5)) \
             (define-syntax double (syntax-rules () ((double e) (+ e e)))) \
             (double x))")

(* --- Bug fix: dotted-pair patterns --- *)

let test_macro_dotted_pair () =
  check_datum "dotted pair pattern" (Datum.Fixnum 1)
    (eval_seq [
      "(define-syntax dot-test \
         (syntax-rules () \
           ((_ a . b) a)))";
      "(dot-test 1 2 3)"])

let test_macro_dotted_pair_rest () =
  check_datum "dotted pair rest" (Datum.Fixnum 6)
    (eval_seq [
      "(define-syntax dot-apply \
         (syntax-rules () \
           ((_ f . args) (f . args))))";
      "(dot-apply + 1 2 3)"])

(* --- Bug fix: nested ellipsis depth --- *)

let test_macro_nested_ellipsis () =
  check_datum "nested ellipsis" (Datum.Fixnum 10)
    (eval_seq [
      "(define-syntax my-append \
         (syntax-rules () \
           ((_ (a ...) ...) \
            (append (list a ...) ...))))";
      "(let ((result (my-append (1 2) (3 4)))) \
         (apply + result))"])

(* --- Bug fix: internal define-syntax scoping --- *)

let test_internal_define_syntax_scope () =
  check_datum "internal define-syntax does not leak"
    (Datum.Fixnum 20)
    (eval_seq [
      "(define (local-mac x) (* x 2))";
      "(let () \
         (define-syntax local-mac \
           (syntax-rules () ((_ e) (+ e 1)))) \
         (local-mac 5))";
      "(local-mac 10)"])

(* --- Coverage: ellipsis with pre/post elements --- *)

let test_ellipsis_pre_post () =
  check_datum "ellipsis pre/post" (Datum.Fixnum 15)
    (eval_seq [
      "(define-syntax mid \
         (syntax-rules () \
           ((_ a x ... b) (+ a b x ...))))";
      "(mid 1 2 3 4 5)"])

let test_ellipsis_pre_post_zero () =
  check_datum "ellipsis pre/post zero reps"
    (Datum.Fixnum 30)
    (eval_seq [
      "(define-syntax wrap \
         (syntax-rules () \
           ((_ a x ... b) (+ a b))))";
      "(wrap 10 20)"])

(* --- Coverage: vector pre/post ellipsis --- *)

let test_vector_pre_post_ellipsis () =
  check_datum "vector pre/post ellipsis"
    (Datum.Fixnum 100)
    (eval_seq [
      "(define-syntax vec-mid \
         (syntax-rules () \
           ((_ #(a x ... b)) (+ a b x ...))))";
      "(vec-mid #(10 20 30 40))"])

let test_vector_pre_post_ellipsis_zero () =
  check_datum "vector pre/post zero reps"
    (Datum.Fixnum 30)
    (eval_seq [
      "(define-syntax vec-wrap \
         (syntax-rules () \
           ((_ #(a x ... b)) (+ a b))))";
      "(vec-wrap #(10 20))"])

(* --- Coverage: quasiquote splicing edge cases --- *)

let test_quasiquote_splice_empty () =
  check_datum "quasiquote splice empty list"
    (eval "(quote (1 2))")
    (eval "(let ((xs '())) `(1 ,@xs 2))")

let test_quasiquote_splice_tail () =
  check_datum "quasiquote splice at end"
    (eval "(quote (1 2 3 4))")
    (eval "(let ((xs '(3 4))) `(1 2 ,@xs))")

(* --- Coverage: quasiquote inside syntax-rules template --- *)

let test_quasiquote_in_macro () =
  check_datum "quasiquote in macro template"
    (eval "(quote (+ 5 1))")
    (eval_seq [
      "(define-syntax make-expr \
         (syntax-rules () \
           ((_ n) `(+ ,n 1))))";
      "(make-expr 5)"])

(* --- Coverage: macro-generating macros --- *)

let test_macro_generating_macro () =
  check_datum "macro-generating macro" (Datum.Fixnum 10)
    (eval_seq [
      "(define-syntax def-const \
         (syntax-rules () \
           ((_ name val) \
            (define-syntax name \
              (syntax-rules () \
                ((_ ) val))))))";
      "(def-const ten 10)";
      "(ten)"])

(* --- Coverage: use-site shadowing hygiene --- *)

let test_hygiene_use_site_shadow () =
  check_datum "use-site shadow macro-introduced"
    (Datum.Fixnum 99)
    (eval_seq [
      "(define-syntax my-let1 \
         (syntax-rules () \
           ((_ val body) \
            (let ((x val)) body))))";
      "(let ((x 99)) (my-let1 42 x))"])

(* --- Coverage: multiple ellipsis vars (zip) --- *)

let test_ellipsis_zip () =
  check_datum "zip two ellipsis vars"
    (eval "(quote ((1 4) (2 5) (3 6)))")
    (eval_seq [
      "(define-syntax zip \
         (syntax-rules () \
           ((_ (a ...) (b ...)) \
            (list (list a b) ...))))";
      "(zip (1 2 3) (4 5 6))"])

(* --- Coverage: improper list pattern with ellipsis --- *)

let test_ellipsis_dot_pattern () =
  check_datum "ellipsis dot pattern" (Datum.Fixnum 6)
    (eval_seq [
      "(define-syntax ell-dot \
         (syntax-rules () \
           ((_ x ... . rest) (+ x ...))))";
      "(ell-dot 1 2 3)"])

(* --- Coverage: multiple define-syntax in same body --- *)

let test_multi_define_syntax_body () =
  check_datum "multiple define-syntax in body" (Datum.Fixnum 10)
    (eval "(let () \
             (define-syntax inc (syntax-rules () ((inc x) (+ x 1)))) \
             (define-syntax dec (syntax-rules () ((dec x) (- x 1)))) \
             (+ (inc 5) (dec 5)))")

(* --- Library imports --- *)

let test_import_scheme_base () =
  check_datum "import scheme base" Datum.Void
    (eval "(import (scheme base))")

let test_import_only () =
  check_datum "import only +" (Datum.Fixnum 3)
    (eval_seq ["(import (only (scheme base) +))"; "(+ 1 2)"])

let test_import_except () =
  (* After importing except car, + still works *)
  check_datum "import except car" (Datum.Fixnum 5)
    (eval_seq ["(import (except (scheme base) car))"; "(+ 2 3)"])

let test_import_prefix () =
  check_datum "import prefix s:" (Datum.Fixnum 7)
    (eval_seq ["(import (prefix (scheme base) s:))"; "(s:+ 3 4)"])

let test_import_rename () =
  check_datum "import rename car->first" (Datum.Fixnum 1)
    (eval_seq [
      "(import (rename (scheme base) (car first)))";
      "(first '(1 2 3))"])

let test_import_multiple () =
  check_datum "import multiple" (Datum.Fixnum 3)
    (eval_seq [
      "(import (scheme base) (scheme char))";
      "(+ 1 2)"])

let test_import_nested_modifiers () =
  check_datum "nested only+prefix" (Datum.Fixnum 3)
    (eval_seq [
      "(import (only (prefix (scheme base) s:) s:+ s:-))";
      "(s:+ 1 2)"])

let test_import_syntax () =
  check_datum "import let syntax" (Datum.Fixnum 3)
    (eval_seq [
      "(import (only (scheme base) let +))";
      "(let ((x 1) (y 2)) (+ x y))"])

let test_import_then_macro () =
  check_datum "import then define-syntax" (Datum.Fixnum 6)
    (eval_seq [
      "(import (scheme base))";
      "(define-syntax double (syntax-rules () ((double x) (+ x x))))";
      "(double 3)"])

(* --- cond-expand --- *)

let test_cond_expand_feature () =
  check_datum "cond-expand r7rs" (Datum.Fixnum 1)
    (eval "(cond-expand (r7rs 1))")

let test_cond_expand_else () =
  check_datum "cond-expand else" (Datum.Fixnum 2)
    (eval "(cond-expand (foo 1) (else 2))")

let test_cond_expand_and () =
  check_datum "cond-expand and" (Datum.Fixnum 1)
    (eval "(cond-expand ((and r7rs wile) 1) (else 0))")

let test_cond_expand_or () =
  check_datum "cond-expand or" (Datum.Fixnum 1)
    (eval "(cond-expand ((or foo r7rs) 1) (else 0))")

let test_cond_expand_not () =
  check_datum "cond-expand not" (Datum.Fixnum 1)
    (eval "(cond-expand ((not foo) 1) (else 0))")

let test_cond_expand_library () =
  check_datum "cond-expand library" (Datum.Fixnum 1)
    (eval "(cond-expand ((library (scheme base)) 1) (else 0))")

let test_cond_expand_no_match () =
  let inst = Instance.create () in
  Alcotest.check_raises "no match"
    (Compiler.Compile_error (Loc.make "<string>" 1 1, "cond-expand: no matching clause"))
    (fun () -> ignore (Instance.eval_string inst "(cond-expand (foo 1))"))

let test_cond_expand_nested () =
  check_datum "cond-expand nested" (Datum.Fixnum 3)
    (eval "(cond-expand ((and r7rs (not foo)) (+ 1 2)))")

(* --- include / include-ci --- *)

let test_include_basic () =
  let tmp = Filename.temp_file "wile_inc" ".scm" in
  let oc = open_out tmp in
  output_string oc "(define x 42)";
  close_out oc;
  let inst = Instance.create () in
  ignore (Instance.eval_string inst (Printf.sprintf "(include \"%s\")" tmp));
  let result = Instance.eval_string inst "x" in
  check_datum "include basic" (Datum.Fixnum 42) result;
  Sys.remove tmp

let test_include_multiple () =
  let tmp1 = Filename.temp_file "wile_inc" ".scm" in
  let tmp2 = Filename.temp_file "wile_inc" ".scm" in
  let oc1 = open_out tmp1 in output_string oc1 "(define a 1)"; close_out oc1;
  let oc2 = open_out tmp2 in output_string oc2 "(define b 2)"; close_out oc2;
  let inst = Instance.create () in
  ignore (Instance.eval_string inst
    (Printf.sprintf "(include \"%s\" \"%s\")" tmp1 tmp2));
  let result = Instance.eval_string inst "(+ a b)" in
  check_datum "include multiple" (Datum.Fixnum 3) result;
  Sys.remove tmp1; Sys.remove tmp2

let test_include_ci () =
  let tmp = Filename.temp_file "wile_inc" ".scm" in
  let oc = open_out tmp in
  output_string oc "(define ABC 99)";
  close_out oc;
  let inst = Instance.create () in
  ignore (Instance.eval_string inst (Printf.sprintf "(include-ci \"%s\")" tmp));
  (* Case folding means ABC becomes abc *)
  let result = Instance.eval_string inst "abc" in
  check_datum "include-ci" (Datum.Fixnum 99) result;
  Sys.remove tmp

let test_include_not_found () =
  let inst = Instance.create () in
  Alcotest.check_raises "include not found"
    (Sys_error "/tmp/nonexistent_wile_inc.scm: No such file or directory")
    (fun () -> ignore (Instance.eval_string inst
      "(include \"/tmp/nonexistent_wile_inc.scm\")"))

(* --- define-library --- *)

let test_deflib_basic () =
  check_datum "basic define-library" (Datum.Fixnum 42)
    (eval_seq [
      "(define-library (mylib) \
         (export x) \
         (import (scheme base)) \
         (begin (define x 42)))";
      "(import (mylib))";
      "x"])

let test_deflib_multiple_exports () =
  check_datum "multiple exports" (Datum.Fixnum 3)
    (eval_seq [
      "(define-library (mylib2) \
         (export x y) \
         (import (scheme base)) \
         (begin (define x 1) (define y 2)))";
      "(import (mylib2))";
      "(+ x y)"])

let test_deflib_rename_export () =
  check_datum "rename export" (Datum.Fixnum 42)
    (eval_seq [
      "(define-library (mylib3) \
         (export (rename internal external)) \
         (import (scheme base)) \
         (begin (define internal 42)))";
      "(import (mylib3))";
      "external"])

let test_deflib_isolation () =
  let inst = Instance.create () in
  ignore (Instance.eval_string inst
    "(define-library (mylib4) \
       (export pub) \
       (import (scheme base)) \
       (begin (define priv 99) (define pub (+ priv 1))))");
  ignore (Instance.eval_string inst "(import (mylib4))");
  check_datum "pub visible" (Datum.Fixnum 100)
    (Instance.eval_string inst "pub")

let test_deflib_import_internal () =
  check_datum "library imports scheme base" (Datum.Fixnum 6)
    (eval_seq [
      "(define-library (math) \
         (export double) \
         (import (scheme base)) \
         (begin (define (double x) (+ x x))))";
      "(import (math))";
      "(double 3)"])

let test_deflib_with_syntax () =
  check_datum "library exports macro" (Datum.Fixnum 6)
    (eval_seq [
      "(define-library (mac) \
         (export double) \
         (import (scheme base)) \
         (begin (define-syntax double \
           (syntax-rules () ((double x) (+ x x))))))";
      "(import (mac))";
      "(double 3)"])

let test_deflib_with_include () =
  let tmp = Filename.temp_file "wile_lib" ".scm" in
  let oc = open_out tmp in
  output_string oc "(define x 77)";
  close_out oc;
  check_datum "library with include" (Datum.Fixnum 77)
    (eval_seq [
      Printf.sprintf
        "(define-library (inclib) \
           (export x) \
           (import (scheme base)) \
           (include \"%s\"))" tmp;
      "(import (inclib))";
      "x"]);
  Sys.remove tmp

let test_deflib_cond_expand () =
  check_datum "library with cond-expand" (Datum.Fixnum 1)
    (eval_seq [
      "(define-library (condlib) \
         (export val) \
         (import (scheme base)) \
         (cond-expand \
           (r7rs (begin (define val 1))) \
           (else (begin (define val 0)))))";
      "(import (condlib))";
      "val"])

let test_deflib_two_libs () =
  check_datum "two libraries" (Datum.Fixnum 30)
    (eval_seq [
      "(define-library (lib-a) \
         (export a) \
         (import (scheme base)) \
         (begin (define a 10)))";
      "(define-library (lib-b) \
         (export b) \
         (import (scheme base) (lib-a)) \
         (begin (define b (* a 3))))";
      "(import (lib-b))";
      "b"])

let test_deflib_slot_sharing () =
  check_datum "slot sharing across imports" (Datum.Fixnum 42)
    (eval_seq [
      "(define-library (shared) \
         (export get-x set-x!) \
         (import (scheme base)) \
         (begin \
           (define x 0) \
           (define (get-x) x) \
           (define (set-x! v) (set! x v))))";
      "(import (shared))";
      "(set-x! 42)";
      "(get-x)"])

(* --- Library file loading --- *)

let test_load_from_sld () =
  let dir = Filename.temp_dir "wile_lib" "" in
  let sub = Filename.concat dir "myfilelib" in
  Sys.mkdir sub 0o755;
  let sld = Filename.concat sub "stuff.sld" in
  let oc = open_out sld in
  output_string oc "(define-library (myfilelib stuff) \
    (export val) \
    (import (scheme base)) \
    (begin (define val 99)))";
  close_out oc;
  let inst = Instance.create () in
  inst.search_paths := [dir];
  ignore (Instance.eval_string inst "(import (myfilelib stuff))");
  let result = Instance.eval_string inst "val" in
  check_datum "load from sld" (Datum.Fixnum 99) result;
  Sys.remove sld; Sys.rmdir sub; Sys.rmdir dir

let test_load_search_path_order () =
  let dir1 = Filename.temp_dir "wile_lib1" "" in
  let dir2 = Filename.temp_dir "wile_lib2" "" in
  let sub1 = Filename.concat dir1 "order" in
  let sub2 = Filename.concat dir2 "order" in
  Sys.mkdir sub1 0o755; Sys.mkdir sub2 0o755;
  let sld1 = Filename.concat sub1 "test.sld" in
  let sld2 = Filename.concat sub2 "test.sld" in
  let oc1 = open_out sld1 in
  output_string oc1 "(define-library (order test) \
    (export val) \
    (import (scheme base)) \
    (begin (define val 1)))";
  close_out oc1;
  let oc2 = open_out sld2 in
  output_string oc2 "(define-library (order test) \
    (export val) \
    (import (scheme base)) \
    (begin (define val 2)))";
  close_out oc2;
  let inst = Instance.create () in
  inst.search_paths := [dir1; dir2];
  ignore (Instance.eval_string inst "(import (order test))");
  let result = Instance.eval_string inst "val" in
  check_datum "search path order" (Datum.Fixnum 1) result;
  Sys.remove sld1; Sys.remove sld2;
  Sys.rmdir sub1; Sys.rmdir sub2;
  Sys.rmdir dir1; Sys.rmdir dir2

let test_load_not_found () =
  let inst = Instance.create () in
  inst.search_paths := ["/tmp"];
  Alcotest.check_raises "unknown lib"
    (Failure "unknown library: (no such lib)")
    (fun () -> ignore (Instance.eval_string inst "(import (no such lib))"))

let test_load_transitive () =
  let dir = Filename.temp_dir "wile_lib" "" in
  let sub_a = Filename.concat dir "trans" in
  Sys.mkdir sub_a 0o755;
  let sld_a = Filename.concat sub_a "a.sld" in
  let oc_a = open_out sld_a in
  output_string oc_a "(define-library (trans a) \
    (export a-val) \
    (import (scheme base)) \
    (begin (define a-val 10)))";
  close_out oc_a;
  let sld_b = Filename.concat sub_a "b.sld" in
  let oc_b = open_out sld_b in
  output_string oc_b "(define-library (trans b) \
    (export b-val) \
    (import (scheme base) (trans a)) \
    (begin (define b-val (* a-val 5))))";
  close_out oc_b;
  let inst = Instance.create () in
  inst.search_paths := [dir];
  ignore (Instance.eval_string inst "(import (trans b))");
  let result = Instance.eval_string inst "b-val" in
  check_datum "transitive load" (Datum.Fixnum 50) result;
  Sys.remove sld_a; Sys.remove sld_b;
  Sys.rmdir sub_a; Sys.rmdir dir

(* --- Library bugfix regression --- *)

let test_loading_libs_per_instance () =
  (* Two instances loading from the same dir should not interfere *)
  let dir = Filename.temp_dir "wile_lib" "" in
  let sub = Filename.concat dir "iso" in
  Sys.mkdir sub 0o755;
  let sld = Filename.concat sub "lib.sld" in
  let oc = open_out sld in
  output_string oc "(define-library (iso lib) \
    (export v) \
    (import (scheme base)) \
    (begin (define v 42)))";
  close_out oc;
  let inst1 = Instance.create () in
  let inst2 = Instance.create () in
  inst1.search_paths := [dir];
  inst2.search_paths := [dir];
  ignore (Instance.eval_string inst1 "(import (iso lib))");
  (* inst2 should independently load the same library *)
  ignore (Instance.eval_string inst2 "(import (iso lib))");
  check_datum "inst1" (Datum.Fixnum 42) (Instance.eval_string inst1 "v");
  check_datum "inst2" (Datum.Fixnum 42) (Instance.eval_string inst2 "v");
  Sys.remove sld; Sys.rmdir sub; Sys.rmdir dir

let test_loading_libs_cleanup_on_error () =
  (* If loading a library fails, loading_libs must be cleaned up so
     a corrected version can be loaded later *)
  let dir = Filename.temp_dir "wile_lib" "" in
  let sub = Filename.concat dir "retry" in
  Sys.mkdir sub 0o755;
  let sld = Filename.concat sub "lib.sld" in
  (* Write a broken .sld file *)
  let oc = open_out sld in
  output_string oc "(define-library (retry lib) \
    (export v) \
    (import (scheme base)) \
    (begin (define v (/ 1 0))))";
  close_out oc;
  let inst = Instance.create () in
  inst.search_paths := [dir];
  (* First attempt should fail *)
  (try ignore (Instance.eval_string inst "(import (retry lib))"); ()
   with _ -> ());
  (* Write a correct .sld file *)
  let oc2 = open_out sld in
  output_string oc2 "(define-library (retry lib) \
    (export v) \
    (import (scheme base)) \
    (begin (define v 77)))";
  close_out oc2;
  (* Second attempt should succeed — loading_libs was cleaned up *)
  ignore (Instance.eval_string inst "(import (retry lib))");
  check_datum "retry" (Datum.Fixnum 77) (Instance.eval_string inst "v");
  Sys.remove sld; Sys.rmdir sub; Sys.rmdir dir

let test_cond_expand_library_autoload () =
  (* cond-expand (library ...) should auto-load from .sld files *)
  let dir = Filename.temp_dir "wile_lib" "" in
  let sub = Filename.concat dir "probe" in
  Sys.mkdir sub 0o755;
  let sld = Filename.concat sub "lib.sld" in
  let oc = open_out sld in
  output_string oc "(define-library (probe lib) \
    (export val) \
    (import (scheme base)) \
    (begin (define val 55)))";
  close_out oc;
  let inst = Instance.create () in
  inst.search_paths := [dir];
  let result = Instance.eval_string inst
    "(cond-expand ((library (probe lib)) 1) (else 0))" in
  check_datum "autoload library check" (Datum.Fixnum 1) result;
  Sys.remove sld; Sys.rmdir sub; Sys.rmdir dir

let test_import_rename_bad_source () =
  let inst = Instance.create () in
  Alcotest.check_raises "rename bad source"
    (Failure "rename: name not in export set: no-such-name")
    (fun () -> ignore (Instance.eval_string inst
       "(import (rename (scheme base) (no-such-name x)))"))

let test_import_only_empty () =
  let inst = Instance.create () in
  (try ignore (Instance.eval_string inst "(import (only (scheme base)))");
       Alcotest.fail "expected error"
   with Compiler.Compile_error (_, msg) ->
     Alcotest.(check string) "msg" "only: expected import set and identifiers" msg)

let test_import_rename_empty () =
  let inst = Instance.create () in
  (try ignore (Instance.eval_string inst "(import (rename (scheme base)))");
       Alcotest.fail "expected error"
   with Compiler.Compile_error (_, msg) ->
     Alcotest.(check string) "msg" "rename: expected import set and pairs" msg)

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
    ; ("apply",
       [ Alcotest.test_case "apply basic" `Quick test_apply_basic
       ; Alcotest.test_case "apply spread" `Quick test_apply_spread
       ; Alcotest.test_case "apply cons" `Quick test_apply_cons
       ; Alcotest.test_case "apply empty" `Quick test_apply_empty
       ; Alcotest.test_case "apply multi spread" `Quick test_apply_multi_spread
       ; Alcotest.test_case "apply lambda" `Quick test_apply_lambda
       ; Alcotest.test_case "apply first class" `Quick test_apply_first_class
       ; Alcotest.test_case "apply error" `Quick test_apply_error
       ])
    ; ("call/cc",
       [ Alcotest.test_case "unused k" `Quick test_callcc_unused
       ; Alcotest.test_case "invoke k" `Quick test_callcc_invoke
       ; Alcotest.test_case "k aborts" `Quick test_callcc_abort
       ; Alcotest.test_case "expr context" `Quick test_callcc_expr_context
       ; Alcotest.test_case "saved cont" `Quick test_callcc_saved
       ; Alcotest.test_case "long name" `Quick test_callcc_long_name
       ; Alcotest.test_case "first class" `Quick test_callcc_first_class
       ; Alcotest.test_case "multi shot" `Quick test_callcc_multi_shot
       ; Alcotest.test_case "nested call/cc" `Quick test_callcc_nested
       ; Alcotest.test_case "tail call/cc" `Quick test_callcc_tail
       ; Alcotest.test_case "closure captures k" `Quick test_callcc_closure
       ])
    ; ("values/call-with-values",
       [ Alcotest.test_case "cwv basic" `Quick test_cwv_basic
       ; Alcotest.test_case "cwv single" `Quick test_cwv_single
       ; Alcotest.test_case "cwv triple" `Quick test_cwv_triple
       ; Alcotest.test_case "values single" `Quick test_values_single
       ; Alcotest.test_case "cwv no values" `Quick test_cwv_no_values
       ; Alcotest.test_case "cwv nested" `Quick test_cwv_nested
       ])
    ; ("dynamic-wind",
       [ Alcotest.test_case "basic" `Quick test_dw_basic
       ; Alcotest.test_case "order" `Quick test_dw_order
       ; Alcotest.test_case "escape" `Quick test_dw_escape
       ; Alcotest.test_case "reenter" `Quick test_dw_reenter
       ; Alcotest.test_case "nested" `Quick test_dw_nested
       ; Alcotest.test_case "escape nested" `Quick test_dw_escape_nested
       ; Alcotest.test_case "first class" `Quick test_dw_first_class
       ; Alcotest.test_case "thunk result" `Quick test_dw_thunk_result
       ])
    ; ("numeric",
       [ Alcotest.test_case "division" `Quick test_division
       ; Alcotest.test_case "abs" `Quick test_abs
       ; Alcotest.test_case "min/max" `Quick test_min_max
       ; Alcotest.test_case "quotient/remainder/modulo" `Quick test_quotient_remainder_modulo
       ; Alcotest.test_case "floor/ceiling/truncate/round" `Quick test_floor_ceil_trunc_round
       ; Alcotest.test_case "gcd/lcm" `Quick test_gcd_lcm
       ; Alcotest.test_case "exact/inexact" `Quick test_exact_inexact
       ; Alcotest.test_case "expt/sqrt" `Quick test_expt_sqrt
       ; Alcotest.test_case "chain comparisons" `Quick test_chain_compare
       ; Alcotest.test_case "number->string/string->number" `Quick test_number_string
       ])
    ; ("pair & list",
       [ Alcotest.test_case "set-car!/set-cdr!" `Quick test_set_car_cdr
       ; Alcotest.test_case "caar/cadr/cdar/cddr" `Quick test_cxxr
       ; Alcotest.test_case "make-list" `Quick test_make_list
       ; Alcotest.test_case "length" `Quick test_length
       ; Alcotest.test_case "append" `Quick test_append
       ; Alcotest.test_case "reverse" `Quick test_reverse
       ; Alcotest.test_case "list-tail/list-ref/list-set!" `Quick test_list_tail_ref_set
       ; Alcotest.test_case "list-copy" `Quick test_list_copy
       ; Alcotest.test_case "memq/memv/member" `Quick test_memq_member
       ; Alcotest.test_case "assq/assv/assoc" `Quick test_assq_assoc
       ])
    ; ("character",
       [ Alcotest.test_case "char comparisons" `Quick test_char_compare
       ; Alcotest.test_case "char-ci comparisons" `Quick test_char_ci_compare
       ; Alcotest.test_case "char->integer/integer->char" `Quick test_char_integer
       ; Alcotest.test_case "char case conversion" `Quick test_char_case
       ; Alcotest.test_case "char classification" `Quick test_char_classification
       ; Alcotest.test_case "digit-value" `Quick test_digit_value
       ])
    ; ("string",
       [ Alcotest.test_case "construction" `Quick test_string_construct
       ; Alcotest.test_case "ref/set!" `Quick test_string_ref_set
       ; Alcotest.test_case "comparisons" `Quick test_string_compare
       ; Alcotest.test_case "ci comparisons" `Quick test_string_ci_compare
       ; Alcotest.test_case "substring" `Quick test_substring
       ; Alcotest.test_case "string-append" `Quick test_string_append
       ; Alcotest.test_case "string<->list" `Quick test_string_list_conv
       ; Alcotest.test_case "copy/copy!/fill!" `Quick test_string_copy_fill
       ; Alcotest.test_case "case conversion" `Quick test_string_case
       ])
    ; ("vector",
       [ Alcotest.test_case "construction" `Quick test_vector_construct
       ; Alcotest.test_case "ref/set!" `Quick test_vector_ref_set
       ; Alcotest.test_case "conversions" `Quick test_vector_conversions
       ; Alcotest.test_case "copy/copy!/append/fill!" `Quick test_vector_copy_append
       ])
    ; ("bytevector",
       [ Alcotest.test_case "construction" `Quick test_bytevector_construct
       ; Alcotest.test_case "ref/set!" `Quick test_bytevector_ref_set
       ; Alcotest.test_case "copy/append" `Quick test_bytevector_copy_append
       ; Alcotest.test_case "utf8<->string" `Quick test_utf8_string
       ])
    ; ("exceptions",
       [ Alcotest.test_case "error objects" `Quick test_error_objects
       ; Alcotest.test_case "raise/handle" `Quick test_raise_handle
       ; Alcotest.test_case "nested handlers" `Quick test_nested_handlers
       ; Alcotest.test_case "raise-continuable" `Quick test_raise_continuable
       ; Alcotest.test_case "error raises" `Quick test_error_raise
       ; Alcotest.test_case "exception + dw" `Quick test_exception_dw_interaction
       ; Alcotest.test_case "unhandled error" `Quick test_unhandled_error
       ])
    ; ("higher-order",
       [ Alcotest.test_case "map" `Quick test_map
       ; Alcotest.test_case "for-each" `Quick test_for_each
       ; Alcotest.test_case "string-map" `Quick test_string_map
       ; Alcotest.test_case "string-for-each" `Quick test_string_for_each
       ; Alcotest.test_case "vector-map" `Quick test_vector_map
       ; Alcotest.test_case "vector-for-each" `Quick test_vector_for_each
       ; Alcotest.test_case "write" `Quick test_write
       ])
    ; ("equal?",
       [ Alcotest.test_case "equal? basic" `Quick test_equal_basic
       ])
    ; ("type predicates",
       [ Alcotest.test_case "boolean?" `Quick test_type_boolean
       ; Alcotest.test_case "boolean=?" `Quick test_type_boolean_eq
       ; Alcotest.test_case "number?/complex?/real?/rational?" `Quick test_type_number
       ; Alcotest.test_case "integer?" `Quick test_type_integer
       ; Alcotest.test_case "exact?/inexact?/exact-integer?" `Quick test_type_exact
       ; Alcotest.test_case "zero?/positive?/negative?/odd?/even?" `Quick test_type_numeric_preds
       ; Alcotest.test_case "symbol?/symbol=?" `Quick test_type_symbol
       ; Alcotest.test_case "symbol->string/string->symbol" `Quick test_symbol_string
       ; Alcotest.test_case "char?/string?/vector?/bytevector?/procedure?/list?/eof" `Quick test_type_predicates
       ])
    ; ("define-syntax",
       [ Alcotest.test_case "constant macro" `Quick test_macro_constant
       ; Alcotest.test_case "pattern variable" `Quick test_macro_pattern_var
       ; Alcotest.test_case "multi rule" `Quick test_macro_multi_rule
       ; Alcotest.test_case "ellipsis" `Quick test_macro_ellipsis
       ; Alcotest.test_case "ellipsis zero" `Quick test_macro_ellipsis_zero
       ; Alcotest.test_case "recursive" `Quick test_macro_recursive
       ; Alcotest.test_case "hygiene" `Quick test_macro_hygiene
       ; Alcotest.test_case "persistent" `Quick test_macro_persistent
       ; Alcotest.test_case "nested" `Quick test_macro_nested
       ; Alcotest.test_case "no match" `Quick test_macro_no_match
       ; Alcotest.test_case "literal" `Quick test_macro_literal
       ; Alcotest.test_case "underscore" `Quick test_macro_underscore
       ; Alcotest.test_case "vector ellipsis" `Quick test_macro_vector_ellipsis
       ; Alcotest.test_case "underscore literal" `Quick test_macro_underscore_literal
       ; Alcotest.test_case "vector tmpl ellipsis" `Quick test_macro_vector_tmpl_ellipsis
       ])
    ; ("let-syntax",
       [ Alcotest.test_case "basic" `Quick test_let_syntax_basic
       ; Alcotest.test_case "scope" `Quick test_let_syntax_scope
       ; Alcotest.test_case "shadow" `Quick test_let_syntax_shadow
       ; Alcotest.test_case "letrec-syntax" `Quick test_letrec_syntax_self_ref
       ; Alcotest.test_case "multi" `Quick test_let_syntax_multi
       ; Alcotest.test_case "hygiene" `Quick test_let_syntax_hygiene
       ])
    ; ("quasiquote",
       [ Alcotest.test_case "simple" `Quick test_quasiquote_simple
       ; Alcotest.test_case "unquote" `Quick test_quasiquote_unquote
       ; Alcotest.test_case "splicing" `Quick test_quasiquote_splicing
       ; Alcotest.test_case "nested" `Quick test_quasiquote_nested
       ; Alcotest.test_case "no unquote" `Quick test_quasiquote_no_unquote
       ; Alcotest.test_case "expr" `Quick test_quasiquote_expr
       ; Alcotest.test_case "nested splicing" `Quick test_quasiquote_nested_splicing
       ])
    ; ("guard",
       [ Alcotest.test_case "basic" `Quick test_guard_basic
       ; Alcotest.test_case "else" `Quick test_guard_else
       ; Alcotest.test_case "no match" `Quick test_guard_no_match
       ; Alcotest.test_case "error object" `Quick test_guard_error_object
       ; Alcotest.test_case "body value" `Quick test_guard_body
       ; Alcotest.test_case "multi clause" `Quick test_guard_multi_clause
       ; Alcotest.test_case "test-only clause" `Quick test_guard_test_only_clause
       ; Alcotest.test_case "arrow clause" `Quick test_guard_arrow_clause
       ; Alcotest.test_case "single eval" `Quick test_guard_single_eval
       ; Alcotest.test_case "multi values" `Quick test_guard_multi_values
       ; Alcotest.test_case "reraise dyn env" `Quick test_guard_reraise_dyn_env
       ])
    ; ("define-record-type",
       [ Alcotest.test_case "basic" `Quick test_record_basic
       ; Alcotest.test_case "predicate" `Quick test_record_predicate
       ; Alcotest.test_case "accessor" `Quick test_record_accessor
       ; Alcotest.test_case "mutator" `Quick test_record_mutator
       ; Alcotest.test_case "distinct types" `Quick test_record_distinct_types
       ; Alcotest.test_case "R7RS pare" `Quick test_record_r7rs_example
       ; Alcotest.test_case "partial ctor" `Quick test_record_partial_ctor
       ; Alcotest.test_case "type name" `Quick test_record_type_name
       ])
    ; ("syntax-error",
       [ Alcotest.test_case "syntax-error" `Quick test_syntax_error
       ; Alcotest.test_case "in template" `Quick test_syntax_error_in_template
       ; Alcotest.test_case "internal define-syntax" `Quick test_internal_define_syntax
       ])
    ; ("macro bugfixes",
       [ Alcotest.test_case "dotted pair pattern" `Quick test_macro_dotted_pair
       ; Alcotest.test_case "dotted pair rest" `Quick test_macro_dotted_pair_rest
       ; Alcotest.test_case "nested ellipsis" `Quick test_macro_nested_ellipsis
       ; Alcotest.test_case "internal define-syntax scope" `Quick test_internal_define_syntax_scope
       ; Alcotest.test_case "ellipsis pre/post" `Quick test_ellipsis_pre_post
       ; Alcotest.test_case "ellipsis pre/post zero" `Quick test_ellipsis_pre_post_zero
       ; Alcotest.test_case "vector pre/post ellipsis" `Quick test_vector_pre_post_ellipsis
       ; Alcotest.test_case "vector pre/post zero" `Quick test_vector_pre_post_ellipsis_zero
       ; Alcotest.test_case "splice empty list" `Quick test_quasiquote_splice_empty
       ; Alcotest.test_case "splice at tail" `Quick test_quasiquote_splice_tail
       ; Alcotest.test_case "quasiquote in macro" `Quick test_quasiquote_in_macro
       ; Alcotest.test_case "macro-generating macro" `Quick test_macro_generating_macro
       ; Alcotest.test_case "use-site shadow hygiene" `Quick test_hygiene_use_site_shadow
       ; Alcotest.test_case "ellipsis zip" `Quick test_ellipsis_zip
       ; Alcotest.test_case "ellipsis dot pattern" `Quick test_ellipsis_dot_pattern
       ; Alcotest.test_case "multi define-syntax body" `Quick test_multi_define_syntax_body
       ])
    ; ("imports",
       [ Alcotest.test_case "import (scheme base)" `Quick test_import_scheme_base
       ; Alcotest.test_case "import only" `Quick test_import_only
       ; Alcotest.test_case "import except" `Quick test_import_except
       ; Alcotest.test_case "import prefix" `Quick test_import_prefix
       ; Alcotest.test_case "import rename" `Quick test_import_rename
       ; Alcotest.test_case "import multiple" `Quick test_import_multiple
       ; Alcotest.test_case "import nested modifiers" `Quick test_import_nested_modifiers
       ; Alcotest.test_case "import syntax" `Quick test_import_syntax
       ; Alcotest.test_case "import then macro" `Quick test_import_then_macro
       ])
    ; ("cond-expand",
       [ Alcotest.test_case "feature match" `Quick test_cond_expand_feature
       ; Alcotest.test_case "else clause" `Quick test_cond_expand_else
       ; Alcotest.test_case "and" `Quick test_cond_expand_and
       ; Alcotest.test_case "or" `Quick test_cond_expand_or
       ; Alcotest.test_case "not" `Quick test_cond_expand_not
       ; Alcotest.test_case "library" `Quick test_cond_expand_library
       ; Alcotest.test_case "no match error" `Quick test_cond_expand_no_match
       ; Alcotest.test_case "nested" `Quick test_cond_expand_nested
       ])
    ; ("include",
       [ Alcotest.test_case "include basic" `Quick test_include_basic
       ; Alcotest.test_case "include multiple" `Quick test_include_multiple
       ; Alcotest.test_case "include-ci" `Quick test_include_ci
       ; Alcotest.test_case "include not found" `Quick test_include_not_found
       ])
    ; ("define-library",
       [ Alcotest.test_case "basic" `Quick test_deflib_basic
       ; Alcotest.test_case "multiple exports" `Quick test_deflib_multiple_exports
       ; Alcotest.test_case "rename export" `Quick test_deflib_rename_export
       ; Alcotest.test_case "isolation" `Quick test_deflib_isolation
       ; Alcotest.test_case "import internal" `Quick test_deflib_import_internal
       ; Alcotest.test_case "with syntax" `Quick test_deflib_with_syntax
       ; Alcotest.test_case "with include" `Quick test_deflib_with_include
       ; Alcotest.test_case "cond-expand" `Quick test_deflib_cond_expand
       ; Alcotest.test_case "two libs" `Quick test_deflib_two_libs
       ; Alcotest.test_case "slot sharing" `Quick test_deflib_slot_sharing
       ])
    ; ("library-loading",
       [ Alcotest.test_case "load from .sld" `Quick test_load_from_sld
       ; Alcotest.test_case "search path order" `Quick test_load_search_path_order
       ; Alcotest.test_case "not found" `Quick test_load_not_found
       ; Alcotest.test_case "transitive" `Quick test_load_transitive
       ])
    ; ("library-bugfixes",
       [ Alcotest.test_case "per-instance loading" `Quick test_loading_libs_per_instance
       ; Alcotest.test_case "cleanup on error" `Quick test_loading_libs_cleanup_on_error
       ; Alcotest.test_case "cond-expand autoload" `Quick test_cond_expand_library_autoload
       ; Alcotest.test_case "rename bad source" `Quick test_import_rename_bad_source
       ; Alcotest.test_case "only empty" `Quick test_import_only_empty
       ; Alcotest.test_case "rename empty" `Quick test_import_rename_empty
       ])
    ]
