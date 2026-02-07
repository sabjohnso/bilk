open Wile

(* Regression tests for bugs found during M6 code review. *)

let datum_testable = Alcotest.testable Datum.pp Datum.equal

let eval s =
  let inst = Instance.create () in
  Instance.eval_string inst s

(* Bug 1: char-ci=? crashed on non-Latin-1 chars *)
let test_char_ci_non_latin1 () =
  (try
    ignore (eval "(char-ci=? #\\x0100 #\\x0101)");
    () (* no crash = pass *)
  with
  | Vm.Runtime_error _ -> () (* proper error = pass *)
  | Invalid_argument _ -> Alcotest.fail "char-ci=? crashed with Invalid_argument")

(* Bug 2: list->string crashed on non-Latin-1 chars *)
let test_list_to_string_non_latin1 () =
  (try
    ignore (eval "(list->string (list #\\x0100))");
    Alcotest.fail "should reject non-Latin1 chars"
  with
  | Vm.Runtime_error _ -> ()
  | Invalid_argument _ -> Alcotest.fail "crashed with Invalid_argument")

(* Bug 3: string-fill! crashed on non-Latin-1 chars *)
let test_string_fill_non_latin1 () =
  (try
    ignore (eval "(let ((s (make-string 3 #\\x0041))) (string-fill! s #\\x0100) s)");
    Alcotest.fail "should reject non-Latin1 chars"
  with
  | Vm.Runtime_error _ -> ()
  | Invalid_argument _ -> Alcotest.fail "crashed with Invalid_argument")

(* Bug 4: integer->char crashed on invalid values *)
let test_integer_to_char_negative () =
  (try
    ignore (eval "(integer->char -1)");
    Alcotest.fail "should reject -1"
  with
  | Vm.Runtime_error _ -> ()
  | Invalid_argument _ -> Alcotest.fail "crashed with Invalid_argument")

let test_integer_to_char_surrogate () =
  (try
    ignore (eval "(integer->char #xD800)");
    Alcotest.fail "should reject surrogate"
  with
  | Vm.Runtime_error _ -> ()
  | Invalid_argument _ -> Alcotest.fail "crashed with Invalid_argument")

(* Bug 5: raise-continuable pushed handler before calling, so handler caught own raises *)
let test_raise_continuable_handler_scope () =
  let result = eval
    "(call/cc (lambda (escape) \
       (with-exception-handler \
         (lambda (e) (escape (list 'outer e))) \
         (lambda () \
           (with-exception-handler \
             (lambda (e) (raise (list 'from-inner e))) \
             (lambda () (raise-continuable 'test)))))))" in
  let expected = eval "'(outer (from-inner test))" in
  Alcotest.check datum_testable "outer catches inner's re-raise" expected result

(* Bug 6: with-exception-handler leaked handlers on continuation escape *)
let test_handler_no_leak () =
  let inst = Instance.create () in
  (* Escape from with-exception-handler via continuation *)
  ignore (Instance.eval_string inst
    "(call/cc (lambda (k) \
       (with-exception-handler \
         (lambda (e) e) \
         (lambda () (k 42)))))");
  (* Now raise — should be unhandled (no leaked handler) *)
  (try
    ignore (Instance.eval_string inst "(raise 'test)");
    Alcotest.fail "raise should fail with no handler"
  with Vm.Runtime_error _ -> ())

let () =
  Alcotest.run "M6 bugfix" [
    "char-ci", [
      Alcotest.test_case "non-latin1 no crash" `Quick test_char_ci_non_latin1;
    ];
    "list->string", [
      Alcotest.test_case "non-latin1 error" `Quick test_list_to_string_non_latin1;
    ];
    "string-fill!", [
      Alcotest.test_case "non-latin1 error" `Quick test_string_fill_non_latin1;
    ];
    "integer->char", [
      Alcotest.test_case "negative error" `Quick test_integer_to_char_negative;
      Alcotest.test_case "surrogate error" `Quick test_integer_to_char_surrogate;
    ];
    "raise-continuable", [
      Alcotest.test_case "handler scope" `Quick test_raise_continuable_handler_scope;
    ];
    "handler leak", [
      Alcotest.test_case "no leak on escape" `Quick test_handler_no_leak;
    ];
  ]
