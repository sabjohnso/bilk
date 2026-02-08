open Wile

let rt = Readtable.default

(* Helper to check edit results *)
let check_edit msg expected_text expected_cursor (result : Paredit.edit_result) =
  Alcotest.(check string) (msg ^ " text") expected_text result.text;
  Alcotest.(check int) (msg ^ " cursor") expected_cursor result.cursor

(* === Navigation === *)

let test_matching_paren_forward () =
  let result = Paredit.find_matching_paren rt "(abc)" 0 in
  Alcotest.(check (option int)) "open→close" (Some 4) result

let test_matching_paren_backward () =
  let result = Paredit.find_matching_paren rt "(abc)" 4 in
  Alcotest.(check (option int)) "close→open" (Some 0) result

let test_matching_paren_nested () =
  let result = Paredit.find_matching_paren rt "(a (b c) d)" 0 in
  Alcotest.(check (option int)) "outer open" (Some 10) result;
  let result2 = Paredit.find_matching_paren rt "(a (b c) d)" 3 in
  Alcotest.(check (option int)) "inner open" (Some 7) result2

let test_matching_paren_no_match () =
  let result = Paredit.find_matching_paren rt "(abc" 0 in
  Alcotest.(check (option int)) "unmatched" None result

let test_matching_paren_not_paren () =
  let result = Paredit.find_matching_paren rt "abc" 1 in
  Alcotest.(check (option int)) "not a paren" None result

let test_sexp_forward_atom () =
  let result = Paredit.sexp_forward rt "hello world" 0 in
  Alcotest.(check (option int)) "atom" (Some 5) result

let test_sexp_forward_list () =
  let result = Paredit.sexp_forward rt "(a b) c" 0 in
  Alcotest.(check (option int)) "list" (Some 5) result

let test_sexp_forward_skip_ws () =
  let result = Paredit.sexp_forward rt "  hello" 0 in
  Alcotest.(check (option int)) "skip ws" (Some 7) result

let test_sexp_forward_at_close () =
  let result = Paredit.sexp_forward rt "(a b)" 4 in
  Alcotest.(check (option int)) "at close" None result

let test_sexp_forward_string () =
  let result = Paredit.sexp_forward rt "\"hello\" x" 0 in
  Alcotest.(check (option int)) "string" (Some 7) result

let test_sexp_forward_quoted () =
  let result = Paredit.sexp_forward rt "'(a b) c" 0 in
  Alcotest.(check (option int)) "quoted list" (Some 6) result

let test_sexp_backward_atom () =
  let result = Paredit.sexp_backward rt "hello world" 11 in
  Alcotest.(check (option int)) "atom" (Some 6) result

let test_sexp_backward_list () =
  let result = Paredit.sexp_backward rt "a (b c)" 7 in
  Alcotest.(check (option int)) "list" (Some 2) result

let test_sexp_backward_at_open () =
  let result = Paredit.sexp_backward rt "(a b)" 1 in
  Alcotest.(check (option int)) "at open" None result

let test_enclosing_paren () =
  let result = Paredit.enclosing_paren rt "(a b c)" 3 in
  Alcotest.(check (option (pair int int))) "enclosing" (Some (0, 6)) result

let test_enclosing_paren_nested () =
  let result = Paredit.enclosing_paren rt "(a (b c) d)" 5 in
  Alcotest.(check (option (pair int int))) "inner" (Some (3, 7)) result

let test_enclosing_paren_none () =
  let result = Paredit.enclosing_paren rt "a b c" 2 in
  Alcotest.(check (option (pair int int))) "none" None result

(* === Balanced insertion === *)

let test_insert_open_paren () =
  let result = Paredit.insert_open_paren "abc" 1 in
  check_edit "insert (" "a()bc" 2 result

let test_insert_open_paren_empty () =
  let result = Paredit.insert_open_paren "" 0 in
  check_edit "insert ( empty" "()" 1 result

let test_insert_close_paren_move_past () =
  let result = Paredit.insert_close_paren rt "(abc)" 4 in
  check_edit "move past )" "(abc)" 5 result

let test_insert_close_paren_skip_ws () =
  let result = Paredit.insert_close_paren rt "(abc  )" 4 in
  check_edit "skip ws to )" "(abc  )" 7 result

let test_insert_close_paren_insert () =
  let result = Paredit.insert_close_paren rt "abc" 2 in
  check_edit "insert )" "ab)c" 3 result

let test_insert_double_quote_pair () =
  let result = Paredit.insert_double_quote rt "abc" 1 in
  check_edit "insert pair" "a\"\"bc" 2 result

let test_insert_double_quote_move_past () =
  let result = Paredit.insert_double_quote rt "\"hello\"" 6 in
  check_edit "move past closing" "\"hello\"" 7 result

(* === Balanced deletion === *)

let test_backspace_normal () =
  let result = Paredit.backspace_paredit rt "abc" 2 in
  check_edit "normal bs" "ac" 1 result

let test_backspace_skip_open () =
  let result = Paredit.backspace_paredit rt "(abc)" 1 in
  check_edit "skip open" "(abc)" 1 result

let test_backspace_skip_close () =
  (* Backspace after close paren moves cursor inside *)
  let result = Paredit.backspace_paredit rt "(abc)" 5 in
  check_edit "move inside close" "(abc)" 4 result

let test_backspace_empty_pair () =
  let result = Paredit.backspace_paredit rt "()" 1 in
  check_edit "empty pair" "" 0 result

let test_backspace_at_start () =
  let result = Paredit.backspace_paredit rt "abc" 0 in
  check_edit "at start" "abc" 0 result

let test_delete_normal () =
  let result = Paredit.delete_paredit rt "abc" 1 in
  check_edit "normal del" "ac" 1 result

let test_delete_skip_close () =
  let result = Paredit.delete_paredit rt "(abc)" 4 in
  check_edit "skip close" "(abc)" 4 result

let test_delete_skip_open () =
  let result = Paredit.delete_paredit rt "(abc)" 0 in
  check_edit "skip open" "(abc)" 0 result

let test_delete_empty_pair () =
  let result = Paredit.delete_paredit rt "()" 0 in
  check_edit "empty pair" "" 0 result

let test_delete_at_end () =
  let result = Paredit.delete_paredit rt "abc" 3 in
  check_edit "at end" "abc" 3 result

(* String-aware backspace *)
let test_backspace_skip_open_quote () =
  (* Cursor after opening quote of non-empty string — skip *)
  let result = Paredit.backspace_paredit rt "\"abc\"" 1 in
  check_edit "skip open quote" "\"abc\"" 1 result

let test_backspace_skip_close_quote () =
  (* Cursor after closing quote — move inside *)
  let result = Paredit.backspace_paredit rt "\"abc\"" 5 in
  check_edit "move inside close quote" "\"abc\"" 4 result

let test_backspace_empty_string () =
  (* Empty string "" — delete both quotes *)
  let result = Paredit.backspace_paredit rt "\"\"" 1 in
  check_edit "empty string" "" 0 result

let test_backspace_inside_string () =
  (* Inside string body — normal delete *)
  let result = Paredit.backspace_paredit rt "\"abc\"" 3 in
  check_edit "inside string" "\"ac\"" 2 result

let test_backspace_string_after_text () =
  (* x"abc" with cursor at pos 2 (after opening quote) — skip *)
  let result = Paredit.backspace_paredit rt "x\"abc\"" 2 in
  check_edit "skip open quote after text" "x\"abc\"" 2 result

(* String-aware delete *)
let test_delete_skip_open_quote () =
  (* Cursor on opening quote of non-empty string — skip *)
  let result = Paredit.delete_paredit rt "\"abc\"" 0 in
  check_edit "skip open quote" "\"abc\"" 0 result

let test_delete_skip_close_quote () =
  (* Cursor on closing quote — skip *)
  let result = Paredit.delete_paredit rt "\"abc\"" 4 in
  check_edit "skip close quote" "\"abc\"" 4 result

let test_delete_empty_string () =
  (* Empty string "" — delete both quotes *)
  let result = Paredit.delete_paredit rt "\"\"" 0 in
  check_edit "empty string" "" 0 result

let test_delete_inside_string () =
  (* Inside string body — normal delete *)
  let result = Paredit.delete_paredit rt "\"abc\"" 1 in
  check_edit "inside string" "\"bc\"" 1 result

(* === Structural operations === *)

let test_slurp_forward () =
  let result = Paredit.slurp_forward rt "(a b) c" 2 in
  check_edit "slurp fwd" "(a b c)" 2 result

let test_slurp_forward_nothing () =
  let result = Paredit.slurp_forward rt "(a b)" 2 in
  check_edit "nothing to slurp" "(a b)" 2 result

let test_barf_forward () =
  let result = Paredit.barf_forward rt "(a b c)" 2 in
  check_edit "barf fwd" "(a b) c" 2 result

let test_barf_forward_single () =
  (* With only one element, barf should be a no-op *)
  let result = Paredit.barf_forward rt "(a)" 1 in
  check_edit "barf single" "(a)" 1 result

let test_slurp_backward () =
  let result = Paredit.slurp_backward rt "a (b c)" 4 in
  check_edit "slurp bwd" "(a b c)" 4 result

let test_slurp_backward_nothing () =
  let result = Paredit.slurp_backward rt "(a b)" 2 in
  check_edit "nothing to slurp" "(a b)" 2 result

let test_barf_backward () =
  let result = Paredit.barf_backward rt "(a b c)" 4 in
  check_edit "barf bwd" "a (b c)" 4 result

let test_barf_backward_single () =
  let result = Paredit.barf_backward rt "(a)" 1 in
  check_edit "barf single" "(a)" 1 result

let test_wrap_round () =
  let result = Paredit.wrap_round rt "a b c" 2 in
  check_edit "wrap" "a (b) c" 3 result

let test_wrap_round_list () =
  let result = Paredit.wrap_round rt "(a b) c" 0 in
  check_edit "wrap list" "((a b)) c" 1 result

let test_splice () =
  let result = Paredit.splice rt "(a (b c) d)" 5 in
  check_edit "splice" "(a b c d)" 4 result

let test_splice_outer () =
  let result = Paredit.splice rt "(a b c)" 3 in
  check_edit "splice outer" "a b c" 2 result

let test_raise_sexp () =
  let result = Paredit.raise_sexp rt "(a b c)" 2 in
  check_edit "raise" "b" 0 result

let test_raise_sexp_list () =
  let result = Paredit.raise_sexp rt "(a (b c) d)" 3 in
  check_edit "raise list" "(b c)" 0 result

let test_no_enclosing () =
  (* Structural operations on top-level should be no-ops *)
  let result = Paredit.slurp_forward rt "a b" 0 in
  check_edit "no enclosing slurp" "a b" 0 result;
  let result2 = Paredit.splice rt "a b" 0 in
  check_edit "no enclosing splice" "a b" 0 result2

(* === Indentation === *)

let test_indent_top_level () =
  Alcotest.(check int) "top level" 0 (Paredit.compute_indent rt "" 0);
  Alcotest.(check int) "top level after expr" 0
    (Paredit.compute_indent rt "(+ 1 2)\n" 8)

let test_indent_after_open () =
  (* Just an open paren — indent 2 *)
  Alcotest.(check int) "after (" 2 (Paredit.compute_indent rt "(" 1)

let test_indent_define () =
  Alcotest.(check int) "define body" 2
    (Paredit.compute_indent rt "(define x" 9)

let test_indent_define_fn () =
  Alcotest.(check int) "define fn body" 2
    (Paredit.compute_indent rt "(define (foo x)" 15)

let test_indent_lambda () =
  Alcotest.(check int) "lambda body" 2
    (Paredit.compute_indent rt "(lambda (x)" 11)

let test_indent_let () =
  Alcotest.(check int) "let body" 2
    (Paredit.compute_indent rt "(let ((x 1))" 12)

let test_indent_begin () =
  Alcotest.(check int) "begin body" 2
    (Paredit.compute_indent rt "(begin" 6)

let test_indent_cond () =
  Alcotest.(check int) "cond clause" 2
    (Paredit.compute_indent rt "(cond" 5)

let test_indent_call_align () =
  (* (foo bar → align with first arg "bar" at col 5 *)
  Alcotest.(check int) "align with arg" 5
    (Paredit.compute_indent rt "(foo bar" 8)

let test_indent_call_no_args () =
  (* (foo → no args on same line, default 2 *)
  Alcotest.(check int) "no args" 2
    (Paredit.compute_indent rt "(foo" 4)

let test_indent_if_align () =
  (* if is not a body form — aligns with first arg *)
  Alcotest.(check int) "if aligns" 4
    (Paredit.compute_indent rt "(if #t" 6)

let test_indent_nested () =
  (* (define (foo x)\n  (let ((y 1)) *)
  (* Inner (let is at column 2, let is body form → 2 + 2 = 4 *)
  let text = "(define (foo x)\n  (let ((y 1))" in
  Alcotest.(check int) "nested let body" 4
    (Paredit.compute_indent rt text (String.length text))

let test_indent_nested_call () =
  (* (map (lambda (x) → inside lambda, body indent = 5 + 2 = 7 *)
  let text = "(map (lambda (x)" in
  Alcotest.(check int) "lambda in map" 7
    (Paredit.compute_indent rt text (String.length text))

let test_indent_deep_nesting () =
  (* (define (foo x)\n  (if (= x 0) → inside if, align with (= x 0) at col 6 *)
  let text = "(define (foo x)\n  (if (= x 0)" in
  Alcotest.(check int) "if in define" 6
    (Paredit.compute_indent rt text (String.length text))

let test_indent_binding_pairs () =
  (* (let ((x 1) → inside bindings list, align (y 2) with (x 1) at col 6 *)
  let text = "(let ((x 1)" in
  Alcotest.(check int) "let binding pair" 6
    (Paredit.compute_indent rt text (String.length text));
  (* Same for let* *)
  let text2 = "(let* ((x 1)" in
  Alcotest.(check int) "let* binding pair" 7
    (Paredit.compute_indent rt text2 (String.length text2));
  (* Nested: (define (foo)\n  (let ((x 1) → inner bindings at col 8 *)
  let text3 = "(define (foo)\n  (let ((x 1)" in
  Alcotest.(check int) "nested let binding pair" 8
    (Paredit.compute_indent rt text3 (String.length text3))

let () =
  Alcotest.run "Paredit" [
    "navigation", [
      Alcotest.test_case "matching paren forward" `Quick test_matching_paren_forward;
      Alcotest.test_case "matching paren backward" `Quick test_matching_paren_backward;
      Alcotest.test_case "matching paren nested" `Quick test_matching_paren_nested;
      Alcotest.test_case "matching paren no match" `Quick test_matching_paren_no_match;
      Alcotest.test_case "matching paren not paren" `Quick test_matching_paren_not_paren;
      Alcotest.test_case "sexp forward atom" `Quick test_sexp_forward_atom;
      Alcotest.test_case "sexp forward list" `Quick test_sexp_forward_list;
      Alcotest.test_case "sexp forward skip ws" `Quick test_sexp_forward_skip_ws;
      Alcotest.test_case "sexp forward at close" `Quick test_sexp_forward_at_close;
      Alcotest.test_case "sexp forward string" `Quick test_sexp_forward_string;
      Alcotest.test_case "sexp forward quoted" `Quick test_sexp_forward_quoted;
      Alcotest.test_case "sexp backward atom" `Quick test_sexp_backward_atom;
      Alcotest.test_case "sexp backward list" `Quick test_sexp_backward_list;
      Alcotest.test_case "sexp backward at open" `Quick test_sexp_backward_at_open;
      Alcotest.test_case "enclosing paren" `Quick test_enclosing_paren;
      Alcotest.test_case "enclosing paren nested" `Quick test_enclosing_paren_nested;
      Alcotest.test_case "enclosing paren none" `Quick test_enclosing_paren_none;
    ];
    "balanced insertion", [
      Alcotest.test_case "insert open paren" `Quick test_insert_open_paren;
      Alcotest.test_case "insert open paren empty" `Quick test_insert_open_paren_empty;
      Alcotest.test_case "insert close paren move past" `Quick test_insert_close_paren_move_past;
      Alcotest.test_case "insert close paren skip ws" `Quick test_insert_close_paren_skip_ws;
      Alcotest.test_case "insert close paren insert" `Quick test_insert_close_paren_insert;
      Alcotest.test_case "insert double quote pair" `Quick test_insert_double_quote_pair;
      Alcotest.test_case "insert double quote move past" `Quick test_insert_double_quote_move_past;
    ];
    "balanced deletion", [
      Alcotest.test_case "backspace normal" `Quick test_backspace_normal;
      Alcotest.test_case "backspace skip open" `Quick test_backspace_skip_open;
      Alcotest.test_case "backspace skip close" `Quick test_backspace_skip_close;
      Alcotest.test_case "backspace empty pair" `Quick test_backspace_empty_pair;
      Alcotest.test_case "backspace at start" `Quick test_backspace_at_start;
      Alcotest.test_case "delete normal" `Quick test_delete_normal;
      Alcotest.test_case "delete skip close" `Quick test_delete_skip_close;
      Alcotest.test_case "delete skip open" `Quick test_delete_skip_open;
      Alcotest.test_case "delete empty pair" `Quick test_delete_empty_pair;
      Alcotest.test_case "delete at end" `Quick test_delete_at_end;
      Alcotest.test_case "backspace skip open quote" `Quick test_backspace_skip_open_quote;
      Alcotest.test_case "backspace skip close quote" `Quick test_backspace_skip_close_quote;
      Alcotest.test_case "backspace empty string" `Quick test_backspace_empty_string;
      Alcotest.test_case "backspace inside string" `Quick test_backspace_inside_string;
      Alcotest.test_case "backspace string after text" `Quick test_backspace_string_after_text;
      Alcotest.test_case "delete skip open quote" `Quick test_delete_skip_open_quote;
      Alcotest.test_case "delete skip close quote" `Quick test_delete_skip_close_quote;
      Alcotest.test_case "delete empty string" `Quick test_delete_empty_string;
      Alcotest.test_case "delete inside string" `Quick test_delete_inside_string;
    ];
    "structural operations", [
      Alcotest.test_case "slurp forward" `Quick test_slurp_forward;
      Alcotest.test_case "slurp forward nothing" `Quick test_slurp_forward_nothing;
      Alcotest.test_case "barf forward" `Quick test_barf_forward;
      Alcotest.test_case "barf forward single" `Quick test_barf_forward_single;
      Alcotest.test_case "slurp backward" `Quick test_slurp_backward;
      Alcotest.test_case "slurp backward nothing" `Quick test_slurp_backward_nothing;
      Alcotest.test_case "barf backward" `Quick test_barf_backward;
      Alcotest.test_case "barf backward single" `Quick test_barf_backward_single;
      Alcotest.test_case "wrap round" `Quick test_wrap_round;
      Alcotest.test_case "wrap round list" `Quick test_wrap_round_list;
      Alcotest.test_case "splice" `Quick test_splice;
      Alcotest.test_case "splice outer" `Quick test_splice_outer;
      Alcotest.test_case "raise sexp" `Quick test_raise_sexp;
      Alcotest.test_case "raise sexp list" `Quick test_raise_sexp_list;
      Alcotest.test_case "no enclosing" `Quick test_no_enclosing;
    ];
    "indentation", [
      Alcotest.test_case "top level" `Quick test_indent_top_level;
      Alcotest.test_case "after open" `Quick test_indent_after_open;
      Alcotest.test_case "define" `Quick test_indent_define;
      Alcotest.test_case "define fn" `Quick test_indent_define_fn;
      Alcotest.test_case "lambda" `Quick test_indent_lambda;
      Alcotest.test_case "let" `Quick test_indent_let;
      Alcotest.test_case "begin" `Quick test_indent_begin;
      Alcotest.test_case "cond" `Quick test_indent_cond;
      Alcotest.test_case "call align" `Quick test_indent_call_align;
      Alcotest.test_case "call no args" `Quick test_indent_call_no_args;
      Alcotest.test_case "if align" `Quick test_indent_if_align;
      Alcotest.test_case "nested" `Quick test_indent_nested;
      Alcotest.test_case "nested call" `Quick test_indent_nested_call;
      Alcotest.test_case "deep nesting" `Quick test_indent_deep_nesting;
      Alcotest.test_case "binding pairs" `Quick test_indent_binding_pairs;
    ];
  ]
