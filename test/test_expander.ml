open Wile

let loc = Loc.make "test" 1 1

let gensym_counter = ref 0

let gensym () =
  let n = !gensym_counter in
  gensym_counter := n + 1;
  Printf.sprintf "%%g%d" n

let reset_gensym () = gensym_counter := 0

let syn_env () = Expander.core_env ()

let expand_str s =
  reset_gensym ();
  let port = Port.of_string s in
  let inst = Instance.create () in
  let expr = Reader.read_syntax inst.readtable port in
  Expander.expand ~syn_env:(syn_env ()) ~gensym expr

let check_syntax_datum = Alcotest.check
  (Alcotest.testable Syntax.pp Syntax.equal_datum)

(* --- Identity expansion tests --- *)

let test_self_eval () =
  let s = Syntax.make loc (Syntax.Fixnum 42) in
  let result = Expander.expand ~syn_env:(syn_env ()) ~gensym s in
  check_syntax_datum "fixnum" s result;
  let s2 = Syntax.make loc (Syntax.Bool true) in
  let result2 = Expander.expand ~syn_env:(syn_env ()) ~gensym s2 in
  check_syntax_datum "bool" s2 result2;
  let s3 = Syntax.make loc (Syntax.Str "hello") in
  let result3 = Expander.expand ~syn_env:(syn_env ()) ~gensym s3 in
  check_syntax_datum "string" s3 result3

let test_symbol_passthrough () =
  let s = Syntax.make loc (Syntax.Symbol "x") in
  let result = Expander.expand ~syn_env:(syn_env ()) ~gensym s in
  check_syntax_datum "symbol" s result

let test_quote_passthrough () =
  let s = expand_str "'(1 2 3)" in
  (* Should remain (quote (1 2 3)) *)
  match s.datum with
  | Syntax.Pair (head, _) ->
    (match head.datum with
     | Syntax.Symbol "quote" -> ()
     | _ -> Alcotest.fail "expected quote")
  | _ -> Alcotest.fail "expected pair"

let test_if_expansion () =
  let s = expand_str "(if #t 1 2)" in
  match s.datum with
  | Syntax.Pair (head, _) ->
    (match head.datum with
     | Syntax.Symbol "if" -> ()
     | _ -> Alcotest.fail "expected if")
  | _ -> Alcotest.fail "expected pair"

let test_lambda_expansion () =
  let s = expand_str "(lambda (x) x)" in
  match s.datum with
  | Syntax.Pair (head, _) ->
    (match head.datum with
     | Syntax.Symbol "lambda" -> ()
     | _ -> Alcotest.fail "expected lambda")
  | _ -> Alcotest.fail "expected pair"

let test_define_expansion () =
  let s = expand_str "(define x 42)" in
  match s.datum with
  | Syntax.Pair (head, _) ->
    (match head.datum with
     | Syntax.Symbol "define" -> ()
     | _ -> Alcotest.fail "expected define")
  | _ -> Alcotest.fail "expected pair"

let test_begin_expansion () =
  let s = expand_str "(begin 1 2 3)" in
  match s.datum with
  | Syntax.Pair (head, _) ->
    (match head.datum with
     | Syntax.Symbol "begin" -> ()
     | _ -> Alcotest.fail "expected begin")
  | _ -> Alcotest.fail "expected pair"

let test_application_expansion () =
  let s = expand_str "(+ 1 2)" in
  match s.datum with
  | Syntax.Pair (head, _) ->
    (match head.datum with
     | Syntax.Symbol "+" -> ()
     | _ -> Alcotest.fail "expected +")
  | _ -> Alcotest.fail "expected pair"

let () =
  Alcotest.run "Expander"
    [ ("identity",
       [ Alcotest.test_case "self-eval" `Quick test_self_eval
       ; Alcotest.test_case "symbol passthrough" `Quick test_symbol_passthrough
       ; Alcotest.test_case "quote passthrough" `Quick test_quote_passthrough
       ; Alcotest.test_case "if expansion" `Quick test_if_expansion
       ; Alcotest.test_case "lambda expansion" `Quick test_lambda_expansion
       ; Alcotest.test_case "define expansion" `Quick test_define_expansion
       ; Alcotest.test_case "begin expansion" `Quick test_begin_expansion
       ; Alcotest.test_case "application expansion" `Quick test_application_expansion
       ])
    ]
