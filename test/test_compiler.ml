open Wile

let tbl = Symbol.create_table ()

(* Helper: parse a string into Syntax.t *)
let parse s =
  let port = Port.of_string s in
  Reader.read_syntax Readtable.default port

(* Helper: compile a string *)
let compile_string s =
  Compiler.compile tbl (parse s)

let test_compile_literal () =
  let code = compile_string "42" in
  Alcotest.(check int) "1 constant" 1 (Array.length code.constants);
  Alcotest.(check bool) "constant is 42" true
    (Datum.equal code.constants.(0) (Datum.Fixnum 42));
  (* Should be: CONST 0; HALT *)
  Alcotest.(check int) "2 instructions" 2 (Array.length code.instructions);
  Alcotest.(check string) "instr 0" "CONST 0"
    (Opcode.to_string code.instructions.(0));
  Alcotest.(check string) "instr 1" "HALT"
    (Opcode.to_string code.instructions.(1))

let test_compile_bool () =
  let code = compile_string "#t" in
  Alcotest.(check bool) "constant is #t" true
    (Datum.equal code.constants.(0) (Datum.Bool true))

let test_compile_variable () =
  let code = compile_string "x" in
  Alcotest.(check int) "1 symbol" 1 (Array.length code.symbols);
  Alcotest.(check string) "symbol is x" "x"
    (Symbol.name code.symbols.(0));
  Alcotest.(check string) "instr 0" "LOOKUP 0"
    (Opcode.to_string code.instructions.(0))

let test_compile_if () =
  let code = compile_string "(if #t 1 2)" in
  (* Should have JumpFalse and Jump instructions *)
  let has_jf = Array.exists (fun op ->
    match op with Opcode.JumpFalse _ -> true | _ -> false
  ) code.instructions in
  let has_jump = Array.exists (fun op ->
    match op with Opcode.Jump _ -> true | _ -> false
  ) code.instructions in
  Alcotest.(check bool) "has JumpFalse" true has_jf;
  Alcotest.(check bool) "has Jump" true has_jump

let test_compile_lambda () =
  let code = compile_string "(lambda (x) x)" in
  Alcotest.(check int) "1 child" 1 (Array.length code.children);
  let child = code.children.(0) in
  Alcotest.(check int) "1 param" 1 (Array.length child.params);
  Alcotest.(check string) "param is x" "x"
    (Symbol.name child.params.(0));
  Alcotest.(check bool) "not variadic" false child.variadic;
  (* Top-level should have MakeClosure *)
  let has_mc = Array.exists (fun op ->
    match op with Opcode.MakeClosure _ -> true | _ -> false
  ) code.instructions in
  Alcotest.(check bool) "has MakeClosure" true has_mc

let test_compile_define () =
  let code = compile_string "(define x 42)" in
  let has_define = Array.exists (fun op ->
    match op with Opcode.Define _ -> true | _ -> false
  ) code.instructions in
  Alcotest.(check bool) "has Define" true has_define

let test_compile_call () =
  let code = compile_string "(+ 1 2)" in
  let has_push = Array.exists (fun op ->
    match op with Opcode.Push -> true | _ -> false
  ) code.instructions in
  let has_call = Array.exists (fun op ->
    match op with Opcode.Call _ -> true | _ -> false
  ) code.instructions in
  Alcotest.(check bool) "has Push" true has_push;
  Alcotest.(check bool) "has Call" true has_call

let test_compile_quote () =
  let code = compile_string "'(1 2 3)" in
  Alcotest.(check int) "1 constant" 1 (Array.length code.constants);
  (* The constant should be a proper list *)
  let s = Datum.to_string code.constants.(0) in
  Alcotest.(check string) "quoted list" "(1 2 3)" s

let test_compile_error () =
  Alcotest.check_raises "empty application"
    (Compiler.Compile_error (Loc.make "<string>" 1 1, "empty application ()"))
    (fun () -> ignore (compile_string "()"))

let () =
  Alcotest.run "Compiler"
    [ ("Compiler",
       [ Alcotest.test_case "compile literal" `Quick test_compile_literal
       ; Alcotest.test_case "compile bool" `Quick test_compile_bool
       ; Alcotest.test_case "compile variable" `Quick test_compile_variable
       ; Alcotest.test_case "compile if" `Quick test_compile_if
       ; Alcotest.test_case "compile lambda" `Quick test_compile_lambda
       ; Alcotest.test_case "compile define" `Quick test_compile_define
       ; Alcotest.test_case "compile call" `Quick test_compile_call
       ; Alcotest.test_case "compile quote" `Quick test_compile_quote
       ; Alcotest.test_case "compile error" `Quick test_compile_error
       ])
    ]
