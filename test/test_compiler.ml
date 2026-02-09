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

let test_compile_and () =
  let code = compile_string "(and 1 2 3)" in
  let has_jf = Array.exists (fun op ->
    match op with Opcode.JumpFalse _ -> true | _ -> false
  ) code.instructions in
  Alcotest.(check bool) "and has JumpFalse" true has_jf

let test_compile_let () =
  let code = compile_string "(let ((x 1)) x)" in
  let has_mc = Array.exists (fun op ->
    match op with Opcode.MakeClosure _ -> true | _ -> false
  ) code.instructions in
  let has_call = Array.exists (fun op ->
    match op with Opcode.Call _ -> true | _ -> false
  ) code.instructions in
  Alcotest.(check bool) "let has MakeClosure" true has_mc;
  Alcotest.(check bool) "let has Call" true has_call

let test_compile_cond () =
  let code = compile_string "(cond (#f 1) (#t 2))" in
  let jf_count = Array.fold_left (fun acc op ->
    match op with Opcode.JumpFalse _ -> acc + 1 | _ -> acc
  ) 0 code.instructions in
  Alcotest.(check bool) "cond has JumpFalse chain" true (jf_count >= 2)

let test_compile_error_let () =
  Alcotest.check_raises "malformed let"
    (Compiler.Compile_error (Loc.make "<string>" 1 1,
       "let expects bindings and at least one body expression"))
    (fun () -> ignore (compile_string "(let)"))

let test_compile_error_cond () =
  (* (cond) with no clauses compiles to void — verify it has Const + Halt *)
  let code = compile_string "(cond)" in
  Alcotest.(check int) "2 instructions" 2 (Array.length code.instructions);
  Alcotest.(check bool) "const is void" true
    (Datum.equal code.constants.(0) Datum.Void)

let test_compile_error () =
  Alcotest.check_raises "empty application"
    (Compiler.Compile_error (Loc.make "<string>" 1 1, "empty application ()"))
    (fun () -> ignore (compile_string "()"))

(* --- Source map tests --- *)

let loc_testable = Alcotest.testable Loc.pp Loc.equal

let test_source_map_length () =
  let code = compile_string "42" in
  Alcotest.(check int) "source_map length = instruction count"
    (Array.length code.instructions) (Array.length code.source_map)

let test_source_map_literal_loc () =
  let code = compile_string "42" in
  (* Literal at line 1, col 1 in "<string>" *)
  let loc = code.source_map.(0) in
  Alcotest.(check (loc_testable)) "literal loc"
    (Loc.make "<string>" 1 1) loc

let test_source_map_lambda_child () =
  let code = compile_string "(lambda (x) x)" in
  Alcotest.(check int) "1 child" 1 (Array.length code.children);
  let child = code.children.(0) in
  Alcotest.(check int) "child source_map length = child instruction count"
    (Array.length child.instructions) (Array.length child.source_map)

let test_source_map_call_loc () =
  let code = compile_string "(+ 1 2)" in
  (* Find the Call instruction *)
  let call_idx = ref (-1) in
  Array.iteri (fun i op ->
    match op with Opcode.Call _ -> call_idx := i | _ -> ()
  ) code.instructions;
  Alcotest.(check bool) "found Call" true (!call_idx >= 0);
  let loc = code.source_map.(!call_idx) in
  Alcotest.(check loc_testable) "call loc"
    (Loc.make "<string>" 1 1) loc

let test_source_map_define_loc () =
  let code = compile_string "(define x 42)" in
  (* Find the Define instruction *)
  let def_idx = ref (-1) in
  Array.iteri (fun i op ->
    match op with Opcode.Define _ -> def_idx := i | _ -> ()
  ) code.instructions;
  Alcotest.(check bool) "found Define" true (!def_idx >= 0);
  let loc = code.source_map.(!def_idx) in
  Alcotest.(check loc_testable) "define loc"
    (Loc.make "<string>" 1 1) loc

let test_source_map_nested_locs () =
  (* Two expressions on different columns *)
  let code = compile_string "(if #t 1 2)" in
  (* All instructions should have the file "<string>" *)
  Array.iter (fun (loc : Loc.t) ->
    Alcotest.(check string) "file" "<string>" loc.file
  ) code.source_map

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
       ; Alcotest.test_case "compile and" `Quick test_compile_and
       ; Alcotest.test_case "compile let" `Quick test_compile_let
       ; Alcotest.test_case "compile cond" `Quick test_compile_cond
       ; Alcotest.test_case "compile error let" `Quick test_compile_error_let
       ; Alcotest.test_case "compile error cond" `Quick test_compile_error_cond
       ])
    ; ("source-map",
       [ Alcotest.test_case "length matches instructions" `Quick test_source_map_length
       ; Alcotest.test_case "literal location" `Quick test_source_map_literal_loc
       ; Alcotest.test_case "lambda child has source map" `Quick test_source_map_lambda_child
       ; Alcotest.test_case "call instruction location" `Quick test_source_map_call_loc
       ; Alcotest.test_case "define instruction location" `Quick test_source_map_define_loc
       ; Alcotest.test_case "nested expressions have locations" `Quick test_source_map_nested_locs
       ])
    ]
