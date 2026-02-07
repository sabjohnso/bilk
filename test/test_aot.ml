open Wile

(* --- Helpers --- *)

let make_instance () =
  let inst = Instance.create () in
  inst.fasl_cache := true;
  inst

let make_code instrs consts =
  { Datum.instructions = instrs;
    constants = consts; symbols = [||]; children = [||];
    params = [||]; variadic = false; name = "<test>" }

(* --- Group 1: Program FASL serialization --- *)

let test_empty_program_roundtrip () =
  let prog : Fasl.program_fasl = { declarations = [] } in
  let tbl = Symbol.create_table () in
  let data = Fasl.write_program_bytes prog in
  let prog' = Fasl.read_program_bytes tbl data in
  Alcotest.(check int) "empty" 0 (List.length prog'.declarations)

let test_code_only_roundtrip () =
  let code = make_code [| Opcode.Const 0; Opcode.Halt |]
    [| Datum.Fixnum 42 |] in
  let prog : Fasl.program_fasl = { declarations = [Fasl.Lib_code code] } in
  let tbl = Symbol.create_table () in
  let data = Fasl.write_program_bytes prog in
  let prog' = Fasl.read_program_bytes tbl data in
  Alcotest.(check int) "one decl" 1 (List.length prog'.declarations);
  match prog'.declarations with
  | [Fasl.Lib_code c] ->
    Alcotest.(check int) "instr count" 2 (Array.length c.instructions)
  | _ -> Alcotest.fail "expected Lib_code"

let test_import_only_roundtrip () =
  let iset = Library.Import_lib ["scheme"; "base"] in
  let prog : Fasl.program_fasl = { declarations = [Fasl.Lib_import iset] } in
  let tbl = Symbol.create_table () in
  let data = Fasl.write_program_bytes prog in
  let prog' = Fasl.read_program_bytes tbl data in
  Alcotest.(check int) "one decl" 1 (List.length prog'.declarations);
  match prog'.declarations with
  | [Fasl.Lib_import (Library.Import_lib parts)] ->
    Alcotest.(check (list string)) "parts" ["scheme"; "base"] parts
  | _ -> Alcotest.fail "expected Lib_import"

let test_mixed_roundtrip () =
  let iset = Library.Import_lib ["scheme"; "base"] in
  let code = make_code [| Opcode.Halt |] [||] in
  let prog : Fasl.program_fasl = {
    declarations = [Fasl.Lib_import iset; Fasl.Lib_code code]
  } in
  let tbl = Symbol.create_table () in
  let data = Fasl.write_program_bytes prog in
  let prog' = Fasl.read_program_bytes tbl data in
  Alcotest.(check int) "two decls" 2 (List.length prog'.declarations)

let test_wrong_format_type () =
  (* Write a code FASL (type 0) and try to read as program (type 2) *)
  let code = make_code [| Opcode.Halt |] [||] in
  let data = Fasl.write_code code in
  let tbl = Symbol.create_table () in
  Alcotest.check_raises "wrong type"
    (Fasl.Fasl_error "expected program FASL (type 2), got type 0")
    (fun () -> ignore (Fasl.read_program_bytes tbl data))

let test_file_roundtrip () =
  let iset = Library.Import_lib ["scheme"; "base"] in
  let code = make_code [| Opcode.Const 0; Opcode.Halt |]
    [| Datum.Fixnum 99 |] in
  let prog : Fasl.program_fasl = {
    declarations = [Fasl.Lib_import iset; Fasl.Lib_code code]
  } in
  let tbl = Symbol.create_table () in
  let path = Filename.temp_file "wile_test_" ".fasl" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    Fasl.write_program_fasl path prog;
    let prog' = Fasl.read_program_fasl tbl path in
    Alcotest.(check int) "two decls" 2 (List.length prog'.declarations))

(* --- Group 2: compile_port --- *)

let test_compile_single_expr () =
  let inst = make_instance () in
  let port = Port.of_string "(+ 1 2)" in
  let prog = Instance.compile_port inst port in
  Alcotest.(check int) "one decl" 1 (List.length prog.declarations);
  match prog.declarations with
  | [Fasl.Lib_code _] -> ()
  | _ -> Alcotest.fail "expected Lib_code"

let test_compile_define_and_use () =
  let inst = make_instance () in
  let port = Port.of_string "(define x 10) x" in
  let prog = Instance.compile_port inst port in
  Alcotest.(check int) "two decls" 2 (List.length prog.declarations)

let test_compile_import_and_expr () =
  let inst = make_instance () in
  let port = Port.of_string "(import (scheme base)) (+ 1 2)" in
  let prog = Instance.compile_port inst port in
  Alcotest.(check int) "two decls" 2 (List.length prog.declarations);
  match prog.declarations with
  | [Fasl.Lib_import _; Fasl.Lib_code _] -> ()
  | _ -> Alcotest.fail "expected import then code"

let test_compile_define_library_not_recorded () =
  let inst = make_instance () in
  let port = Port.of_string
    "(define-library (test compile lib) \
       (import (scheme base)) \
       (export x) \
       (begin (define x 42))) \
     (import (test compile lib)) \
     x" in
  let prog = Instance.compile_port inst port in
  (* define-library not recorded, but import and x are *)
  let has_lib_import = List.exists (function
    | Fasl.Lib_import _ -> true
    | _ -> false) prog.declarations in
  Alcotest.(check bool) "has import" true has_lib_import;
  let code_count = List.length (List.filter (function
    | Fasl.Lib_code _ -> true
    | _ -> false) prog.declarations) in
  Alcotest.(check int) "has code" 1 code_count

let test_compile_empty_port () =
  let inst = make_instance () in
  let port = Port.of_string "" in
  let prog = Instance.compile_port inst port in
  Alcotest.(check int) "empty" 0 (List.length prog.declarations)

let test_compile_error_propagated () =
  let inst = make_instance () in
  let port = Port.of_string "(lambda)" in
  match Instance.compile_port inst port with
  | _ -> Alcotest.fail "expected compile error"
  | exception Compiler.Compile_error (_, msg) ->
    Alcotest.(check string) "error msg"
      "lambda expects parameters and at least one body expression" msg

(* --- Group 3: run_program --- *)

let test_run_simple () =
  let inst = make_instance () in
  let port = Port.of_string "(+ 1 2)" in
  let prog = Instance.compile_port inst port in
  let result = Instance.run_program inst prog in
  Alcotest.(check string) "result" "3" (Datum.to_string result)

let test_run_define_and_use () =
  let inst = make_instance () in
  let port = Port.of_string "(define x 10) (+ x 5)" in
  let prog = Instance.compile_port inst port in
  let inst2 = make_instance () in
  let result = Instance.run_program inst2 prog in
  Alcotest.(check string) "result" "15" (Datum.to_string result)

let test_run_import_and_code () =
  let inst = make_instance () in
  let port = Port.of_string "(import (scheme base)) (+ 2 3)" in
  let prog = Instance.compile_port inst port in
  let inst2 = make_instance () in
  let result = Instance.run_program inst2 prog in
  Alcotest.(check string) "result" "5" (Datum.to_string result)

let test_run_empty () =
  let inst = make_instance () in
  let result = Instance.run_program inst { Fasl.declarations = [] } in
  Alcotest.(check string) "void" "#<void>" (Datum.to_string result)

let test_run_multiple_exprs () =
  let inst = make_instance () in
  let port = Port.of_string "1 2 3" in
  let prog = Instance.compile_port inst port in
  let inst2 = make_instance () in
  let result = Instance.run_program inst2 prog in
  Alcotest.(check string) "last" "3" (Datum.to_string result)

(* --- Group 4: End-to-end compile→serialize→load→run --- *)

let test_e2e_simple () =
  let inst = make_instance () in
  let port = Port.of_string "(define (square x) (* x x)) (square 7)" in
  let prog = Instance.compile_port inst port in
  let data = Fasl.write_program_bytes prog in
  let inst2 = make_instance () in
  let prog' = Fasl.read_program_bytes inst2.symbols data in
  let result = Instance.run_program inst2 prog' in
  Alcotest.(check string) "result" "49" (Datum.to_string result)

let test_e2e_with_imports () =
  let inst = make_instance () in
  let port = Port.of_string "(import (scheme base)) (map car '((1 2) (3 4)))" in
  let prog = Instance.compile_port inst port in
  let data = Fasl.write_program_bytes prog in
  let inst2 = make_instance () in
  let prog' = Fasl.read_program_bytes inst2.symbols data in
  let result = Instance.run_program inst2 prog' in
  Alcotest.(check string) "result" "(1 3)" (Datum.to_string result)

let test_e2e_closures () =
  let inst = make_instance () in
  let port = Port.of_string
    "(define (make-adder n) (lambda (x) (+ x n))) \
     (define add5 (make-adder 5)) \
     (add5 10)" in
  let prog = Instance.compile_port inst port in
  let data = Fasl.write_program_bytes prog in
  let inst2 = make_instance () in
  let prog' = Fasl.read_program_bytes inst2.symbols data in
  let result = Instance.run_program inst2 prog' in
  Alcotest.(check string) "result" "15" (Datum.to_string result)

let test_e2e_define_library_then_import () =
  let inst = make_instance () in
  inst.search_paths := [];
  let port = Port.of_string
    "(define-library (test e2e lib) \
       (import (scheme base)) \
       (export double) \
       (begin (define (double x) (* x 2)))) \
     (import (test e2e lib)) \
     (double 21)" in
  let prog = Instance.compile_port inst port in
  (* Running on the same instance: define-library was compile-time only,
     so the library is already registered in this instance. *)
  let result = Instance.run_program inst prog in
  Alcotest.(check string) "result" "42" (Datum.to_string result)

let test_e2e_file_roundtrip () =
  let inst = make_instance () in
  let port = Port.of_string
    "(define (fact n) \
       (if (< n 2) 1 (* n (fact (- n 1))))) \
     (fact 10)" in
  let prog = Instance.compile_port inst port in
  let path = Filename.temp_file "wile_e2e_" ".fasl" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    Fasl.write_program_fasl path prog;
    let inst2 = make_instance () in
    let prog' = Fasl.read_program_fasl inst2.symbols path in
    let result = Instance.run_program inst2 prog' in
    Alcotest.(check string) "result" "3628800" (Datum.to_string result))

(* --- Test runner --- *)

let () =
  Alcotest.run "AOT" [
    ("program-fasl", [
      Alcotest.test_case "empty round-trip" `Quick test_empty_program_roundtrip;
      Alcotest.test_case "code-only round-trip" `Quick test_code_only_roundtrip;
      Alcotest.test_case "import-only round-trip" `Quick test_import_only_roundtrip;
      Alcotest.test_case "mixed round-trip" `Quick test_mixed_roundtrip;
      Alcotest.test_case "wrong format type" `Quick test_wrong_format_type;
      Alcotest.test_case "file round-trip" `Quick test_file_roundtrip;
    ]);
    ("compile-port", [
      Alcotest.test_case "single expr" `Quick test_compile_single_expr;
      Alcotest.test_case "define and use" `Quick test_compile_define_and_use;
      Alcotest.test_case "import + expr" `Quick test_compile_import_and_expr;
      Alcotest.test_case "define-library not recorded" `Quick test_compile_define_library_not_recorded;
      Alcotest.test_case "empty port" `Quick test_compile_empty_port;
      Alcotest.test_case "compile error propagated" `Quick test_compile_error_propagated;
    ]);
    ("run-program", [
      Alcotest.test_case "simple" `Quick test_run_simple;
      Alcotest.test_case "define and use" `Quick test_run_define_and_use;
      Alcotest.test_case "import + code" `Quick test_run_import_and_code;
      Alcotest.test_case "empty" `Quick test_run_empty;
      Alcotest.test_case "multiple exprs" `Quick test_run_multiple_exprs;
    ]);
    ("e2e", [
      Alcotest.test_case "simple" `Quick test_e2e_simple;
      Alcotest.test_case "with imports" `Quick test_e2e_with_imports;
      Alcotest.test_case "closures" `Quick test_e2e_closures;
      Alcotest.test_case "define-library then import" `Quick test_e2e_define_library_then_import;
      Alcotest.test_case "file round-trip" `Quick test_e2e_file_roundtrip;
    ]);
  ]
