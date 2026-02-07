open Wile

(* --- Step 1: Scaffold + Binary Helpers --- *)

let test_version_constants () =
  Alcotest.(check int) "major" 1 Fasl.version_major;
  Alcotest.(check int) "minor" 0 Fasl.version_minor

let test_fasl_error_catchable () =
  Alcotest.check_raises "fasl error"
    (Fasl.Fasl_error "test")
    (fun () -> raise (Fasl.Fasl_error "test"))

(* --- Step 2: Opcode Round-Trip --- *)

let roundtrip_code code =
  let tbl = Symbol.create_table () in
  let data = Fasl.write_code code in
  Fasl.read_code tbl data

let make_code instrs =
  { Datum.instructions = instrs;
    constants = [||]; symbols = [||]; children = [||];
    params = [||]; variadic = false; name = "<test>" }

let test_opcode_halt () =
  let code = make_code [| Opcode.Halt |] in
  let code' = roundtrip_code code in
  Alcotest.(check int) "instr count" 1 (Array.length code'.instructions);
  match code'.instructions.(0) with
  | Opcode.Halt -> ()
  | _ -> Alcotest.fail "expected Halt"

let test_opcode_with_arg () =
  let code = make_code [| Opcode.Const 42; Opcode.Jump 100 |] in
  let code' = roundtrip_code code in
  Alcotest.(check int) "instr count" 2 (Array.length code'.instructions);
  (match code'.instructions.(0) with
   | Opcode.Const 42 -> ()
   | _ -> Alcotest.fail "expected Const 42");
  match code'.instructions.(1) with
  | Opcode.Jump 100 -> ()
  | _ -> Alcotest.fail "expected Jump 100"

let test_opcode_all_variants () =
  let instrs = [|
    Opcode.Halt; Opcode.Const 1; Opcode.Lookup 2; Opcode.Define 3;
    Opcode.SetBang 4; Opcode.Push; Opcode.Jump 5; Opcode.JumpFalse 6;
    Opcode.Call 7; Opcode.TailCall 8; Opcode.Return; Opcode.MakeClosure 9;
  |] in
  let code = make_code instrs in
  let code' = roundtrip_code code in
  Alcotest.(check int) "instr count" 12 (Array.length code'.instructions);
  Array.iteri (fun i expected ->
    let actual = code'.instructions.(i) in
    Alcotest.(check string) (Printf.sprintf "opcode %d" i)
      (Opcode.to_string expected) (Opcode.to_string actual)
  ) instrs

(* --- Step 3: Datum Atom Round-Trip --- *)

let datum_testable = Alcotest.testable Datum.pp Datum.equal

let roundtrip_datum d =
  let tbl = Symbol.create_table () in
  let code = { Datum.instructions = [| Opcode.Const 0; Opcode.Halt |];
               constants = [| d |]; symbols = [||]; children = [||];
               params = [||]; variadic = false; name = "<test>" } in
  let data = Fasl.write_code code in
  let code' = Fasl.read_code tbl data in
  code'.constants.(0)

let test_datum_bool () =
  Alcotest.(check datum_testable) "true" (Datum.Bool true) (roundtrip_datum (Datum.Bool true));
  Alcotest.(check datum_testable) "false" (Datum.Bool false) (roundtrip_datum (Datum.Bool false))

let test_datum_fixnum () =
  Alcotest.(check datum_testable) "zero" (Datum.Fixnum 0) (roundtrip_datum (Datum.Fixnum 0));
  Alcotest.(check datum_testable) "positive" (Datum.Fixnum 42) (roundtrip_datum (Datum.Fixnum 42));
  Alcotest.(check datum_testable) "negative" (Datum.Fixnum (-1)) (roundtrip_datum (Datum.Fixnum (-1)));
  Alcotest.(check datum_testable) "max_int" (Datum.Fixnum max_int) (roundtrip_datum (Datum.Fixnum max_int));
  Alcotest.(check datum_testable) "min_int" (Datum.Fixnum min_int) (roundtrip_datum (Datum.Fixnum min_int))

let test_datum_flonum () =
  Alcotest.(check datum_testable) "pi" (Datum.Flonum 3.14) (roundtrip_datum (Datum.Flonum 3.14));
  Alcotest.(check datum_testable) "neg zero" (Datum.Flonum (-0.0)) (roundtrip_datum (Datum.Flonum (-0.0)));
  Alcotest.(check datum_testable) "infinity" (Datum.Flonum infinity) (roundtrip_datum (Datum.Flonum infinity));
  Alcotest.(check datum_testable) "neg_infinity" (Datum.Flonum neg_infinity) (roundtrip_datum (Datum.Flonum neg_infinity));
  let nan_rt = roundtrip_datum (Datum.Flonum nan) in
  (match nan_rt with
   | Datum.Flonum f -> Alcotest.(check bool) "nan" true (Float.is_nan f)
   | _ -> Alcotest.fail "expected Flonum")

let test_datum_char () =
  Alcotest.(check datum_testable) "A" (Datum.Char (Uchar.of_char 'A')) (roundtrip_datum (Datum.Char (Uchar.of_char 'A')));
  Alcotest.(check datum_testable) "lambda" (Datum.Char (Uchar.of_int 0x03BB)) (roundtrip_datum (Datum.Char (Uchar.of_int 0x03BB)))

let test_datum_str () =
  Alcotest.(check datum_testable) "hello" (Datum.Str (Bytes.of_string "hello")) (roundtrip_datum (Datum.Str (Bytes.of_string "hello")));
  Alcotest.(check datum_testable) "empty" (Datum.Str Bytes.empty) (roundtrip_datum (Datum.Str Bytes.empty));
  Alcotest.(check datum_testable) "unicode" (Datum.Str (Bytes.of_string "\xCE\xBB")) (roundtrip_datum (Datum.Str (Bytes.of_string "\xCE\xBB")))

let test_datum_symbol () =
  Alcotest.(check datum_testable) "foo" (Datum.Symbol "foo") (roundtrip_datum (Datum.Symbol "foo"))

let test_datum_nil_eof_void () =
  Alcotest.(check datum_testable) "nil" Datum.Nil (roundtrip_datum Datum.Nil);
  Alcotest.(check datum_testable) "eof" Datum.Eof (roundtrip_datum Datum.Eof);
  Alcotest.(check datum_testable) "void" Datum.Void (roundtrip_datum Datum.Void)

(* --- Step 4: Datum Compounds + Rejection --- *)

let test_datum_pair () =
  let d = Datum.Pair { car = Fixnum 1; cdr = Pair { car = Fixnum 2; cdr = Nil } } in
  Alcotest.(check datum_testable) "list" d (roundtrip_datum d);
  let d2 = Datum.Pair { car = Fixnum 1; cdr = Fixnum 2 } in
  Alcotest.(check datum_testable) "dotted" d2 (roundtrip_datum d2)

let test_datum_vector () =
  let d = Datum.Vector [| Fixnum 1; Bool true; Symbol "x" |] in
  Alcotest.(check datum_testable) "vector" d (roundtrip_datum d);
  let d2 = Datum.Vector [||] in
  Alcotest.(check datum_testable) "empty vector" d2 (roundtrip_datum d2)

let test_datum_bytevector () =
  let d = Datum.Bytevector (Bytes.of_string "\x01\x02\x03") in
  Alcotest.(check datum_testable) "bv" d (roundtrip_datum d)

let test_datum_unserializable () =
  let prim = Datum.Primitive { prim_name = "test"; prim_fn = (fun _ -> Datum.Void); prim_intrinsic = None } in
  let code = { Datum.instructions = [| Opcode.Const 0; Opcode.Halt |];
               constants = [| prim |]; symbols = [||]; children = [||];
               params = [||]; variadic = false; name = "<test>" } in
  Alcotest.check_raises "primitive"
    (Fasl.Fasl_error "cannot serialize primitive: test")
    (fun () -> ignore (Fasl.write_code code))

(* --- Step 5: Code Object Round-Trip --- *)

let test_code_simple () =
  let tbl = Symbol.create_table () in
  let sym_x = Symbol.intern tbl "x" in
  let code : Datum.code = {
    instructions = [| Opcode.Const 0; Opcode.Define 0; Opcode.Halt |];
    constants = [| Datum.Fixnum 42 |];
    symbols = [| sym_x |];
    children = [||];
    params = [||];
    variadic = false;
    name = "<top>";
  } in
  let data = Fasl.write_code code in
  let tbl2 = Symbol.create_table () in
  let code' = Fasl.read_code tbl2 data in
  Alcotest.(check string) "name" "<top>" code'.name;
  Alcotest.(check bool) "variadic" false code'.variadic;
  Alcotest.(check int) "params" 0 (Array.length code'.params);
  Alcotest.(check int) "symbols" 1 (Array.length code'.symbols);
  Alcotest.(check string) "sym name" "x" (Symbol.name code'.symbols.(0));
  Alcotest.(check int) "constants" 1 (Array.length code'.constants);
  Alcotest.(check datum_testable) "const" (Datum.Fixnum 42) code'.constants.(0);
  Alcotest.(check int) "instrs" 3 (Array.length code'.instructions)

let test_code_nested_children () =
  let child : Datum.code = {
    instructions = [| Opcode.Lookup 0; Opcode.Return |];
    constants = [||]; symbols = [||]; children = [||];
    params = [||]; variadic = false; name = "<child>";
  } in
  let parent : Datum.code = {
    instructions = [| Opcode.MakeClosure 0; Opcode.Halt |];
    constants = [||]; symbols = [||]; children = [| child |];
    params = [||]; variadic = false; name = "<parent>";
  } in
  let code' = roundtrip_code parent in
  Alcotest.(check int) "children" 1 (Array.length code'.children);
  Alcotest.(check string) "child name" "<child>" code'.children.(0).name;
  Alcotest.(check int) "child instrs" 2 (Array.length code'.children.(0).instructions)

let test_code_symbol_reintern () =
  let tbl1 = Symbol.create_table () in
  let sym_a = Symbol.intern tbl1 "a" in
  let sym_b = Symbol.intern tbl1 "b" in
  let code : Datum.code = {
    instructions = [| Opcode.Halt |];
    constants = [||]; symbols = [| sym_a; sym_b |]; children = [||];
    params = [| sym_a |]; variadic = false; name = "<test>";
  } in
  let data = Fasl.write_code code in
  let tbl2 = Symbol.create_table () in
  (* Pre-intern "b" so it gets a different id than in tbl1 *)
  let _ = Symbol.intern tbl2 "z" in
  let code' = Fasl.read_code tbl2 data in
  Alcotest.(check string) "sym 0" "a" (Symbol.name code'.symbols.(0));
  Alcotest.(check string) "sym 1" "b" (Symbol.name code'.symbols.(1));
  Alcotest.(check string) "param 0" "a" (Symbol.name code'.params.(0));
  (* The ids should be from tbl2, not tbl1 *)
  let a2 = Symbol.intern tbl2 "a" in
  let b2 = Symbol.intern tbl2 "b" in
  Alcotest.(check int) "a id matches tbl2" (Symbol.id a2) (Symbol.id code'.symbols.(0));
  Alcotest.(check int) "b id matches tbl2" (Symbol.id b2) (Symbol.id code'.symbols.(1))

let test_code_variadic () =
  let tbl = Symbol.create_table () in
  let sym_x = Symbol.intern tbl "x" in
  let sym_rest = Symbol.intern tbl "rest" in
  let code : Datum.code = {
    instructions = [| Opcode.Halt |];
    constants = [||]; symbols = [||]; children = [||];
    params = [| sym_x; sym_rest |]; variadic = true; name = "<variadic>";
  } in
  let code' = roundtrip_code code in
  Alcotest.(check bool) "variadic" true code'.variadic;
  Alcotest.(check int) "params" 2 (Array.length code'.params)

let test_code_end_to_end () =
  let inst = Instance.create () in
  let _ = Instance.eval_string inst "(define (square x) (* x x))" in
  (* Now compile a simple expression and serialize it *)
  let port = Port.of_string "(square 5)" in
  let expr = Reader.read_syntax inst.readtable port in
  let expanded = Expander.expand ~syn_env:inst.syn_env
    ~gensym:(fun () -> let n = !(inst.gensym_counter) in inst.gensym_counter := n + 1; Printf.sprintf "%%g%d" n)
    ~features:inst.features
    ~has_library:(fun _ -> false)
    ~read_include:(fun ~fold_case:_ _ -> []) expr in
  let code = Compiler.compile inst.symbols expanded in
  let data = Fasl.write_code code in
  let code' = Fasl.read_code inst.symbols data in
  let result = Vm.execute ~winds:inst.winds inst.global_env code' in
  Alcotest.(check datum_testable) "square 5" (Datum.Fixnum 25) result

(* --- Step 6: File I/O + Header Validation --- *)

let with_temp_file suffix fn =
  let path = Filename.temp_file "wile_fasl_test" suffix in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () -> fn path)

let test_file_roundtrip () =
  with_temp_file ".fasl" (fun path ->
    let code = make_code [| Opcode.Const 0; Opcode.Halt |] in
    let code = { code with Datum.constants = [| Datum.Fixnum 99 |] } in
    Fasl.write_code_to_file path code;
    let tbl = Symbol.create_table () in
    let code' = Fasl.read_code_from_file tbl path in
    Alcotest.(check datum_testable) "const" (Datum.Fixnum 99) code'.constants.(0))

let test_bad_magic () =
  let data = Bytes.of_string "BADMxxxxxxxxxxxxxxxx" in
  let tbl = Symbol.create_table () in
  Alcotest.check_raises "bad magic"
    (Fasl.Fasl_error "bad FASL magic: expected WFAS, got BADM")
    (fun () -> ignore (Fasl.read_code tbl data))

let test_version_mismatch () =
  let data = Bytes.create 16 in
  Bytes.blit_string "WFAS" 0 data 0 4;
  Bytes.set_uint16_le data 4 99;  (* major = 99 *)
  Bytes.set_uint16_le data 6 0;
  let tbl = Symbol.create_table () in
  Alcotest.check_raises "version"
    (Fasl.Fasl_error "FASL version mismatch: expected 1.x, got 99.0")
    (fun () -> ignore (Fasl.read_code tbl data))

let test_truncated () =
  let data = Bytes.of_string "WFAS" in
  let tbl = Symbol.create_table () in
  Alcotest.check_raises "truncated"
    (Fasl.Fasl_error "unexpected end of FASL data")
    (fun () -> ignore (Fasl.read_code tbl data))

let test_empty_data () =
  let data = Bytes.empty in
  let tbl = Symbol.create_table () in
  Alcotest.check_raises "empty"
    (Fasl.Fasl_error "unexpected end of FASL data")
    (fun () -> ignore (Fasl.read_code tbl data))

(* --- Step 7: Library FASL Serialization --- *)

let test_lib_fasl_simple () =
  with_temp_file ".fasl" (fun path ->
    let child_code : Datum.code = {
      instructions = [| Opcode.Const 0; Opcode.Return |];
      constants = [| Datum.Fixnum 1 |]; symbols = [||]; children = [||];
      params = [||]; variadic = false; name = "<lib-body>";
    } in
    let fasl : Fasl.lib_fasl = {
      lib_name = ["test"; "lib"];
      has_syntax_exports = false;
      exports = [ Library.Export_id "foo"; Library.Export_rename ("bar", "baz") ];
      declarations = [
        Fasl.Lib_import (Library.Import_lib ["scheme"; "base"]);
        Fasl.Lib_code child_code;
      ];
    } in
    Fasl.write_lib_fasl path fasl;
    let tbl = Symbol.create_table () in
    let fasl' = Fasl.read_lib_fasl tbl path in
    Alcotest.(check (list string)) "name" ["test"; "lib"] fasl'.lib_name;
    Alcotest.(check bool) "no syntax" false fasl'.has_syntax_exports;
    Alcotest.(check int) "exports" 2 (List.length fasl'.exports);
    Alcotest.(check int) "decls" 2 (List.length fasl'.declarations);
    (match List.nth fasl'.exports 0 with
     | Library.Export_id "foo" -> ()
     | _ -> Alcotest.fail "expected Export_id foo");
    (match List.nth fasl'.exports 1 with
     | Library.Export_rename ("bar", "baz") -> ()
     | _ -> Alcotest.fail "expected Export_rename bar baz");
    (match List.nth fasl'.declarations 0 with
     | Fasl.Lib_import (Library.Import_lib ["scheme"; "base"]) -> ()
     | _ -> Alcotest.fail "expected Import_lib scheme base");
    (match List.nth fasl'.declarations 1 with
     | Fasl.Lib_code c ->
       Alcotest.(check string) "code name" "<lib-body>" c.name
     | _ -> Alcotest.fail "expected Lib_code"))

let test_lib_fasl_nested_import () =
  with_temp_file ".fasl" (fun path ->
    let iset = Library.Import_only (
      Library.Import_prefix (Library.Import_lib ["scheme"; "base"], "s:"),
      ["s:+"; "s:-"]
    ) in
    let fasl : Fasl.lib_fasl = {
      lib_name = ["my"; "lib"];
      has_syntax_exports = false;
      exports = [];
      declarations = [ Fasl.Lib_import iset ];
    } in
    Fasl.write_lib_fasl path fasl;
    let tbl = Symbol.create_table () in
    let fasl' = Fasl.read_lib_fasl tbl path in
    match List.nth fasl'.declarations 0 with
    | Fasl.Lib_import (Library.Import_only (
        Library.Import_prefix (Library.Import_lib ["scheme"; "base"], "s:"),
        ["s:+"; "s:-"])) -> ()
    | _ -> Alcotest.fail "expected nested import set")

let test_lib_fasl_syntax_flag () =
  with_temp_file ".fasl" (fun path ->
    let fasl : Fasl.lib_fasl = {
      lib_name = ["my"; "macros"];
      has_syntax_exports = true;
      exports = [ Library.Export_id "my-macro" ];
      declarations = [];
    } in
    Fasl.write_lib_fasl path fasl;
    let tbl = Symbol.create_table () in
    let fasl' = Fasl.read_lib_fasl tbl path in
    Alcotest.(check bool) "has syntax" true fasl'.has_syntax_exports)

let test_lib_fasl_all_import_variants () =
  with_temp_file ".fasl" (fun path ->
    let fasl : Fasl.lib_fasl = {
      lib_name = ["test"];
      has_syntax_exports = false;
      exports = [];
      declarations = [
        Fasl.Lib_import (Library.Import_lib ["a"]);
        Fasl.Lib_import (Library.Import_only (Library.Import_lib ["b"], ["x"; "y"]));
        Fasl.Lib_import (Library.Import_except (Library.Import_lib ["c"], ["z"]));
        Fasl.Lib_import (Library.Import_prefix (Library.Import_lib ["d"], "p:"));
        Fasl.Lib_import (Library.Import_rename (Library.Import_lib ["e"], [("old", "new")]));
      ];
    } in
    Fasl.write_lib_fasl path fasl;
    let tbl = Symbol.create_table () in
    let fasl' = Fasl.read_lib_fasl tbl path in
    Alcotest.(check int) "5 decls" 5 (List.length fasl'.declarations);
    (* Verify each variant round-trips *)
    (match List.nth fasl'.declarations 0 with
     | Fasl.Lib_import (Library.Import_lib ["a"]) -> ()
     | _ -> Alcotest.fail "import_lib");
    (match List.nth fasl'.declarations 1 with
     | Fasl.Lib_import (Library.Import_only (Library.Import_lib ["b"], ["x"; "y"])) -> ()
     | _ -> Alcotest.fail "import_only");
    (match List.nth fasl'.declarations 2 with
     | Fasl.Lib_import (Library.Import_except (Library.Import_lib ["c"], ["z"])) -> ()
     | _ -> Alcotest.fail "import_except");
    (match List.nth fasl'.declarations 3 with
     | Fasl.Lib_import (Library.Import_prefix (Library.Import_lib ["d"], "p:")) -> ()
     | _ -> Alcotest.fail "import_prefix");
    (match List.nth fasl'.declarations 4 with
     | Fasl.Lib_import (Library.Import_rename (Library.Import_lib ["e"], [("old", "new")])) -> ()
     | _ -> Alcotest.fail "import_rename"))

(* --- Step 8: Cache Helpers --- *)

let test_fasl_path_for () =
  Alcotest.(check string) "sld to fasl"
    "/path/to/lib.fasl" (Fasl.fasl_path_for "/path/to/lib.sld");
  Alcotest.(check string) "nested"
    "scheme/base.fasl" (Fasl.fasl_path_for "scheme/base.sld")

let test_cache_missing_fasl () =
  with_temp_file ".sld" (fun sld_path ->
    let fasl_path = Fasl.fasl_path_for sld_path in
    Alcotest.(check bool) "missing" false
      (Fasl.is_cache_valid ~sld_path ~fasl_path))

let test_cache_stale () =
  with_temp_file ".sld" (fun sld_path ->
    let fasl_path = Filename.chop_extension sld_path ^ ".fasl" in
    (* Write fasl first, then sld, so fasl is older *)
    let oc = open_out fasl_path in
    close_out oc;
    Unix.sleepf 0.05;
    let oc = open_out sld_path in
    output_string oc "(define-library ...)";
    close_out oc;
    Fun.protect ~finally:(fun () -> try Sys.remove fasl_path with _ -> ())
      (fun () ->
        Alcotest.(check bool) "stale" false
          (Fasl.is_cache_valid ~sld_path ~fasl_path)))

let test_cache_fresh () =
  with_temp_file ".sld" (fun sld_path ->
    let fasl_path = Filename.chop_extension sld_path ^ ".fasl" in
    (* Write sld first, then fasl, so fasl is newer *)
    let oc = open_out sld_path in
    output_string oc "(define-library ...)";
    close_out oc;
    Unix.sleepf 0.05;
    let oc = open_out fasl_path in
    close_out oc;
    Fun.protect ~finally:(fun () -> try Sys.remove fasl_path with _ -> ())
      (fun () ->
        Alcotest.(check bool) "fresh" true
          (Fasl.is_cache_valid ~sld_path ~fasl_path)))

(* --- Step 9: Instance Integration --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "wile_fasl_test" "" in
  Fun.protect ~finally:(fun () ->
    (* Recursively clean up *)
    let rec rm path =
      if Sys.is_directory path then begin
        Array.iter (fun f -> rm (Filename.concat path f)) (Sys.readdir path);
        Sys.rmdir path
      end else
        Sys.remove path
    in
    (try rm dir with _ -> ()))
    (fun () -> fn dir)

let test_fasl_cache_created () =
  with_temp_dir (fun dir ->
    let sub = Filename.concat dir "mylib" in
    Sys.mkdir sub 0o755;
    let sld = Filename.concat sub "stuff.sld" in
    let oc = open_out sld in
    output_string oc "(define-library (mylib stuff) \
      (export val) \
      (import (scheme base)) \
      (begin (define val 42)))";
    close_out oc;
    let inst = Instance.create () in
    inst.search_paths := [dir];
    inst.fasl_cache := true;
    ignore (Instance.eval_string inst "(import (mylib stuff))");
    let result = Instance.eval_string inst "val" in
    Alcotest.(check (of_pp Datum.pp)) "value" (Datum.Fixnum 42) result;
    let fasl_path = Fasl.fasl_path_for sld in
    Alcotest.(check bool) "fasl created" true (Sys.file_exists fasl_path))

let test_fasl_cache_used () =
  with_temp_dir (fun dir ->
    let sub = Filename.concat dir "cached" in
    Sys.mkdir sub 0o755;
    let sld = Filename.concat sub "lib.sld" in
    let oc = open_out sld in
    output_string oc "(define-library (cached lib) \
      (export val) \
      (import (scheme base)) \
      (begin (define val 100)))";
    close_out oc;
    (* First load creates the cache *)
    let inst1 = Instance.create () in
    inst1.search_paths := [dir];
    inst1.fasl_cache := true;
    ignore (Instance.eval_string inst1 "(import (cached lib))");
    let fasl_path = Fasl.fasl_path_for sld in
    Alcotest.(check bool) "fasl exists" true (Sys.file_exists fasl_path);
    (* Second instance loads from cache *)
    let inst2 = Instance.create () in
    inst2.search_paths := [dir];
    inst2.fasl_cache := true;
    ignore (Instance.eval_string inst2 "(import (cached lib))");
    let result = Instance.eval_string inst2 "val" in
    Alcotest.(check (of_pp Datum.pp)) "cached value" (Datum.Fixnum 100) result)

let test_fasl_cache_invalidation () =
  with_temp_dir (fun dir ->
    let sub = Filename.concat dir "inval" in
    Sys.mkdir sub 0o755;
    let sld = Filename.concat sub "lib.sld" in
    let oc = open_out sld in
    output_string oc "(define-library (inval lib) \
      (export val) \
      (import (scheme base)) \
      (begin (define val 1)))";
    close_out oc;
    (* Create cache *)
    let inst1 = Instance.create () in
    inst1.search_paths := [dir];
    inst1.fasl_cache := true;
    ignore (Instance.eval_string inst1 "(import (inval lib))");
    Alcotest.(check (of_pp Datum.pp)) "v1" (Datum.Fixnum 1) (Instance.eval_string inst1 "val");
    (* Touch the .sld to make cache stale *)
    Unix.sleepf 0.05;
    let oc2 = open_out sld in
    output_string oc2 "(define-library (inval lib) \
      (export val) \
      (import (scheme base)) \
      (begin (define val 2)))";
    close_out oc2;
    (* New instance should recompile from source *)
    let inst2 = Instance.create () in
    inst2.search_paths := [dir];
    inst2.fasl_cache := true;
    ignore (Instance.eval_string inst2 "(import (inval lib))");
    Alcotest.(check (of_pp Datum.pp)) "v2" (Datum.Fixnum 2) (Instance.eval_string inst2 "val"))

(* --- Step 10: Edge Cases --- *)

let test_large_constants_pool () =
  let n = 500 in
  let constants = Array.init n (fun i -> Datum.Fixnum i) in
  let instrs = Array.init n (fun i -> Opcode.Const i) in
  let instrs = Array.append instrs [| Opcode.Halt |] in
  let code = { Datum.instructions = instrs; constants; symbols = [||];
               children = [||]; params = [||]; variadic = false; name = "<large>" } in
  let code' = roundtrip_code code in
  Alcotest.(check int) "consts" n (Array.length code'.constants);
  Alcotest.(check datum_testable) "last const"
    (Datum.Fixnum (n - 1)) code'.constants.(n - 1)

let test_deeply_nested_children () =
  let leaf : Datum.code = {
    instructions = [| Opcode.Return |]; constants = [||]; symbols = [||];
    children = [||]; params = [||]; variadic = false; name = "<leaf>";
  } in
  let mid : Datum.code = {
    instructions = [| Opcode.MakeClosure 0; Opcode.Return |];
    constants = [||]; symbols = [||]; children = [| leaf |];
    params = [||]; variadic = false; name = "<mid>";
  } in
  let top : Datum.code = {
    instructions = [| Opcode.MakeClosure 0; Opcode.Halt |];
    constants = [||]; symbols = [||]; children = [| mid |];
    params = [||]; variadic = false; name = "<top>";
  } in
  let top' = roundtrip_code top in
  Alcotest.(check string) "top" "<top>" top'.name;
  Alcotest.(check string) "mid" "<mid>" top'.children.(0).name;
  Alcotest.(check string) "leaf" "<leaf>" top'.children.(0).children.(0).name

let test_empty_code_object () =
  let code : Datum.code = {
    instructions = [||]; constants = [||]; symbols = [||];
    children = [||]; params = [||]; variadic = false; name = "";
  } in
  let code' = roundtrip_code code in
  Alcotest.(check string) "name" "" code'.name;
  Alcotest.(check int) "instrs" 0 (Array.length code'.instructions);
  Alcotest.(check int) "consts" 0 (Array.length code'.constants)

(* --- Runner --- *)

let () =
  Alcotest.run "Fasl"
    [ ("scaffold",
       [ Alcotest.test_case "version constants" `Quick test_version_constants
       ; Alcotest.test_case "Fasl_error catchable" `Quick test_fasl_error_catchable
       ])
    ; ("opcode",
       [ Alcotest.test_case "halt round-trip" `Quick test_opcode_halt
       ; Alcotest.test_case "opcodes with args" `Quick test_opcode_with_arg
       ; Alcotest.test_case "all 12 variants" `Quick test_opcode_all_variants
       ])
    ; ("datum-atoms",
       [ Alcotest.test_case "bool" `Quick test_datum_bool
       ; Alcotest.test_case "fixnum" `Quick test_datum_fixnum
       ; Alcotest.test_case "flonum" `Quick test_datum_flonum
       ; Alcotest.test_case "char" `Quick test_datum_char
       ; Alcotest.test_case "str" `Quick test_datum_str
       ; Alcotest.test_case "symbol" `Quick test_datum_symbol
       ; Alcotest.test_case "nil/eof/void" `Quick test_datum_nil_eof_void
       ])
    ; ("datum-compounds",
       [ Alcotest.test_case "pair" `Quick test_datum_pair
       ; Alcotest.test_case "vector" `Quick test_datum_vector
       ; Alcotest.test_case "bytevector" `Quick test_datum_bytevector
       ; Alcotest.test_case "unserializable" `Quick test_datum_unserializable
       ])
    ; ("code",
       [ Alcotest.test_case "simple code" `Quick test_code_simple
       ; Alcotest.test_case "nested children" `Quick test_code_nested_children
       ; Alcotest.test_case "symbol re-interning" `Quick test_code_symbol_reintern
       ; Alcotest.test_case "variadic" `Quick test_code_variadic
       ; Alcotest.test_case "end-to-end" `Quick test_code_end_to_end
       ])
    ; ("file-io",
       [ Alcotest.test_case "file round-trip" `Quick test_file_roundtrip
       ; Alcotest.test_case "bad magic" `Quick test_bad_magic
       ; Alcotest.test_case "version mismatch" `Quick test_version_mismatch
       ; Alcotest.test_case "truncated" `Quick test_truncated
       ; Alcotest.test_case "empty data" `Quick test_empty_data
       ])
    ; ("lib-fasl",
       [ Alcotest.test_case "simple lib_fasl" `Quick test_lib_fasl_simple
       ; Alcotest.test_case "nested import set" `Quick test_lib_fasl_nested_import
       ; Alcotest.test_case "syntax flag" `Quick test_lib_fasl_syntax_flag
       ; Alcotest.test_case "all import variants" `Quick test_lib_fasl_all_import_variants
       ])
    ; ("cache",
       [ Alcotest.test_case "fasl_path_for" `Quick test_fasl_path_for
       ; Alcotest.test_case "missing fasl" `Quick test_cache_missing_fasl
       ; Alcotest.test_case "stale cache" `Quick test_cache_stale
       ; Alcotest.test_case "fresh cache" `Quick test_cache_fresh
       ])
    ; ("integration",
       [ Alcotest.test_case "cache created" `Quick test_fasl_cache_created
       ; Alcotest.test_case "cache used" `Quick test_fasl_cache_used
       ; Alcotest.test_case "cache invalidation" `Quick test_fasl_cache_invalidation
       ])
    ; ("edge-cases",
       [ Alcotest.test_case "large constants pool" `Quick test_large_constants_pool
       ; Alcotest.test_case "deeply nested children" `Quick test_deeply_nested_children
       ; Alcotest.test_case "empty code object" `Quick test_empty_code_object
       ])
    ]
