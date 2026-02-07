open Wile

(* -- Unit tests -- *)

let test_create () =
  let inst = Instance.create () in
  ignore (inst : Instance.t)

let test_intern () =
  let inst = Instance.create () in
  let s1 = Instance.intern inst "foo" in
  let s2 = Instance.intern inst "foo" in
  Alcotest.(check bool) "same symbol" true (Symbol.equal s1 s2);
  Alcotest.(check string) "name" "foo" (Symbol.name s1)

let test_independent_tables () =
  let i1 = Instance.create () in
  let i2 = Instance.create () in
  let s1 = Instance.intern i1 "x" in
  let s2 = Instance.intern i2 "x" in
  Alcotest.(check string) "same name" (Symbol.name s1) (Symbol.name s2);
  (* Different tables — ids are independent, both get the same id for "x" *)
  Alcotest.(check int) "same id in both tables" (Symbol.id s1) (Symbol.id s2)

let test_default_readtable () =
  let inst = Instance.create () in
  (* Default readtable should be Readtable.default — check a known property *)
  Alcotest.(check bool) "space is whitespace" true
    (Readtable.is_whitespace inst.readtable ' ');
  Alcotest.(check bool) "( is delimiter" true
    (Readtable.is_delimiter inst.readtable '(')

let test_custom_readtable () =
  let rt = Readtable.with_fold_case true Readtable.default in
  let inst = Instance.create ~readtable:rt () in
  Alcotest.(check bool) "fold_case on" true
    (Readtable.fold_case inst.readtable)

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let check_datum = Alcotest.check datum_testable

let test_eval_string () =
  let inst = Instance.create () in
  check_datum "eval fixnum" (Datum.Fixnum 42) (Instance.eval_string inst "42");
  check_datum "eval add" (Datum.Fixnum 3) (Instance.eval_string inst "(+ 1 2)")

let test_eval_syntax () =
  let inst = Instance.create () in
  let port = Port.of_string "42" in
  let expr = Reader.read_syntax inst.readtable port in
  check_datum "eval syntax" (Datum.Fixnum 42) (Instance.eval_syntax inst expr)

let test_primitives_registered () =
  let inst = Instance.create () in
  let plus_sym = Instance.intern inst "+" in
  let v = Env.lookup inst.global_env plus_sym in
  Alcotest.(check bool) "plus is bound" true (Option.is_some v);
  match v with
  | Some (Datum.Primitive p) ->
    Alcotest.(check string) "plus name" "+" p.prim_name
  | _ -> Alcotest.fail "expected primitive"

let test_scheme_base_registered () =
  let inst = Instance.create () in
  match Library.lookup inst.libraries ["scheme"; "base"] with
  | Some lib ->
    Alcotest.(check bool) "has +" true (Hashtbl.mem lib.exports "+");
    Alcotest.(check bool) "has car" true (Hashtbl.mem lib.exports "car");
    Alcotest.(check bool) "has define" true (Hashtbl.mem lib.syntax_exports "define")
  | None -> Alcotest.fail "expected (scheme base) library"

let test_scheme_char_registered () =
  let inst = Instance.create () in
  match Library.lookup inst.libraries ["scheme"; "char"] with
  | Some _ -> ()
  | None -> Alcotest.fail "expected (scheme char) library"

let test_features () =
  let inst = Instance.create () in
  Alcotest.(check bool) "has r7rs" true (List.mem "r7rs" inst.features);
  Alcotest.(check bool) "has wile" true (List.mem "wile" inst.features)

let test_eval_port_multiple () =
  let inst = Instance.create () in
  let port = Port.of_string "(define x 10) (define y 20) (+ x y)" in
  check_datum "last result" (Datum.Fixnum 30) (Instance.eval_port inst port)

let test_eval_port_empty () =
  let inst = Instance.create () in
  let port = Port.of_string "" in
  check_datum "empty port" Datum.Void (Instance.eval_port inst port)

let test_eval_port_define_use () =
  let inst = Instance.create () in
  let port = Port.of_string "(define (square x) (* x x)) (square 7)" in
  check_datum "define+use" (Datum.Fixnum 49) (Instance.eval_port inst port)

let test_lookup_known () =
  let inst = Instance.create () in
  match Instance.lookup inst "+" with
  | Some (Datum.Primitive p) ->
    Alcotest.(check string) "name" "+" p.prim_name
  | _ -> Alcotest.fail "expected primitive for +"

let test_lookup_unknown () =
  let inst = Instance.create () in
  Alcotest.(check bool) "not bound" true
    (Option.is_none (Instance.lookup inst "no-such-thing"))

let test_define_primitive () =
  let inst = Instance.create () in
  Instance.define_primitive inst "my-add" (fun args ->
    match args with
    | [Datum.Fixnum a; Datum.Fixnum b] -> Datum.Fixnum (a + b)
    | _ -> Datum.Void);
  check_datum "call from Scheme" (Datum.Fixnum 5)
    (Instance.eval_string inst "(my-add 2 3)")

let test_define_primitive_override () =
  let inst = Instance.create () in
  Instance.define_primitive inst "zero" (fun _ -> Datum.Fixnum 0);
  check_datum "first" (Datum.Fixnum 0) (Instance.eval_string inst "(zero)");
  Instance.define_primitive inst "zero" (fun _ -> Datum.Fixnum 99);
  check_datum "override" (Datum.Fixnum 99) (Instance.eval_string inst "(zero)")

let test_call_primitive () =
  let inst = Instance.create () in
  let plus = Option.get (Instance.lookup inst "+") in
  check_datum "call +" (Datum.Fixnum 6)
    (Instance.call inst plus [Datum.Fixnum 1; Datum.Fixnum 2; Datum.Fixnum 3])

let test_call_closure () =
  let inst = Instance.create () in
  ignore (Instance.eval_string inst "(define (square x) (* x x))");
  let square = Option.get (Instance.lookup inst "square") in
  check_datum "call square" (Datum.Fixnum 49)
    (Instance.call inst square [Datum.Fixnum 7])

let test_call_zero_args () =
  let inst = Instance.create () in
  Instance.define_primitive inst "forty-two" (fun _ -> Datum.Fixnum 42);
  let f = Option.get (Instance.lookup inst "forty-two") in
  check_datum "call zero args" (Datum.Fixnum 42)
    (Instance.call inst f [])

let test_call_non_procedure () =
  let inst = Instance.create () in
  Alcotest.check_raises "call non-proc" (Vm.Runtime_error "not a procedure: 42")
    (fun () -> ignore (Instance.call inst (Datum.Fixnum 42) []))

let test_eval_datum_self () =
  let inst = Instance.create () in
  check_datum "fixnum" (Datum.Fixnum 42) (Instance.eval_datum inst (Datum.Fixnum 42));
  check_datum "bool" (Datum.Bool true) (Instance.eval_datum inst (Datum.Bool true));
  check_datum "string" (Datum.Str (Bytes.of_string "hi"))
    (Instance.eval_datum inst (Datum.Str (Bytes.of_string "hi")))

let test_eval_datum_symbol () =
  let inst = Instance.create () in
  ignore (Instance.eval_string inst "(define x 99)");
  check_datum "symbol lookup" (Datum.Fixnum 99)
    (Instance.eval_datum inst (Datum.Symbol "x"))

let test_eval_datum_call () =
  let inst = Instance.create () in
  (* Build (+ 1 2) as a Datum *)
  let expr = Datum.list_of [Datum.Symbol "+"; Datum.Fixnum 1; Datum.Fixnum 2] in
  check_datum "call form" (Datum.Fixnum 3) (Instance.eval_datum inst expr)

let test_load_file () =
  let inst = Instance.create () in
  let path = Filename.temp_file "wile_test" ".scm" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let oc = open_out path in
    output_string oc "(define load-test-var 123)";
    close_out oc;
    Instance.load_file inst path;
    check_datum "loaded define" (Datum.Fixnum 123)
      (Option.get (Instance.lookup inst "load-test-var")))

let test_load_fasl () =
  let inst = Instance.create () in
  let path = Filename.temp_file "wile_test" ".fasl" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    (* Compile a simple expression to a code object and serialize it *)
    let code = Compiler.compile inst.symbols
      (Syntax.from_datum Loc.none
        (Datum.list_of [Datum.Symbol "define"; Datum.Symbol "fasl-var";
                        Datum.Fixnum 456])) in
    Fasl.write_code_to_file path code;
    Instance.load_fasl inst path;
    check_datum "loaded fasl define" (Datum.Fixnum 456)
      (Option.get (Instance.lookup inst "fasl-var")))

let test_eval_port_import () =
  let inst = Instance.create () in
  let port = Port.of_string "(import (scheme base)) (+ 1 2)" in
  check_datum "import+expr" (Datum.Fixnum 3) (Instance.eval_port inst port)

let () =
  Alcotest.run "Instance"
    [ ("Instance",
       [ Alcotest.test_case "create" `Quick test_create
       ; Alcotest.test_case "intern" `Quick test_intern
       ; Alcotest.test_case "independent tables" `Quick test_independent_tables
       ; Alcotest.test_case "default readtable" `Quick test_default_readtable
       ; Alcotest.test_case "custom readtable" `Quick test_custom_readtable
       ; Alcotest.test_case "eval_string" `Quick test_eval_string
       ; Alcotest.test_case "eval_syntax" `Quick test_eval_syntax
       ; Alcotest.test_case "primitives registered" `Quick test_primitives_registered
       ])
    ; ("Libraries",
       [ Alcotest.test_case "(scheme base) registered" `Quick test_scheme_base_registered
       ; Alcotest.test_case "(scheme char) registered" `Quick test_scheme_char_registered
       ; Alcotest.test_case "features" `Quick test_features
       ])
    ; ("Embedding",
       [ Alcotest.test_case "lookup known" `Quick test_lookup_known
       ; Alcotest.test_case "lookup unknown" `Quick test_lookup_unknown
       ; Alcotest.test_case "define_primitive" `Quick test_define_primitive
       ; Alcotest.test_case "define_primitive override" `Quick test_define_primitive_override
       ; Alcotest.test_case "call primitive" `Quick test_call_primitive
       ; Alcotest.test_case "call closure" `Quick test_call_closure
       ; Alcotest.test_case "call zero args" `Quick test_call_zero_args
       ; Alcotest.test_case "call non-procedure" `Quick test_call_non_procedure
       ; Alcotest.test_case "eval_datum self-eval" `Quick test_eval_datum_self
       ; Alcotest.test_case "eval_datum symbol" `Quick test_eval_datum_symbol
       ; Alcotest.test_case "eval_datum call" `Quick test_eval_datum_call
       ; Alcotest.test_case "load_file" `Quick test_load_file
       ; Alcotest.test_case "load_fasl" `Quick test_load_fasl
       ])
    ; ("eval_port",
       [ Alcotest.test_case "multiple expressions" `Quick test_eval_port_multiple
       ; Alcotest.test_case "empty port" `Quick test_eval_port_empty
       ; Alcotest.test_case "define + use" `Quick test_eval_port_define_use
       ; Alcotest.test_case "import + expr" `Quick test_eval_port_import
       ])
    ]
