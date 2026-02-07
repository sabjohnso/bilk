open Wile

let loc = Loc.make "test" 1 1
let mk d = Syntax.make loc d
let sym s = mk (Syntax.Symbol s)
let fix n = mk (Syntax.Fixnum n)

let pair a b = mk (Syntax.Pair (a, b))
let nil = mk Syntax.Nil
let list_ elts = List.fold_right (fun e acc -> pair e acc) elts nil

(* --- parse_library_name --- *)

let test_parse_name_basic () =
  let s = list_ [ sym "scheme"; sym "base" ] in
  Alcotest.(check (list string)) "scheme base"
    ["scheme"; "base"] (Library.parse_library_name s)

let test_parse_name_integer () =
  let s = list_ [ sym "srfi"; fix 1 ] in
  Alcotest.(check (list string)) "srfi 1"
    ["srfi"; "1"] (Library.parse_library_name s)

(* --- parse_export_spec --- *)

let test_parse_export_id () =
  let s = sym "foo" in
  match Library.parse_export_spec s with
  | Library.Export_id "foo" -> ()
  | _ -> Alcotest.fail "expected Export_id foo"

let test_parse_export_rename () =
  let s = list_ [ sym "rename"; sym "internal"; sym "external" ] in
  match Library.parse_export_spec s with
  | Library.Export_rename ("internal", "external") -> ()
  | _ -> Alcotest.fail "expected Export_rename"

(* --- parse_import_set --- *)

let test_parse_import_lib () =
  let s = list_ [ sym "scheme"; sym "base" ] in
  match Library.parse_import_set s with
  | Library.Import_lib ["scheme"; "base"] -> ()
  | _ -> Alcotest.fail "expected Import_lib"

let test_parse_import_only () =
  let s = list_ [ sym "only"; list_ [ sym "scheme"; sym "base" ]; sym "+"; sym "-" ] in
  match Library.parse_import_set s with
  | Library.Import_only (Library.Import_lib ["scheme"; "base"], ["+"; "-"]) -> ()
  | _ -> Alcotest.fail "expected Import_only"

let test_parse_import_except () =
  let s = list_ [ sym "except"; list_ [ sym "scheme"; sym "base" ]; sym "car" ] in
  match Library.parse_import_set s with
  | Library.Import_except (Library.Import_lib ["scheme"; "base"], ["car"]) -> ()
  | _ -> Alcotest.fail "expected Import_except"

let test_parse_import_prefix () =
  let s = list_ [ sym "prefix"; list_ [ sym "scheme"; sym "base" ]; sym "s:" ] in
  match Library.parse_import_set s with
  | Library.Import_prefix (Library.Import_lib ["scheme"; "base"], "s:") -> ()
  | _ -> Alcotest.fail "expected Import_prefix"

let test_parse_import_rename () =
  let s = list_ [ sym "rename"; list_ [ sym "scheme"; sym "base" ];
                   list_ [ sym "car"; sym "first" ] ] in
  match Library.parse_import_set s with
  | Library.Import_rename (Library.Import_lib ["scheme"; "base"],
                           [("car", "first")]) -> ()
  | _ -> Alcotest.fail "expected Import_rename"

let test_parse_import_nested () =
  let s = list_ [ sym "only";
                   list_ [ sym "prefix"; list_ [ sym "scheme"; sym "base" ]; sym "s:" ];
                   sym "s:+"; sym "s:-" ] in
  match Library.parse_import_set s with
  | Library.Import_only (Library.Import_prefix (Library.Import_lib ["scheme"; "base"], "s:"),
                         ["s:+"; "s:-"]) -> ()
  | _ -> Alcotest.fail "expected nested import set"

(* --- resolve_import --- *)

let make_test_lib () =
  let tbl = Symbol.create_table () in
  let env = Env.empty () in
  let plus_sym = Symbol.intern tbl "+" in
  let car_sym = Symbol.intern tbl "car" in
  Env.define env plus_sym (Datum.Fixnum 0);
  Env.define env car_sym (Datum.Fixnum 0);
  let exports = Hashtbl.create 4 in
  let plus_slot = match Env.lookup_slot env plus_sym with Some s -> s | None -> assert false in
  let car_slot = match Env.lookup_slot env car_sym with Some s -> s | None -> assert false in
  Hashtbl.replace exports "+" (Symbol.id plus_sym, plus_slot);
  Hashtbl.replace exports "car" (Symbol.id car_sym, car_slot);
  let syn = Hashtbl.create 4 in
  (* Add a syntax binding for "let" *)
  let se = Expander.core_env () in
  (match Expander.lookup_binding se "let" with
   | Some b -> Hashtbl.replace syn "let" b
   | None -> ());
  let lib : Library.t = {
    name = ["test"; "lib"];
    env;
    exports;
    syntax_exports = syn;
  } in
  lib

let lookup_test_lib lib name =
  if name = lib.Library.name then Some lib else None

let test_resolve_basic () =
  let lib = make_test_lib () in
  let (rt, syn) = Library.resolve_import (lookup_test_lib lib)
    (Library.Import_lib ["test"; "lib"]) in
  Alcotest.(check int) "runtime count" 2 (List.length rt);
  Alcotest.(check int) "syntax count" 1 (List.length syn)

let test_resolve_only () =
  let lib = make_test_lib () in
  let (rt, _syn) = Library.resolve_import (lookup_test_lib lib)
    (Library.Import_only (Library.Import_lib ["test"; "lib"], ["+"])) in
  Alcotest.(check int) "only +" 1 (List.length rt);
  Alcotest.(check bool) "has +" true
    (List.exists (fun (n, _, _) -> n = "+") rt)

let test_resolve_except () =
  let lib = make_test_lib () in
  let (rt, _syn) = Library.resolve_import (lookup_test_lib lib)
    (Library.Import_except (Library.Import_lib ["test"; "lib"], ["car"])) in
  Alcotest.(check int) "except car" 1 (List.length rt);
  Alcotest.(check bool) "has +" true
    (List.exists (fun (n, _, _) -> n = "+") rt)

let test_resolve_prefix () =
  let lib = make_test_lib () in
  let (rt, syn) = Library.resolve_import (lookup_test_lib lib)
    (Library.Import_prefix (Library.Import_lib ["test"; "lib"], "s:")) in
  Alcotest.(check bool) "has s:+" true
    (List.exists (fun (n, _, _) -> n = "s:+") rt);
  Alcotest.(check bool) "has s:let" true
    (List.exists (fun (n, _) -> n = "s:let") syn)

let test_resolve_rename () =
  let lib = make_test_lib () in
  let (rt, _syn) = Library.resolve_import (lookup_test_lib lib)
    (Library.Import_rename (Library.Import_lib ["test"; "lib"],
                            [("car", "first")])) in
  Alcotest.(check bool) "has first" true
    (List.exists (fun (n, _, _) -> n = "first") rt);
  Alcotest.(check bool) "has +" true
    (List.exists (fun (n, _, _) -> n = "+") rt)

let test_resolve_unknown_lib () =
  let lib = make_test_lib () in
  Alcotest.check_raises "unknown lib"
    (Failure "unknown library: (no such)")
    (fun () ->
       ignore (Library.resolve_import (lookup_test_lib lib)
         (Library.Import_lib ["no"; "such"])))

let test_resolve_only_missing () =
  let lib = make_test_lib () in
  Alcotest.check_raises "only missing"
    (Failure "only: name not in export set: nonexistent")
    (fun () ->
       ignore (Library.resolve_import (lookup_test_lib lib)
         (Library.Import_only (Library.Import_lib ["test"; "lib"],
                               ["nonexistent"]))))

(* --- parse error tests --- *)

let test_parse_only_empty () =
  let s = list_ [ sym "only"; list_ [ sym "scheme"; sym "base" ] ] in
  Alcotest.check_raises "only empty"
    (Compiler.Compile_error (loc, "only: expected import set and identifiers"))
    (fun () -> ignore (Library.parse_import_set s))

let test_parse_except_empty () =
  let s = list_ [ sym "except"; list_ [ sym "scheme"; sym "base" ] ] in
  Alcotest.check_raises "except empty"
    (Compiler.Compile_error (loc, "except: expected import set and identifiers"))
    (fun () -> ignore (Library.parse_import_set s))

let test_parse_rename_empty () =
  let s = list_ [ sym "rename"; list_ [ sym "scheme"; sym "base" ] ] in
  Alcotest.check_raises "rename empty"
    (Compiler.Compile_error (loc, "rename: expected import set and pairs"))
    (fun () -> ignore (Library.parse_import_set s))

(* --- resolve error tests --- *)

let test_resolve_rename_missing () =
  let lib = make_test_lib () in
  Alcotest.check_raises "rename missing"
    (Failure "rename: name not in export set: nonexistent")
    (fun () ->
       ignore (Library.resolve_import (lookup_test_lib lib)
         (Library.Import_rename (Library.Import_lib ["test"; "lib"],
                                 [("nonexistent", "x")]))))

let test_resolve_rename_dup_from () =
  let lib = make_test_lib () in
  Alcotest.check_raises "rename dup from"
    (Failure "rename: duplicate source name: car")
    (fun () ->
       ignore (Library.resolve_import (lookup_test_lib lib)
         (Library.Import_rename (Library.Import_lib ["test"; "lib"],
                                 [("car", "first"); ("car", "second")]))))

let test_resolve_rename_dup_to () =
  let lib = make_test_lib () in
  Alcotest.check_raises "rename dup to"
    (Failure "rename: duplicate target name: same")
    (fun () ->
       ignore (Library.resolve_import (lookup_test_lib lib)
         (Library.Import_rename (Library.Import_lib ["test"; "lib"],
                                 [("car", "same"); ("+", "same")]))))

(* --- Registry --- *)

let test_registry () =
  let reg = Library.create_registry () in
  let lib = make_test_lib () in
  Library.register reg lib;
  (match Library.lookup reg ["test"; "lib"] with
   | Some l -> Alcotest.(check (list string)) "name" ["test"; "lib"] l.name
   | None -> Alcotest.fail "expected Some");
  match Library.lookup reg ["no"; "such"] with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let () =
  Alcotest.run "Library"
    [ ("parse",
       [ Alcotest.test_case "parse name basic" `Quick test_parse_name_basic
       ; Alcotest.test_case "parse name integer" `Quick test_parse_name_integer
       ; Alcotest.test_case "parse export id" `Quick test_parse_export_id
       ; Alcotest.test_case "parse export rename" `Quick test_parse_export_rename
       ; Alcotest.test_case "parse import lib" `Quick test_parse_import_lib
       ; Alcotest.test_case "parse import only" `Quick test_parse_import_only
       ; Alcotest.test_case "parse import except" `Quick test_parse_import_except
       ; Alcotest.test_case "parse import prefix" `Quick test_parse_import_prefix
       ; Alcotest.test_case "parse import rename" `Quick test_parse_import_rename
       ; Alcotest.test_case "parse import nested" `Quick test_parse_import_nested
       ])
    ; ("resolve",
       [ Alcotest.test_case "resolve basic" `Quick test_resolve_basic
       ; Alcotest.test_case "resolve only" `Quick test_resolve_only
       ; Alcotest.test_case "resolve except" `Quick test_resolve_except
       ; Alcotest.test_case "resolve prefix" `Quick test_resolve_prefix
       ; Alcotest.test_case "resolve rename" `Quick test_resolve_rename
       ; Alcotest.test_case "resolve unknown lib" `Quick test_resolve_unknown_lib
       ; Alcotest.test_case "resolve only missing" `Quick test_resolve_only_missing
       ; Alcotest.test_case "resolve rename missing" `Quick test_resolve_rename_missing
       ; Alcotest.test_case "resolve rename dup from" `Quick test_resolve_rename_dup_from
       ; Alcotest.test_case "resolve rename dup to" `Quick test_resolve_rename_dup_to
       ])
    ; ("parse-errors",
       [ Alcotest.test_case "only empty" `Quick test_parse_only_empty
       ; Alcotest.test_case "except empty" `Quick test_parse_except_empty
       ; Alcotest.test_case "rename empty" `Quick test_parse_rename_empty
       ])
    ; ("registry",
       [ Alcotest.test_case "register + lookup" `Quick test_registry
       ])
    ]
