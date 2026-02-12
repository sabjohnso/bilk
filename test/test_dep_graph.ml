open Wile

(* --- Helpers --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "wile_depgraph_test" "" in
  Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ dir))) (fun () -> fn dir)

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let ensure_dir path =
  if not (Sys.file_exists path) then Sys.mkdir path 0o755

let write_sld dir lib_name contents =
  (* Library (foo bar) goes to <dir>/foo/bar.sld *)
  let rec make_path d = function
    | [] -> failwith "empty library name"
    | [last] -> Filename.concat d (last ^ ".sld")
    | segment :: rest ->
      let sub = Filename.concat d segment in
      ensure_dir sub;
      make_path sub rest
  in
  let path = make_path dir lib_name in
  write_file path contents;
  path

let rt = Readtable.default

let lib_name = Alcotest.testable
    (fun fmt names -> Format.fprintf fmt "(%s)" (String.concat " " names))
    (fun a b -> a = b)

let lib_name_list = Alcotest.(list lib_name)

let node_name (n : Dep_graph.node) = n.name

let has_substr s sub =
  let len_s = String.length s and len_sub = String.length sub in
  if len_sub > len_s then false
  else
    let rec check i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = sub then true
      else check (i + 1)
    in
    check 0

(* Standard builtins used in tests *)
let builtins = [
  ["scheme"; "base"]; ["scheme"; "write"]; ["scheme"; "read"];
  ["scheme"; "char"]; ["scheme"; "cxr"]; ["scheme"; "file"];
  ["scheme"; "inexact"]; ["scheme"; "complex"]; ["scheme"; "lazy"];
  ["scheme"; "case-lambda"]; ["scheme"; "process-context"];
  ["scheme"; "time"]; ["scheme"; "eval"]; ["scheme"; "load"];
  ["scheme"; "repl"]; ["scheme"; "r5rs"];
  ["srfi"; "13"]; ["srfi"; "14"]; ["srfi"; "69"];
  ["srfi"; "115"]; ["srfi"; "151"];
]

(* --- base_library_name tests --- *)

let test_base_library_name_direct () =
  let iset = Library.Import_lib ["scheme"; "base"] in
  Alcotest.check lib_name "direct import"
    ["scheme"; "base"] (Dep_graph.base_library_name iset)

let test_base_library_name_only () =
  let iset = Library.Import_only (
    Library.Import_lib ["srfi"; "1"], ["map"; "filter"]) in
  Alcotest.check lib_name "import only"
    ["srfi"; "1"] (Dep_graph.base_library_name iset)

let test_base_library_name_nested () =
  let iset = Library.Import_prefix (
    Library.Import_only (
      Library.Import_lib ["my"; "lib"], ["foo"]),
    "pre-") in
  Alcotest.check lib_name "nested modifiers"
    ["my"; "lib"] (Dep_graph.base_library_name iset)

let test_base_library_name_except () =
  let iset = Library.Import_except (
    Library.Import_lib ["scheme"; "write"], ["display"]) in
  Alcotest.check lib_name "import except"
    ["scheme"; "write"] (Dep_graph.base_library_name iset)

let test_base_library_name_rename () =
  let iset = Library.Import_rename (
    Library.Import_lib ["scheme"; "base"], [("cons", "kons")]) in
  Alcotest.check lib_name "import rename"
    ["scheme"; "base"] (Dep_graph.base_library_name iset)

(* --- parse_sld tests --- *)

let test_parse_sld_declared_name () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "a"]
        "(define-library (mylib a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let info = Dep_graph.parse_sld rt _path in
    Alcotest.check lib_name "declared name"
      ["mylib"; "a"] info.declared_name)

let test_parse_sld_imports () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "b"]
        "(define-library (mylib b)\n\
         (import (scheme base) (scheme write))\n\
         (export greet)\n\
         (begin (define (greet) (display \"hi\"))))" in
    let info = Dep_graph.parse_sld rt _path in
    Alcotest.check lib_name "declared name"
      ["mylib"; "b"] info.declared_name;
    Alcotest.check lib_name_list "imports"
      [["scheme"; "base"]; ["scheme"; "write"]] info.imports)

let test_parse_sld_no_imports () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "empty"]
        "(define-library (mylib empty)\n\
         (export)\n\
         (begin))" in
    let info = Dep_graph.parse_sld rt _path in
    Alcotest.check lib_name "declared name"
      ["mylib"; "empty"] info.declared_name;
    Alcotest.check lib_name_list "no imports" [] info.imports)

let test_parse_sld_numeric_name () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["srfi"; "1"]
        "(define-library (srfi 1)\n\
         (import (scheme base))\n\
         (export map)\n\
         (begin (define (map f l) l)))" in
    let info = Dep_graph.parse_sld rt _path in
    Alcotest.check lib_name "numeric component"
      ["srfi"; "1"] info.declared_name)

(* --- imports_of_sld tests --- *)

let test_imports_of_sld_simple () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "a"]
        "(define-library (mylib a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let imports = Dep_graph.imports_of_sld rt _path in
    Alcotest.check lib_name_list "single import"
      [["scheme"; "base"]] imports)

let test_imports_of_sld_multiple () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "b"]
        "(define-library (mylib b)\n\
         (import (scheme base) (scheme write))\n\
         (export greet)\n\
         (begin (define (greet) (display \"hi\"))))" in
    let imports = Dep_graph.imports_of_sld rt _path in
    Alcotest.check lib_name_list "multiple imports"
      [["scheme"; "base"]; ["scheme"; "write"]] imports)

let test_imports_of_sld_with_modifiers () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "c"]
        "(define-library (mylib c)\n\
         (import (only (srfi 1) map filter)\n\
                 (prefix (scheme base) base-))\n\
         (export y)\n\
         (begin (define y 2)))" in
    let imports = Dep_graph.imports_of_sld rt _path in
    Alcotest.check lib_name_list "imports with modifiers"
      [["srfi"; "1"]; ["scheme"; "base"]] imports)

let test_imports_of_sld_multiple_import_decls () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "d"]
        "(define-library (mylib d)\n\
         (import (scheme base))\n\
         (import (scheme write))\n\
         (export z)\n\
         (begin (define z 3)))" in
    let imports = Dep_graph.imports_of_sld rt _path in
    Alcotest.check lib_name_list "two import declarations"
      [["scheme"; "base"]; ["scheme"; "write"]] imports)

let test_imports_of_sld_no_imports () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "e"]
        "(define-library (mylib e)\n\
         (export)\n\
         (begin))" in
    let imports = Dep_graph.imports_of_sld rt _path in
    Alcotest.check lib_name_list "no imports" [] imports)

(* --- cond-expand tests --- *)

let test_imports_of_sld_cond_expand () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "cond"]
        "(define-library (mylib cond)\n\
         (import (scheme base))\n\
         (cond-expand\n\
           (r7rs (import (scheme write)))\n\
           (else (import (scheme read))))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let imports = Dep_graph.imports_of_sld rt _path in
    (* Should include imports from ALL branches *)
    Alcotest.check lib_name_list "cond-expand all branches"
      [["scheme"; "base"]; ["scheme"; "write"]; ["scheme"; "read"]] imports)

let test_imports_of_sld_cond_expand_nested () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "nested"]
        "(define-library (mylib nested)\n\
         (cond-expand\n\
           (wile\n\
             (import (scheme base))\n\
             (import (scheme write)))\n\
           (else\n\
             (import (scheme base))\n\
             (import (scheme read))))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let imports = Dep_graph.imports_of_sld rt _path in
    Alcotest.check lib_name_list "nested cond-expand"
      [["scheme"; "base"]; ["scheme"; "write"];
       ["scheme"; "base"]; ["scheme"; "read"]] imports)

let test_imports_of_sld_cond_expand_no_imports () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["mylib"; "condno"]
        "(define-library (mylib condno)\n\
         (import (scheme base))\n\
         (cond-expand\n\
           (r7rs (export x))\n\
           (else (export y)))\n\
         (begin (define x 1) (define y 2)))" in
    let imports = Dep_graph.imports_of_sld rt _path in
    (* cond-expand with no import clauses should add nothing *)
    Alcotest.check lib_name_list "cond-expand without imports"
      [["scheme"; "base"]] imports)

(* --- imports_of_scm tests --- *)

let test_imports_of_scm_basic () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "prog.scm" in
    write_file path
      "(import (scheme base) (scheme write))\n\
       (display \"hello\")\n";
    let imports = Dep_graph.imports_of_scm rt path in
    Alcotest.check lib_name_list "program imports"
      [["scheme"; "base"]; ["scheme"; "write"]] imports)

let test_imports_of_scm_multiple_import_forms () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "prog2.scm" in
    write_file path
      "(import (scheme base))\n\
       (import (scheme write))\n\
       (display \"hello\")\n";
    let imports = Dep_graph.imports_of_scm rt path in
    Alcotest.check lib_name_list "multiple import forms"
      [["scheme"; "base"]; ["scheme"; "write"]] imports)

let test_imports_of_scm_no_imports () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "prog3.scm" in
    write_file path "(+ 1 2)\n";
    let imports = Dep_graph.imports_of_scm rt path in
    Alcotest.check lib_name_list "no imports" [] imports)

(* --- resolve_sld tests --- *)

let test_resolve_sld_found () =
  with_temp_dir (fun dir ->
    let path = write_sld dir ["mylib"; "x"]
        "(define-library (mylib x) (export) (begin))" in
    let result = Dep_graph.resolve_sld ~search_paths:[dir] ["mylib"; "x"] in
    Alcotest.(check (option string)) "found" (Some path) result)

let test_resolve_sld_not_found () =
  with_temp_dir (fun dir ->
    let result = Dep_graph.resolve_sld ~search_paths:[dir] ["no"; "such"] in
    Alcotest.(check (option string)) "not found" None result)

let test_resolve_sld_search_order () =
  with_temp_dir (fun dir ->
    let dir1 = Filename.concat dir "first" in
    let dir2 = Filename.concat dir "second" in
    Sys.mkdir dir1 0o755; Sys.mkdir dir2 0o755;
    let path1 = write_sld dir1 ["order"; "lib"]
        "(define-library (order lib) (export) (begin))" in
    let _path2 = write_sld dir2 ["order"; "lib"]
        "(define-library (order lib) (export) (begin))" in
    let result = Dep_graph.resolve_sld ~search_paths:[dir1; dir2] ["order"; "lib"] in
    Alcotest.(check (option string)) "first dir wins" (Some path1) result)

(* --- build_graph tests --- *)

let test_build_graph_single () =
  with_temp_dir (fun dir ->
    let _path = write_sld dir ["leaf"; "lib"]
        "(define-library (leaf lib)\n\
         (import (scheme base))\n\
         (export val)\n\
         (begin (define val 1)))" in
    let nodes = Dep_graph.build_graph ~builtins
        ~search_paths:[dir] rt [["leaf"; "lib"]] in
    Alcotest.(check int) "one node" 1 (List.length nodes);
    Alcotest.check lib_name "node name"
      ["leaf"; "lib"] (node_name (List.hd nodes)))

let test_build_graph_transitive () =
  with_temp_dir (fun dir ->
    let _path_a = write_sld dir ["trans"; "a"]
        "(define-library (trans a)\n\
         (import (scheme base))\n\
         (export a-val)\n\
         (begin (define a-val 10)))" in
    let _path_b = write_sld dir ["trans"; "b"]
        "(define-library (trans b)\n\
         (import (scheme base) (trans a))\n\
         (export b-val)\n\
         (begin (define b-val 20)))" in
    let nodes = Dep_graph.build_graph ~builtins
        ~search_paths:[dir] rt [["trans"; "b"]] in
    let names = List.map node_name nodes in
    Alcotest.check lib_name_list "topological order: a before b"
      [["trans"; "a"]; ["trans"; "b"]] names)

let test_build_graph_diamond () =
  with_temp_dir (fun dir ->
    let _pa = write_sld dir ["diam"; "a"]
        "(define-library (diam a)\n\
         (import (scheme base))\n\
         (export a)\n\
         (begin (define a 1)))" in
    let _pb = write_sld dir ["diam"; "b"]
        "(define-library (diam b)\n\
         (import (scheme base) (diam a))\n\
         (export b)\n\
         (begin (define b 2)))" in
    let _pc = write_sld dir ["diam"; "c"]
        "(define-library (diam c)\n\
         (import (scheme base) (diam a))\n\
         (export c)\n\
         (begin (define c 3)))" in
    let _pd = write_sld dir ["diam"; "d"]
        "(define-library (diam d)\n\
         (import (scheme base) (diam b) (diam c))\n\
         (export d)\n\
         (begin (define d 4)))" in
    let nodes = Dep_graph.build_graph ~builtins
        ~search_paths:[dir] rt [["diam"; "d"]] in
    let names = List.map node_name nodes in
    let idx name =
      match List.mapi (fun i n -> (i, n)) names
            |> List.find_opt (fun (_, n) -> n = name) with
      | Some (i, _) -> i
      | None -> Alcotest.fail ("missing: " ^ String.concat " " name)
    in
    Alcotest.(check bool) "a before b" true (idx ["diam";"a"] < idx ["diam";"b"]);
    Alcotest.(check bool) "a before c" true (idx ["diam";"a"] < idx ["diam";"c"]);
    Alcotest.(check bool) "b before d" true (idx ["diam";"b"] < idx ["diam";"d"]);
    Alcotest.(check bool) "c before d" true (idx ["diam";"c"] < idx ["diam";"d"]);
    Alcotest.(check int) "four nodes" 4 (List.length nodes))

let test_build_graph_cycle () =
  with_temp_dir (fun dir ->
    let _pa = write_sld dir ["cyc"; "a"]
        "(define-library (cyc a)\n\
         (import (scheme base) (cyc b))\n\
         (export a)\n\
         (begin (define a 1)))" in
    let _pb = write_sld dir ["cyc"; "b"]
        "(define-library (cyc b)\n\
         (import (scheme base) (cyc a))\n\
         (export b)\n\
         (begin (define b 2)))" in
    match Dep_graph.build_graph ~builtins
            ~search_paths:[dir] rt [["cyc"; "a"]] with
    | _ -> Alcotest.fail "expected Cycle_error"
    | exception Dep_graph.Cycle_error cycles ->
      let has_both cycle =
        List.mem ["cyc"; "a"] cycle && List.mem ["cyc"; "b"] cycle
      in
      Alcotest.(check bool) "cycle contains both libs"
        true (List.exists has_both cycles))

let test_build_graph_self_cycle () =
  with_temp_dir (fun dir ->
    let _pa = write_sld dir ["selfc"; "a"]
        "(define-library (selfc a)\n\
         (import (scheme base) (selfc a))\n\
         (export a)\n\
         (begin (define a 1)))" in
    Alcotest.check_raises "self-cycle detected"
      (Dep_graph.Cycle_error [[["selfc"; "a"]; ["selfc"; "a"]]])
      (fun () ->
         ignore (Dep_graph.build_graph ~builtins
                   ~search_paths:[dir] rt [["selfc"; "a"]])))

let test_build_graph_builtin_omitted () =
  with_temp_dir (fun dir ->
    let _pa = write_sld dir ["uses"; "builtins"]
        "(define-library (uses builtins)\n\
         (import (scheme base) (scheme write) (scheme read))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let nodes = Dep_graph.build_graph ~builtins
        ~search_paths:[dir] rt [["uses"; "builtins"]] in
    let names = List.map node_name nodes in
    Alcotest.(check bool) "no scheme base"
      true (not (List.mem ["scheme"; "base"] names));
    Alcotest.(check bool) "no scheme write"
      true (not (List.mem ["scheme"; "write"] names));
    Alcotest.(check int) "only one node" 1 (List.length nodes))

let test_build_graph_resolve_error () =
  with_temp_dir (fun dir ->
    let _pa = write_sld dir ["resolve"; "err"]
        "(define-library (resolve err)\n\
         (import (scheme base) (no such lib))\n\
         (export x)\n\
         (begin (define x 1)))" in
    match Dep_graph.build_graph ~builtins
            ~search_paths:[dir] rt [["resolve"; "err"]] with
    | _ -> Alcotest.fail "expected Resolve_error"
    | exception Dep_graph.Resolve_error (name, _msg) ->
      Alcotest.check lib_name "missing lib name"
        ["no"; "such"; "lib"] name)

let test_build_graph_no_builtins_skips () =
  (* Without ~builtins, unresolvable libraries are silently skipped *)
  with_temp_dir (fun dir ->
    let _pa = write_sld dir ["skip"; "test"]
        "(define-library (skip test)\n\
         (import (scheme base) (nonexistent lib))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let nodes = Dep_graph.build_graph
        ~search_paths:[dir] rt [["skip"; "test"]] in
    Alcotest.(check int) "one node (missing lib skipped)" 1 (List.length nodes))

(* --- to_dot tests --- *)

let test_to_dot_empty () =
  let dot = Dep_graph.to_dot [] in
  Alcotest.(check bool) "contains digraph"
    true (has_substr dot "digraph")

let test_to_dot_single_node () =
  let nodes = [{ Dep_graph.name = ["my"; "lib"];
                 sld_path = "/tmp/my/lib.sld";
                 imports = [["scheme"; "base"]] }] in
  let dot = Dep_graph.to_dot nodes in
  Alcotest.(check bool) "contains node label"
    true (has_substr dot "my lib" || has_substr dot "(my lib)")

let test_to_dot_edge () =
  let nodes = [
    { Dep_graph.name = ["dep"; "a"];
      sld_path = "/tmp/dep/a.sld";
      imports = [] };
    { Dep_graph.name = ["dep"; "b"];
      sld_path = "/tmp/dep/b.sld";
      imports = [["dep"; "a"]] };
  ] in
  let dot = Dep_graph.to_dot nodes in
  Alcotest.(check bool) "contains edge"
    true (has_substr dot "->")

let test_to_dot_cycle_red () =
  let nodes = [
    { Dep_graph.name = ["cyc"; "a"];
      sld_path = "/tmp/cyc/a.sld";
      imports = [["cyc"; "b"]] };
    { Dep_graph.name = ["cyc"; "b"];
      sld_path = "/tmp/cyc/b.sld";
      imports = [["cyc"; "a"]] };
  ] in
  let dot = Dep_graph.to_dot nodes in
  Alcotest.(check bool) "contains red for cycle"
    true (has_substr dot "red")

(* --- format_tree tests --- *)

let test_format_tree_linear () =
  let nodes = [
    { Dep_graph.name = ["c"]; sld_path = "/c.sld"; imports = [] };
    { Dep_graph.name = ["b"]; sld_path = "/b.sld"; imports = [["c"]] };
    { Dep_graph.name = ["a"]; sld_path = "/a.sld"; imports = [["b"]] };
  ] in
  let result = Dep_graph.format_tree nodes ["a"] in
  Alcotest.(check string) "linear chain"
    "(a)\n  (b)\n    (c)\n"
    result

let test_format_tree_diamond () =
  let nodes = [
    { Dep_graph.name = ["d"]; sld_path = "/d.sld"; imports = [] };
    { Dep_graph.name = ["b"]; sld_path = "/b.sld"; imports = [["d"]] };
    { Dep_graph.name = ["c"]; sld_path = "/c.sld"; imports = [["d"]] };
    { Dep_graph.name = ["a"]; sld_path = "/a.sld"; imports = [["b"]; ["c"]] };
  ] in
  let result = Dep_graph.format_tree nodes ["a"] in
  Alcotest.(check string) "diamond (second d shows ...)"
    "(a)\n  (b)\n    (d)\n  (c)\n    (d) ...\n"
    result

(* --- Property tests --- *)

let test_topological_order_property =
  QCheck2.Test.make ~count:50 ~name:"topological order is valid"
    (QCheck2.Gen.int_range 2 6)
    (fun n ->
       let dir = Filename.temp_dir "wile_depgraph_prop" "" in
       Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ dir)))
         (fun () ->
            let sub = Filename.concat dir "chain" in
            ensure_dir sub;
            for i = 0 to n - 1 do
              let name = Printf.sprintf "%d" i in
              let imports = if i = 0 then "(scheme base)"
                else Printf.sprintf "(scheme base) (chain %d)" (i - 1) in
              let sld = Filename.concat sub (name ^ ".sld") in
              write_file sld
                (Printf.sprintf
                   "(define-library (chain %s)\n\
                    (import %s)\n\
                    (export x)\n\
                    (begin (define x %d)))" name imports i)
            done;
            let root = ["chain"; string_of_int (n - 1)] in
            let nodes = Dep_graph.build_graph ~builtins
                ~search_paths:[dir] rt [root] in
            let names = List.map node_name nodes in
            let idx_of name =
              List.mapi (fun i n -> (i, n)) names
              |> List.find (fun (_, nm) -> nm = name)
              |> fst in
            List.for_all (fun (node : Dep_graph.node) ->
              List.for_all (fun imp ->
                match List.find_opt (fun n -> n = imp) names with
                | None -> true
                | Some _ -> idx_of imp < idx_of node.name
              ) node.imports
            ) nodes))

let test_dot_edge_roundtrip =
  QCheck2.Test.make ~count:30 ~name:"DOT has edge for every import"
    (QCheck2.Gen.int_range 2 5)
    (fun n ->
       let dir = Filename.temp_dir "wile_depgraph_dot_prop" "" in
       Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ dir)))
         (fun () ->
            let sub = Filename.concat dir "dotchain" in
            ensure_dir sub;
            for i = 0 to n - 1 do
              let name = Printf.sprintf "%d" i in
              let imports = if i = 0 then "(scheme base)"
                else Printf.sprintf "(scheme base) (dotchain %d)" (i - 1) in
              let sld = Filename.concat sub (name ^ ".sld") in
              write_file sld
                (Printf.sprintf
                   "(define-library (dotchain %s)\n\
                    (import %s)\n\
                    (export x)\n\
                    (begin (define x %d)))" name imports i)
            done;
            let root = ["dotchain"; string_of_int (n - 1)] in
            let nodes = Dep_graph.build_graph ~builtins
                ~search_paths:[dir] rt [root] in
            let dot = Dep_graph.to_dot nodes in
            let node_names = List.map node_name nodes in
            List.for_all (fun (node : Dep_graph.node) ->
              List.for_all (fun imp ->
                match List.find_opt (fun n -> n = imp) node_names with
                | None -> true
                | Some _ ->
                  has_substr dot "->"
              ) node.imports
            ) nodes))

(* --- Test runner --- *)

let () =
  Alcotest.run "Dep_graph" [
    "base_library_name", [
      Alcotest.test_case "direct" `Quick test_base_library_name_direct;
      Alcotest.test_case "only" `Quick test_base_library_name_only;
      Alcotest.test_case "nested" `Quick test_base_library_name_nested;
      Alcotest.test_case "except" `Quick test_base_library_name_except;
      Alcotest.test_case "rename" `Quick test_base_library_name_rename;
    ];
    "parse_sld", [
      Alcotest.test_case "declared name" `Quick test_parse_sld_declared_name;
      Alcotest.test_case "imports" `Quick test_parse_sld_imports;
      Alcotest.test_case "no imports" `Quick test_parse_sld_no_imports;
      Alcotest.test_case "numeric name" `Quick test_parse_sld_numeric_name;
    ];
    "imports_of_sld", [
      Alcotest.test_case "simple" `Quick test_imports_of_sld_simple;
      Alcotest.test_case "multiple" `Quick test_imports_of_sld_multiple;
      Alcotest.test_case "with modifiers" `Quick test_imports_of_sld_with_modifiers;
      Alcotest.test_case "multiple import decls" `Quick test_imports_of_sld_multiple_import_decls;
      Alcotest.test_case "no imports" `Quick test_imports_of_sld_no_imports;
    ];
    "cond-expand", [
      Alcotest.test_case "all branches" `Quick test_imports_of_sld_cond_expand;
      Alcotest.test_case "nested" `Quick test_imports_of_sld_cond_expand_nested;
      Alcotest.test_case "no imports in branches" `Quick test_imports_of_sld_cond_expand_no_imports;
    ];
    "imports_of_scm", [
      Alcotest.test_case "basic" `Quick test_imports_of_scm_basic;
      Alcotest.test_case "multiple import forms" `Quick test_imports_of_scm_multiple_import_forms;
      Alcotest.test_case "no imports" `Quick test_imports_of_scm_no_imports;
    ];
    "resolve_sld", [
      Alcotest.test_case "found" `Quick test_resolve_sld_found;
      Alcotest.test_case "not found" `Quick test_resolve_sld_not_found;
      Alcotest.test_case "search order" `Quick test_resolve_sld_search_order;
    ];
    "build_graph", [
      Alcotest.test_case "single" `Quick test_build_graph_single;
      Alcotest.test_case "transitive" `Quick test_build_graph_transitive;
      Alcotest.test_case "diamond" `Quick test_build_graph_diamond;
      Alcotest.test_case "cycle" `Quick test_build_graph_cycle;
      Alcotest.test_case "self-cycle" `Quick test_build_graph_self_cycle;
      Alcotest.test_case "builtin omitted" `Quick test_build_graph_builtin_omitted;
      Alcotest.test_case "resolve error" `Quick test_build_graph_resolve_error;
      Alcotest.test_case "no builtins skips" `Quick test_build_graph_no_builtins_skips;
    ];
    "format_tree", [
      Alcotest.test_case "linear" `Quick test_format_tree_linear;
      Alcotest.test_case "diamond" `Quick test_format_tree_diamond;
    ];
    "to_dot", [
      Alcotest.test_case "empty" `Quick test_to_dot_empty;
      Alcotest.test_case "single node" `Quick test_to_dot_single_node;
      Alcotest.test_case "edge" `Quick test_to_dot_edge;
      Alcotest.test_case "cycle red" `Quick test_to_dot_cycle_red;
    ];
    "properties", [
      QCheck_alcotest.to_alcotest test_topological_order_property;
      QCheck_alcotest.to_alcotest test_dot_edge_roundtrip;
    ];
  ]
