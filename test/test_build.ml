open Wile

(* --- Helpers --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "wile_build_test" "" in
  Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ dir))) (fun () -> fn dir)

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let ensure_dir path =
  if not (Sys.file_exists path) then Sys.mkdir path 0o755

let write_sld dir lib_name contents =
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

let write_dummy_fasl path =
  write_file path "dummy"

let set_mtime path time =
  Unix.utimes path time time

let lib_name_testable = Alcotest.testable
    (fun fmt names -> Format.fprintf fmt "(%s)" (String.concat " " names))
    (fun a b -> a = b)

let stale_reason_testable = Alcotest.testable
    (fun fmt r -> match r with
      | Build.No_fasl -> Format.fprintf fmt "No_fasl"
      | Build.Source_newer -> Format.fprintf fmt "Source_newer"
      | Build.Dep_newer name ->
        Format.fprintf fmt "Dep_newer(%s)" (Library.name_to_string name))
    (fun a b -> match a, b with
      | Build.No_fasl, Build.No_fasl -> true
      | Build.Source_newer, Build.Source_newer -> true
      | Build.Dep_newer a, Build.Dep_newer b -> a = b
      | _ -> false)

let stale_reason_opt = Alcotest.option stale_reason_testable

(* --- is_stale tests --- *)

let test_no_fasl () =
  with_temp_dir (fun dir ->
    let sld_path = write_sld dir ["test"; "a"]
        "(define-library (test a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let node = { Dep_graph.name = ["test"; "a"]; sld_path; imports = [["scheme"; "base"]] } in
    Alcotest.check stale_reason_opt "no fasl -> stale"
      (Some Build.No_fasl) (Build.is_stale [node] node))

let test_fresh () =
  with_temp_dir (fun dir ->
    let sld_path = write_sld dir ["test"; "b"]
        "(define-library (test b)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let fasl_path = Fasl.fasl_path_for sld_path in
    write_dummy_fasl fasl_path;
    (* Set sld to t=100, fasl to t=200 (fasl is newer) *)
    set_mtime sld_path 100.0;
    set_mtime fasl_path 200.0;
    let node = { Dep_graph.name = ["test"; "b"]; sld_path; imports = [["scheme"; "base"]] } in
    Alcotest.check stale_reason_opt "fresh -> None"
      None (Build.is_stale [node] node))

let test_source_newer () =
  with_temp_dir (fun dir ->
    let sld_path = write_sld dir ["test"; "c"]
        "(define-library (test c)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let fasl_path = Fasl.fasl_path_for sld_path in
    write_dummy_fasl fasl_path;
    (* Set fasl to t=100, sld to t=200 (source is newer) *)
    set_mtime fasl_path 100.0;
    set_mtime sld_path 200.0;
    let node = { Dep_graph.name = ["test"; "c"]; sld_path; imports = [["scheme"; "base"]] } in
    Alcotest.check stale_reason_opt "source newer -> stale"
      (Some Build.Source_newer) (Build.is_stale [node] node))

let test_dep_newer () =
  with_temp_dir (fun dir ->
    let sld_a = write_sld dir ["test"; "dep"]
        "(define-library (test dep)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let sld_b = write_sld dir ["test"; "user"]
        "(define-library (test user)\n\
         (import (scheme base) (test dep))\n\
         (export y)\n\
         (begin (define y 2)))" in
    let fasl_a = Fasl.fasl_path_for sld_a in
    let fasl_b = Fasl.fasl_path_for sld_b in
    write_dummy_fasl fasl_a;
    write_dummy_fasl fasl_b;
    (* Both sources old, dep's fasl newer than user's fasl *)
    set_mtime sld_a 100.0;
    set_mtime sld_b 100.0;
    set_mtime fasl_b 200.0;
    set_mtime fasl_a 300.0;
    let node_a = { Dep_graph.name = ["test"; "dep"]; sld_path = sld_a;
                   imports = [["scheme"; "base"]] } in
    let node_b = { Dep_graph.name = ["test"; "user"]; sld_path = sld_b;
                   imports = [["scheme"; "base"]; ["test"; "dep"]] } in
    let nodes = [node_a; node_b] in
    Alcotest.check stale_reason_opt "dep newer -> stale"
      (Some (Build.Dep_newer ["test"; "dep"])) (Build.is_stale nodes node_b))

(* --- plan tests --- *)

let test_plan_nothing () =
  with_temp_dir (fun dir ->
    let sld_a = write_sld dir ["plan"; "a"]
        "(define-library (plan a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let fasl_a = Fasl.fasl_path_for sld_a in
    write_dummy_fasl fasl_a;
    set_mtime sld_a 100.0;
    set_mtime fasl_a 200.0;
    let node_a = { Dep_graph.name = ["plan"; "a"]; sld_path = sld_a;
                   imports = [["scheme"; "base"]] } in
    let actions = Build.plan [node_a] in
    Alcotest.(check int) "nothing to build" 0 (List.length actions))

let test_plan_leaf_stale () =
  with_temp_dir (fun dir ->
    let sld_a = write_sld dir ["plan"; "leaf"]
        "(define-library (plan leaf)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    (* No fasl -> stale *)
    let node_a = { Dep_graph.name = ["plan"; "leaf"]; sld_path = sld_a;
                   imports = [["scheme"; "base"]] } in
    let actions = Build.plan [node_a] in
    Alcotest.(check int) "one action" 1 (List.length actions);
    Alcotest.check lib_name_testable "correct lib"
      ["plan"; "leaf"] (List.hd actions).node.name)

let test_plan_propagates () =
  with_temp_dir (fun dir ->
    (* A -> B -> C chain, A is stale (no fasl), so B and C should also be in plan *)
    let sld_a = write_sld dir ["prop"; "a"]
        "(define-library (prop a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let sld_b = write_sld dir ["prop"; "b"]
        "(define-library (prop b)\n\
         (import (scheme base) (prop a))\n\
         (export y)\n\
         (begin (define y 2)))" in
    let sld_c = write_sld dir ["prop"; "c"]
        "(define-library (prop c)\n\
         (import (scheme base) (prop b))\n\
         (export z)\n\
         (begin (define z 3)))" in
    (* B and C have fresh fasls, but A has none -> propagation *)
    let fasl_b = Fasl.fasl_path_for sld_b in
    let fasl_c = Fasl.fasl_path_for sld_c in
    write_dummy_fasl fasl_b;
    write_dummy_fasl fasl_c;
    set_mtime sld_a 100.0;
    set_mtime sld_b 100.0;
    set_mtime sld_c 100.0;
    set_mtime fasl_b 200.0;
    set_mtime fasl_c 200.0;
    let node_a = { Dep_graph.name = ["prop"; "a"]; sld_path = sld_a;
                   imports = [["scheme"; "base"]] } in
    let node_b = { Dep_graph.name = ["prop"; "b"]; sld_path = sld_b;
                   imports = [["scheme"; "base"]; ["prop"; "a"]] } in
    let node_c = { Dep_graph.name = ["prop"; "c"]; sld_path = sld_c;
                   imports = [["scheme"; "base"]; ["prop"; "b"]] } in
    let actions = Build.plan [node_a; node_b; node_c] in
    let names = List.map (fun (a : Build.build_action) -> a.node.name) actions in
    Alcotest.(check int) "all three in plan" 3 (List.length actions);
    Alcotest.(check bool) "a in plan" true (List.mem ["prop"; "a"] names);
    Alcotest.(check bool) "b in plan" true (List.mem ["prop"; "b"] names);
    Alcotest.(check bool) "c in plan" true (List.mem ["prop"; "c"] names))

let test_plan_partial () =
  with_temp_dir (fun dir ->
    (* Diamond: A is shared base, B and C depend on A, D depends on B and C.
       Only B is stale (no fasl), so plan should include B and D but not A or C. *)
    let sld_a = write_sld dir ["partial"; "a"]
        "(define-library (partial a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let sld_b = write_sld dir ["partial"; "b"]
        "(define-library (partial b)\n\
         (import (scheme base) (partial a))\n\
         (export y)\n\
         (begin (define y 2)))" in
    let sld_c = write_sld dir ["partial"; "c"]
        "(define-library (partial c)\n\
         (import (scheme base) (partial a))\n\
         (export z)\n\
         (begin (define z 3)))" in
    let sld_d = write_sld dir ["partial"; "d"]
        "(define-library (partial d)\n\
         (import (scheme base) (partial b) (partial c))\n\
         (export w)\n\
         (begin (define w 4)))" in
    (* A and C have fresh fasls; B has no fasl; D has fresh fasl *)
    let fasl_a = Fasl.fasl_path_for sld_a in
    let fasl_c = Fasl.fasl_path_for sld_c in
    let fasl_d = Fasl.fasl_path_for sld_d in
    write_dummy_fasl fasl_a;
    write_dummy_fasl fasl_c;
    write_dummy_fasl fasl_d;
    set_mtime sld_a 100.0; set_mtime fasl_a 200.0;
    set_mtime sld_b 100.0; (* no fasl for B *)
    set_mtime sld_c 100.0; set_mtime fasl_c 200.0;
    set_mtime sld_d 100.0; set_mtime fasl_d 200.0;
    let node_a = { Dep_graph.name = ["partial"; "a"]; sld_path = sld_a;
                   imports = [["scheme"; "base"]] } in
    let node_b = { Dep_graph.name = ["partial"; "b"]; sld_path = sld_b;
                   imports = [["scheme"; "base"]; ["partial"; "a"]] } in
    let node_c = { Dep_graph.name = ["partial"; "c"]; sld_path = sld_c;
                   imports = [["scheme"; "base"]; ["partial"; "a"]] } in
    let node_d = { Dep_graph.name = ["partial"; "d"]; sld_path = sld_d;
                   imports = [["scheme"; "base"]; ["partial"; "b"]; ["partial"; "c"]] } in
    let actions = Build.plan [node_a; node_b; node_c; node_d] in
    let names = List.map (fun (a : Build.build_action) -> a.node.name) actions in
    Alcotest.(check bool) "a NOT in plan" false (List.mem ["partial"; "a"] names);
    Alcotest.(check bool) "b in plan" true (List.mem ["partial"; "b"] names);
    Alcotest.(check bool) "c NOT in plan" false (List.mem ["partial"; "c"] names);
    Alcotest.(check bool) "d in plan" true (List.mem ["partial"; "d"] names))

(* --- execute tests --- *)

let test_execute_creates_fasl () =
  with_temp_dir (fun dir ->
    let sld_path = write_sld dir ["exec"; "a"]
        "(define-library (exec a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 42)))" in
    let node = { Dep_graph.name = ["exec"; "a"]; sld_path;
                 imports = [["scheme"; "base"]] } in
    let action = { Build.node; reason = Build.No_fasl } in
    let _results = Build.execute ~search_paths:[dir] [action] in
    let fasl_path = Fasl.fasl_path_for sld_path in
    Alcotest.(check bool) "fasl exists" true (Sys.file_exists fasl_path))

let test_execute_timing () =
  with_temp_dir (fun dir ->
    let sld_path = write_sld dir ["exec"; "time"]
        "(define-library (exec time)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let node = { Dep_graph.name = ["exec"; "time"]; sld_path;
                 imports = [["scheme"; "base"]] } in
    let action = { Build.node; reason = Build.No_fasl } in
    let results = Build.execute ~search_paths:[dir] [action] in
    Alcotest.(check int) "one result" 1 (List.length results);
    let r = List.hd results in
    Alcotest.(check bool) "elapsed >= 0" true (r.elapsed >= 0.0))

let test_execute_incremental () =
  with_temp_dir (fun dir ->
    let sld_a = write_sld dir ["incr"; "a"]
        "(define-library (incr a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let sld_b = write_sld dir ["incr"; "b"]
        "(define-library (incr b)\n\
         (import (scheme base) (incr a))\n\
         (export y)\n\
         (begin (define y 2)))" in
    let node_a = { Dep_graph.name = ["incr"; "a"]; sld_path = sld_a;
                   imports = [["scheme"; "base"]] } in
    let node_b = { Dep_graph.name = ["incr"; "b"]; sld_path = sld_b;
                   imports = [["scheme"; "base"]; ["incr"; "a"]] } in
    let nodes = [node_a; node_b] in
    (* Build everything from scratch *)
    let actions = Build.plan nodes in
    Alcotest.(check int) "initial: 2 actions" 2 (List.length actions);
    let _results = Build.execute ~search_paths:[dir] actions in
    (* Now everything is fresh *)
    let actions2 = Build.plan nodes in
    Alcotest.(check int) "incremental: 0 actions" 0 (List.length actions2);
    (* Touch leaf source *)
    Unix.sleepf 0.05;
    set_mtime sld_a (Unix.gettimeofday ());
    let actions3 = Build.plan nodes in
    Alcotest.(check bool) "leaf touched: at least 1 action" true
      (List.length actions3 >= 1))

let test_execute_error () =
  with_temp_dir (fun dir ->
    let sld_path = write_sld dir ["exec"; "err"]
        "(define-library (exec err)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x (/ 1 \"bad\"))))" in
    let node = { Dep_graph.name = ["exec"; "err"]; sld_path;
                 imports = [["scheme"; "base"]] } in
    let action = { Build.node; reason = Build.No_fasl } in
    match Build.execute ~search_paths:[dir] [action] with
    | _ -> Alcotest.fail "expected Build_error"
    | exception Build.Build_error (name, _msg) ->
      Alcotest.check lib_name_testable "error lib name"
        ["exec"; "err"] name)

(* --- clean tests --- *)

let test_clean_removes () =
  with_temp_dir (fun dir ->
    let sld_path = write_sld dir ["clean"; "a"]
        "(define-library (clean a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let node = { Dep_graph.name = ["clean"; "a"]; sld_path;
                 imports = [["scheme"; "base"]] } in
    (* Build first *)
    let action = { Build.node; reason = Build.No_fasl } in
    let _results = Build.execute ~search_paths:[dir] [action] in
    let fasl_path = Fasl.fasl_path_for sld_path in
    Alcotest.(check bool) "fasl exists before clean" true (Sys.file_exists fasl_path);
    let _count = Build.clean [node] in
    Alcotest.(check bool) "fasl gone after clean" false (Sys.file_exists fasl_path))

let test_clean_count () =
  with_temp_dir (fun dir ->
    let sld_a = write_sld dir ["cnt"; "a"]
        "(define-library (cnt a)\n\
         (import (scheme base))\n\
         (export x)\n\
         (begin (define x 1)))" in
    let sld_b = write_sld dir ["cnt"; "b"]
        "(define-library (cnt b)\n\
         (import (scheme base))\n\
         (export y)\n\
         (begin (define y 2)))" in
    let node_a = { Dep_graph.name = ["cnt"; "a"]; sld_path = sld_a;
                   imports = [["scheme"; "base"]] } in
    let node_b = { Dep_graph.name = ["cnt"; "b"]; sld_path = sld_b;
                   imports = [["scheme"; "base"]] } in
    (* Build both *)
    let action_a = { Build.node = node_a; reason = Build.No_fasl } in
    let action_b = { Build.node = node_b; reason = Build.No_fasl } in
    let _results = Build.execute ~search_paths:[dir] [action_a; action_b] in
    let count = Build.clean [node_a; node_b] in
    Alcotest.(check int) "cleaned 2 files" 2 count)

(* --- builtin_library_names tests --- *)

let test_builtin_library_names () =
  let inst = Instance.create () in
  let names = Build.builtin_library_names inst in
  Alcotest.(check bool) "(scheme base) is builtin" true
    (List.mem ["scheme"; "base"] names);
  Alcotest.(check bool) "(srfi 1) is not builtin" false
    (List.mem ["srfi"; "1"] names)

(* --- collect_roots tests --- *)

let write_scm dir rel_path contents =
  let path = Filename.concat dir rel_path in
  let parent = Filename.dirname path in
  let rec mkdirs d =
    if not (Sys.file_exists d) then begin
      mkdirs (Filename.dirname d);
      Sys.mkdir d 0o755
    end
  in
  if parent <> dir then mkdirs parent;
  write_file path contents;
  path

let lib_name_list_testable =
  Alcotest.testable
    (fun fmt names ->
      Format.fprintf fmt "[%s]"
        (String.concat "; "
           (List.map (fun n -> "(" ^ String.concat " " n ^ ")") names)))
    (fun a b ->
      let sort = List.sort compare in
      sort a = sort b)

let test_collect_roots_libs_only () =
  with_temp_dir (fun dir ->
    (* Write an sld for the declared library *)
    ignore (write_sld dir ["my-pkg"; "core"]
      "(define-library (my-pkg core)\n\
       (import (scheme base))\n\
       (export x)\n\
       (begin (define x 1)))");
    let pkg_path = Filename.concat dir "package.scm" in
    write_file pkg_path
      {|(define-package
          (name my-pkg)
          (version "1.0.0")
          (description "X")
          (license "MIT")
          (libraries (my-pkg core)))|};
    let pkg = Package.parse Readtable.default pkg_path in
    let roots = Build.collect_roots ~readtable:Readtable.default ~pkg_dir:dir pkg in
    Alcotest.check lib_name_list_testable "libs only"
      [["my-pkg"; "core"]] roots)

let test_collect_roots_with_programs () =
  with_temp_dir (fun dir ->
    (* Library *)
    ignore (write_sld dir ["my-pkg"; "core"]
      "(define-library (my-pkg core)\n\
       (import (scheme base))\n\
       (export x)\n\
       (begin (define x 1)))");
    (* Program that imports the same library *)
    ignore (write_scm dir "main.scm"
      "(import (scheme base) (my-pkg core))\n\
       (display x)\n");
    let pkg_path = Filename.concat dir "package.scm" in
    write_file pkg_path
      {|(define-package
          (name my-pkg)
          (version "1.0.0")
          (description "X")
          (license "MIT")
          (libraries (my-pkg core))
          (programs main.scm))|};
    let pkg = Package.parse Readtable.default pkg_path in
    let roots = Build.collect_roots ~readtable:Readtable.default ~pkg_dir:dir pkg in
    (* (my-pkg core) and (scheme base) from program imports, deduplicated *)
    Alcotest.check lib_name_list_testable "with programs"
      [["my-pkg"; "core"]; ["scheme"; "base"]] roots)

let test_collect_roots_missing_program () =
  with_temp_dir (fun dir ->
    ignore (write_sld dir ["my-pkg"; "core"]
      "(define-library (my-pkg core)\n\
       (import (scheme base))\n\
       (export x)\n\
       (begin (define x 1)))");
    let pkg_path = Filename.concat dir "package.scm" in
    write_file pkg_path
      {|(define-package
          (name my-pkg)
          (version "1.0.0")
          (description "X")
          (license "MIT")
          (libraries (my-pkg core))
          (programs nonexistent.scm))|};
    let pkg = Package.parse Readtable.default pkg_path in
    (* Should silently skip missing program *)
    let roots = Build.collect_roots ~readtable:Readtable.default ~pkg_dir:dir pkg in
    Alcotest.check lib_name_list_testable "missing program skipped"
      [["my-pkg"; "core"]] roots)

(* --- auto_build tests --- *)

let test_auto_build_compiles () =
  with_temp_dir (fun dir ->
    ignore (write_sld dir ["ab"; "a"]
      "(define-library (ab a)\n\
       (import (scheme base))\n\
       (export x)\n\
       (begin (define x 42)))");
    let inst = Instance.create () in
    let builtins = Build.builtin_library_names inst in
    let result = Build.auto_build
      ~search_paths:[dir] ~builtins
      ~readtable:Readtable.default
      [["ab"; "a"]] in
    Alcotest.(check int) "one action" 1 (List.length result.actions_taken);
    let r = List.hd result.actions_taken in
    Alcotest.check lib_name_testable "compiled lib"
      ["ab"; "a"] r.name;
    let fasl_path = Fasl.fasl_path_for
      (Filename.concat (Filename.concat dir "ab") "a.sld") in
    Alcotest.(check bool) "fasl created" true (Sys.file_exists fasl_path))

let test_auto_build_noop_when_fresh () =
  with_temp_dir (fun dir ->
    ignore (write_sld dir ["ab"; "b"]
      "(define-library (ab b)\n\
       (import (scheme base))\n\
       (export x)\n\
       (begin (define x 1)))");
    let inst = Instance.create () in
    let builtins = Build.builtin_library_names inst in
    let _first = Build.auto_build
      ~search_paths:[dir] ~builtins
      ~readtable:Readtable.default
      [["ab"; "b"]] in
    let second = Build.auto_build
      ~search_paths:[dir] ~builtins
      ~readtable:Readtable.default
      [["ab"; "b"]] in
    Alcotest.(check int) "no actions" 0 (List.length second.actions_taken))

let test_auto_build_idempotent =
  QCheck2.Test.make ~count:5 ~name:"auto_build twice gives empty second result"
    (QCheck2.Gen.return ())
    (fun () ->
       let dir = Filename.temp_dir "wile_ab_prop" "" in
       Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ dir)))
         (fun () ->
            ignore (write_sld dir ["idm"; "a"]
              "(define-library (idm a)\n\
               (import (scheme base))\n\
               (export x)\n\
               (begin (define x 1)))");
            let inst = Instance.create () in
            let builtins = Build.builtin_library_names inst in
            let _first = Build.auto_build
              ~search_paths:[dir] ~builtins
              ~readtable:Readtable.default
              [["idm"; "a"]] in
            let second = Build.auto_build
              ~search_paths:[dir] ~builtins
              ~readtable:Readtable.default
              [["idm"; "a"]] in
            second.actions_taken = []))

(* --- Property tests --- *)

let test_execute_makes_fresh =
  QCheck2.Test.make ~count:5 ~name:"execute makes all nodes fresh"
    (QCheck2.Gen.return ())
    (fun () ->
       let dir = Filename.temp_dir "wile_build_prop" "" in
       Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ dir)))
         (fun () ->
            let sld_a = write_sld dir ["fresh"; "a"]
                "(define-library (fresh a)\n\
                 (import (scheme base))\n\
                 (export x)\n\
                 (begin (define x 1)))" in
            let sld_b = write_sld dir ["fresh"; "b"]
                "(define-library (fresh b)\n\
                 (import (scheme base) (fresh a))\n\
                 (export y)\n\
                 (begin (define y 2)))" in
            let node_a = { Dep_graph.name = ["fresh"; "a"]; sld_path = sld_a;
                           imports = [["scheme"; "base"]] } in
            let node_b = { Dep_graph.name = ["fresh"; "b"]; sld_path = sld_b;
                           imports = [["scheme"; "base"]; ["fresh"; "a"]] } in
            let nodes = [node_a; node_b] in
            let actions = Build.plan nodes in
            let _results = Build.execute ~search_paths:[dir] actions in
            List.for_all (fun node ->
              Build.is_stale nodes node = None
            ) nodes))

let test_plan_preserves_topo_order =
  QCheck2.Test.make ~count:5 ~name:"plan preserves topological order"
    (QCheck2.Gen.return ())
    (fun () ->
       let dir = Filename.temp_dir "wile_build_topo_prop" "" in
       Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ dir)))
         (fun () ->
            let sld_a = write_sld dir ["topo"; "a"]
                "(define-library (topo a)\n\
                 (import (scheme base))\n\
                 (export x)\n\
                 (begin (define x 1)))" in
            let sld_b = write_sld dir ["topo"; "b"]
                "(define-library (topo b)\n\
                 (import (scheme base) (topo a))\n\
                 (export y)\n\
                 (begin (define y 2)))" in
            let sld_c = write_sld dir ["topo"; "c"]
                "(define-library (topo c)\n\
                 (import (scheme base) (topo b))\n\
                 (export z)\n\
                 (begin (define z 3)))" in
            let node_a = { Dep_graph.name = ["topo"; "a"]; sld_path = sld_a;
                           imports = [["scheme"; "base"]] } in
            let node_b = { Dep_graph.name = ["topo"; "b"]; sld_path = sld_b;
                           imports = [["scheme"; "base"]; ["topo"; "a"]] } in
            let node_c = { Dep_graph.name = ["topo"; "c"]; sld_path = sld_c;
                           imports = [["scheme"; "base"]; ["topo"; "b"]] } in
            let nodes = [node_a; node_b; node_c] in
            let actions = Build.plan nodes in
            let plan_names = List.map (fun (a : Build.build_action) -> a.node.name) actions in
            let node_names = List.map (fun (n : Dep_graph.node) -> n.name) nodes in
            (* plan_names should be a subsequence of node_names *)
            let rec is_subsequence sub full = match sub, full with
              | [], _ -> true
              | _, [] -> false
              | s :: ss, f :: fs ->
                if s = f then is_subsequence ss fs
                else is_subsequence sub fs
            in
            is_subsequence plan_names node_names))

(* --- stdlib validation tests --- *)

let test_stdlib_graph () =
  let stdlib = Wile_config.stdlib_source_dir in
  let inst = Instance.create () in
  let builtins = Build.builtin_library_names inst in
  let search_paths = [stdlib] in
  let rt = Readtable.default in
  let roots = Instance.discover_available_libraries search_paths in
  (* 32 libraries: 31 SRFIs + (srfi 41 primitive) *)
  Alcotest.(check bool) "found 32+ roots" true (List.length roots >= 32);
  let nodes = Dep_graph.build_graph ~builtins ~search_paths rt roots in
  Alcotest.(check bool) "graph has 32+ nodes" true (List.length nodes >= 32);
  (* Sub-library (srfi 41 primitive) is included *)
  Alcotest.(check bool) "(srfi 41 primitive) in graph" true
    (List.exists (fun (n : Dep_graph.node) ->
       n.name = ["srfi"; "41"; "primitive"]) nodes);
  (* Topological order: every in-graph import appears before its dependent *)
  let pos = Hashtbl.create 64 in
  List.iteri (fun i (n : Dep_graph.node) ->
    Hashtbl.replace pos (String.concat "\x00" n.name) i) nodes;
  List.iter (fun (n : Dep_graph.node) ->
    let my_pos = Hashtbl.find pos (String.concat "\x00" n.name) in
    List.iter (fun imp ->
      let key = String.concat "\x00" imp in
      match Hashtbl.find_opt pos key with
      | Some dep_pos ->
        Alcotest.(check bool)
          (Printf.sprintf "%s before %s"
             (Library.name_to_string imp) (Library.name_to_string n.name))
          true (dep_pos < my_pos)
      | None -> ()  (* builtin, not in graph *)
    ) n.imports
  ) nodes

let test_stdlib_auto_build () =
  let stdlib = Wile_config.stdlib_source_dir in
  let inst = Instance.create () in
  let builtins = Build.builtin_library_names inst in
  let search_paths = [stdlib] in
  let rt = Readtable.default in
  let roots = Instance.discover_available_libraries search_paths in
  (* Should complete without exceptions *)
  let _result = Build.auto_build ~search_paths ~builtins ~readtable:rt roots in
  (* All libraries should now be fresh *)
  let nodes = Dep_graph.build_graph ~builtins ~search_paths rt roots in
  let stale = List.filter (fun node ->
    Build.is_stale nodes node <> None
  ) nodes in
  Alcotest.(check int) "all fresh after build" 0 (List.length stale)

(* --- Test runner --- *)

let () =
  Alcotest.run "Build" [
    "is_stale", [
      Alcotest.test_case "no fasl" `Quick test_no_fasl;
      Alcotest.test_case "fresh" `Quick test_fresh;
      Alcotest.test_case "source newer" `Quick test_source_newer;
      Alcotest.test_case "dep newer" `Quick test_dep_newer;
    ];
    "plan", [
      Alcotest.test_case "nothing" `Quick test_plan_nothing;
      Alcotest.test_case "leaf stale" `Quick test_plan_leaf_stale;
      Alcotest.test_case "propagates" `Quick test_plan_propagates;
      Alcotest.test_case "partial" `Quick test_plan_partial;
    ];
    "execute", [
      Alcotest.test_case "creates fasl" `Quick test_execute_creates_fasl;
      Alcotest.test_case "timing" `Quick test_execute_timing;
      Alcotest.test_case "incremental" `Quick test_execute_incremental;
      Alcotest.test_case "error" `Quick test_execute_error;
    ];
    "clean", [
      Alcotest.test_case "removes" `Quick test_clean_removes;
      Alcotest.test_case "count" `Quick test_clean_count;
    ];
    "builtin_library_names", [
      Alcotest.test_case "scheme base present" `Quick test_builtin_library_names;
    ];
    "collect_roots", [
      Alcotest.test_case "libs only" `Quick test_collect_roots_libs_only;
      Alcotest.test_case "with programs" `Quick test_collect_roots_with_programs;
      Alcotest.test_case "missing program" `Quick test_collect_roots_missing_program;
    ];
    "auto_build", [
      Alcotest.test_case "compiles" `Quick test_auto_build_compiles;
      Alcotest.test_case "noop when fresh" `Quick test_auto_build_noop_when_fresh;
    ];
    "properties", [
      QCheck_alcotest.to_alcotest test_execute_makes_fresh;
      QCheck_alcotest.to_alcotest test_plan_preserves_topo_order;
      QCheck_alcotest.to_alcotest test_auto_build_idempotent;
    ];
    "stdlib", [
      Alcotest.test_case "graph" `Quick test_stdlib_graph;
      Alcotest.test_case "auto_build" `Slow test_stdlib_auto_build;
    ];
  ]
