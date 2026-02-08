open Wile

(* --- Helpers --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "wile_pkgmgr_test" "" in
  Fun.protect ~finally:(fun () ->
    let rec rm path =
      if Sys.is_directory path then begin
        Array.iter (fun f -> rm (Filename.concat path f)) (Sys.readdir path);
        Sys.rmdir path
      end else
        Sys.remove path
    in
    (try rm dir with _ -> ()))
    (fun () -> fn dir)

let write_file path content =
  let parent = Filename.dirname path in
  let rec mkdir_p d =
    if not (Sys.file_exists d) then begin
      mkdir_p (Filename.dirname d);
      Sys.mkdir d 0o755
    end
  in
  mkdir_p parent;
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc content)

let make_package dir name version ?(description="A package") ?(license="MIT")
    ?(depends="") ?(libraries="") () =
  let pkg_file = Filename.concat dir "package.scm" in
  let content = Printf.sprintf
    {|(define-package
        (name %s)
        (version "%s")
        (description "%s")
        (license "%s")
        (depends %s)
        (libraries %s))|}
    name version description license depends libraries
  in
  write_file pkg_file content

let v major minor patch : Semver.t = { major; minor; patch }

let semver_testable : Semver.t Alcotest.testable =
  Alcotest.testable
    (fun fmt v -> Format.pp_print_string fmt (Semver.to_string v))
    Semver.equal

let pair_testable : (string * Semver.t) Alcotest.testable =
  Alcotest.pair Alcotest.string semver_testable

(* --- Install tests --- *)

let test_install_basic () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "my-pkg" "1.0.0" ();
    let src_sub = Filename.concat src "src" in
    Sys.mkdir src_sub 0o755;
    let my_pkg_sub = Filename.concat src_sub "my-pkg" in
    Sys.mkdir my_pkg_sub 0o755;
    write_file (Filename.concat my_pkg_sub "hello.sld")
      "(define-library (my-pkg hello) (export) (import (scheme base)) (begin))";
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let pkg_scm = Filename.concat
      (Filename.concat (Filename.concat registry "my-pkg") "1.0.0")
      "package.scm" in
    Alcotest.(check bool) "package.scm exists" true (Sys.file_exists pkg_scm);
    let sld = Filename.concat
      (Filename.concat
        (Filename.concat
          (Filename.concat
            (Filename.concat registry "my-pkg") "1.0.0") "src") "my-pkg")
      "hello.sld" in
    Alcotest.(check bool) "sld exists" true (Sys.file_exists sld))

let test_install_duplicate () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "dup" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    Alcotest.check_raises "duplicate install"
      (Pkg_manager.Pkg_error "package dup 1.0.0 is already installed")
      (fun () -> Pkg_manager.install ~registry_root:registry ~src_dir:src))

let test_install_no_package_scm () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    Alcotest.check_raises "no package.scm"
      (Pkg_manager.Pkg_error (Printf.sprintf "no package.scm found in %s" src))
      (fun () -> Pkg_manager.install ~registry_root:registry ~src_dir:src))

let test_install_multiple_versions () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src1 = Filename.concat dir "src1" in
    Sys.mkdir src1 0o755;
    make_package src1 "multi" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src1;
    let src2 = Filename.concat dir "src2" in
    Sys.mkdir src2 0o755;
    make_package src2 "multi" "2.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src2;
    let pkgs = Pkg_manager.list_packages ~registry_root:registry in
    Alcotest.(check int) "one package" 1 (List.length pkgs);
    let (name, versions) = List.hd pkgs in
    Alcotest.(check string) "name" "multi" name;
    Alcotest.(check int) "two versions" 2 (List.length versions))

(* --- Remove tests --- *)

let test_remove_basic () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "rm-pkg" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    Pkg_manager.remove ~registry_root:registry ~name:"rm-pkg" ~version:"1.0.0";
    let pkgs = Pkg_manager.list_packages ~registry_root:registry in
    Alcotest.(check int) "empty" 0 (List.length pkgs))

let test_remove_not_installed () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    Alcotest.check_raises "not installed"
      (Pkg_manager.Pkg_error "package ghost 1.0.0 is not installed")
      (fun () -> Pkg_manager.remove ~registry_root:registry ~name:"ghost" ~version:"1.0.0"))

let test_remove_keeps_other_versions () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src1 = Filename.concat dir "src1" in
    Sys.mkdir src1 0o755;
    make_package src1 "kept" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src1;
    let src2 = Filename.concat dir "src2" in
    Sys.mkdir src2 0o755;
    make_package src2 "kept" "2.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src2;
    Pkg_manager.remove ~registry_root:registry ~name:"kept" ~version:"1.0.0";
    let pkgs = Pkg_manager.list_packages ~registry_root:registry in
    Alcotest.(check int) "one package" 1 (List.length pkgs);
    let (_, versions) = List.hd pkgs in
    Alcotest.(check int) "one version" 1 (List.length versions);
    Alcotest.check semver_testable "remaining" (v 2 0 0) (List.hd versions))

(* --- List tests --- *)

let test_list_empty () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let pkgs = Pkg_manager.list_packages ~registry_root:registry in
    Alcotest.(check int) "empty" 0 (List.length pkgs))

let test_list_sorted () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src_b = Filename.concat dir "srcb" in
    Sys.mkdir src_b 0o755;
    make_package src_b "beta" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_b;
    let src_a = Filename.concat dir "srca" in
    Sys.mkdir src_a 0o755;
    make_package src_a "alpha" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_a;
    let pkgs = Pkg_manager.list_packages ~registry_root:registry in
    Alcotest.(check int) "two packages" 2 (List.length pkgs);
    Alcotest.(check string) "first" "alpha" (fst (List.nth pkgs 0));
    Alcotest.(check string) "second" "beta" (fst (List.nth pkgs 1)))

(* --- Package info tests --- *)

let test_info_basic () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "info-pkg" "1.0.0" ~description:"Info test" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let pkg = Pkg_manager.package_info ~registry_root:registry
        ~name:"info-pkg" ~version:"1.0.0" in
    Alcotest.(check string) "name" "info-pkg" pkg.name;
    Alcotest.(check string) "desc" "Info test" pkg.description)

let test_info_not_installed () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    Alcotest.check_raises "not installed"
      (Pkg_manager.Pkg_error "package ghost 1.0.0 is not installed")
      (fun () -> ignore (Pkg_manager.package_info ~registry_root:registry
                           ~name:"ghost" ~version:"1.0.0")))

(* --- Resolve tests --- *)

let test_resolve_no_deps () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let resolved = Pkg_manager.resolve ~registry_root:registry [] in
    Alcotest.(check int) "empty" 0 (List.length resolved))

let test_resolve_simple () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "dep-a" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "dep-a"; dep_constraints = [] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    Alcotest.(check (list pair_testable)) "resolved"
      [("dep-a", v 1 0 0)] resolved)

let test_resolve_picks_latest () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src1 = Filename.concat dir "src1" in
    Sys.mkdir src1 0o755;
    make_package src1 "latest" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src1;
    let src2 = Filename.concat dir "src2" in
    Sys.mkdir src2 0o755;
    make_package src2 "latest" "2.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src2;
    let deps : Package.dependency list =
      [{ dep_name = "latest"; dep_constraints = [] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    Alcotest.(check (list pair_testable)) "resolved"
      [("latest", v 2 0 0)] resolved)

let test_resolve_with_constraints () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src1 = Filename.concat dir "src1" in
    Sys.mkdir src1 0o755;
    make_package src1 "constrained" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src1;
    let src2 = Filename.concat dir "src2" in
    Sys.mkdir src2 0o755;
    make_package src2 "constrained" "2.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src2;
    let deps : Package.dependency list =
      [{ dep_name = "constrained";
         dep_constraints = [(Semver.Lt, v 2 0 0)] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    Alcotest.(check (list pair_testable)) "resolved"
      [("constrained", v 1 0 0)] resolved)

let test_resolve_transitive () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* Install dep-b first (no deps) *)
    let src_b = Filename.concat dir "srcb" in
    Sys.mkdir src_b 0o755;
    make_package src_b "dep-b" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_b;
    (* Install dep-a which depends on dep-b *)
    let src_a = Filename.concat dir "srca" in
    Sys.mkdir src_a 0o755;
    make_package src_a "dep-a" "1.0.0" ~depends:"(dep-b)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_a;
    let deps : Package.dependency list =
      [{ dep_name = "dep-a"; dep_constraints = [] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    Alcotest.(check int) "two deps" 2 (List.length resolved);
    Alcotest.(check (list pair_testable)) "resolved"
      [("dep-a", v 1 0 0); ("dep-b", v 1 0 0)] resolved)

let test_resolve_diamond () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* shared: no deps *)
    let src_s = Filename.concat dir "srcs" in
    Sys.mkdir src_s 0o755;
    make_package src_s "shared" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_s;
    (* left depends on shared *)
    let src_l = Filename.concat dir "srcl" in
    Sys.mkdir src_l 0o755;
    make_package src_l "left" "1.0.0" ~depends:"(shared)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_l;
    (* right depends on shared *)
    let src_r = Filename.concat dir "srcr" in
    Sys.mkdir src_r 0o755;
    make_package src_r "right" "1.0.0" ~depends:"(shared)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_r;
    let deps : Package.dependency list =
      [{ dep_name = "left"; dep_constraints = [] };
       { dep_name = "right"; dep_constraints = [] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    Alcotest.(check int) "three deps" 3 (List.length resolved))

let test_resolve_conflict () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* Install dep-c v1 and v2 *)
    let src1 = Filename.concat dir "src1" in
    Sys.mkdir src1 0o755;
    make_package src1 "dep-c" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src1;
    let src2 = Filename.concat dir "src2" in
    Sys.mkdir src2 0o755;
    make_package src2 "dep-c" "2.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src2;
    (* left requires dep-c >= 2.0.0 *)
    let src_l = Filename.concat dir "srcl" in
    Sys.mkdir src_l 0o755;
    make_package src_l "left-c" "1.0.0"
      ~depends:{|(dep-c (>= "2.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_l;
    (* right requires dep-c < 2.0.0 *)
    let src_r = Filename.concat dir "srcr" in
    Sys.mkdir src_r 0o755;
    make_package src_r "right-c" "1.0.0"
      ~depends:{|(dep-c (< "2.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_r;
    (* Resolve both: conflict *)
    let deps : Package.dependency list =
      [{ dep_name = "left-c"; dep_constraints = [] };
       { dep_name = "right-c"; dep_constraints = [] }] in
    Alcotest.check_raises "conflict"
      (Pkg_manager.Pkg_error
         "version conflict for dep-c: 2.0.0 does not satisfy new constraints")
      (fun () -> ignore (Pkg_manager.resolve ~registry_root:registry deps)))

let test_resolve_circular () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* circ-a depends on circ-b *)
    let src_a = Filename.concat dir "srca" in
    Sys.mkdir src_a 0o755;
    make_package src_a "circ-a" "1.0.0" ~depends:"(circ-b)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_a;
    (* circ-b depends on circ-a *)
    let src_b = Filename.concat dir "srcb" in
    Sys.mkdir src_b 0o755;
    make_package src_b "circ-b" "1.0.0" ~depends:"(circ-a)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_b;
    let deps : Package.dependency list =
      [{ dep_name = "circ-a"; dep_constraints = [] }] in
    Alcotest.check_raises "circular"
      (Pkg_manager.Pkg_error "circular dependency: circ-a")
      (fun () -> ignore (Pkg_manager.resolve ~registry_root:registry deps)))

let test_resolve_missing_package () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let deps : Package.dependency list =
      [{ dep_name = "nonexistent"; dep_constraints = [] }] in
    Alcotest.check_raises "missing"
      (Pkg_manager.Pkg_error "package not found: nonexistent")
      (fun () -> ignore (Pkg_manager.resolve ~registry_root:registry deps)))

let test_resolve_no_satisfying_version () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "old" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "old";
         dep_constraints = [(Semver.Ge, v 5 0 0)] }] in
    Alcotest.check_raises "no satisfying"
      (Pkg_manager.Pkg_error "no installed version of old satisfies constraints")
      (fun () -> ignore (Pkg_manager.resolve ~registry_root:registry deps)))

(* --- Search paths tests --- *)

let test_search_paths_basic () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let resolved = [("my-pkg", v 1 0 0); ("other", v 2 0 0)] in
    let paths = Pkg_manager.search_paths_for ~registry_root:registry resolved in
    Alcotest.(check int) "two paths" 2 (List.length paths);
    let expected0 = Filename.concat
      (Filename.concat (Filename.concat registry "my-pkg") "1.0.0") "src" in
    let expected1 = Filename.concat
      (Filename.concat (Filename.concat registry "other") "2.0.0") "src" in
    Alcotest.(check string) "path0" expected0 (List.nth paths 0);
    Alcotest.(check string) "path1" expected1 (List.nth paths 1))

let test_search_paths_empty () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let paths = Pkg_manager.search_paths_for ~registry_root:registry [] in
    Alcotest.(check int) "empty" 0 (List.length paths))

(* --- Install without src dir --- *)

let test_install_no_src_dir () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "no-src" "1.0.0" ();
    (* Don't create src/ subdirectory *)
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let pkg_scm = Filename.concat
      (Filename.concat (Filename.concat registry "no-src") "1.0.0")
      "package.scm" in
    Alcotest.(check bool) "package.scm exists" true (Sys.file_exists pkg_scm))

(* --- Resolve deduplication --- *)

let test_resolve_dedup () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "common" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* Request same dep twice *)
    let deps : Package.dependency list =
      [{ dep_name = "common"; dep_constraints = [] };
       { dep_name = "common"; dep_constraints = [] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    Alcotest.(check int) "deduplicated" 1 (List.length resolved))

(* --- Test suite --- *)

let () =
  Alcotest.run "Pkg_manager" [
    "install", [
      Alcotest.test_case "basic" `Quick test_install_basic;
      Alcotest.test_case "duplicate" `Quick test_install_duplicate;
      Alcotest.test_case "no package.scm" `Quick test_install_no_package_scm;
      Alcotest.test_case "multiple versions" `Quick test_install_multiple_versions;
      Alcotest.test_case "no src dir" `Quick test_install_no_src_dir;
    ];
    "remove", [
      Alcotest.test_case "basic" `Quick test_remove_basic;
      Alcotest.test_case "not installed" `Quick test_remove_not_installed;
      Alcotest.test_case "keeps other versions" `Quick test_remove_keeps_other_versions;
    ];
    "list", [
      Alcotest.test_case "empty" `Quick test_list_empty;
      Alcotest.test_case "sorted" `Quick test_list_sorted;
    ];
    "info", [
      Alcotest.test_case "basic" `Quick test_info_basic;
      Alcotest.test_case "not installed" `Quick test_info_not_installed;
    ];
    "resolve", [
      Alcotest.test_case "no deps" `Quick test_resolve_no_deps;
      Alcotest.test_case "simple" `Quick test_resolve_simple;
      Alcotest.test_case "picks latest" `Quick test_resolve_picks_latest;
      Alcotest.test_case "with constraints" `Quick test_resolve_with_constraints;
      Alcotest.test_case "transitive" `Quick test_resolve_transitive;
      Alcotest.test_case "diamond" `Quick test_resolve_diamond;
      Alcotest.test_case "conflict" `Quick test_resolve_conflict;
      Alcotest.test_case "circular" `Quick test_resolve_circular;
      Alcotest.test_case "missing package" `Quick test_resolve_missing_package;
      Alcotest.test_case "no satisfying version" `Quick test_resolve_no_satisfying_version;
      Alcotest.test_case "dedup" `Quick test_resolve_dedup;
    ];
    "search_paths", [
      Alcotest.test_case "basic" `Quick test_search_paths_basic;
      Alcotest.test_case "empty" `Quick test_search_paths_empty;
    ];
  ]
