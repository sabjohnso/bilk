open Bilk

(* --- Helpers --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "bilk_pkgmgr_test" "" in
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

let contains haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let rec check i =
      if i > hlen - nlen then false
      else if String.sub haystack i nlen = needle then true
      else check (i + 1)
    in
    check 0

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
    let raised = ref false in
    (try ignore (Pkg_manager.resolve ~registry_root:registry deps)
     with Pkg_manager.Pkg_error msg ->
       raised := true;
       Alcotest.(check bool) "mentions dep-c"
         true (contains msg "dep-c"));
    Alcotest.(check bool) "raised" true !raised)

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
    (try ignore (Pkg_manager.resolve ~registry_root:registry deps);
         Alcotest.fail "expected Pkg_error"
     with Pkg_manager.Pkg_error msg ->
       Alcotest.(check bool) "mentions old"
         true (contains msg "old");
       Alcotest.(check bool) "mentions available"
         true (contains msg "available")))

(* --- Conflict diagnostic tests --- *)

let test_conflict_names_both_requesters () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* dep-c v1 and v2 *)
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
    let deps : Package.dependency list =
      [{ dep_name = "left-c"; dep_constraints = [] };
       { dep_name = "right-c"; dep_constraints = [] }] in
    (try ignore (Pkg_manager.resolve ~registry_root:registry deps);
         Alcotest.fail "expected Pkg_error"
     with Pkg_manager.Pkg_error msg ->
       Alcotest.(check bool) "mentions left-c"
         true (contains msg "left-c");
       Alcotest.(check bool) "mentions right-c"
         true (contains msg "right-c")))

let test_conflict_lists_available_versions () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* dep-c v1 and v2 *)
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
    let deps : Package.dependency list =
      [{ dep_name = "left-c"; dep_constraints = [] };
       { dep_name = "right-c"; dep_constraints = [] }] in
    (try ignore (Pkg_manager.resolve ~registry_root:registry deps);
         Alcotest.fail "expected Pkg_error"
     with Pkg_manager.Pkg_error msg ->
       Alcotest.(check bool) "lists 1.0.0"
         true (contains msg "1.0.0");
       Alcotest.(check bool) "lists 2.0.0"
         true (contains msg "2.0.0")))

let test_deep_conflict_diagnostic () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* dep-z v1 only *)
    let src_z = Filename.concat dir "srcz" in
    Sys.mkdir src_z 0o755;
    make_package src_z "dep-z" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_z;
    (* mid depends on dep-z >= 2.0.0 (unsatisfiable) *)
    let src_m = Filename.concat dir "srcm" in
    Sys.mkdir src_m 0o755;
    make_package src_m "mid" "1.0.0"
      ~depends:{|(dep-z (>= "2.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_m;
    (* top depends on mid *)
    let src_t = Filename.concat dir "srct" in
    Sys.mkdir src_t 0o755;
    make_package src_t "top" "1.0.0" ~depends:"(mid)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_t;
    let deps : Package.dependency list =
      [{ dep_name = "top"; dep_constraints = [] }] in
    (try ignore (Pkg_manager.resolve ~registry_root:registry deps);
         Alcotest.fail "expected Pkg_error"
     with Pkg_manager.Pkg_error msg ->
       (* Error should mention mid as the requester *)
       Alcotest.(check bool) "mentions mid"
         true (contains msg "mid")))

(* --- Backtracking solver tests --- *)

let test_backtrack_resolves_conflict () =
  (* left-c 1.0.0 requires dep-c >= 2.0.0
     right-c 1.0.0 requires dep-c < 2.0.0
     right-c 2.0.0 requires dep-c >= 1.0.0  (relaxed)
     Solver should backtrack and pick right-c 2.0.0 *)
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* dep-c v1 and v2 *)
    let src = Filename.concat dir "src_c1" in
    Sys.mkdir src 0o755;
    make_package src "dep-c" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let src = Filename.concat dir "src_c2" in
    Sys.mkdir src 0o755;
    make_package src "dep-c" "2.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* left-c 1.0.0 requires dep-c >= 2.0.0 *)
    let src = Filename.concat dir "srcl" in
    Sys.mkdir src 0o755;
    make_package src "left-c" "1.0.0"
      ~depends:{|(dep-c (>= "2.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* right-c 1.0.0 requires dep-c < 2.0.0 (conflicts with left-c) *)
    let src = Filename.concat dir "srcr1" in
    Sys.mkdir src 0o755;
    make_package src "right-c" "1.0.0"
      ~depends:{|(dep-c (< "2.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* right-c 2.0.0 requires dep-c >= 1.0.0 (compatible with left-c) *)
    let src = Filename.concat dir "srcr2" in
    Sys.mkdir src 0o755;
    make_package src "right-c" "2.0.0"
      ~depends:{|(dep-c (>= "1.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "left-c"; dep_constraints = [] };
       { dep_name = "right-c"; dep_constraints = [] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    (* Should pick right-c 2.0.0 and dep-c 2.0.0 *)
    let right_ver = List.assoc "right-c" resolved in
    Alcotest.check semver_testable "right-c 2.0.0" (v 2 0 0) right_ver;
    let dep_ver = List.assoc "dep-c" resolved in
    Alcotest.check semver_testable "dep-c 2.0.0" (v 2 0 0) dep_ver)

let test_backtrack_multiple_levels () =
  (* A depends on B, B v2 depends on C v2 (not installed),
     B v1 depends on C v1 (installed) → solver backtracks to B v1 *)
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* C v1 only *)
    let src = Filename.concat dir "src_c" in
    Sys.mkdir src 0o755;
    make_package src "pkg-c" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* B v1 depends on C (any) *)
    let src = Filename.concat dir "src_b1" in
    Sys.mkdir src 0o755;
    make_package src "pkg-b" "1.0.0" ~depends:"(pkg-c)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* B v2 depends on C >= 2.0.0 (impossible) *)
    let src = Filename.concat dir "src_b2" in
    Sys.mkdir src 0o755;
    make_package src "pkg-b" "2.0.0"
      ~depends:{|(pkg-c (>= "2.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* A depends on B *)
    let src = Filename.concat dir "src_a" in
    Sys.mkdir src 0o755;
    make_package src "pkg-a" "1.0.0" ~depends:"(pkg-b)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "pkg-a"; dep_constraints = [] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    (* Should backtrack to B v1 *)
    let b_ver = List.assoc "pkg-b" resolved in
    Alcotest.check semver_testable "pkg-b 1.0.0" (v 1 0 0) b_ver;
    let c_ver = List.assoc "pkg-c" resolved in
    Alcotest.check semver_testable "pkg-c 1.0.0" (v 1 0 0) c_ver)

let test_backtrack_no_solution () =
  (* All version combinations lead to conflict *)
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* dep-x v1 only *)
    let src = Filename.concat dir "src_x" in
    Sys.mkdir src 0o755;
    make_package src "dep-x" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* alpha requires dep-x >= 2.0.0 (impossible) *)
    let src = Filename.concat dir "src_a" in
    Sys.mkdir src 0o755;
    make_package src "alpha" "1.0.0"
      ~depends:{|(dep-x (>= "2.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "alpha"; dep_constraints = [] }] in
    (try ignore (Pkg_manager.resolve ~registry_root:registry deps);
         Alcotest.fail "expected Pkg_error"
     with Pkg_manager.Pkg_error _ -> ()))

let test_backtrack_greedy_preferred () =
  (* When no conflict, latest version is still picked *)
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src1 = Filename.concat dir "src1" in
    Sys.mkdir src1 0o755;
    make_package src1 "greedy" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src1;
    let src2 = Filename.concat dir "src2" in
    Sys.mkdir src2 0o755;
    make_package src2 "greedy" "2.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src2;
    let src3 = Filename.concat dir "src3" in
    Sys.mkdir src3 0o755;
    make_package src3 "greedy" "3.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src3;
    let deps : Package.dependency list =
      [{ dep_name = "greedy"; dep_constraints = [] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    Alcotest.(check (list pair_testable)) "picks latest"
      [("greedy", v 3 0 0)] resolved)

let test_backtrack_depth_limit () =
  (* Create a combinatorial structure that forces > 1000 backtracks.
     12 packages each with 3 versions, each depending on the next,
     gives 3^12 = 531441 combinations. All paths fail because the
     leaf depends on an impossible constraint, ensuring we hit the limit. *)
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let n_packages = 12 in
    let n_versions = 3 in
    for i = 1 to n_packages do
      let name = Printf.sprintf "dl-%d" i in
      for j = 1 to n_versions do
        let src = Filename.concat dir
            (Printf.sprintf "src_%d_%d" i j) in
        Sys.mkdir src 0o755;
        let dep =
          if i < n_packages then
            Printf.sprintf "(dl-%d)" (i + 1)
          else
            {|(dl-leaf (>= "99.0.0"))|}
        in
        make_package src name (Printf.sprintf "%d.0.0" j)
          ~depends:dep ();
        Pkg_manager.install ~registry_root:registry ~src_dir:src;
      done
    done;
    let src = Filename.concat dir "src_leaf" in
    Sys.mkdir src 0o755;
    make_package src "dl-leaf" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "dl-1"; dep_constraints = [] }] in
    (try ignore (Pkg_manager.resolve ~registry_root:registry deps);
         Alcotest.fail "expected Pkg_error"
     with Pkg_manager.Pkg_error msg ->
       Alcotest.(check bool) "mentions backtrack limit"
         true (contains msg "backtrack")))

let test_backtrack_diamond () =
  (* Diamond dependency where one path constrains, solver backtracks on the other.
     top depends on left and right.
     left v2 depends on shared >= 2.0.0
     left v1 depends on shared (any)
     right depends on shared < 2.0.0
     shared v1 and v2 installed.
     Solver should backtrack left to v1, pick shared v1. *)
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* shared v1 and v2 *)
    let src = Filename.concat dir "src_s1" in
    Sys.mkdir src 0o755;
    make_package src "shared" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let src = Filename.concat dir "src_s2" in
    Sys.mkdir src 0o755;
    make_package src "shared" "2.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* left v1: depends on shared (any) *)
    let src = Filename.concat dir "src_l1" in
    Sys.mkdir src 0o755;
    make_package src "left" "1.0.0" ~depends:"(shared)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* left v2: depends on shared >= 2.0.0 *)
    let src = Filename.concat dir "src_l2" in
    Sys.mkdir src 0o755;
    make_package src "left" "2.0.0"
      ~depends:{|(shared (>= "2.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    (* right v1: depends on shared < 2.0.0 *)
    let src = Filename.concat dir "src_r" in
    Sys.mkdir src 0o755;
    make_package src "right" "1.0.0"
      ~depends:{|(shared (< "2.0.0"))|} ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "left"; dep_constraints = [] };
       { dep_name = "right"; dep_constraints = [] }] in
    let resolved = Pkg_manager.resolve ~registry_root:registry deps in
    (* Should backtrack left to v1, pick shared v1 *)
    let left_ver = List.assoc "left" resolved in
    Alcotest.check semver_testable "left 1.0.0" (v 1 0 0) left_ver;
    let shared_ver = List.assoc "shared" resolved in
    Alcotest.check semver_testable "shared 1.0.0" (v 1 0 0) shared_ver)

(* --- Why tests --- *)

let test_why_direct_dependency () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "dep-a" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "dep-a"; dep_constraints = [] }] in
    let reasons = Pkg_manager.why ~registry_root:registry deps "dep-a" in
    Alcotest.(check int) "one reason" 1 (List.length reasons);
    Alcotest.(check bool) "mentions direct"
      true (contains (List.hd reasons) "direct dependency"))

let test_why_transitive_dependency () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* dep-b has no deps *)
    let src_b = Filename.concat dir "srcb" in
    Sys.mkdir src_b 0o755;
    make_package src_b "dep-b" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_b;
    (* dep-a depends on dep-b *)
    let src_a = Filename.concat dir "srca" in
    Sys.mkdir src_a 0o755;
    make_package src_a "dep-a" "1.0.0" ~depends:"(dep-b)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_a;
    let deps : Package.dependency list =
      [{ dep_name = "dep-a"; dep_constraints = [] }] in
    let reasons = Pkg_manager.why ~registry_root:registry deps "dep-b" in
    Alcotest.(check int) "one reason" 1 (List.length reasons);
    Alcotest.(check bool) "mentions dep-a"
      true (contains (List.hd reasons) "dep-a"))

let test_why_diamond_paths () =
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
    let reasons = Pkg_manager.why ~registry_root:registry deps "shared" in
    Alcotest.(check int) "two reasons" 2 (List.length reasons);
    let combined = String.concat " " reasons in
    Alcotest.(check bool) "mentions left"
      true (contains combined "left");
    Alcotest.(check bool) "mentions right"
      true (contains combined "right"))

let test_why_not_in_tree () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "dep-a" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "dep-a"; dep_constraints = [] }] in
    let reasons = Pkg_manager.why ~registry_root:registry deps "unknown" in
    Alcotest.(check (list string)) "empty" [] reasons)

(* --- Local registry root tests --- *)

let test_local_registry_root () =
  let result = Pkg_manager.local_registry_root "/foo/bar" in
  Alcotest.(check string) "path" "/foo/bar/_packages" result

let test_effective_in_project_with_local () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "package.scm")
      {|(define-package (name test) (version "1.0.0")
        (description "") (license "MIT"))|};
    let pkg_dir = Filename.concat dir "_packages" in
    Sys.mkdir pkg_dir 0o755;
    let result = Pkg_manager.effective_registry_root dir in
    Alcotest.(check string) "local" pkg_dir result)

let test_effective_in_project_without_local () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "package.scm")
      {|(define-package (name test) (version "1.0.0")
        (description "") (license "MIT"))|};
    let result = Pkg_manager.effective_registry_root dir in
    Alcotest.(check string) "global"
      (Pkg_manager.default_registry_root ()) result)

let test_effective_no_project () =
  with_temp_dir (fun dir ->
    let result = Pkg_manager.effective_registry_root dir in
    Alcotest.(check string) "global"
      (Pkg_manager.default_registry_root ()) result)

(* --- Init project tests --- *)

let test_init_creates_package_scm () =
  with_temp_dir (fun dir ->
    Pkg_manager.init_project ~dir ~name:"hello";
    let pkg_file = Filename.concat dir "package.scm" in
    Alcotest.(check bool) "exists" true (Sys.file_exists pkg_file);
    let content =
      let ic = open_in pkg_file in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
        let n = in_channel_length ic in
        really_input_string ic n)
    in
    Alcotest.(check bool) "has name" true (contains content "(name hello)");
    Alcotest.(check bool) "has version" true (contains content {|(version "0.1.0")|}))

let test_init_creates_packages_dir () =
  with_temp_dir (fun dir ->
    Pkg_manager.init_project ~dir ~name:"hello";
    let pkg_dir = Filename.concat dir "_packages" in
    Alcotest.(check bool) "exists" true (Sys.file_exists pkg_dir);
    Alcotest.(check bool) "is dir" true (Sys.is_directory pkg_dir))

let test_init_fails_if_package_exists () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "package.scm") "(define-package)";
    let raised = ref false in
    (try Pkg_manager.init_project ~dir ~name:"hello"
     with Pkg_manager.Pkg_error msg ->
       raised := true;
       Alcotest.(check bool) "mentions exists"
         true (contains msg "already exists"));
    Alcotest.(check bool) "raised" true !raised)

let test_init_gitignore () =
  with_temp_dir (fun dir ->
    Sys.mkdir (Filename.concat dir ".git") 0o755;
    Pkg_manager.init_project ~dir ~name:"hello";
    let gitignore = Filename.concat dir ".gitignore" in
    Alcotest.(check bool) "gitignore exists" true (Sys.file_exists gitignore);
    let content =
      let ic = open_in gitignore in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
        let n = in_channel_length ic in
        really_input_string ic n)
    in
    Alcotest.(check bool) "has _packages/" true (contains content "_packages/"))

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
    "conflict diagnostics", [
      Alcotest.test_case "names both requesters" `Quick test_conflict_names_both_requesters;
      Alcotest.test_case "lists available versions" `Quick test_conflict_lists_available_versions;
      Alcotest.test_case "deep conflict" `Quick test_deep_conflict_diagnostic;
    ];
    "backtracking", [
      Alcotest.test_case "resolves conflict" `Quick test_backtrack_resolves_conflict;
      Alcotest.test_case "multiple levels" `Quick test_backtrack_multiple_levels;
      Alcotest.test_case "no solution" `Quick test_backtrack_no_solution;
      Alcotest.test_case "greedy preferred" `Quick test_backtrack_greedy_preferred;
      Alcotest.test_case "depth limit" `Quick test_backtrack_depth_limit;
      Alcotest.test_case "diamond" `Quick test_backtrack_diamond;
    ];
    "why", [
      Alcotest.test_case "direct dependency" `Quick test_why_direct_dependency;
      Alcotest.test_case "transitive dependency" `Quick test_why_transitive_dependency;
      Alcotest.test_case "diamond paths" `Quick test_why_diamond_paths;
      Alcotest.test_case "not in tree" `Quick test_why_not_in_tree;
    ];
    "search_paths", [
      Alcotest.test_case "basic" `Quick test_search_paths_basic;
      Alcotest.test_case "empty" `Quick test_search_paths_empty;
    ];
    "local registry", [
      Alcotest.test_case "local_registry_root" `Quick test_local_registry_root;
      Alcotest.test_case "effective with local" `Quick test_effective_in_project_with_local;
      Alcotest.test_case "effective without local" `Quick test_effective_in_project_without_local;
      Alcotest.test_case "effective no project" `Quick test_effective_no_project;
    ];
    "init", [
      Alcotest.test_case "creates package.scm" `Quick test_init_creates_package_scm;
      Alcotest.test_case "creates _packages dir" `Quick test_init_creates_packages_dir;
      Alcotest.test_case "fails if package exists" `Quick test_init_fails_if_package_exists;
      Alcotest.test_case "gitignore" `Quick test_init_gitignore;
    ];
  ]
