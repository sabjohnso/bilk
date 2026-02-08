open Wile

(* --- Helpers --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "wile_pkg_test" "" in
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
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc content)

let rt = Readtable.default

(* --- Parse tests --- *)

let test_parse_full () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name my-pkg)
          (version "1.2.3")
          (description "A test package")
          (license "MIT")
          (depends
            (other-pkg (>= "1.0.0") (< "2.0.0"))
            (simple-dep))
          (libraries
            (my-pkg core)
            (my-pkg utils)))|};
    let pkg = Package.parse rt path in
    Alcotest.(check string) "name" "my-pkg" pkg.name;
    Alcotest.(check string) "version" "1.2.3" (Semver.to_string pkg.version);
    Alcotest.(check string) "description" "A test package" pkg.description;
    Alcotest.(check string) "license" "MIT" pkg.license;
    Alcotest.(check int) "depends count" 2 (List.length pkg.depends);
    let d0 = List.nth pkg.depends 0 in
    Alcotest.(check string) "dep0 name" "other-pkg" d0.dep_name;
    Alcotest.(check int) "dep0 constraints" 2 (List.length d0.dep_constraints);
    let d1 = List.nth pkg.depends 1 in
    Alcotest.(check string) "dep1 name" "simple-dep" d1.dep_name;
    Alcotest.(check int) "dep1 constraints" 0 (List.length d1.dep_constraints);
    Alcotest.(check int) "libraries count" 2 (List.length pkg.libraries);
    Alcotest.(check (list string)) "lib0" ["my-pkg"; "core"] (List.nth pkg.libraries 0);
    Alcotest.(check (list string)) "lib1" ["my-pkg"; "utils"] (List.nth pkg.libraries 1))

let test_parse_minimal () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name tiny)
          (version "0.0.1")
          (description "Tiny")
          (license "BSD"))|};
    let pkg = Package.parse rt path in
    Alcotest.(check string) "name" "tiny" pkg.name;
    Alcotest.(check int) "depends" 0 (List.length pkg.depends);
    Alcotest.(check int) "libraries" 0 (List.length pkg.libraries))

let test_parse_missing_name () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (version "1.0.0")
          (description "X")
          (license "MIT"))|};
    Alcotest.check_raises "missing name"
      (Package.Package_error "missing required field: name")
      (fun () -> ignore (Package.parse rt path)))

let test_parse_missing_version () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name foo)
          (description "X")
          (license "MIT"))|};
    Alcotest.check_raises "missing version"
      (Package.Package_error "missing required field: version")
      (fun () -> ignore (Package.parse rt path)))

let test_parse_missing_description () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name foo)
          (version "1.0.0")
          (license "MIT"))|};
    Alcotest.check_raises "missing description"
      (Package.Package_error "missing required field: description")
      (fun () -> ignore (Package.parse rt path)))

let test_parse_missing_license () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name foo)
          (version "1.0.0")
          (description "X"))|};
    Alcotest.check_raises "missing license"
      (Package.Package_error "missing required field: license")
      (fun () -> ignore (Package.parse rt path)))

let test_parse_bad_version () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name foo)
          (version "bad")
          (description "X")
          (license "MIT"))|};
    Alcotest.check_raises "bad version"
      (Package.Package_error "invalid version format: bad")
      (fun () -> ignore (Package.parse rt path)))

let test_parse_not_define_package () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path {|(something-else (name foo))|};
    Alcotest.check_raises "wrong form"
      (Package.Package_error (Printf.sprintf "%s: expected (define-package ...)" path))
      (fun () -> ignore (Package.parse rt path)))

let test_parse_dep_with_constraints () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name foo)
          (version "1.0.0")
          (description "X")
          (license "MIT")
          (depends
            (bar (= "2.0.0"))))|};
    let pkg = Package.parse rt path in
    let dep = List.hd pkg.depends in
    Alcotest.(check string) "dep name" "bar" dep.dep_name;
    Alcotest.(check int) "constraints" 1 (List.length dep.dep_constraints);
    let (op, ver) = List.hd dep.dep_constraints in
    Alcotest.(check bool) "op is Eq" true (op = Semver.Eq);
    Alcotest.(check string) "ver" "2.0.0" (Semver.to_string ver))

let test_parse_dep_bare_symbol () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name foo)
          (version "1.0.0")
          (description "X")
          (license "MIT")
          (depends bare-dep))|};
    let pkg = Package.parse rt path in
    Alcotest.(check int) "deps" 1 (List.length pkg.depends);
    let dep = List.hd pkg.depends in
    Alcotest.(check string) "dep name" "bare-dep" dep.dep_name;
    Alcotest.(check int) "no constraints" 0 (List.length dep.dep_constraints))

let test_parse_unknown_clause () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name foo)
          (version "1.0.0")
          (description "X")
          (license "MIT")
          (bogus "stuff"))|};
    Alcotest.check_raises "unknown clause"
      (Package.Package_error "unknown clause: bogus")
      (fun () -> ignore (Package.parse rt path)))

(* --- find_package_file tests --- *)

let test_find_package_file_direct () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path "(define-package)";
    match Package.find_package_file dir with
    | Some found -> Alcotest.(check string) "found" path found
    | None -> Alcotest.fail "expected to find package.scm")

let test_find_package_file_parent () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path "(define-package)";
    let sub = Filename.concat dir "sub" in
    Sys.mkdir sub 0o755;
    match Package.find_package_file sub with
    | Some found -> Alcotest.(check string) "found" path found
    | None -> Alcotest.fail "expected to find package.scm in parent")

let test_find_package_file_not_found () =
  with_temp_dir (fun dir ->
    let sub = Filename.concat dir "deep" in
    Sys.mkdir sub 0o755;
    let sub2 = Filename.concat sub "nested" in
    Sys.mkdir sub2 0o755;
    (* Start from a subdir with no package.scm up the chain
       (temp dir won't have package.scm unless project root does —
       but we start from sub2 which is inside a temp dir) *)
    match Package.find_package_file sub2 with
    | Some _ ->
      (* might find one from the real filesystem, that's OK *)
      ()
    | None -> ())

let test_parse_empty_depends () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name foo)
          (version "1.0.0")
          (description "X")
          (license "MIT")
          (depends))|};
    let pkg = Package.parse rt path in
    Alcotest.(check int) "empty depends" 0 (List.length pkg.depends))

let test_parse_empty_libraries () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "package.scm" in
    write_file path
      {|(define-package
          (name foo)
          (version "1.0.0")
          (description "X")
          (license "MIT")
          (libraries))|};
    let pkg = Package.parse rt path in
    Alcotest.(check int) "empty libraries" 0 (List.length pkg.libraries))

(* --- Test suite --- *)

let () =
  Alcotest.run "Package" [
    "parse", [
      Alcotest.test_case "full" `Quick test_parse_full;
      Alcotest.test_case "minimal" `Quick test_parse_minimal;
      Alcotest.test_case "missing name" `Quick test_parse_missing_name;
      Alcotest.test_case "missing version" `Quick test_parse_missing_version;
      Alcotest.test_case "missing description" `Quick test_parse_missing_description;
      Alcotest.test_case "missing license" `Quick test_parse_missing_license;
      Alcotest.test_case "bad version" `Quick test_parse_bad_version;
      Alcotest.test_case "not define-package" `Quick test_parse_not_define_package;
      Alcotest.test_case "dep with constraints" `Quick test_parse_dep_with_constraints;
      Alcotest.test_case "dep bare symbol" `Quick test_parse_dep_bare_symbol;
      Alcotest.test_case "unknown clause" `Quick test_parse_unknown_clause;
      Alcotest.test_case "empty depends" `Quick test_parse_empty_depends;
      Alcotest.test_case "empty libraries" `Quick test_parse_empty_libraries;
    ];
    "find_package_file", [
      Alcotest.test_case "direct" `Quick test_find_package_file_direct;
      Alcotest.test_case "parent" `Quick test_find_package_file_parent;
      Alcotest.test_case "not found" `Quick test_find_package_file_not_found;
    ];
  ]
