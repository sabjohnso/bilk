open Wile

(* --- Helpers --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "wile_lockfile_test" "" in
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

let make_package dir name version ?(depends="") () =
  let pkg_file = Filename.concat dir "package.scm" in
  let content = Printf.sprintf
    {|(define-package
        (name %s)
        (version "%s")
        (description "test")
        (license "MIT")
        (depends %s)
        (libraries))|}
    name version depends
  in
  write_file pkg_file content

let v major minor patch : Semver.t = { major; minor; patch }

let locked_pkg_testable : Lockfile.locked_package Alcotest.testable =
  Alcotest.testable
    (fun fmt lp ->
       Format.fprintf fmt "{name=%s; version=%s; sha256=%s}"
         lp.Lockfile.name (Semver.to_string lp.version) lp.sha256)
    (fun a b ->
       a.Lockfile.name = b.Lockfile.name
       && Semver.equal a.version b.version
       && a.sha256 = b.sha256)

let mismatch_testable : Lockfile.mismatch Alcotest.testable =
  Alcotest.testable
    (fun fmt m ->
       match m with
       | Lockfile.Hash_mismatch { name; expected; actual } ->
         Format.fprintf fmt "Hash_mismatch{%s: %s vs %s}" name expected actual
       | Lockfile.Not_installed { name; version } ->
         Format.fprintf fmt "Not_installed{%s %s}" name version)
    (fun a b ->
       match a, b with
       | Lockfile.Hash_mismatch a, Lockfile.Hash_mismatch b ->
         a.name = b.name && a.expected = b.expected && a.actual = b.actual
       | Lockfile.Not_installed a, Lockfile.Not_installed b ->
         a.name = b.name && a.version = b.version
       | _ -> false)

(* --- Step 1: lockfile_path, parse, write --- *)

let test_lockfile_path () =
  let p = Lockfile.lockfile_path "/some/project" in
  Alcotest.(check string) "path" "/some/project/wile.lock" p

let test_write_parse_roundtrip_one () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "wile.lock" in
    let lock : Lockfile.t = {
      created = "2026-01-15T10:30:00Z";
      packages = [
        { name = "json"; version = v 0 3 1; sha256 = "abc123" };
      ];
    } in
    Lockfile.write path lock;
    let parsed = Lockfile.parse Readtable.default path in
    Alcotest.(check string) "created" lock.created parsed.created;
    Alcotest.(check (list locked_pkg_testable)) "packages"
      lock.packages parsed.packages)

let test_write_parse_roundtrip_multi () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "wile.lock" in
    let lock : Lockfile.t = {
      created = "2026-02-12T12:00:00Z";
      packages = [
        { name = "json"; version = v 0 3 1; sha256 = "abc123" };
        { name = "srfi-extra"; version = v 1 1 0; sha256 = "def456" };
      ];
    } in
    Lockfile.write path lock;
    let parsed = Lockfile.parse Readtable.default path in
    Alcotest.(check string) "created" lock.created parsed.created;
    Alcotest.(check (list locked_pkg_testable)) "packages"
      lock.packages parsed.packages)

let test_parse_empty_packages () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "wile.lock" in
    let lock : Lockfile.t = {
      created = "2026-01-01T00:00:00Z";
      packages = [];
    } in
    Lockfile.write path lock;
    let parsed = Lockfile.parse Readtable.default path in
    Alcotest.(check string) "created" "2026-01-01T00:00:00Z" parsed.created;
    Alcotest.(check (list locked_pkg_testable)) "empty" [] parsed.packages)

let test_parse_malformed () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "wile.lock" in
    write_file path "(not-a-lockfile)";
    Alcotest.check_raises "malformed"
      (Lockfile.Lockfile_error "expected (lock ...)")
      (fun () -> ignore (Lockfile.parse Readtable.default path)))

let test_parse_missing_created () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "wile.lock" in
    write_file path "(lock (packages))";
    Alcotest.check_raises "missing created"
      (Lockfile.Lockfile_error "missing required field: created")
      (fun () -> ignore (Lockfile.parse Readtable.default path)))

(* --- Step 2: hash_directory --- *)

let test_hash_nonempty () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "a.txt") "hello world";
    let hash = Lockfile.hash_directory dir in
    Alcotest.(check bool) "non-empty" true (String.length hash > 0);
    (* SHA-256 hex digest is 64 characters *)
    Alcotest.(check int) "hex length" 64 (String.length hash))

let test_hash_deterministic () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "a.txt") "content A";
    write_file (Filename.concat dir "b.txt") "content B";
    let h1 = Lockfile.hash_directory dir in
    let h2 = Lockfile.hash_directory dir in
    Alcotest.(check string) "same hash" h1 h2)

let test_hash_changes_on_modification () =
  with_temp_dir (fun dir ->
    let file = Filename.concat dir "data.txt" in
    write_file file "original";
    let h1 = Lockfile.hash_directory dir in
    write_file file "modified";
    let h2 = Lockfile.hash_directory dir in
    Alcotest.(check bool) "different hash" true (h1 <> h2))

(* --- Step 3: create --- *)

let test_create_simple () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "json" "0.3.1" ();
    Sys.mkdir (Filename.concat src "src") 0o755;
    write_file (Filename.concat (Filename.concat src "src") "json.sld")
      "(define-library (json) (export) (import (scheme base)) (begin))";
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "json"; dep_constraints = [] }] in
    let lock = Lockfile.create ~registry_root:registry deps in
    Alcotest.(check int) "one package" 1 (List.length lock.packages);
    let lp = List.hd lock.packages in
    Alcotest.(check string) "name" "json" lp.name;
    Alcotest.(check bool) "version" true (Semver.equal (v 0 3 1) lp.version);
    Alcotest.(check int) "hash length" 64 (String.length lp.sha256);
    Alcotest.(check bool) "created non-empty" true (String.length lock.created > 0))

let test_create_transitive () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    (* dep-b: no deps *)
    let src_b = Filename.concat dir "srcb" in
    Sys.mkdir src_b 0o755;
    make_package src_b "dep-b" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_b;
    (* dep-a: depends on dep-b *)
    let src_a = Filename.concat dir "srca" in
    Sys.mkdir src_a 0o755;
    make_package src_a "dep-a" "1.0.0" ~depends:"(dep-b)" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src_a;
    let deps : Package.dependency list =
      [{ dep_name = "dep-a"; dep_constraints = [] }] in
    let lock = Lockfile.create ~registry_root:registry deps in
    Alcotest.(check int) "two packages" 2 (List.length lock.packages);
    let names = List.map (fun lp -> lp.Lockfile.name) lock.packages in
    Alcotest.(check bool) "has dep-a" true (List.mem "dep-a" names);
    Alcotest.(check bool) "has dep-b" true (List.mem "dep-b" names))

let test_create_no_deps () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let lock = Lockfile.create ~registry_root:registry [] in
    Alcotest.(check (list locked_pkg_testable)) "empty" [] lock.packages;
    Alcotest.(check bool) "timestamp present" true (String.length lock.created > 0))

let test_create_missing_package () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let deps : Package.dependency list =
      [{ dep_name = "nonexistent"; dep_constraints = [] }] in
    Alcotest.check_raises "missing"
      (Pkg_manager.Pkg_error "package not found: nonexistent")
      (fun () -> ignore (Lockfile.create ~registry_root:registry deps)))

(* --- Step 4: verify --- *)

let test_verify_all_valid () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "vpkg" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "vpkg"; dep_constraints = [] }] in
    let lock = Lockfile.create ~registry_root:registry deps in
    let mismatches = Lockfile.verify ~registry_root:registry lock in
    Alcotest.(check (list mismatch_testable)) "no mismatches" [] mismatches)

let test_verify_hash_mismatch () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let src = Filename.concat dir "src" in
    Sys.mkdir src 0o755;
    make_package src "tampered" "1.0.0" ();
    Pkg_manager.install ~registry_root:registry ~src_dir:src;
    let deps : Package.dependency list =
      [{ dep_name = "tampered"; dep_constraints = [] }] in
    let lock = Lockfile.create ~registry_root:registry deps in
    (* Tamper with the installed package *)
    let pkg_scm = Filename.concat
      (Filename.concat (Filename.concat registry "tampered") "1.0.0")
      "package.scm" in
    let oc = open_out_gen [Open_append; Open_wronly] 0o644 pkg_scm in
    Fun.protect ~finally:(fun () -> close_out oc)
      (fun () -> output_string oc "\n;; tampered");
    let mismatches = Lockfile.verify ~registry_root:registry lock in
    Alcotest.(check int) "one mismatch" 1 (List.length mismatches);
    match List.hd mismatches with
    | Lockfile.Hash_mismatch { name; _ } ->
      Alcotest.(check string) "name" "tampered" name
    | _ -> Alcotest.fail "expected Hash_mismatch")

let test_verify_not_installed () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let lock : Lockfile.t = {
      created = "2026-01-01T00:00:00Z";
      packages = [
        { name = "ghost"; version = v 1 0 0; sha256 = "deadbeef" };
      ];
    } in
    let mismatches = Lockfile.verify ~registry_root:registry lock in
    Alcotest.(check int) "one mismatch" 1 (List.length mismatches);
    Alcotest.(check mismatch_testable) "not installed"
      (Lockfile.Not_installed { name = "ghost"; version = "1.0.0" })
      (List.hd mismatches))

let test_verify_empty () =
  with_temp_dir (fun dir ->
    let registry = Filename.concat dir "reg" in
    let lock : Lockfile.t = {
      created = "2026-01-01T00:00:00Z";
      packages = [];
    } in
    let mismatches = Lockfile.verify ~registry_root:registry lock in
    Alcotest.(check (list mismatch_testable)) "empty" [] mismatches)

(* --- Test suite --- *)

let () =
  Alcotest.run "Lockfile" [
    "path", [
      Alcotest.test_case "lockfile_path" `Quick test_lockfile_path;
    ];
    "parse_write", [
      Alcotest.test_case "roundtrip one" `Quick test_write_parse_roundtrip_one;
      Alcotest.test_case "roundtrip multi" `Quick test_write_parse_roundtrip_multi;
      Alcotest.test_case "empty packages" `Quick test_parse_empty_packages;
      Alcotest.test_case "malformed" `Quick test_parse_malformed;
      Alcotest.test_case "missing created" `Quick test_parse_missing_created;
    ];
    "hash_directory", [
      Alcotest.test_case "non-empty hex" `Quick test_hash_nonempty;
      Alcotest.test_case "deterministic" `Quick test_hash_deterministic;
      Alcotest.test_case "changes on modification" `Quick test_hash_changes_on_modification;
    ];
    "create", [
      Alcotest.test_case "simple" `Quick test_create_simple;
      Alcotest.test_case "transitive" `Quick test_create_transitive;
      Alcotest.test_case "no deps" `Quick test_create_no_deps;
      Alcotest.test_case "missing package" `Quick test_create_missing_package;
    ];
    "verify", [
      Alcotest.test_case "all valid" `Quick test_verify_all_valid;
      Alcotest.test_case "hash mismatch" `Quick test_verify_hash_mismatch;
      Alcotest.test_case "not installed" `Quick test_verify_not_installed;
      Alcotest.test_case "empty lockfile" `Quick test_verify_empty;
    ];
  ]
