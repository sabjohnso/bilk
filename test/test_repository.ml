open Bilk

(* --- Helpers --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "bilk_repo_test" "" in
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

(* --- Step 1: Config tests --- *)

let test_repos_dir () =
  let result = Repository.repos_dir "/home/user/.bilk" in
  Alcotest.(check string) "repos dir"
    "/home/user/.bilk/repos" result

let test_repos_config_path () =
  let result = Repository.repos_config_path "/home/user/.bilk" in
  Alcotest.(check string) "config path"
    "/home/user/.bilk/repositories" result

let test_load_repos_missing_file () =
  with_temp_dir (fun dir ->
    let repos = Repository.load_repos dir in
    Alcotest.(check int) "empty" 0 (List.length repos))

let test_load_repos_basic () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "repositories")
      {|((default . "https://github.com/bilk-scheme/packages"))|};
    let repos = Repository.load_repos dir in
    Alcotest.(check int) "one repo" 1 (List.length repos);
    let r = List.hd repos in
    Alcotest.(check string) "name" "default" r.name;
    Alcotest.(check string) "url"
      "https://github.com/bilk-scheme/packages" r.url)

let test_load_repos_multiple () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "repositories")
      {|((default . "https://example.com/a")
        (extra . "https://example.com/b"))|};
    let repos = Repository.load_repos dir in
    Alcotest.(check int) "two repos" 2 (List.length repos);
    Alcotest.(check string) "first name" "default" (List.nth repos 0).name;
    Alcotest.(check string) "second name" "extra" (List.nth repos 1).name)

let test_load_repos_malformed () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "repositories") "not-a-list";
    Alcotest.check_raises "malformed"
      (Repository.Repository_error "malformed repository config")
      (fun () -> ignore (Repository.load_repos dir)))

let test_save_load_roundtrip () =
  with_temp_dir (fun dir ->
    let repos : Repository.repo list = [
      { name = "default"; url = "https://example.com/a" };
      { name = "extra"; url = "https://example.com/b" };
    ] in
    Repository.save_repos dir repos;
    let loaded = Repository.load_repos dir in
    Alcotest.(check int) "count" 2 (List.length loaded);
    Alcotest.(check string) "name0" "default" (List.nth loaded 0).name;
    Alcotest.(check string) "url0" "https://example.com/a"
      (List.nth loaded 0).url;
    Alcotest.(check string) "name1" "extra" (List.nth loaded 1).name;
    Alcotest.(check string) "url1" "https://example.com/b"
      (List.nth loaded 1).url)

(* --- Step 2: Index parsing tests --- *)

let v major minor patch : Semver.t = { major; minor; patch }

let semver_testable : Semver.t Alcotest.testable =
  Alcotest.testable
    (fun fmt sv -> Format.pp_print_string fmt (Semver.to_string sv))
    Semver.equal

let test_parse_index_basic () =
  let content = {|(repository
    (name "bilk-packages")
    (packages
      (json "0.2.0" "0.3.0")
      (srfi-extra "1.0.0")))|} in
  let idx = Repository.parse_index Readtable.default content in
  Alcotest.(check string) "repo name" "bilk-packages" idx.repo_name;
  Alcotest.(check int) "entry count" 2 (List.length idx.entries);
  let json_entry = List.hd idx.entries in
  Alcotest.(check string) "pkg name" "json" json_entry.pkg_name;
  Alcotest.(check (list semver_testable)) "versions"
    [v 0 2 0; v 0 3 0] json_entry.versions

let test_parse_index_empty_packages () =
  let content = {|(repository
    (name "empty")
    (packages))|} in
  let idx = Repository.parse_index Readtable.default content in
  Alcotest.(check string) "repo name" "empty" idx.repo_name;
  Alcotest.(check int) "no entries" 0 (List.length idx.entries)

let test_parse_index_many_versions () =
  let content = {|(repository
    (name "multi")
    (packages
      (lib "1.0.0" "1.1.0" "2.0.0" "2.1.0")))|} in
  let idx = Repository.parse_index Readtable.default content in
  Alcotest.(check int) "one entry" 1 (List.length idx.entries);
  let entry = List.hd idx.entries in
  Alcotest.(check int) "four versions" 4 (List.length entry.versions)

let test_parse_index_missing_name () =
  let content = {|(repository (packages (lib "1.0.0")))|} in
  Alcotest.check_raises "missing name"
    (Repository.Repository_error "index: missing required field: name")
    (fun () -> ignore (Repository.parse_index Readtable.default content))

let test_parse_index_bad_version () =
  let content = {|(repository
    (name "bad")
    (packages
      (lib "not-a-version")))|} in
  Alcotest.check_raises "bad version"
    (Repository.Repository_error "index: invalid version \"not-a-version\"")
    (fun () -> ignore (Repository.parse_index Readtable.default content))

(* --- Step 3: Search tests --- *)

let make_test_index () : Repository.index =
  { repo_name = "test";
    entries = [
      { pkg_name = "json-parser"; versions = [v 1 0 0] };
      { pkg_name = "http-client"; versions = [v 2 0 0] };
      { pkg_name = "json-rpc"; versions = [v 0 1 0] };
    ] }

let test_search_exact () =
  let idx = make_test_index () in
  let results = Repository.search_index idx "json-parser" in
  Alcotest.(check int) "one match" 1 (List.length results);
  Alcotest.(check string) "name" "json-parser"
    (List.hd results).pkg_name

let test_search_substring () =
  let idx = make_test_index () in
  let results = Repository.search_index idx "json" in
  Alcotest.(check int) "two matches" 2 (List.length results)

let test_search_case_insensitive () =
  let idx = make_test_index () in
  let results = Repository.search_index idx "JSON" in
  Alcotest.(check int) "case insensitive" 2 (List.length results)

let test_search_no_match () =
  let idx = make_test_index () in
  let results = Repository.search_index idx "xml" in
  Alcotest.(check int) "no matches" 0 (List.length results)

(* --- Step 4: Git clone management tests --- *)

(** Create a local bare git repo with an initial commit as a test fixture. *)
let make_bare_repo dir =
  let bare = Filename.concat dir "bare.git" in
  let work = Filename.concat dir "work" in
  (* init bare repo *)
  assert (Sys.command (Printf.sprintf "git init --bare %s >/dev/null 2>&1" bare) = 0);
  (* clone into work, make initial commit *)
  assert (Sys.command (Printf.sprintf "git clone %s %s >/dev/null 2>&1" bare work) = 0);
  assert (Sys.command (Printf.sprintf
    "cd %s && git config user.email test@test && git config user.name test && \
     echo hello > README && git add README && git commit -m init >/dev/null 2>&1"
    work) = 0);
  (* push to bare *)
  assert (Sys.command (Printf.sprintf "cd %s && git push >/dev/null 2>&1" work) = 0);
  (bare, work)

let test_clone_dir () =
  let repo : Repository.repo = { name = "default"; url = "https://example.com" } in
  let result = Repository.clone_dir "/home/user/.bilk" repo in
  Alcotest.(check string) "clone dir"
    "/home/user/.bilk/repos/default" result

let test_sync_creates_clone () =
  with_temp_dir (fun dir ->
    let (bare, _work) = make_bare_repo dir in
    let bilk_home = Filename.concat dir "home" in
    let repo : Repository.repo = { name = "test-repo"; url = bare } in
    Repository.sync bilk_home repo;
    let clone = Repository.clone_dir bilk_home repo in
    Alcotest.(check bool) "clone exists" true (Sys.file_exists clone);
    Alcotest.(check bool) "README exists" true
      (Sys.file_exists (Filename.concat clone "README")))

let test_sync_idempotent () =
  with_temp_dir (fun dir ->
    let (bare, _work) = make_bare_repo dir in
    let bilk_home = Filename.concat dir "home" in
    let repo : Repository.repo = { name = "test-repo"; url = bare } in
    Repository.sync bilk_home repo;
    Repository.sync bilk_home repo;
    let clone = Repository.clone_dir bilk_home repo in
    Alcotest.(check bool) "clone still exists" true (Sys.file_exists clone))

let test_sync_bad_url () =
  with_temp_dir (fun dir ->
    let bilk_home = Filename.concat dir "home" in
    let repo : Repository.repo =
      { name = "bad"; url = "/nonexistent/repo.git" } in
    Alcotest.check_raises "bad url"
      (Repository.Repository_error
         "failed to clone repository \"bad\" from /nonexistent/repo.git")
      (fun () -> Repository.sync bilk_home repo))

let test_sync_pulls_new_commits () =
  with_temp_dir (fun dir ->
    let (bare, work) = make_bare_repo dir in
    let bilk_home = Filename.concat dir "home" in
    let repo : Repository.repo = { name = "test-repo"; url = bare } in
    Repository.sync bilk_home repo;
    (* Add a new file to the work tree and push *)
    assert (Sys.command (Printf.sprintf
      "cd %s && echo world > NEW && git add NEW && \
       git commit -m update >/dev/null 2>&1 && git push >/dev/null 2>&1"
      work) = 0);
    Repository.sync bilk_home repo;
    let clone = Repository.clone_dir bilk_home repo in
    Alcotest.(check bool) "NEW file pulled" true
      (Sys.file_exists (Filename.concat clone "NEW")))

(* --- Step 5: Package access tests --- *)

(** Add a package to the bare repo fixture via the working copy. *)
let add_package_to_repo work name version =
  let pkg_dir = Filename.concat
    (Filename.concat (Filename.concat work "packages") name) version in
  let rec mkdir_p d =
    if not (Sys.file_exists d) then begin
      mkdir_p (Filename.dirname d);
      Sys.mkdir d 0o755
    end
  in
  mkdir_p pkg_dir;
  write_file (Filename.concat pkg_dir "package.scm")
    (Printf.sprintf
      {|(define-package
        (name %s)
        (version "%s")
        (description "Test package")
        (license "MIT"))|}
      name version);
  let src_dir = Filename.concat pkg_dir "src" in
  mkdir_p src_dir;
  write_file (Filename.concat src_dir (name ^ ".sld"))
    (Printf.sprintf "(define-library (%s) (export) (import (scheme base)) (begin))" name);
  assert (Sys.command (Printf.sprintf
    "cd %s && git add -A && git commit -m 'add %s %s' >/dev/null 2>&1 \
     && git push >/dev/null 2>&1" work name version) = 0)

let test_package_dir () =
  let repo : Repository.repo = { name = "r"; url = "x" } in
  let result = Repository.package_dir "/home/user/.bilk" repo
    ~name:"json" ~version:"1.0.0" in
  Alcotest.(check string) "package dir"
    "/home/user/.bilk/repos/r/packages/json/1.0.0" result

let test_has_package_true () =
  with_temp_dir (fun dir ->
    let (bare, work) = make_bare_repo dir in
    add_package_to_repo work "json" "1.0.0";
    let bilk_home = Filename.concat dir "home" in
    let repo : Repository.repo = { name = "test"; url = bare } in
    Repository.sync bilk_home repo;
    Alcotest.(check bool) "has package" true
      (Repository.has_package bilk_home repo ~name:"json" ~version:"1.0.0"))

let test_has_package_false () =
  with_temp_dir (fun dir ->
    let (bare, _work) = make_bare_repo dir in
    let bilk_home = Filename.concat dir "home" in
    let repo : Repository.repo = { name = "test"; url = bare } in
    Repository.sync bilk_home repo;
    Alcotest.(check bool) "no package" false
      (Repository.has_package bilk_home repo ~name:"ghost" ~version:"1.0.0"))

let test_scan_versions () =
  with_temp_dir (fun dir ->
    let (bare, work) = make_bare_repo dir in
    add_package_to_repo work "json" "1.0.0";
    add_package_to_repo work "json" "2.0.0";
    add_package_to_repo work "json" "1.1.0";
    let bilk_home = Filename.concat dir "home" in
    let repo : Repository.repo = { name = "test"; url = bare } in
    Repository.sync bilk_home repo;
    let versions = Repository.scan_versions bilk_home repo "json" in
    Alcotest.(check (list semver_testable)) "sorted versions"
      [v 1 0 0; v 1 1 0; v 2 0 0] versions)

(* --- Step 6: fetch_package tests --- *)

let test_fetch_installs () =
  with_temp_dir (fun dir ->
    let (bare, work) = make_bare_repo dir in
    add_package_to_repo work "json" "1.0.0";
    let bilk_home = Filename.concat dir "home" in
    let registry = Filename.concat dir "registry" in
    let repo : Repository.repo = { name = "test"; url = bare } in
    Repository.sync bilk_home repo;
    Repository.fetch_package ~bilk_home ~registry_root:registry repo
      ~name:"json" ~version:"1.0.0";
    let pkgs = Pkg_manager.list_packages ~registry_root:registry in
    Alcotest.(check int) "one package" 1 (List.length pkgs);
    Alcotest.(check string) "name" "json" (fst (List.hd pkgs)))

let test_fetch_not_in_repo () =
  with_temp_dir (fun dir ->
    let (bare, _work) = make_bare_repo dir in
    let bilk_home = Filename.concat dir "home" in
    let registry = Filename.concat dir "registry" in
    let repo : Repository.repo = { name = "test"; url = bare } in
    Repository.sync bilk_home repo;
    Alcotest.check_raises "not found"
      (Repository.Repository_error
         "package ghost 1.0.0 not found in repository \"test\"")
      (fun () ->
        Repository.fetch_package ~bilk_home ~registry_root:registry repo
          ~name:"ghost" ~version:"1.0.0"))

let test_fetch_already_installed () =
  with_temp_dir (fun dir ->
    let (bare, work) = make_bare_repo dir in
    add_package_to_repo work "json" "1.0.0";
    let bilk_home = Filename.concat dir "home" in
    let registry = Filename.concat dir "registry" in
    let repo : Repository.repo = { name = "test"; url = bare } in
    Repository.sync bilk_home repo;
    Repository.fetch_package ~bilk_home ~registry_root:registry repo
      ~name:"json" ~version:"1.0.0";
    Alcotest.check_raises "already installed"
      (Pkg_manager.Pkg_error "package json 1.0.0 is already installed")
      (fun () ->
        Repository.fetch_package ~bilk_home ~registry_root:registry repo
          ~name:"json" ~version:"1.0.0"))

(* --- Step 7: load_index + search_all tests --- *)

let test_load_index_present () =
  with_temp_dir (fun dir ->
    let (bare, work) = make_bare_repo dir in
    write_file (Filename.concat work "index.scm")
      {|(repository (name "test") (packages (json "1.0.0")))|};
    assert (Sys.command (Printf.sprintf
      "cd %s && git add -A && git commit -m 'add index' >/dev/null 2>&1 \
       && git push >/dev/null 2>&1" work) = 0);
    let bilk_home = Filename.concat dir "home" in
    let repo : Repository.repo = { name = "test"; url = bare } in
    Repository.sync bilk_home repo;
    match Repository.load_index bilk_home repo with
    | None -> Alcotest.fail "expected Some"
    | Some idx ->
      Alcotest.(check string) "repo name" "test" idx.repo_name;
      Alcotest.(check int) "entries" 1 (List.length idx.entries))

let test_load_index_absent () =
  with_temp_dir (fun dir ->
    let (bare, _work) = make_bare_repo dir in
    let bilk_home = Filename.concat dir "home" in
    let repo : Repository.repo = { name = "test"; url = bare } in
    Repository.sync bilk_home repo;
    Alcotest.(check bool) "absent" true
      (Repository.load_index bilk_home repo = None))

let test_search_all_multi_repos () =
  with_temp_dir (fun dir ->
    (* Create repo A with index *)
    let bare_a = Filename.concat dir "a.git" in
    let work_a = Filename.concat dir "work_a" in
    assert (Sys.command (Printf.sprintf "git init --bare %s >/dev/null 2>&1" bare_a) = 0);
    assert (Sys.command (Printf.sprintf "git clone %s %s >/dev/null 2>&1" bare_a work_a) = 0);
    assert (Sys.command (Printf.sprintf
      "cd %s && git config user.email test@test && git config user.name test && \
       echo x > README && git add README && git commit -m init >/dev/null 2>&1 && \
       git push >/dev/null 2>&1" work_a) = 0);
    write_file (Filename.concat work_a "index.scm")
      {|(repository (name "a") (packages (json "1.0.0")))|};
    assert (Sys.command (Printf.sprintf
      "cd %s && git add -A && git commit -m idx >/dev/null 2>&1 && \
       git push >/dev/null 2>&1" work_a) = 0);
    (* Create repo B with index *)
    let bare_b = Filename.concat dir "b.git" in
    let work_b = Filename.concat dir "work_b" in
    assert (Sys.command (Printf.sprintf "git init --bare %s >/dev/null 2>&1" bare_b) = 0);
    assert (Sys.command (Printf.sprintf "git clone %s %s >/dev/null 2>&1" bare_b work_b) = 0);
    assert (Sys.command (Printf.sprintf
      "cd %s && git config user.email test@test && git config user.name test && \
       echo x > README && git add README && git commit -m init >/dev/null 2>&1 && \
       git push >/dev/null 2>&1" work_b) = 0);
    write_file (Filename.concat work_b "index.scm")
      {|(repository (name "b") (packages (json-rpc "0.1.0") (xml "1.0.0")))|};
    assert (Sys.command (Printf.sprintf
      "cd %s && git add -A && git commit -m idx >/dev/null 2>&1 && \
       git push >/dev/null 2>&1" work_b) = 0);
    let bilk_home = Filename.concat dir "home" in
    let repo_a : Repository.repo = { name = "a"; url = bare_a } in
    let repo_b : Repository.repo = { name = "b"; url = bare_b } in
    Repository.sync bilk_home repo_a;
    Repository.sync bilk_home repo_b;
    let results = Repository.search_all bilk_home [repo_a; repo_b] "json" in
    Alcotest.(check int) "two json matches" 2 (List.length results))

(* --- Test suite --- *)

let () =
  Alcotest.run "Repository" [
    "config", [
      Alcotest.test_case "repos_dir" `Quick test_repos_dir;
      Alcotest.test_case "repos_config_path" `Quick test_repos_config_path;
      Alcotest.test_case "load missing" `Quick test_load_repos_missing_file;
      Alcotest.test_case "load basic" `Quick test_load_repos_basic;
      Alcotest.test_case "load multiple" `Quick test_load_repos_multiple;
      Alcotest.test_case "load malformed" `Quick test_load_repos_malformed;
      Alcotest.test_case "save/load roundtrip" `Quick test_save_load_roundtrip;
    ];
    "index", [
      Alcotest.test_case "basic" `Quick test_parse_index_basic;
      Alcotest.test_case "empty packages" `Quick test_parse_index_empty_packages;
      Alcotest.test_case "many versions" `Quick test_parse_index_many_versions;
      Alcotest.test_case "missing name" `Quick test_parse_index_missing_name;
      Alcotest.test_case "bad version" `Quick test_parse_index_bad_version;
    ];
    "search", [
      Alcotest.test_case "exact match" `Quick test_search_exact;
      Alcotest.test_case "substring" `Quick test_search_substring;
      Alcotest.test_case "case insensitive" `Quick test_search_case_insensitive;
      Alcotest.test_case "no match" `Quick test_search_no_match;
    ];
    "clone", [
      Alcotest.test_case "clone_dir" `Quick test_clone_dir;
      Alcotest.test_case "sync creates clone" `Quick test_sync_creates_clone;
      Alcotest.test_case "sync idempotent" `Quick test_sync_idempotent;
      Alcotest.test_case "sync bad url" `Quick test_sync_bad_url;
      Alcotest.test_case "sync pulls new" `Quick test_sync_pulls_new_commits;
    ];
    "package_access", [
      Alcotest.test_case "package_dir" `Quick test_package_dir;
      Alcotest.test_case "has_package true" `Quick test_has_package_true;
      Alcotest.test_case "has_package false" `Quick test_has_package_false;
      Alcotest.test_case "scan_versions" `Quick test_scan_versions;
    ];
    "fetch", [
      Alcotest.test_case "installs" `Quick test_fetch_installs;
      Alcotest.test_case "not in repo" `Quick test_fetch_not_in_repo;
      Alcotest.test_case "already installed" `Quick test_fetch_already_installed;
    ];
    "load_index", [
      Alcotest.test_case "present" `Quick test_load_index_present;
      Alcotest.test_case "absent" `Quick test_load_index_absent;
      Alcotest.test_case "search_all multi" `Quick test_search_all_multi_repos;
    ];
  ]
