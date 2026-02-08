open Wile

(* --- Helpers --- *)

let with_tmpdir f =
  let dir = Filename.temp_dir "wile_sp_test_" "" in
  Fun.protect ~finally:(fun () ->
    let rec rm path =
      if Sys.is_directory path then begin
        Array.iter (fun entry ->
          rm (Filename.concat path entry)
        ) (Sys.readdir path);
        Sys.rmdir path
      end else
        Sys.remove path
    in
    rm dir
  ) (fun () -> f dir)

(* Save and restore env vars around a test *)
let with_env vars f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) vars in
  List.iter (fun (k, v) ->
    match v with
    | Some s -> Unix.putenv k s
    | None ->
      (* OCaml doesn't have unsetenv; use empty string as sentinel
         and handle in tests, OR use a C binding.
         For portability, set to empty which our code treats as unset. *)
      Unix.putenv k ""
  ) vars;
  Fun.protect ~finally:(fun () ->
    List.iter (fun (k, v) ->
      match v with
      | Some s -> Unix.putenv k s
      | None -> Unix.putenv k ""
    ) saved
  ) f

(* --- wile_home tests --- *)

let test_wile_home_default () =
  with_env [("WILE_HOME", None)] (fun () ->
    let home = match Sys.getenv_opt "HOME" with
      | Some h -> h | None -> "."
    in
    let expected = Filename.concat home ".wile" in
    Alcotest.(check string) "default" expected (Search_path.wile_home ()))

let test_wile_home_override () =
  with_env [("WILE_HOME", Some "/custom/wile")] (fun () ->
    Alcotest.(check string) "override" "/custom/wile"
      (Search_path.wile_home ()))

(* --- site_lib tests --- *)

let test_site_lib () =
  with_env [("WILE_HOME", Some "/custom/wile")] (fun () ->
    Alcotest.(check string) "site_lib" "/custom/wile/lib"
      (Search_path.site_lib ()))

(* --- env_paths tests --- *)

let test_env_paths_unset () =
  with_env [("WILE_PATH", None)] (fun () ->
    Alcotest.(check (list string)) "unset" [] (Search_path.env_paths ()))

let test_env_paths_single () =
  with_env [("WILE_PATH", Some "/foo/bar")] (fun () ->
    Alcotest.(check (list string)) "single" ["/foo/bar"]
      (Search_path.env_paths ()))

let test_env_paths_multiple () =
  with_env [("WILE_PATH", Some "/a:/b:/c")] (fun () ->
    Alcotest.(check (list string)) "multiple" ["/a"; "/b"; "/c"]
      (Search_path.env_paths ()))

let test_env_paths_filter_empty () =
  with_env [("WILE_PATH", Some "/a::/b:")] (fun () ->
    Alcotest.(check (list string)) "filter empty" ["/a"; "/b"]
      (Search_path.env_paths ()))

(* --- venv_lib_path tests --- *)

let test_venv_lib_path_unset () =
  with_env [("WILE_VENV", None)] (fun () ->
    Alcotest.(check (option string)) "unset" None
      (Search_path.venv_lib_path ()))

let test_venv_lib_path_valid () =
  with_tmpdir (fun dir ->
    Venv.create ~wile_version:"0.1.0" dir;
    with_env [("WILE_VENV", Some dir)] (fun () ->
      Alcotest.(check (option string)) "valid"
        (Some (Filename.concat dir "lib"))
        (Search_path.venv_lib_path ())))

let test_venv_lib_path_invalid () =
  with_tmpdir (fun dir ->
    (* dir exists but has no wile-venv.cfg *)
    with_env [("WILE_VENV", Some dir)] (fun () ->
      Alcotest.(check (option string)) "invalid" None
        (Search_path.venv_lib_path ())))

(* --- resolve tests --- *)

let test_resolve_base_only () =
  with_tmpdir (fun dir ->
    with_env [("WILE_VENV", None); ("WILE_PATH", None);
              ("WILE_HOME", Some (Filename.concat dir "home"))] (fun () ->
      (* Only base_dirs that exist should appear *)
      let sub = Filename.concat dir "exists" in
      Sys.mkdir sub 0o755;
      let result = Search_path.resolve ~base_dirs:[sub; "/nonexistent/dir"] in
      Alcotest.(check (list string)) "base only" [sub] result))

let test_resolve_order () =
  with_tmpdir (fun dir ->
    let base = Filename.concat dir "base" in
    Sys.mkdir base 0o755;
    let venv_dir = Filename.concat dir "venv" in
    Venv.create ~wile_version:"0.1.0" venv_dir;
    let wpath = Filename.concat dir "wpath" in
    Sys.mkdir wpath 0o755;
    let home_dir = Filename.concat dir "home" in
    Sys.mkdir home_dir 0o755;
    let home_lib = Filename.concat home_dir "lib" in
    Sys.mkdir home_lib 0o755;
    with_env [("WILE_VENV", Some venv_dir);
              ("WILE_PATH", Some wpath);
              ("WILE_HOME", Some home_dir)] (fun () ->
      let result = Search_path.resolve ~base_dirs:[base] in
      let expected = [
        base;
        Filename.concat venv_dir "lib";
        wpath;
        home_lib;
      ] in
      Alcotest.(check (list string)) "order" expected result))

let test_resolve_filters_nonexistent () =
  with_tmpdir (fun dir ->
    with_env [("WILE_VENV", None);
              ("WILE_PATH", Some "/nonexistent/wile/path");
              ("WILE_HOME", Some (Filename.concat dir "home"))] (fun () ->
      let result = Search_path.resolve ~base_dirs:["/nonexistent/base"] in
      Alcotest.(check (list string)) "all filtered" [] result))

(* --- Test suite --- *)

let () =
  Alcotest.run "Search_path" [
    "wile_home", [
      Alcotest.test_case "default" `Quick test_wile_home_default;
      Alcotest.test_case "override" `Quick test_wile_home_override;
    ];
    "site_lib", [
      Alcotest.test_case "path" `Quick test_site_lib;
    ];
    "env_paths", [
      Alcotest.test_case "unset" `Quick test_env_paths_unset;
      Alcotest.test_case "single" `Quick test_env_paths_single;
      Alcotest.test_case "multiple" `Quick test_env_paths_multiple;
      Alcotest.test_case "filter empty" `Quick test_env_paths_filter_empty;
    ];
    "venv_lib_path", [
      Alcotest.test_case "unset" `Quick test_venv_lib_path_unset;
      Alcotest.test_case "valid" `Quick test_venv_lib_path_valid;
      Alcotest.test_case "invalid" `Quick test_venv_lib_path_invalid;
    ];
    "resolve", [
      Alcotest.test_case "base only" `Quick test_resolve_base_only;
      Alcotest.test_case "order" `Quick test_resolve_order;
      Alcotest.test_case "filters nonexistent" `Quick test_resolve_filters_nonexistent;
    ];
  ]
