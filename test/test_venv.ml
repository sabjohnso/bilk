open Wile

(* --- Helpers --- *)

let with_tmpdir f =
  let dir = Filename.temp_dir "wile_venv_test_" "" in
  Fun.protect ~finally:(fun () ->
    (* Clean up recursively *)
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

(* --- Create tests --- *)

let test_create_new_dir () =
  with_tmpdir (fun parent ->
    let dir = Filename.concat parent "myvenv" in
    Venv.create ~wile_version:"0.1.0" dir;
    Alcotest.(check bool) "dir exists" true (Sys.file_exists dir);
    Alcotest.(check bool) "lib exists" true
      (Sys.file_exists (Filename.concat dir "lib"));
    Alcotest.(check bool) "cfg exists" true
      (Sys.file_exists (Filename.concat dir "wile-venv.cfg")))

let test_create_existing_empty_dir () =
  with_tmpdir (fun dir ->
    (* dir already exists but is empty — should succeed *)
    Venv.create ~wile_version:"0.1.0" dir;
    Alcotest.(check bool) "cfg exists" true
      (Sys.file_exists (Filename.concat dir "wile-venv.cfg")))

let test_create_already_venv () =
  with_tmpdir (fun dir ->
    Venv.create ~wile_version:"0.1.0" dir;
    Alcotest.check_raises "already a venv"
      (Venv.Venv_error (Printf.sprintf
        "directory already contains a virtual environment: %s" dir))
      (fun () -> Venv.create ~wile_version:"0.1.0" dir))

let test_create_nested () =
  with_tmpdir (fun parent ->
    let dir = Filename.concat parent "a/b/c" in
    Venv.create ~wile_version:"1.0.0" dir;
    Alcotest.(check bool) "cfg exists" true
      (Sys.file_exists (Filename.concat dir "wile-venv.cfg")))

(* --- is_venv tests --- *)

let test_is_venv_true () =
  with_tmpdir (fun dir ->
    Venv.create ~wile_version:"0.1.0" dir;
    Alcotest.(check bool) "is venv" true (Venv.is_venv dir))

let test_is_venv_false_empty () =
  with_tmpdir (fun dir ->
    Alcotest.(check bool) "not venv" false (Venv.is_venv dir))

let test_is_venv_false_nonexistent () =
  Alcotest.(check bool) "not venv" false
    (Venv.is_venv "/nonexistent/path/to/venv")

(* --- read_config tests --- *)

let test_read_config () =
  with_tmpdir (fun dir ->
    Venv.create ~wile_version:"0.1.0" dir;
    let cfg = Venv.read_config dir in
    Alcotest.(check string) "version" "0.1.0" cfg.wile_version;
    (* created is a timestamp — just check it's not empty *)
    Alcotest.(check bool) "created non-empty" true
      (String.length cfg.created > 0))

let test_read_config_missing () =
  with_tmpdir (fun dir ->
    Alcotest.check_raises "missing cfg"
      (Venv.Venv_error (Printf.sprintf
        "not a virtual environment (missing wile-venv.cfg): %s" dir))
      (fun () -> ignore (Venv.read_config dir)))

let test_read_config_malformed () =
  with_tmpdir (fun dir ->
    let cfg_path = Filename.concat dir "wile-venv.cfg" in
    let oc = open_out cfg_path in
    output_string oc "garbage\n";
    close_out oc;
    Alcotest.check_raises "malformed cfg"
      (Venv.Venv_error (Printf.sprintf
        "malformed wile-venv.cfg: missing wile-version in %s" dir))
      (fun () -> ignore (Venv.read_config dir)))

(* --- lib_path tests --- *)

let test_lib_path () =
  Alcotest.(check string) "lib path"
    "/some/venv/lib" (Venv.lib_path "/some/venv")

(* --- Test suite --- *)

let () =
  Alcotest.run "Venv" [
    "create", [
      Alcotest.test_case "new dir" `Quick test_create_new_dir;
      Alcotest.test_case "existing empty dir" `Quick test_create_existing_empty_dir;
      Alcotest.test_case "already a venv" `Quick test_create_already_venv;
      Alcotest.test_case "nested dirs" `Quick test_create_nested;
    ];
    "is_venv", [
      Alcotest.test_case "true" `Quick test_is_venv_true;
      Alcotest.test_case "false empty" `Quick test_is_venv_false_empty;
      Alcotest.test_case "false nonexistent" `Quick test_is_venv_false_nonexistent;
    ];
    "read_config", [
      Alcotest.test_case "valid" `Quick test_read_config;
      Alcotest.test_case "missing" `Quick test_read_config_missing;
      Alcotest.test_case "malformed" `Quick test_read_config_malformed;
    ];
    "lib_path", [
      Alcotest.test_case "simple" `Quick test_lib_path;
    ];
  ]
