(* Integration tests for the "bilk test" command.
   Each test creates a temporary project directory with package.scm,
   libraries, and test files, then invokes "bilk test" as a subprocess. *)

(* --- Helpers --- *)

let bilk_exe =
  (* Tests run from _build/default/test/; the bilk binary is at
     _build/default/bin/main.exe *)
  let dir = Filename.dirname Sys.executable_name in
  Filename.concat (Filename.concat (Filename.dirname dir) "bin") "main.exe"

let with_temp_dir fn =
  let dir = Filename.temp_dir "bilk_test_cmd" "" in
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
  if not (Sys.file_exists parent) then
    Sys.mkdir parent 0o755;
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc content)

let run_bilk_test ?file () =
  let dev_null = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let out_r, out_w = Unix.pipe () in
  let args = match file with
    | Some f -> [| bilk_exe; "test"; f |]
    | None -> [| bilk_exe; "test" |]
  in
  let pid = Unix.create_process bilk_exe args dev_null out_w out_w in
  Unix.close dev_null;
  Unix.close out_w;
  let buf = Buffer.create 256 in
  let bytes = Bytes.create 4096 in
  let rec read_all () =
    let n = Unix.read out_r bytes 0 4096 in
    if n > 0 then begin
      Buffer.add_subbytes buf bytes 0 n;
      read_all ()
    end
  in
  (try read_all () with Unix.Unix_error _ -> ());
  Unix.close out_r;
  let _, status = Unix.waitpid [] pid in
  let code = match status with
    | Unix.WEXITED c -> c
    | Unix.WSIGNALED _ -> 128
    | Unix.WSTOPPED _ -> 128
  in
  (code, Buffer.contents buf)

let minimal_package_scm =
  {|(define-package
      (name test-proj)
      (version "0.1.0")
      (description "test project")
      (license "MIT"))|}

(* --- Tests --- *)

let test_passing_test () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "package.scm") minimal_package_scm;
    let test_dir = Filename.concat dir "test" in
    Sys.mkdir test_dir 0o755;
    write_file (Filename.concat test_dir "test-pass.scm")
      "(import (scheme base) (scheme process-context)) (exit 0)";
    let saved = Sys.getcwd () in
    Fun.protect ~finally:(fun () -> Sys.chdir saved) (fun () ->
      Sys.chdir dir;
      let (code, output) = run_bilk_test () in
      Alcotest.(check int) "exit code" 0 code;
      Alcotest.(check bool) "output contains PASS"
        true (let r = Str.regexp_string "PASS" in
              try ignore (Str.search_forward r output 0); true
              with Not_found -> false)))

let test_failing_test () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "package.scm") minimal_package_scm;
    let test_dir = Filename.concat dir "test" in
    Sys.mkdir test_dir 0o755;
    write_file (Filename.concat test_dir "test-fail.scm")
      "(import (scheme base) (scheme process-context)) (exit 1)";
    let saved = Sys.getcwd () in
    Fun.protect ~finally:(fun () -> Sys.chdir saved) (fun () ->
      Sys.chdir dir;
      let (code, output) = run_bilk_test () in
      Alcotest.(check bool) "exit code non-zero" true (code <> 0);
      Alcotest.(check bool) "output contains FAIL"
        true (let r = Str.regexp_string "FAIL" in
              try ignore (Str.search_forward r output 0); true
              with Not_found -> false)))

let test_single_file () =
  with_temp_dir (fun dir ->
    write_file (Filename.concat dir "package.scm") minimal_package_scm;
    let test_dir = Filename.concat dir "test" in
    Sys.mkdir test_dir 0o755;
    write_file (Filename.concat test_dir "test-one.scm")
      "(import (scheme base) (scheme process-context)) (exit 0)";
    write_file (Filename.concat test_dir "test-two.scm")
      "(import (scheme base) (scheme process-context)) (exit 1)";
    let saved = Sys.getcwd () in
    Fun.protect ~finally:(fun () -> Sys.chdir saved) (fun () ->
      Sys.chdir dir;
      let file = Filename.concat test_dir "test-one.scm" in
      let (code, output) = run_bilk_test ~file () in
      Alcotest.(check int) "exit code" 0 code;
      Alcotest.(check bool) "output contains PASS"
        true (let r = Str.regexp_string "PASS" in
              try ignore (Str.search_forward r output 0); true
              with Not_found -> false);
      (* Should NOT have run test-two *)
      Alcotest.(check bool) "output does not contain FAIL"
        false (let r = Str.regexp_string "FAIL" in
               try ignore (Str.search_forward r output 0); true
               with Not_found -> false)))

(* --- Test suite --- *)

let () =
  Alcotest.run "Test Command" [
    "bilk test", [
      Alcotest.test_case "passing test" `Slow test_passing_test;
      Alcotest.test_case "failing test" `Slow test_failing_test;
      Alcotest.test_case "single file" `Slow test_single_file;
    ];
  ]
