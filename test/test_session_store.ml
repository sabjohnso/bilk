open Bilk

(* --- Helpers --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "bilk_store_test" "" in
  Fun.protect ~finally:(fun () ->
    (* clean up *)
    let entries = Sys.readdir dir in
    Array.iter (fun f ->
      let sub = Filename.concat dir f in
      if Sys.is_directory sub then begin
        Array.iter (fun g ->
          Sys.remove (Filename.concat sub g)
        ) (Sys.readdir sub);
        Unix.rmdir sub
      end else
        Sys.remove sub
    ) entries;
    Unix.rmdir dir
  ) (fun () -> fn dir)

(* --- Validation tests --- *)

let test_validate_good_name () =
  Alcotest.(check bool) "my-session ok" true
    (Session_store.validate_name "my-session" = Ok "my-session")

let test_validate_slash () =
  Alcotest.(check bool) "foo/bar rejected" true
    (match Session_store.validate_name "foo/bar" with
     | Error _ -> true | Ok _ -> false)

let test_validate_backslash () =
  Alcotest.(check bool) "foo\\bar rejected" true
    (match Session_store.validate_name "foo\\bar" with
     | Error _ -> true | Ok _ -> false)

let test_validate_dotdot () =
  Alcotest.(check bool) ".. rejected" true
    (match Session_store.validate_name ".." with
     | Error _ -> true | Ok _ -> false)

let test_validate_dot () =
  Alcotest.(check bool) ". rejected" true
    (match Session_store.validate_name "." with
     | Error _ -> true | Ok _ -> false)

let test_validate_empty () =
  Alcotest.(check bool) "empty rejected" true
    (match Session_store.validate_name "" with
     | Error _ -> true | Ok _ -> false)

let test_validate_null_byte () =
  Alcotest.(check bool) "null byte rejected" true
    (match Session_store.validate_name "foo\x00bar" with
     | Error _ -> true | Ok _ -> false)

(* Property: names from [a-zA-Z0-9_-]{1,50} all pass *)
let prop_valid_names =
  let open QCheck2 in
  Test.make ~name:"alphanumeric names always valid"
    ~count:200
    Gen.(string_size ~gen:(Gen.oneof [
      Gen.char_range 'a' 'z';
      Gen.char_range 'A' 'Z';
      Gen.char_range '0' '9';
      Gen.return '_';
      Gen.return '-';
    ]) (Gen.int_range 1 50))
    (fun name ->
       Session_store.validate_name name = Ok name)

(* --- Store operation tests --- *)

let test_save_load_round_trip () =
  with_temp_dir (fun home ->
    let inst = Instance.create () in
    ignore (Instance.eval_string inst "(define x 42)");
    let s = Session.create () in
    Session.checkpoint s "alpha" inst;
    Session_store.save ~home s "test1";
    let s2 = Session_store.load ~home "test1" in
    Alcotest.(check (list string)) "checkpoints preserved"
      ["alpha"] (Session.list_checkpoints s2);
    let inst2 = Session.revert s2 "alpha" in
    let v = Instance.eval_string inst2 "x" in
    Alcotest.(check bool) "x is 42" true (v = Datum.Fixnum 42))

let test_load_nonexistent () =
  with_temp_dir (fun home ->
    Alcotest.check_raises "nonexistent raises"
      (Session_store.Store_error "session not found: nonexistent")
      (fun () -> ignore (Session_store.load ~home "nonexistent")))

let test_list_sorted () =
  with_temp_dir (fun home ->
    let inst = Instance.create () in
    let s1 = Session.create () in
    Session.checkpoint s1 "cp1" inst;
    let s2 = Session.create () in
    Session.checkpoint s2 "cp2" inst;
    Session_store.save ~home s1 "zebra";
    Session_store.save ~home s2 "alpha";
    (* Also create a non-.bses file that should be ignored *)
    let sessions_dir = Filename.concat home "sessions" in
    let junk = Filename.concat sessions_dir "notes.txt" in
    let oc = open_out junk in
    output_string oc "not a session";
    close_out oc;
    let names = Session_store.list ~home () in
    Alcotest.(check (list string)) "sorted, no junk"
      ["alpha"; "zebra"] names)

let test_delete () =
  with_temp_dir (fun home ->
    let inst = Instance.create () in
    let s = Session.create () in
    Session.checkpoint s "cp" inst;
    Session_store.save ~home s "doomed";
    Alcotest.(check (list string)) "before delete"
      ["doomed"] (Session_store.list ~home ());
    Session_store.delete ~home "doomed";
    Alcotest.(check (list string)) "after delete"
      [] (Session_store.list ~home ()))

(* --- Test suite --- *)

let () =
  Alcotest.run "Session_store" [
    "validate", [
      Alcotest.test_case "good name" `Quick test_validate_good_name;
      Alcotest.test_case "slash rejected" `Quick test_validate_slash;
      Alcotest.test_case "backslash rejected" `Quick test_validate_backslash;
      Alcotest.test_case "dotdot rejected" `Quick test_validate_dotdot;
      Alcotest.test_case "dot rejected" `Quick test_validate_dot;
      Alcotest.test_case "empty rejected" `Quick test_validate_empty;
      Alcotest.test_case "null byte rejected" `Quick test_validate_null_byte;
      QCheck_alcotest.to_alcotest prop_valid_names;
    ];
    "store", [
      Alcotest.test_case "save/load round-trip" `Quick test_save_load_round_trip;
      Alcotest.test_case "load nonexistent" `Quick test_load_nonexistent;
      Alcotest.test_case "list sorted" `Quick test_list_sorted;
      Alcotest.test_case "delete" `Quick test_delete;
    ];
  ]
