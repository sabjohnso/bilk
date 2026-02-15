open Bilk

(* --- Helpers --- *)

let eval inst s = Instance.eval_string inst s

let datum_testable : Datum.t Alcotest.testable =
  Alcotest.testable Datum.pp Datum.equal

let with_temp_file fn =
  let path = Filename.temp_file "bilk_session_test" ".session" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () -> fn path)

(* --- Create / list_checkpoints --- *)

let test_create_empty () =
  let s = Session.create () in
  Alcotest.(check (list string)) "no checkpoints"
    [] (Session.list_checkpoints s)

(* --- Checkpoint / revert --- *)

let test_checkpoint_and_revert () =
  let inst = Instance.create () in
  ignore (eval inst "(define x 42)");
  let s = Session.create () in
  Session.checkpoint s "alpha" inst;
  ignore (eval inst "(define y 99)");
  let inst2 = Session.revert s "alpha" in
  Alcotest.check datum_testable "x survives" (Datum.Fixnum 42) (eval inst2 "x");
  Alcotest.check_raises "y gone"
    (Vm.Runtime_error "unbound variable: y")
    (fun () -> ignore (eval inst2 "y"))

let test_multiple_checkpoints () =
  let inst = Instance.create () in
  let s = Session.create () in
  ignore (eval inst "(define x 1)");
  Session.checkpoint s "first" inst;
  ignore (eval inst "(define x 2)");
  Session.checkpoint s "second" inst;
  Alcotest.(check (list string)) "two checkpoints"
    ["first"; "second"] (Session.list_checkpoints s);
  let inst1 = Session.revert s "first" in
  Alcotest.check datum_testable "revert to first"
    (Datum.Fixnum 1) (eval inst1 "x");
  let inst2 = Session.revert s "second" in
  Alcotest.check datum_testable "revert to second"
    (Datum.Fixnum 2) (eval inst2 "x")

let test_checkpoint_duplicate_name () =
  let inst = Instance.create () in
  let s = Session.create () in
  ignore (eval inst "(define x 1)");
  Session.checkpoint s "snap" inst;
  ignore (eval inst "(define x 2)");
  Session.checkpoint s "snap" inst;
  Alcotest.(check (list string)) "one checkpoint"
    ["snap"] (Session.list_checkpoints s);
  let inst2 = Session.revert s "snap" in
  Alcotest.check datum_testable "latest snapshot"
    (Datum.Fixnum 2) (eval inst2 "x")

let test_revert_unknown_name () =
  let s = Session.create () in
  Alcotest.check_raises "unknown checkpoint"
    (Session.Session_error "unknown checkpoint: nonexistent")
    (fun () -> ignore (Session.revert s "nonexistent"))

let test_revert_with_closure () =
  let inst = Instance.create () in
  let s = Session.create () in
  ignore (eval inst "(define (square x) (* x x))");
  Session.checkpoint s "with-closure" inst;
  let inst2 = Session.revert s "with-closure" in
  Alcotest.check datum_testable "closure works"
    (Datum.Fixnum 49) (eval inst2 "(square 7)")

(* --- Save / Load --- *)

let test_save_load_empty () =
  with_temp_file (fun path ->
    let s1 = Session.create () in
    Session.save s1 path;
    let s2 = Session.load path in
    Alcotest.(check (list string)) "no checkpoints"
      [] (Session.list_checkpoints s2))

let test_save_load_round_trip () =
  with_temp_file (fun path ->
    let inst = Instance.create () in
    ignore (eval inst "(define x 42)");
    ignore (eval inst "(define (square n) (* n n))");
    let s1 = Session.create () in
    Session.checkpoint s1 "alpha" inst;
    ignore (eval inst "(define y 99)");
    Session.checkpoint s1 "beta" inst;
    Session.save s1 path;
    let s2 = Session.load path in
    Alcotest.(check (list string)) "checkpoints preserved"
      ["alpha"; "beta"] (Session.list_checkpoints s2);
    let inst_alpha = Session.revert s2 "alpha" in
    Alcotest.check datum_testable "x from alpha"
      (Datum.Fixnum 42) (eval inst_alpha "x");
    Alcotest.check datum_testable "square from alpha"
      (Datum.Fixnum 25) (eval inst_alpha "(square 5)");
    let inst_beta = Session.revert s2 "beta" in
    Alcotest.check datum_testable "y from beta"
      (Datum.Fixnum 99) (eval inst_beta "y"))

let test_load_nonexistent () =
  Alcotest.check_raises "file not found"
    (Session.Session_error "session file not found: /nonexistent/path.session")
    (fun () -> ignore (Session.load "/nonexistent/path.session"))

(* --- Property: save/load preserves checkpoint names --- *)

let prop_save_load_preserves_names =
  QCheck2.Test.make ~name:"save/load preserves checkpoint names"
    ~count:50
    QCheck2.Gen.(list_size (int_range 0 5)
      (string_size ~gen:(char_range 'a' 'z') (int_range 1 10)))
    (fun names ->
       let path = Filename.temp_file "bilk_session_prop" ".session" in
       Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ())
         (fun () ->
            let s1 = Session.create () in
            (* Deduplicate names *)
            let seen = Hashtbl.create 8 in
            let unique_names = List.filter (fun name ->
              if Hashtbl.mem seen name then false
              else begin Hashtbl.replace seen name (); true end
            ) names in
            List.iter (fun name ->
              let inst = Instance.create () in
              Session.checkpoint s1 name inst
            ) unique_names;
            Session.save s1 path;
            let s2 = Session.load path in
            Session.list_checkpoints s2 = Session.list_checkpoints s1))

(* --- Test suite --- *)

let () =
  Alcotest.run "Session" [
    "create", [
      Alcotest.test_case "create empty" `Quick test_create_empty;
    ];
    "checkpoint_revert", [
      Alcotest.test_case "checkpoint and revert" `Quick test_checkpoint_and_revert;
      Alcotest.test_case "multiple checkpoints" `Quick test_multiple_checkpoints;
      Alcotest.test_case "duplicate name" `Quick test_checkpoint_duplicate_name;
      Alcotest.test_case "unknown name" `Quick test_revert_unknown_name;
      Alcotest.test_case "revert with closure" `Quick test_revert_with_closure;
    ];
    "save_load", [
      Alcotest.test_case "empty" `Quick test_save_load_empty;
      Alcotest.test_case "round-trip" `Quick test_save_load_round_trip;
      Alcotest.test_case "nonexistent file" `Quick test_load_nonexistent;
    ];
    "properties", [
      QCheck_alcotest.to_alcotest prop_save_load_preserves_names;
    ];
  ]
