open Wile

(* --- Helpers --- *)

let eval inst s = ignore (Instance.eval_string inst s)

let fresh () =
  let inst = Instance.create () in
  let prof = Profiler.create () in
  Profiler.install prof inst;
  (inst, prof)

let finish inst prof =
  Profiler.uninstall inst;
  Profiler.finalize prof

let find_entry prof name =
  List.find_opt (fun ((pk : Profiler.proc_key), _) ->
    pk.pk_name = name) (Profiler.entries prof)

(* --- Tests --- *)

let test_create_install_uninstall () =
  let inst = Instance.create () in
  let prof = Profiler.create () in
  Alcotest.(check bool) "on_call initially None"
    true (!(inst.on_call) = None);
  Profiler.install prof inst;
  Alcotest.(check bool) "on_call installed"
    true (!(inst.on_call) <> None);
  Alcotest.(check bool) "on_return installed"
    true (!(inst.on_return) <> None);
  Alcotest.(check bool) "debug_state installed"
    true (!(inst.debug_state) <> None);
  Profiler.uninstall inst;
  Alcotest.(check bool) "on_call removed"
    true (!(inst.on_call) = None);
  Alcotest.(check bool) "on_return removed"
    true (!(inst.on_return) = None);
  Alcotest.(check bool) "debug_state removed"
    true (!(inst.debug_state) = None)

let test_double_install_raises () =
  let inst = Instance.create () in
  let prof = Profiler.create () in
  Profiler.install prof inst;
  Alcotest.check_raises "double install"
    (Profiler.Profiler_error "hooks already installed on instance")
    (fun () -> Profiler.install prof inst);
  Profiler.uninstall inst

let test_primitive_counting () =
  let (inst, prof) = fresh () in
  eval inst "(+ 1 2)";
  eval inst "(+ 3 4)";
  eval inst "(+ 5 6)";
  finish inst prof;
  match find_entry prof "+" with
  | Some (_, stats) ->
    Alcotest.(check int) "call count" 3 stats.call_count
  | None -> Alcotest.fail "expected entry for +"

let test_closure_counting () =
  let (inst, prof) = fresh () in
  eval inst "(define (f x) (+ x 1))";
  eval inst "(f 1)";
  eval inst "(f 2)";
  finish inst prof;
  match find_entry prof "f" with
  | Some (_, stats) ->
    Alcotest.(check int) "call count" 2 stats.call_count
  | None -> Alcotest.fail "expected entry for f"

let test_closure_has_location () =
  let (inst, prof) = fresh () in
  eval inst "(define (g x) x)";
  eval inst "(g 42)";
  finish inst prof;
  match find_entry prof "g" with
  | Some (pk, _) ->
    Alcotest.(check bool) "location not Loc.none"
      false (Loc.equal pk.pk_loc Loc.none)
  | None -> Alcotest.fail "expected entry for g"

let test_primitive_has_loc_none () =
  let (inst, prof) = fresh () in
  eval inst "(+ 1 2)";
  finish inst prof;
  match find_entry prof "+" with
  | Some (pk, _) ->
    Alcotest.(check bool) "location is Loc.none"
      true (Loc.equal pk.pk_loc Loc.none)
  | None -> Alcotest.fail "expected entry for +"

let test_timing_self_vs_total () =
  let (inst, prof) = fresh () in
  (* inner does some work, outer calls inner *)
  eval inst "(define (inner) (let loop ((i 0)) (if (< i 100) (loop (+ i 1)) i)))";
  eval inst "(define (outer) (inner))";
  eval inst "(outer)";
  finish inst prof;
  match find_entry prof "outer" with
  | Some (_, stats) ->
    (* outer's self time should be less than its total time because
       most time is spent in inner *)
    Alcotest.(check bool) "total_time > 0" true (stats.total_time > 0.0);
    Alcotest.(check bool) "self < total" true (stats.self_time <= stats.total_time)
  | None -> Alcotest.fail "expected entry for outer"

let test_tail_call_no_stack_leak () =
  let (inst, prof) = fresh () in
  (* A tail-recursive loop: should not build up unbounded stack entries *)
  eval inst "(define (loop n) (if (= n 0) 'done (loop (- n 1))))";
  eval inst "(loop 1000)";
  finish inst prof;
  match find_entry prof "loop" with
  | Some (_, stats) ->
    Alcotest.(check int) "call count" 1001 stats.call_count;
    Alcotest.(check bool) "total_time >= 0" true (stats.total_time >= 0.0)
  | None -> Alcotest.fail "expected entry for loop"

let test_recursive_calls () =
  let (inst, prof) = fresh () in
  eval inst "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))";
  eval inst "(fib 10)";
  finish inst prof;
  match find_entry prof "fib" with
  | Some (_, stats) ->
    (* fib(10) = 177 calls *)
    Alcotest.(check int) "call count" 177 stats.call_count;
    Alcotest.(check bool) "total > 0" true (stats.total_time > 0.0);
    Alcotest.(check bool) "self > 0" true (stats.self_time > 0.0)
  | None -> Alcotest.fail "expected entry for fib"

let test_trace_events () =
  let (inst, prof) = fresh () in
  eval inst "(define (f) 42)";
  eval inst "(f)";
  finish inst prof;
  let events = Profiler.trace_events prof in
  (* Should have at least one B and one E for f *)
  let bs = List.filter (fun (e : Profiler.trace_event) ->
    e.te_ph = 'B' && e.te_name = "f") events in
  let es = List.filter (fun (e : Profiler.trace_event) ->
    e.te_ph = 'E' && e.te_name = "f") events in
  Alcotest.(check bool) "has B event" true (List.length bs > 0);
  Alcotest.(check bool) "has E event" true (List.length es > 0);
  (* Timestamps should be non-negative *)
  List.iter (fun (e : Profiler.trace_event) ->
    Alcotest.(check bool) "ts >= 0" true (e.te_ts >= 0.0)
  ) events

let test_flame_stacks () =
  let (inst, prof) = fresh () in
  eval inst "(define (inner) 42)";
  eval inst "(define (outer) (inner))";
  eval inst "(outer)";
  finish inst prof;
  let stacks = Profiler.flame_stacks prof in
  Alcotest.(check bool) "has entries" true (List.length stacks > 0);
  (* At least one entry should contain "outer" *)
  let has_outer = List.exists (fun (path, _) ->
    let parts = String.split_on_char ';' path in
    List.mem "outer" parts) stacks in
  Alcotest.(check bool) "has outer in stacks" true has_outer

let test_empty_profile () =
  let prof = Profiler.create () in
  Profiler.finalize prof;
  Alcotest.(check int) "no entries" 0 (List.length (Profiler.entries prof));
  Alcotest.(check int) "no trace events" 0 (List.length (Profiler.trace_events prof));
  Alcotest.(check int) "no flame stacks" 0 (List.length (Profiler.flame_stacks prof))

let () =
  Alcotest.run "Profiler" [
    "lifecycle", [
      Alcotest.test_case "create/install/uninstall" `Quick
        test_create_install_uninstall;
      Alcotest.test_case "double install raises" `Quick
        test_double_install_raises;
    ];
    "counting", [
      Alcotest.test_case "primitive counting" `Quick
        test_primitive_counting;
      Alcotest.test_case "closure counting" `Quick
        test_closure_counting;
      Alcotest.test_case "closure has location" `Quick
        test_closure_has_location;
      Alcotest.test_case "primitive has Loc.none" `Quick
        test_primitive_has_loc_none;
    ];
    "timing", [
      Alcotest.test_case "self vs total" `Quick
        test_timing_self_vs_total;
      Alcotest.test_case "tail call no stack leak" `Quick
        test_tail_call_no_stack_leak;
      Alcotest.test_case "recursive calls" `Quick
        test_recursive_calls;
    ];
    "output", [
      Alcotest.test_case "trace events" `Quick
        test_trace_events;
      Alcotest.test_case "flame stacks" `Quick
        test_flame_stacks;
      Alcotest.test_case "empty profile" `Quick
        test_empty_profile;
    ];
  ]
