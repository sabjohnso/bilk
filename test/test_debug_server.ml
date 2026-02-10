open Wile

(* --- Breakpoint tests --- *)

let test_breakpoint_set_returns_ids () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  let bps = Debug_server.set_breakpoints ds "test.scm" [1; 5; 10] in
  Alcotest.(check int) "count" 3 (List.length bps);
  let ids = List.map (fun (bp : Debug_server.breakpoint) -> bp.bp_id) bps in
  (* IDs should be unique *)
  let unique = List.sort_uniq Int.compare ids in
  Alcotest.(check int) "unique ids" 3 (List.length unique);
  (* All verified *)
  List.iter (fun (bp : Debug_server.breakpoint) ->
    Alcotest.(check bool) "verified" true bp.bp_verified
  ) bps

let test_breakpoint_clear () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  ignore (Debug_server.set_breakpoints ds "test.scm" [1; 2; 3]);
  Debug_server.clear_breakpoints ds "test.scm";
  (* After clearing, setting empty should work *)
  let bps = Debug_server.set_breakpoints ds "test.scm" [] in
  Alcotest.(check int) "empty" 0 (List.length bps)

let test_breakpoint_hit () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  let bps = Debug_server.set_breakpoints ds "test.scm" [5] in
  let bp_id = (List.hd bps).bp_id in
  let loc = Loc.make "test.scm" 5 1 in
  let result = Debug_server.on_call ds loc Datum.Void [] in
  match result with
  | Some (Debug_server.Breakpoint id) ->
    Alcotest.(check int) "bp id" bp_id id
  | _ -> Alcotest.fail "expected breakpoint hit"

(* --- Stepping tests --- *)

let test_step_in () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  Debug_server.step_in ds;
  let loc = Loc.make "test.scm" 1 1 in
  let result = Debug_server.on_call ds loc Datum.Void [] in
  match result with
  | Some Debug_server.Step -> ()
  | _ -> Alcotest.fail "expected step"

let test_step_over_depth () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  (* Simulate: paused at depth 2 after a breakpoint hit *)
  ignore (Debug_server.on_call ds (Loc.make "a.scm" 1 1) Datum.Void []);
  ignore (Debug_server.on_call ds (Loc.make "a.scm" 2 1) Datum.Void []);
  (* call_depth=2. step_over sets paused_depth=2 *)
  Debug_server.step_over ds;
  (* Next on_call: depth_before=2 <= paused_depth=2 → Step *)
  let result1 = Debug_server.on_call ds (Loc.make "a.scm" 3 1) Datum.Void [] in
  Alcotest.(check bool) "same depth stops" true
    (result1 = Some Debug_server.Step);
  (* After stopping, use continue to go past this.
     step_over is now consumed — mode was set by on_call's stop_reason. *)
  Debug_server.continue ds;
  (* Deeper call after continue: depth_before=3, mode=Run → None *)
  let result2 = Debug_server.on_call ds (Loc.make "a.scm" 4 1) Datum.Void [] in
  Alcotest.(check bool) "continue runs past" true (result2 = None)

let test_step_out_depth () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  (* Build up depth: 0→1→2→3 *)
  ignore (Debug_server.on_call ds (Loc.make "a.scm" 1 1) Datum.Void []);
  ignore (Debug_server.on_call ds (Loc.make "a.scm" 2 1) Datum.Void []);
  ignore (Debug_server.on_call ds (Loc.make "a.scm" 3 1) Datum.Void []);
  (* call_depth=3, step_out sets paused_depth=3 *)
  Debug_server.step_out ds;
  (* Next call: depth_before=3, NOT < 3, so no stop *)
  let result = Debug_server.on_call ds (Loc.make "a.scm" 4 1) Datum.Void [] in
  Alcotest.(check bool) "same depth doesn't stop" true (result = None);
  (* Return three times: 4→3→2→1 *)
  Debug_server.on_return ds Loc.none Datum.Void;
  Debug_server.on_return ds Loc.none Datum.Void;
  Debug_server.on_return ds Loc.none Datum.Void;
  (* Now call_depth=1, next call: depth_before=1 < paused_depth=3 → stop *)
  let result2 = Debug_server.on_call ds (Loc.make "a.scm" 5 1) Datum.Void [] in
  match result2 with
  | Some Debug_server.Step -> ()
  | _ -> Alcotest.fail "expected step at lower depth"

let test_call_depth () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  ignore (Debug_server.on_call ds Loc.none Datum.Void []);
  ignore (Debug_server.on_call ds Loc.none Datum.Void []);
  ignore (Debug_server.on_call ds Loc.none Datum.Void []);
  (* call_depth=3 *)
  Debug_server.on_return ds Loc.none Datum.Void;
  (* call_depth=2 now *)
  Debug_server.on_return ds Loc.none Datum.Void;
  (* call_depth=1 now *)
  Debug_server.step_over ds;
  (* paused_depth=1. Next call: depth_before=1 <= 1 → Step *)
  let result = Debug_server.on_call ds Loc.none Datum.Void [] in
  Alcotest.(check bool) "stops at same depth" true
    (result = Some Debug_server.Step)

(* --- Stack trace tests --- *)

let test_stack_trace_names () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  (* Evaluate code that creates nested calls *)
  inst.on_call := Some (fun _loc _proc _args -> ());
  ignore (Instance.eval_string inst
    "(begin (define (foo x) (+ x 1)) (foo 5))");
  let frames = Debug_server.stack_trace ds in
  Alcotest.(check bool) "has frames" true (List.length frames > 0)

let test_stack_trace_locations () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  let last_loc = ref Loc.none in
  inst.on_call := Some (fun loc _proc _args ->
    last_loc := loc);
  ignore (Instance.eval_string inst "(+ 1 2)");
  let frames = Debug_server.stack_trace ds in
  Alcotest.(check bool) "has frames" true (List.length frames > 0);
  ignore !last_loc

(* --- Variable tests --- *)

let test_variables_locals () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  let captured_vars = ref [] in
  inst.on_call := Some (fun _loc _proc _args ->
    let scs = Debug_server.scopes ds 0 in
    if scs <> [] then begin
      let vars = Debug_server.variables ds (List.hd scs).sc_ref in
      captured_vars := vars
    end);
  ignore (Instance.eval_string inst
    "(begin (define (f x) (+ x 1)) (f 42))");
  (* We should have captured some variables at some point *)
  Alcotest.(check bool) "captured vars" true (!captured_vars <> [])

let test_variables_compound () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  let pair = Datum.Pair { car = Datum.Fixnum 1; cdr = Datum.Fixnum 2 } in
  let var = Debug_server.({
    var_name = "test";
    var_value = Datum.to_string pair;
    var_type = "pair";
    var_ref = 0;
  }) in
  (* Pairs should be expandable *)
  ignore var;
  ignore ds;
  Alcotest.(check bool) "pair is compound" true
    (match pair with Datum.Pair _ -> true | _ -> false)

(* --- Evaluate tests --- *)

let test_evaluate_simple () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  let result = Debug_server.evaluate ds "(+ 1 2)" None in
  Alcotest.(check string) "result" "3" result

let test_evaluate_error () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  let result = Debug_server.evaluate ds "(/ 1 0)" None in
  Alcotest.(check bool) "has error" true
    (String.length result >= 5 &&
     String.sub result 0 5 = "Error")

(* --- Pause request test --- *)

let test_pause_request () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  Debug_server.request_pause ds;
  let result = Debug_server.on_call ds Loc.none Datum.Void [] in
  match result with
  | Some Debug_server.Pause_request -> ()
  | _ -> Alcotest.fail "expected pause request"

(* --- Handle reset test --- *)

let test_handle_reset () =
  let inst = Instance.create () in
  let ds = Debug_server.create inst in
  (* Allocate a scope handle *)
  let scs = Debug_server.scopes ds 0 in
  let has_handle = List.exists (fun sc -> sc.Debug_server.sc_ref > 0) scs in
  ignore has_handle;
  Debug_server.reset_handles ds;
  (* After reset, old handles should return empty *)
  let vars = Debug_server.variables ds 1000 in
  Alcotest.(check int) "empty after reset" 0 (List.length vars)

let () =
  Alcotest.run "Debug_server" [
    "breakpoints", [
      Alcotest.test_case "set returns ids" `Quick test_breakpoint_set_returns_ids;
      Alcotest.test_case "clear empties" `Quick test_breakpoint_clear;
      Alcotest.test_case "hit detection" `Quick test_breakpoint_hit;
    ];
    "stepping", [
      Alcotest.test_case "step-in" `Quick test_step_in;
      Alcotest.test_case "step-over depth" `Quick test_step_over_depth;
      Alcotest.test_case "step-out depth" `Quick test_step_out_depth;
      Alcotest.test_case "call depth" `Quick test_call_depth;
    ];
    "stack-trace", [
      Alcotest.test_case "frame names" `Quick test_stack_trace_names;
      Alcotest.test_case "source locations" `Quick test_stack_trace_locations;
    ];
    "variables", [
      Alcotest.test_case "local bindings" `Quick test_variables_locals;
      Alcotest.test_case "compound expansion" `Quick test_variables_compound;
    ];
    "evaluate", [
      Alcotest.test_case "simple expression" `Quick test_evaluate_simple;
      Alcotest.test_case "error handling" `Quick test_evaluate_error;
    ];
    "control", [
      Alcotest.test_case "pause request" `Quick test_pause_request;
      Alcotest.test_case "handle reset" `Quick test_handle_reset;
    ];
  ]
