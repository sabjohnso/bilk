open Wile

(* -- Helpers -- *)

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let datum_opt_testable =
  Alcotest.option datum_testable

let check_datum_opt = Alcotest.check datum_opt_testable

let tbl = Symbol.create_table ()
let sym name = Symbol.intern tbl name

(* -- Unit tests -- *)

let test_lookup_empty () =
  let e = Env.empty () in
  check_datum_opt "not found" None (Env.lookup e (sym "x"))

let test_define_then_lookup () =
  let e = Env.empty () in
  let x = sym "x" in
  Env.define e x (Datum.Fixnum 42);
  check_datum_opt "found" (Some (Datum.Fixnum 42)) (Env.lookup e x)

let test_extend_with_bindings () =
  let e = Env.empty () in
  let a = sym "a" in
  let b = sym "b" in
  let e2 = Env.extend e [ (a, Datum.Fixnum 1); (b, Datum.Fixnum 2) ] in
  check_datum_opt "a" (Some (Datum.Fixnum 1)) (Env.lookup e2 a);
  check_datum_opt "b" (Some (Datum.Fixnum 2)) (Env.lookup e2 b)

let test_shadowing () =
  let e = Env.empty () in
  let x = sym "x" in
  Env.define e x (Datum.Fixnum 1);
  let e2 = Env.extend e [ (x, Datum.Fixnum 2) ] in
  check_datum_opt "inner shadows" (Some (Datum.Fixnum 2)) (Env.lookup e2 x);
  check_datum_opt "outer unchanged" (Some (Datum.Fixnum 1)) (Env.lookup e x)

let test_set_mutates () =
  let e = Env.empty () in
  let x = sym "x" in
  Env.define e x (Datum.Fixnum 1);
  Env.set e x (Datum.Fixnum 99);
  check_datum_opt "mutated" (Some (Datum.Fixnum 99)) (Env.lookup e x)

let test_set_unbound_raises () =
  let e = Env.empty () in
  let x = sym "x" in
  Alcotest.check_raises "unbound"
    (Env.Unbound_variable x)
    (fun () -> Env.set e x (Datum.Fixnum 1))

let test_define_replaces () =
  let e = Env.empty () in
  let x = sym "x" in
  Env.define e x (Datum.Fixnum 1);
  Env.define e x (Datum.Fixnum 2);
  check_datum_opt "replaced" (Some (Datum.Fixnum 2)) (Env.lookup e x)

let test_chain_lookup () =
  let e = Env.empty () in
  let x = sym "x" in
  let y = sym "y" in
  Env.define e x (Datum.Fixnum 1);
  let e2 = Env.extend e [ (y, Datum.Fixnum 2) ] in
  check_datum_opt "x from outer" (Some (Datum.Fixnum 1)) (Env.lookup e2 x);
  check_datum_opt "y from inner" (Some (Datum.Fixnum 2)) (Env.lookup e2 y);
  check_datum_opt "z not found" None (Env.lookup e2 (sym "z"))

let test_set_in_outer_frame () =
  let e = Env.empty () in
  let x = sym "x" in
  Env.define e x (Datum.Fixnum 1);
  let e2 = Env.extend e [ (sym "y", Datum.Fixnum 2) ] in
  Env.set e2 x (Datum.Fixnum 99);
  check_datum_opt "mutated via inner" (Some (Datum.Fixnum 99)) (Env.lookup e2 x);
  check_datum_opt "visible from outer" (Some (Datum.Fixnum 99)) (Env.lookup e x)

let test_define_in_inner_frame () =
  let e = Env.empty () in
  let x = sym "x" in
  Env.define e x (Datum.Fixnum 1);
  let e2 = Env.extend e [] in
  Env.define e2 x (Datum.Fixnum 2);
  check_datum_opt "inner has new" (Some (Datum.Fixnum 2)) (Env.lookup e2 x);
  check_datum_opt "outer unchanged" (Some (Datum.Fixnum 1)) (Env.lookup e x)

let test_lookup_slot_found () =
  let e = Env.empty () in
  let x = sym "x" in
  Env.define e x (Datum.Fixnum 42);
  match Env.lookup_slot e x with
  | Some slot ->
    Alcotest.(check int) "slot value" 42
      (match !slot with Datum.Fixnum n -> n | _ -> -1)
  | None -> Alcotest.fail "expected Some slot"

let test_lookup_slot_not_found () =
  let e = Env.empty () in
  match Env.lookup_slot e (sym "z") with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let test_define_slot_new () =
  let e = Env.empty () in
  let x = sym "x" in
  let slot = ref (Datum.Fixnum 99) in
  Env.define_slot e x slot;
  match Env.lookup_slot e x with
  | Some s ->
    Alcotest.(check bool) "same ref" true (s == slot)
  | None -> Alcotest.fail "expected Some slot"

let test_slot_sharing () =
  let env_a = Env.empty () in
  let env_b = Env.empty () in
  let x_a = sym "x" in
  let x_b = sym "x" in
  Env.define env_a x_a (Datum.Fixnum 1);
  let slot = match Env.lookup_slot env_a x_a with
    | Some s -> s
    | None -> Alcotest.fail "expected slot"
  in
  Env.define_slot env_b x_b slot;
  (* mutate via env_a *)
  Env.set env_a x_a (Datum.Fixnum 42);
  (* observe via env_b *)
  check_datum_opt "shared slot" (Some (Datum.Fixnum 42)) (Env.lookup env_b x_b)

let () =
  Alcotest.run "Env"
    [ ("Env",
       [ Alcotest.test_case "lookup empty" `Quick test_lookup_empty
       ; Alcotest.test_case "define then lookup" `Quick test_define_then_lookup
       ; Alcotest.test_case "extend with bindings" `Quick test_extend_with_bindings
       ; Alcotest.test_case "shadowing" `Quick test_shadowing
       ; Alcotest.test_case "set mutates" `Quick test_set_mutates
       ; Alcotest.test_case "set unbound raises" `Quick test_set_unbound_raises
       ; Alcotest.test_case "define replaces" `Quick test_define_replaces
       ; Alcotest.test_case "chain lookup" `Quick test_chain_lookup
       ; Alcotest.test_case "set in outer frame" `Quick test_set_in_outer_frame
       ; Alcotest.test_case "define in inner frame" `Quick test_define_in_inner_frame
       ])
    ; ("Slots",
       [ Alcotest.test_case "lookup_slot found" `Quick test_lookup_slot_found
       ; Alcotest.test_case "lookup_slot not found" `Quick test_lookup_slot_not_found
       ; Alcotest.test_case "define_slot new" `Quick test_define_slot_new
       ; Alcotest.test_case "slot sharing" `Quick test_slot_sharing
       ])
    ]
