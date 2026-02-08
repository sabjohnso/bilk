open Wile

(* --- Helpers --- *)

let v major minor patch : Semver.t = { major; minor; patch }

let semver_testable : Semver.t Alcotest.testable =
  Alcotest.testable
    (fun fmt v -> Format.pp_print_string fmt (Semver.to_string v))
    Semver.equal

let semver_option = Alcotest.option semver_testable

(* --- Parse tests --- *)

let test_parse_simple () =
  Alcotest.check semver_testable "1.2.3"
    (v 1 2 3) (Semver.parse "1.2.3")

let test_parse_zeros () =
  Alcotest.check semver_testable "0.0.0"
    (v 0 0 0) (Semver.parse "0.0.0")

let test_parse_large () =
  Alcotest.check semver_testable "100.200.300"
    (v 100 200 300) (Semver.parse "100.200.300")

let test_parse_no_dots () =
  Alcotest.check_raises "no dots"
    (Semver.Parse_error "invalid version format: 123")
    (fun () -> ignore (Semver.parse "123"))

let test_parse_two_parts () =
  Alcotest.check_raises "two parts"
    (Semver.Parse_error "invalid version format: 1.2")
    (fun () -> ignore (Semver.parse "1.2"))

let test_parse_four_parts () =
  Alcotest.check_raises "four parts"
    (Semver.Parse_error "invalid version format: 1.2.3.4")
    (fun () -> ignore (Semver.parse "1.2.3.4"))

let test_parse_non_numeric () =
  Alcotest.check_raises "non-numeric"
    (Semver.Parse_error "invalid version: 1.a.3")
    (fun () -> ignore (Semver.parse "1.a.3"))

let test_parse_empty () =
  Alcotest.check_raises "empty"
    (Semver.Parse_error "invalid version format: ")
    (fun () -> ignore (Semver.parse ""))

let test_parse_negative () =
  Alcotest.check_raises "negative"
    (Semver.Parse_error "negative component in version: 1.-1.0")
    (fun () -> ignore (Semver.parse "1.-1.0"))

(* --- to_string tests --- *)

let test_to_string () =
  Alcotest.(check string) "to_string"
    "1.2.3" (Semver.to_string (v 1 2 3))

let test_to_string_zeros () =
  Alcotest.(check string) "to_string zeros"
    "0.0.0" (Semver.to_string (v 0 0 0))

(* --- Compare tests --- *)

let test_compare_equal () =
  Alcotest.(check int) "equal" 0
    (Semver.compare (v 1 2 3) (v 1 2 3))

let test_compare_major () =
  Alcotest.(check bool) "major less" true
    (Semver.compare (v 1 0 0) (v 2 0 0) < 0)

let test_compare_minor () =
  Alcotest.(check bool) "minor less" true
    (Semver.compare (v 1 2 0) (v 1 3 0) < 0)

let test_compare_patch () =
  Alcotest.(check bool) "patch less" true
    (Semver.compare (v 1 2 3) (v 1 2 4) < 0)

let test_compare_major_dominates () =
  Alcotest.(check bool) "major dominates" true
    (Semver.compare (v 2 0 0) (v 1 9 9) > 0)

(* --- parse_constraint tests --- *)

let test_parse_constraint_eq () =
  Alcotest.(check bool) "=" true
    (Semver.parse_constraint "=" = Semver.Eq)

let test_parse_constraint_lt () =
  Alcotest.(check bool) "<" true
    (Semver.parse_constraint "<" = Semver.Lt)

let test_parse_constraint_le () =
  Alcotest.(check bool) "<=" true
    (Semver.parse_constraint "<=" = Semver.Le)

let test_parse_constraint_gt () =
  Alcotest.(check bool) ">" true
    (Semver.parse_constraint ">" = Semver.Gt)

let test_parse_constraint_ge () =
  Alcotest.(check bool) ">=" true
    (Semver.parse_constraint ">=" = Semver.Ge)

let test_parse_constraint_invalid () =
  Alcotest.check_raises "invalid op"
    (Semver.Parse_error "invalid constraint operator: !=")
    (fun () -> ignore (Semver.parse_constraint "!="))

(* --- satisfies tests --- *)

let test_satisfies_empty () =
  Alcotest.(check bool) "empty constraints" true
    (Semver.satisfies (v 1 0 0) [])

let test_satisfies_eq () =
  Alcotest.(check bool) "eq match" true
    (Semver.satisfies (v 1 0 0) [(Semver.Eq, v 1 0 0)])

let test_satisfies_eq_no () =
  Alcotest.(check bool) "eq no match" false
    (Semver.satisfies (v 1 0 1) [(Semver.Eq, v 1 0 0)])

let test_satisfies_ge_lt () =
  Alcotest.(check bool) ">= 1.0.0 AND < 2.0.0" true
    (Semver.satisfies (v 1 5 0) [(Semver.Ge, v 1 0 0); (Semver.Lt, v 2 0 0)])

let test_satisfies_ge_lt_no () =
  Alcotest.(check bool) ">= 1.0.0 AND < 2.0.0 (too high)" false
    (Semver.satisfies (v 2 0 0) [(Semver.Ge, v 1 0 0); (Semver.Lt, v 2 0 0)])

let test_satisfies_gt () =
  Alcotest.(check bool) "> 1.0.0" true
    (Semver.satisfies (v 1 0 1) [(Semver.Gt, v 1 0 0)])

let test_satisfies_le () =
  Alcotest.(check bool) "<= 2.0.0" true
    (Semver.satisfies (v 2 0 0) [(Semver.Le, v 2 0 0)])

(* --- latest_satisfying tests --- *)

let test_latest_satisfying_found () =
  let versions = [v 1 0 0; v 1 1 0; v 1 2 0; v 2 0 0] in
  Alcotest.check semver_option "latest in range"
    (Some (v 1 2 0))
    (Semver.latest_satisfying versions [(Semver.Ge, v 1 0 0); (Semver.Lt, v 2 0 0)])

let test_latest_satisfying_none () =
  let versions = [v 1 0 0; v 1 1 0] in
  Alcotest.check semver_option "none match"
    None
    (Semver.latest_satisfying versions [(Semver.Ge, v 3 0 0)])

let test_latest_satisfying_empty_versions () =
  Alcotest.check semver_option "empty list"
    None
    (Semver.latest_satisfying [] [(Semver.Ge, v 1 0 0)])

let test_latest_satisfying_no_constraints () =
  let versions = [v 1 0 0; v 2 0 0; v 0 5 0] in
  Alcotest.check semver_option "no constraints = latest"
    (Some (v 2 0 0))
    (Semver.latest_satisfying versions [])

(* --- Round-trip --- *)

let test_parse_to_string_roundtrip () =
  Alcotest.(check string) "roundtrip"
    "1.2.3" (Semver.to_string (Semver.parse "1.2.3"))

(* --- Test suite --- *)

let () =
  Alcotest.run "Semver" [
    "parse", [
      Alcotest.test_case "simple" `Quick test_parse_simple;
      Alcotest.test_case "zeros" `Quick test_parse_zeros;
      Alcotest.test_case "large" `Quick test_parse_large;
      Alcotest.test_case "no dots" `Quick test_parse_no_dots;
      Alcotest.test_case "two parts" `Quick test_parse_two_parts;
      Alcotest.test_case "four parts" `Quick test_parse_four_parts;
      Alcotest.test_case "non-numeric" `Quick test_parse_non_numeric;
      Alcotest.test_case "empty" `Quick test_parse_empty;
      Alcotest.test_case "negative" `Quick test_parse_negative;
    ];
    "to_string", [
      Alcotest.test_case "basic" `Quick test_to_string;
      Alcotest.test_case "zeros" `Quick test_to_string_zeros;
    ];
    "compare", [
      Alcotest.test_case "equal" `Quick test_compare_equal;
      Alcotest.test_case "major" `Quick test_compare_major;
      Alcotest.test_case "minor" `Quick test_compare_minor;
      Alcotest.test_case "patch" `Quick test_compare_patch;
      Alcotest.test_case "major dominates" `Quick test_compare_major_dominates;
    ];
    "parse_constraint", [
      Alcotest.test_case "=" `Quick test_parse_constraint_eq;
      Alcotest.test_case "<" `Quick test_parse_constraint_lt;
      Alcotest.test_case "<=" `Quick test_parse_constraint_le;
      Alcotest.test_case ">" `Quick test_parse_constraint_gt;
      Alcotest.test_case ">=" `Quick test_parse_constraint_ge;
      Alcotest.test_case "invalid" `Quick test_parse_constraint_invalid;
    ];
    "satisfies", [
      Alcotest.test_case "empty" `Quick test_satisfies_empty;
      Alcotest.test_case "eq match" `Quick test_satisfies_eq;
      Alcotest.test_case "eq no match" `Quick test_satisfies_eq_no;
      Alcotest.test_case "ge + lt" `Quick test_satisfies_ge_lt;
      Alcotest.test_case "ge + lt no" `Quick test_satisfies_ge_lt_no;
      Alcotest.test_case "gt" `Quick test_satisfies_gt;
      Alcotest.test_case "le" `Quick test_satisfies_le;
    ];
    "latest_satisfying", [
      Alcotest.test_case "found" `Quick test_latest_satisfying_found;
      Alcotest.test_case "none" `Quick test_latest_satisfying_none;
      Alcotest.test_case "empty versions" `Quick test_latest_satisfying_empty_versions;
      Alcotest.test_case "no constraints" `Quick test_latest_satisfying_no_constraints;
    ];
    "roundtrip", [
      Alcotest.test_case "parse-to_string" `Quick test_parse_to_string_roundtrip;
    ];
  ]
