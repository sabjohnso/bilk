open Wile

let check_bool = Alcotest.(check bool)

let cs_make () = Bytes.make 32 '\x00'
let cs_set bits n =
  Bytes.set bits (n / 8)
    (Char.chr (Char.code (Bytes.get bits (n / 8)) lor (1 lsl (n mod 8))))

(* Helper: match a node against a full string *)
let full_match node str =
  let s = Bytes.of_string str in
  let compiled = Regexp_engine.compile (Regexp_engine.Group (0, node)) 1 in
  let result = Regexp_engine.exec compiled s 0 (Bytes.length s) in
  result.matched

(* Helper: search for a node in a string *)
let search_match node str =
  let s = Bytes.of_string str in
  let compiled = Regexp_engine.compile (Regexp_engine.Group (0, node)) 1 in
  let result = Regexp_engine.search compiled s 0 (Bytes.length s) in
  result

(* --- Literal tests --- *)

let test_lit_match () =
  check_bool "lit match" true
    (full_match (Regexp_engine.Lit (Char.code 'a')) "a")

let test_lit_no_match () =
  check_bool "lit no match" false
    (full_match (Regexp_engine.Lit (Char.code 'a')) "b")

let test_lit_too_long () =
  check_bool "lit too long" false
    (full_match (Regexp_engine.Lit (Char.code 'a')) "ab")

(* --- Sequence tests --- *)

let test_seq_match () =
  check_bool "seq match" true
    (full_match (Regexp_engine.Seq [Lit (Char.code 'a'); Lit (Char.code 'b')]) "ab")

let test_seq_no_match () =
  check_bool "seq no match" false
    (full_match (Regexp_engine.Seq [Lit (Char.code 'a'); Lit (Char.code 'b')]) "ac")

let test_seq_empty () =
  check_bool "seq empty" true
    (full_match (Regexp_engine.Seq []) "")

(* --- Any tests --- *)

let test_any_match () =
  check_bool "any match" true
    (full_match Regexp_engine.Any "x")

let test_any_newline () =
  check_bool "any doesn't match newline" false
    (full_match Regexp_engine.Any "\n")

let test_any_empty () =
  check_bool "any empty" false
    (full_match Regexp_engine.Any "")

(* --- Class tests --- *)

let test_class_match () =
  let bits = cs_make () in
  cs_set bits (Char.code 'a');
  cs_set bits (Char.code 'b');
  check_bool "class match" true
    (full_match (Regexp_engine.Class bits) "a")

let test_class_no_match () =
  let bits = cs_make () in
  cs_set bits (Char.code 'a');
  check_bool "class no match" false
    (full_match (Regexp_engine.Class bits) "b")

(* --- Alternation tests --- *)

let test_alt_first () =
  check_bool "alt first" true
    (full_match (Regexp_engine.Alt (Lit (Char.code 'a'), Lit (Char.code 'b'))) "a")

let test_alt_second () =
  check_bool "alt second" true
    (full_match (Regexp_engine.Alt (Lit (Char.code 'a'), Lit (Char.code 'b'))) "b")

let test_alt_neither () =
  check_bool "alt neither" false
    (full_match (Regexp_engine.Alt (Lit (Char.code 'a'), Lit (Char.code 'b'))) "c")

(* --- Repetition tests --- *)

let test_star_empty () =
  check_bool "star empty" true
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 0, None, true)) "")

let test_star_multiple () =
  check_bool "star multiple" true
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 0, None, true)) "aaa")

let test_plus_one () =
  check_bool "plus one" true
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 1, None, true)) "a")

let test_plus_empty () =
  check_bool "plus empty fails" false
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 1, None, true)) "")

let test_question_present () =
  check_bool "question present" true
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 0, Some 1, true)) "a")

let test_question_absent () =
  check_bool "question absent" true
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 0, Some 1, true)) "")

let test_exact_count () =
  check_bool "exact count" true
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 3, Some 3, true)) "aaa")

let test_exact_count_wrong () =
  check_bool "exact count wrong" false
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 3, Some 3, true)) "aa")

let test_range () =
  check_bool "range 2-4 with 3" true
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 2, Some 4, true)) "aaa")

let test_range_too_few () =
  check_bool "range 2-4 with 1" false
    (full_match (Regexp_engine.Rep (Lit (Char.code 'a'), 2, Some 4, true)) "a")

(* --- Anchor tests --- *)

let test_bos () =
  check_bool "bos" true
    (full_match (Regexp_engine.Seq [Bos; Lit (Char.code 'a')]) "a")

let test_eos () =
  check_bool "eos" true
    (full_match (Regexp_engine.Seq [Lit (Char.code 'a'); Eos]) "a")

(* --- Group tests --- *)

let test_group () =
  let s = Bytes.of_string "abc" in
  let node = Regexp_engine.Seq [
    Lit (Char.code 'a');
    Group (1, Lit (Char.code 'b'));
    Lit (Char.code 'c');
  ] in
  let compiled = Regexp_engine.compile (Regexp_engine.Group (0, node)) 2 in
  let result = Regexp_engine.exec compiled s 0 3 in
  check_bool "group matched" true result.matched;
  (match result.groups.(1) with
   | Some (1, 2) -> ()
   | _ -> Alcotest.fail "expected group 1 = (1, 2)")

(* --- Search tests --- *)

let test_search_found () =
  let result = search_match (Regexp_engine.Lit (Char.code 'c')) "abcde" in
  check_bool "search found" true result.matched;
  (match result.groups.(0) with
   | Some (2, 3) -> ()
   | _ -> Alcotest.fail "expected match at (2, 3)")

let test_search_not_found () =
  let result = search_match (Regexp_engine.Lit (Char.code 'z')) "abcde" in
  check_bool "search not found" false result.matched

let test_search_multi_char () =
  let result = search_match
    (Regexp_engine.Seq [Lit (Char.code 'c'); Lit (Char.code 'd')]) "abcde" in
  check_bool "search multi" true result.matched;
  (match result.groups.(0) with
   | Some (2, 4) -> ()
   | _ -> Alcotest.fail "expected match at (2, 4)")

(* --- NClass tests --- *)

let test_nclass () =
  let bits = cs_make () in
  cs_set bits (Char.code 'a');
  check_bool "nclass matches non-a" true
    (full_match (Regexp_engine.NClass bits) "b")

let test_nclass_excluded () =
  let bits = cs_make () in
  cs_set bits (Char.code 'a');
  check_bool "nclass excludes a" false
    (full_match (Regexp_engine.NClass bits) "a")

let () =
  Alcotest.run "Regexp_engine"
    [ ("literal",
       [ Alcotest.test_case "match" `Quick test_lit_match
       ; Alcotest.test_case "no match" `Quick test_lit_no_match
       ; Alcotest.test_case "too long" `Quick test_lit_too_long
       ])
    ; ("sequence",
       [ Alcotest.test_case "match" `Quick test_seq_match
       ; Alcotest.test_case "no match" `Quick test_seq_no_match
       ; Alcotest.test_case "empty" `Quick test_seq_empty
       ])
    ; ("any",
       [ Alcotest.test_case "match" `Quick test_any_match
       ; Alcotest.test_case "newline" `Quick test_any_newline
       ; Alcotest.test_case "empty" `Quick test_any_empty
       ])
    ; ("class",
       [ Alcotest.test_case "match" `Quick test_class_match
       ; Alcotest.test_case "no match" `Quick test_class_no_match
       ; Alcotest.test_case "nclass match" `Quick test_nclass
       ; Alcotest.test_case "nclass excluded" `Quick test_nclass_excluded
       ])
    ; ("alternation",
       [ Alcotest.test_case "first" `Quick test_alt_first
       ; Alcotest.test_case "second" `Quick test_alt_second
       ; Alcotest.test_case "neither" `Quick test_alt_neither
       ])
    ; ("repetition",
       [ Alcotest.test_case "star empty" `Quick test_star_empty
       ; Alcotest.test_case "star multiple" `Quick test_star_multiple
       ; Alcotest.test_case "plus one" `Quick test_plus_one
       ; Alcotest.test_case "plus empty" `Quick test_plus_empty
       ; Alcotest.test_case "question present" `Quick test_question_present
       ; Alcotest.test_case "question absent" `Quick test_question_absent
       ; Alcotest.test_case "exact count" `Quick test_exact_count
       ; Alcotest.test_case "exact count wrong" `Quick test_exact_count_wrong
       ; Alcotest.test_case "range" `Quick test_range
       ; Alcotest.test_case "range too few" `Quick test_range_too_few
       ])
    ; ("anchors",
       [ Alcotest.test_case "bos" `Quick test_bos
       ; Alcotest.test_case "eos" `Quick test_eos
       ])
    ; ("groups",
       [ Alcotest.test_case "submatch" `Quick test_group
       ])
    ; ("search",
       [ Alcotest.test_case "found" `Quick test_search_found
       ; Alcotest.test_case "not found" `Quick test_search_not_found
       ; Alcotest.test_case "multi char" `Quick test_search_multi_char
       ])
    ]
