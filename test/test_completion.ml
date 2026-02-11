open Wile

(* --- Alcotest unit tests --- *)

(* extract_prefix *)

let test_extract_prefix_simple () =
  let (prefix, start) = Completion.extract_prefix "defin" 5 in
  Alcotest.(check string) "prefix" "defin" prefix;
  Alcotest.(check int) "start" 0 start

let test_extract_prefix_after_paren () =
  let (prefix, start) = Completion.extract_prefix "(def" 4 in
  Alcotest.(check string) "prefix" "def" prefix;
  Alcotest.(check int) "start" 1 start

let test_extract_prefix_mid_word () =
  let (prefix, start) = Completion.extract_prefix "foo bar" 3 in
  Alcotest.(check string) "prefix" "foo" prefix;
  Alcotest.(check int) "start" 0 start

let test_extract_prefix_empty () =
  let (prefix, start) = Completion.extract_prefix "" 0 in
  Alcotest.(check string) "prefix" "" prefix;
  Alcotest.(check int) "start" 0 start

let test_extract_prefix_comma () =
  let (prefix, start) = Completion.extract_prefix ",hel" 4 in
  Alcotest.(check string) "prefix" ",hel" prefix;
  Alcotest.(check int) "start" 0 start

let test_extract_prefix_after_space () =
  let (prefix, start) = Completion.extract_prefix "(string-" 8 in
  Alcotest.(check string) "prefix" "string-" prefix;
  Alcotest.(check int) "start" 1 start

(* find_matches *)

let test_find_matches_basic () =
  let candidates = ["define"; "define-syntax"; "display"; "car"] in
  let result = Completion.find_matches "def" candidates in
  Alcotest.(check (list string)) "matches" ["define"; "define-syntax"] result

let test_find_matches_empty_prefix () =
  let candidates = ["b"; "a"; "c"] in
  let result = Completion.find_matches "" candidates in
  Alcotest.(check (list string)) "all sorted" ["a"; "b"; "c"] result

let test_find_matches_no_match () =
  let candidates = ["car"; "cdr"] in
  let result = Completion.find_matches "z" candidates in
  Alcotest.(check (list string)) "none" [] result

let test_find_matches_comma () =
  let candidates = [",help"; ",quit"; ",load"; ",libs"] in
  let result = Completion.find_matches ",l" candidates in
  Alcotest.(check (list string)) "comma matches" [",libs"; ",load"] result

(* common_prefix *)

let test_common_prefix_basic () =
  let result = Completion.common_prefix ["define"; "define-syntax"; "define-record-type"] in
  Alcotest.(check string) "prefix" "define" result

let test_common_prefix_single () =
  let result = Completion.common_prefix ["hello"] in
  Alcotest.(check string) "single" "hello" result

let test_common_prefix_empty_list () =
  let result = Completion.common_prefix [] in
  Alcotest.(check string) "empty" "" result

let test_common_prefix_no_common () =
  let result = Completion.common_prefix ["abc"; "xyz"] in
  Alcotest.(check string) "none" "" result

(* format_columns *)

let test_format_columns_basic () =
  let result = Completion.format_columns ~width:40 ["abc"; "defgh"; "ij"] in
  (* Should contain all candidates *)
  Alcotest.(check bool) "has abc" true (String.length result > 0);
  List.iter (fun s ->
    Alcotest.(check bool) (s ^ " present") true
      (let len = String.length s in
       let rlen = String.length result in
       let found = ref false in
       for i = 0 to rlen - len do
         if String.sub result i len = s then found := true
       done;
       !found)
  ) ["abc"; "defgh"; "ij"]

let test_format_columns_empty () =
  let result = Completion.format_columns ~width:80 [] in
  Alcotest.(check string) "empty" "" result

(* --- QCheck property tests --- *)

let ident_char_gen =
  QCheck2.Gen.(oneof [
    return '+'; return '-'; return '*'; return '/';
    return '?'; return '!';
    char_range 'a' 'z';
    char_range 'A' 'Z';
    char_range '0' '9';
  ])

let ident_gen =
  QCheck2.Gen.(string_size (int_range 1 20) ~gen:ident_char_gen)

let ident_list_gen =
  QCheck2.Gen.(list_size (int_range 0 30) ident_gen)

let prop_common_prefix_is_prefix =
  QCheck2.Test.make ~count:200
    ~name:"common_prefix is a prefix of every input"
    ident_list_gen
    (fun strs ->
       let cp = Completion.common_prefix strs in
       let cp_len = String.length cp in
       List.for_all (fun s ->
         String.length s >= cp_len &&
         String.sub s 0 cp_len = cp
       ) strs)

let prop_find_matches_subset =
  QCheck2.Test.make ~count:200
    ~name:"find_matches result is subset of candidates"
    QCheck2.Gen.(pair ident_gen ident_list_gen)
    (fun (prefix, candidates) ->
       let result = Completion.find_matches prefix candidates in
       List.for_all (fun r -> List.mem r candidates) result)

let prop_find_matches_all_start_with_prefix =
  QCheck2.Test.make ~count:200
    ~name:"find_matches results all start with prefix"
    QCheck2.Gen.(pair ident_gen ident_list_gen)
    (fun (prefix, candidates) ->
       let result = Completion.find_matches prefix candidates in
       let plen = String.length prefix in
       List.for_all (fun s ->
         String.length s >= plen &&
         String.sub s 0 plen = prefix
       ) result)

let prop_format_columns_contains_all =
  QCheck2.Test.make ~count:200
    ~name:"format_columns contains every candidate"
    ident_list_gen
    (fun strs ->
       let result = Completion.format_columns ~width:80 strs in
       List.for_all (fun s ->
         let slen = String.length s in
         let rlen = String.length result in
         if slen = 0 then true
         else
           let found = ref false in
           for i = 0 to rlen - slen do
             if String.sub result i slen = s then found := true
           done;
           !found
       ) strs)

let () =
  Alcotest.run "Completion"
    [ ("extract_prefix",
       [ Alcotest.test_case "simple" `Quick test_extract_prefix_simple
       ; Alcotest.test_case "after paren" `Quick test_extract_prefix_after_paren
       ; Alcotest.test_case "mid word" `Quick test_extract_prefix_mid_word
       ; Alcotest.test_case "empty" `Quick test_extract_prefix_empty
       ; Alcotest.test_case "comma" `Quick test_extract_prefix_comma
       ; Alcotest.test_case "after space" `Quick test_extract_prefix_after_space
       ])
    ; ("find_matches",
       [ Alcotest.test_case "basic" `Quick test_find_matches_basic
       ; Alcotest.test_case "empty prefix" `Quick test_find_matches_empty_prefix
       ; Alcotest.test_case "no match" `Quick test_find_matches_no_match
       ; Alcotest.test_case "comma" `Quick test_find_matches_comma
       ])
    ; ("common_prefix",
       [ Alcotest.test_case "basic" `Quick test_common_prefix_basic
       ; Alcotest.test_case "single" `Quick test_common_prefix_single
       ; Alcotest.test_case "empty list" `Quick test_common_prefix_empty_list
       ; Alcotest.test_case "no common" `Quick test_common_prefix_no_common
       ])
    ; ("format_columns",
       [ Alcotest.test_case "basic" `Quick test_format_columns_basic
       ; Alcotest.test_case "empty" `Quick test_format_columns_empty
       ])
    ; ("properties",
       List.map QCheck_alcotest.to_alcotest
         [ prop_common_prefix_is_prefix
         ; prop_find_matches_subset
         ; prop_find_matches_all_start_with_prefix
         ; prop_format_columns_contains_all
         ])
    ]
