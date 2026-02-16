open Bilk

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

(* format_columns_highlighted *)

let test_format_highlighted_basic () =
  let result = Completion.format_columns_highlighted ~width:80 ~highlight:1
    ["alpha"; "beta"; "gamma"] in
  (* Only "beta" should be wrapped in reverse video *)
  Alcotest.(check bool) "has reverse on beta" true
    (let pat = "\x1b[7mbeta\x1b[0m" in
     let plen = String.length pat in
     let rlen = String.length result in
     let found = ref false in
     for i = 0 to rlen - plen do
       if String.sub result i plen = pat then found := true
     done;
     !found);
  (* "alpha" should NOT be wrapped *)
  Alcotest.(check bool) "alpha not reversed" false
    (let pat = "\x1b[7malpha\x1b[0m" in
     let plen = String.length pat in
     let rlen = String.length result in
     let found = ref false in
     for i = 0 to rlen - plen do
       if String.sub result i plen = pat then found := true
     done;
     !found)

let test_format_highlighted_first () =
  let result = Completion.format_columns_highlighted ~width:80 ~highlight:0
    ["alpha"; "beta"; "gamma"] in
  Alcotest.(check bool) "has reverse on alpha" true
    (let pat = "\x1b[7malpha\x1b[0m" in
     let plen = String.length pat in
     let rlen = String.length result in
     let found = ref false in
     for i = 0 to rlen - plen do
       if String.sub result i plen = pat then found := true
     done;
     !found)

let test_format_highlighted_empty () =
  let result = Completion.format_columns_highlighted ~width:80 ~highlight:0 [] in
  Alcotest.(check string) "empty" "" result

(* should_complete_at *)

let test_should_complete_after_ident () =
  Alcotest.(check bool) "after letter" true
    (Completion.should_complete_at "def" 3)

let test_should_complete_empty () =
  Alcotest.(check bool) "empty string" false
    (Completion.should_complete_at "" 0)

let test_should_complete_line_start () =
  Alcotest.(check bool) "cursor at 0" false
    (Completion.should_complete_at "def" 0)

let test_should_complete_after_space () =
  Alcotest.(check bool) "after space" false
    (Completion.should_complete_at "foo " 4)

let test_should_complete_after_paren () =
  Alcotest.(check bool) "after open paren" false
    (Completion.should_complete_at "(" 1)

let test_should_complete_after_newline () =
  Alcotest.(check bool) "after newline" false
    (Completion.should_complete_at "foo\n" 4)

let test_should_complete_after_comma () =
  Alcotest.(check bool) "after comma (REPL cmd)" true
    (Completion.should_complete_at ",hel" 4)

(* match_library_name *)

let all_libs = [
  ["scheme"; "base"];
  ["scheme"; "write"];
  ["scheme"; "read"];
  ["scheme"; "case-lambda"];
  ["srfi"; "1"];
  ["srfi"; "13"];
  ["bilk"; "test"];
]

let test_match_library_scheme_b () =
  let result = Completion.match_library_name ["scheme"] "b" all_libs in
  Alcotest.(check (list (list string))) "scheme b"
    [["scheme"; "base"]] result

let test_match_library_scheme_empty () =
  let result = Completion.match_library_name ["scheme"] "" all_libs in
  Alcotest.(check int) "scheme *" 4 (List.length result)

let test_match_library_empty_parts () =
  let result = Completion.match_library_name [] "s" all_libs in
  (* scheme (4 entries) + srfi (2 entries) = 6 *)
  Alcotest.(check int) "s*" 6 (List.length result)

let test_match_library_no_match () =
  let result = Completion.match_library_name ["zzz"] "a" all_libs in
  Alcotest.(check (list (list string))) "no match" [] result

let test_format_library_name () =
  Alcotest.(check string) "format"
    "(scheme base)" (Completion.format_library_name ["scheme"; "base"])

(* complete_path *)

let with_tmpdir f =
  let dir = Filename.temp_dir "bilk_test" "" in
  let rec cleanup d =
    Array.iter (fun name ->
      let path = Filename.concat d name in
      if Sys.is_directory path then begin
        cleanup path; Sys.rmdir path
      end else
        Sys.remove path
    ) (Sys.readdir d)
  in
  Fun.protect ~finally:(fun () -> cleanup dir; Sys.rmdir dir)
    (fun () -> f dir)

let mkdir path = Sys.command ("mkdir -p " ^ Filename.quote path) |> ignore

let test_complete_path_basic () =
  with_tmpdir (fun dir ->
    let f = open_out (Filename.concat dir "hello.scm") in
    close_out f;
    let f = open_out (Filename.concat dir "help.txt") in
    close_out f;
    let results = Completion.complete_path (dir ^ "/hel") in
    Alcotest.(check int) "two matches" 2 (List.length results);
    Alcotest.(check bool) "has hello.scm" true
      (List.mem (dir ^ "/hello.scm") results);
    Alcotest.(check bool) "has help.txt" true
      (List.mem (dir ^ "/help.txt") results))

let test_complete_path_nonexistent_dir () =
  let results = Completion.complete_path "/nonexistent_dir_xyz/foo" in
  Alcotest.(check (list string)) "empty" [] results

let test_complete_path_dir_gets_slash () =
  with_tmpdir (fun dir ->
    mkdir (Filename.concat dir "subdir");
    let results = Completion.complete_path (dir ^ "/sub") in
    Alcotest.(check (list string)) "dir with slash"
      [dir ^ "/subdir/"] results)

let test_complete_path_subdir () =
  with_tmpdir (fun dir ->
    mkdir (Filename.concat dir "sub");
    let f = open_out (Filename.concat dir "sub/foo.scm") in
    close_out f;
    let results = Completion.complete_path (dir ^ "/sub/fo") in
    Alcotest.(check (list string)) "subdir match"
      [dir ^ "/sub/foo.scm"] results)

let test_complete_path_empty_prefix () =
  with_tmpdir (fun dir ->
    let f = open_out (Filename.concat dir "a.txt") in
    close_out f;
    let results = Completion.complete_path (dir ^ "/") in
    Alcotest.(check bool) "has entries" true (List.length results > 0))

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

let prop_should_complete_iff_extract_nonempty =
  QCheck2.Test.make ~count:200
    ~name:"should_complete_at iff extract_prefix returns non-empty"
    QCheck2.Gen.(pair
      (string_size (int_range 0 40))
      (int_range 0 40))
    (fun (text, cursor) ->
       let cursor = min cursor (String.length text) in
       let should = Completion.should_complete_at text cursor in
       let (prefix, _) = Completion.extract_prefix text cursor in
       should = (prefix <> ""))

let prop_complete_path_results_start_with_prefix =
  QCheck2.Test.make ~count:100
    ~name:"complete_path results start with input prefix"
    QCheck2.Gen.(return "/tmp/")
    (fun dir ->
       let results = Completion.complete_path dir in
       List.for_all (fun r ->
         String.length r >= String.length dir &&
         String.sub r 0 (String.length dir) = dir
       ) results)

let prop_format_highlighted_exactly_one_reverse =
  QCheck2.Test.make ~count:200
    ~name:"format_columns_highlighted has exactly one reverse video marker"
    QCheck2.Gen.(pair
      (list_size (int_range 1 20) ident_gen)
      (int_range 0 19))
    (fun (strs, idx) ->
       let idx = idx mod List.length strs in
       let result = Completion.format_columns_highlighted ~width:80 ~highlight:idx strs in
       let marker = "\x1b[7m" in
       let mlen = String.length marker in
       let rlen = String.length result in
       let count = ref 0 in
       for i = 0 to rlen - mlen do
         if String.sub result i mlen = marker then incr count
       done;
       !count = 1)

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
    ; ("format_columns_highlighted",
       [ Alcotest.test_case "basic" `Quick test_format_highlighted_basic
       ; Alcotest.test_case "first" `Quick test_format_highlighted_first
       ; Alcotest.test_case "empty" `Quick test_format_highlighted_empty
       ])
    ; ("match_library_name",
       [ Alcotest.test_case "scheme b" `Quick test_match_library_scheme_b
       ; Alcotest.test_case "scheme empty" `Quick test_match_library_scheme_empty
       ; Alcotest.test_case "empty parts" `Quick test_match_library_empty_parts
       ; Alcotest.test_case "no match" `Quick test_match_library_no_match
       ; Alcotest.test_case "format name" `Quick test_format_library_name
       ])
    ; ("complete_path",
       [ Alcotest.test_case "basic" `Quick test_complete_path_basic
       ; Alcotest.test_case "nonexistent" `Quick test_complete_path_nonexistent_dir
       ; Alcotest.test_case "dir slash" `Quick test_complete_path_dir_gets_slash
       ; Alcotest.test_case "subdir" `Quick test_complete_path_subdir
       ; Alcotest.test_case "empty prefix" `Quick test_complete_path_empty_prefix
       ])
    ; ("should_complete_at",
       [ Alcotest.test_case "after ident" `Quick test_should_complete_after_ident
       ; Alcotest.test_case "empty" `Quick test_should_complete_empty
       ; Alcotest.test_case "line start" `Quick test_should_complete_line_start
       ; Alcotest.test_case "after space" `Quick test_should_complete_after_space
       ; Alcotest.test_case "after paren" `Quick test_should_complete_after_paren
       ; Alcotest.test_case "after newline" `Quick test_should_complete_after_newline
       ; Alcotest.test_case "after comma" `Quick test_should_complete_after_comma
       ])
    ; ("properties",
       List.map QCheck_alcotest.to_alcotest
         [ prop_common_prefix_is_prefix
         ; prop_find_matches_subset
         ; prop_find_matches_all_start_with_prefix
         ; prop_format_columns_contains_all
         ; prop_format_highlighted_exactly_one_reverse
         ; prop_complete_path_results_start_with_prefix
         ; prop_should_complete_iff_extract_nonempty
         ])
    ]
