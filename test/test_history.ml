open Wile

let list_eq = Alcotest.(check (list string))

(* --- Basic operations --- *)

let test_empty () =
  let h = History.create () in
  Alcotest.(check int) "empty length" 0 (History.length h);
  list_eq "empty list" [] (History.to_list h)

let test_add_single () =
  let h = History.create () in
  History.add h "hello";
  Alcotest.(check int) "length 1" 1 (History.length h);
  list_eq "single entry" ["hello"] (History.to_list h)

let test_add_multiple () =
  let h = History.create () in
  History.add h "first";
  History.add h "second";
  History.add h "third";
  Alcotest.(check int) "length 3" 3 (History.length h);
  list_eq "ordered oldest to newest" ["first"; "second"; "third"]
    (History.to_list h)

let test_ignore_empty_string () =
  let h = History.create () in
  History.add h "hello";
  History.add h "";
  Alcotest.(check int) "still 1" 1 (History.length h)

let test_dedup_consecutive () =
  let h = History.create () in
  History.add h "hello";
  History.add h "hello";
  History.add h "hello";
  Alcotest.(check int) "deduped to 1" 1 (History.length h);
  list_eq "single entry" ["hello"] (History.to_list h)

let test_dedup_non_consecutive () =
  let h = History.create () in
  History.add h "a";
  History.add h "b";
  History.add h "a";
  Alcotest.(check int) "no dedup for non-consecutive" 3 (History.length h);
  list_eq "all present" ["a"; "b"; "a"] (History.to_list h)

(* --- Max length --- *)

let test_max_length () =
  let h = History.create ~max_length:3 () in
  History.add h "one";
  History.add h "two";
  History.add h "three";
  History.add h "four";
  Alcotest.(check int) "capped at 3" 3 (History.length h);
  list_eq "oldest dropped" ["two"; "three"; "four"] (History.to_list h)

let test_max_length_one () =
  let h = History.create ~max_length:1 () in
  History.add h "a";
  History.add h "b";
  Alcotest.(check int) "only 1" 1 (History.length h);
  list_eq "last entry" ["b"] (History.to_list h)

(* --- Navigation --- *)

let test_nav_prev_basic () =
  let h = History.create () in
  History.add h "first";
  History.add h "second";
  History.add h "third";
  History.reset_nav h;
  Alcotest.(check (option string)) "prev 1" (Some "third") (History.prev h);
  Alcotest.(check (option string)) "prev 2" (Some "second") (History.prev h);
  Alcotest.(check (option string)) "prev 3" (Some "first") (History.prev h);
  Alcotest.(check (option string)) "prev at start" None (History.prev h)

let test_nav_next_basic () =
  let h = History.create () in
  History.add h "first";
  History.add h "second";
  History.add h "third";
  History.reset_nav h;
  ignore (History.prev h);
  ignore (History.prev h);
  ignore (History.prev h);
  Alcotest.(check (option string)) "next 1" (Some "second") (History.next h);
  Alcotest.(check (option string)) "next 2" (Some "third") (History.next h);
  Alcotest.(check (option string)) "next past end" None (History.next h)

let test_nav_next_without_prev () =
  let h = History.create () in
  History.add h "first";
  History.reset_nav h;
  Alcotest.(check (option string)) "next without prev" None (History.next h)

let test_nav_reset () =
  let h = History.create () in
  History.add h "first";
  History.add h "second";
  History.reset_nav h;
  ignore (History.prev h);
  History.reset_nav h;
  Alcotest.(check (option string)) "after reset" (Some "second") (History.prev h)

let test_nav_empty () =
  let h = History.create () in
  Alcotest.(check (option string)) "prev empty" None (History.prev h);
  Alcotest.(check (option string)) "next empty" None (History.next h)

(* --- Prefix matching navigation --- *)

let test_prev_matching_basic () =
  let h = History.create () in
  History.add h "(define x 1)";
  History.add h "(+ 1 2)";
  History.add h "(define y 2)";
  History.reset_nav h;
  Alcotest.(check (option string)) "match 1"
    (Some "(define y 2)") (History.prev_matching h "(def");
  Alcotest.(check (option string)) "match 2"
    (Some "(define x 1)") (History.prev_matching h "(def");
  Alcotest.(check (option string)) "no more"
    None (History.prev_matching h "(def")

let test_prev_matching_no_match () =
  let h = History.create () in
  History.add h "(+ 1 2)";
  History.add h "(* 3 4)";
  History.reset_nav h;
  Alcotest.(check (option string)) "no match"
    None (History.prev_matching h "(def")

let test_next_matching_basic () =
  let h = History.create () in
  History.add h "(define x 1)";
  History.add h "(+ 1 2)";
  History.add h "(define y 2)";
  History.reset_nav h;
  (* Navigate back past all *)
  ignore (History.prev_matching h "(def");
  ignore (History.prev_matching h "(def");
  (* Now go forward *)
  Alcotest.(check (option string)) "fwd match"
    (Some "(define y 2)") (History.next_matching h "(def");
  Alcotest.(check (option string)) "fwd past end"
    None (History.next_matching h "(def")

let test_next_matching_without_prev () =
  let h = History.create () in
  History.add h "(define x 1)";
  History.reset_nav h;
  Alcotest.(check (option string)) "next without prev"
    None (History.next_matching h "(def")

(* --- File I/O --- *)

let test_save_load_roundtrip () =
  let tmp = Filename.temp_file "wile_hist_test" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove tmp) (fun () ->
    let h1 = History.create () in
    History.add h1 "alpha";
    History.add h1 "beta";
    History.add h1 "gamma";
    History.save_to_file h1 tmp;
    let h2 = History.create () in
    History.load_from_file h2 tmp;
    list_eq "round-trip" ["alpha"; "beta"; "gamma"] (History.to_list h2))

let test_load_nonexistent () =
  let h = History.create () in
  History.load_from_file h "/nonexistent/path/that/does/not/exist";
  Alcotest.(check int) "still empty" 0 (History.length h)

let test_load_respects_max_length () =
  let tmp = Filename.temp_file "wile_hist_test" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove tmp) (fun () ->
    let h1 = History.create () in
    History.add h1 "one";
    History.add h1 "two";
    History.add h1 "three";
    History.add h1 "four";
    History.add h1 "five";
    History.save_to_file h1 tmp;
    let h2 = History.create ~max_length:3 () in
    History.load_from_file h2 tmp;
    Alcotest.(check int) "capped at 3" 3 (History.length h2);
    list_eq "newest 3" ["three"; "four"; "five"] (History.to_list h2))

let () =
  Alcotest.run "History" [
    "basic", [
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "add single" `Quick test_add_single;
      Alcotest.test_case "add multiple" `Quick test_add_multiple;
      Alcotest.test_case "ignore empty string" `Quick test_ignore_empty_string;
      Alcotest.test_case "dedup consecutive" `Quick test_dedup_consecutive;
      Alcotest.test_case "dedup non-consecutive" `Quick test_dedup_non_consecutive;
    ];
    "max length", [
      Alcotest.test_case "max length" `Quick test_max_length;
      Alcotest.test_case "max length one" `Quick test_max_length_one;
    ];
    "navigation", [
      Alcotest.test_case "prev basic" `Quick test_nav_prev_basic;
      Alcotest.test_case "next basic" `Quick test_nav_next_basic;
      Alcotest.test_case "next without prev" `Quick test_nav_next_without_prev;
      Alcotest.test_case "reset" `Quick test_nav_reset;
      Alcotest.test_case "empty" `Quick test_nav_empty;
    ];
    "prefix matching", [
      Alcotest.test_case "prev matching basic" `Quick test_prev_matching_basic;
      Alcotest.test_case "prev matching no match" `Quick test_prev_matching_no_match;
      Alcotest.test_case "next matching basic" `Quick test_next_matching_basic;
      Alcotest.test_case "next matching without prev" `Quick test_next_matching_without_prev;
    ];
    "file I/O", [
      Alcotest.test_case "save/load roundtrip" `Quick test_save_load_roundtrip;
      Alcotest.test_case "load nonexistent" `Quick test_load_nonexistent;
      Alcotest.test_case "load respects max length" `Quick test_load_respects_max_length;
    ];
  ]
