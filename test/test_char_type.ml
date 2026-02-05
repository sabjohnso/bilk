open Wile

let char_type_testable =
  Alcotest.testable
    (fun fmt ct -> Format.fprintf fmt "%s" (Char_type.to_string ct))
    Char_type.equal

let check_char_type = Alcotest.check char_type_testable

let test_char_type_construction () =
  let open Char_type in
  check_char_type "constituent" Constituent Constituent;
  check_char_type "whitespace" Whitespace Whitespace;
  check_char_type "terminating" Terminating_macro Terminating_macro;
  check_char_type "non-terminating" Non_terminating_macro Non_terminating_macro;
  check_char_type "single-escape" Single_escape Single_escape;
  check_char_type "multiple-escape" Multiple_escape Multiple_escape

let test_char_type_equal () =
  let open Char_type in
  Alcotest.(check bool) "same" true (equal Constituent Constituent);
  Alcotest.(check bool) "diff" false (equal Constituent Whitespace);
  Alcotest.(check bool) "diff2" false (equal Terminating_macro Non_terminating_macro)

let test_char_type_to_string () =
  let open Char_type in
  Alcotest.(check string) "constituent" "constituent" (to_string Constituent);
  Alcotest.(check string) "whitespace" "whitespace" (to_string Whitespace);
  Alcotest.(check string) "terminating" "terminating-macro" (to_string Terminating_macro);
  Alcotest.(check string) "non-terminating" "non-terminating-macro" (to_string Non_terminating_macro);
  Alcotest.(check string) "single-escape" "single-escape" (to_string Single_escape);
  Alcotest.(check string) "multiple-escape" "multiple-escape" (to_string Multiple_escape)

let test_char_type_all () =
  Alcotest.(check int) "all has 6 elements" 6 (List.length Char_type.all);
  List.iter
    (fun ct ->
      Alcotest.(check bool) "in all" true (List.exists (Char_type.equal ct) Char_type.all))
    Char_type.[ Constituent; Whitespace; Terminating_macro;
                Non_terminating_macro; Single_escape; Multiple_escape ]

let () =
  Alcotest.run "Char_type"
    [ ("Char_type",
       [ Alcotest.test_case "construction" `Quick test_char_type_construction
       ; Alcotest.test_case "equal" `Quick test_char_type_equal
       ; Alcotest.test_case "to_string" `Quick test_char_type_to_string
       ; Alcotest.test_case "all" `Quick test_char_type_all
       ])
    ]
