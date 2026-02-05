open Wile

(* -- Helpers -- *)

let char_type_testable =
  Alcotest.testable
    (fun fmt ct -> Format.fprintf fmt "%s" (Char_type.to_string ct))
    Char_type.equal

let check_char_type = Alcotest.check char_type_testable

(* -- Empty readtable tests -- *)

let test_readtable_empty_char_type () =
  let rt = Readtable.empty in
  List.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "char %C is constituent" c)
        true
        (Char_type.equal Char_type.Constituent (Readtable.char_type_of rt c)))
    ['a'; 'z'; '0'; '9'; '+'; '-'; '('; ' '; '#']

let test_readtable_empty_macro () =
  let rt = Readtable.empty in
  Alcotest.(check bool) "no macro for a" true
    (Option.is_none (Readtable.macro_of rt 'a'));
  Alcotest.(check bool) "no macro for (" true
    (Option.is_none (Readtable.macro_of rt '('));
  Alcotest.(check bool) "no dispatch for t" true
    (Option.is_none (Readtable.dispatch_macro_of rt 't'))

let test_readtable_empty_fold_case () =
  Alcotest.(check bool) "fold_case false" false
    (Readtable.fold_case Readtable.empty)

let readtable_empty_tests =
  [ Alcotest.test_case "all chars constituent" `Quick test_readtable_empty_char_type
  ; Alcotest.test_case "no macros" `Quick test_readtable_empty_macro
  ; Alcotest.test_case "fold_case false" `Quick test_readtable_empty_fold_case
  ]

(* -- Functional update tests -- *)

let test_readtable_set_char_type () =
  let rt = Readtable.empty |> Readtable.set_char_type '(' Char_type.Terminating_macro in
  check_char_type "( is terminating" Char_type.Terminating_macro
    (Readtable.char_type_of rt '(');
  check_char_type "a still constituent" Char_type.Constituent
    (Readtable.char_type_of rt 'a')

let test_readtable_set_macro () =
  let stub_macro _c = Datum.Nil in
  let rt = Readtable.empty
    |> Readtable.set_macro '(' Char_type.Terminating_macro stub_macro in
  check_char_type "( is terminating" Char_type.Terminating_macro
    (Readtable.char_type_of rt '(');
  Alcotest.(check bool) "macro present" true
    (Option.is_some (Readtable.macro_of rt '('));
  Alcotest.(check bool) "no macro for a" true
    (Option.is_none (Readtable.macro_of rt 'a'))

let test_readtable_set_dispatch_macro () =
  let stub_dispatch _c = Datum.Bool true in
  let rt = Readtable.empty |> Readtable.set_dispatch_macro 't' stub_dispatch in
  Alcotest.(check bool) "dispatch present" true
    (Option.is_some (Readtable.dispatch_macro_of rt 't'));
  Alcotest.(check bool) "no dispatch for x" true
    (Option.is_none (Readtable.dispatch_macro_of rt 'x'))

let test_readtable_with_fold_case () =
  let rt = Readtable.empty |> Readtable.with_fold_case true in
  Alcotest.(check bool) "fold_case true" true (Readtable.fold_case rt);
  let rt2 = rt |> Readtable.with_fold_case false in
  Alcotest.(check bool) "fold_case false" false (Readtable.fold_case rt2);
  Alcotest.(check bool) "original unchanged" true (Readtable.fold_case rt)

let test_readtable_original_unchanged () =
  let rt = Readtable.empty in
  let rt2 = rt |> Readtable.set_char_type '(' Char_type.Terminating_macro in
  check_char_type "original ( is constituent" Char_type.Constituent
    (Readtable.char_type_of rt '(');
  check_char_type "copy ( is terminating" Char_type.Terminating_macro
    (Readtable.char_type_of rt2 '(')

(* QCheck generators *)

let gen_printable_char =
  QCheck2.Gen.(map Char.chr (int_range 32 126))

let gen_char_type =
  QCheck2.Gen.(oneof_list Char_type.all)

let qcheck_set_get_roundtrip =
  QCheck2.Test.make
    ~name:"set_char_type/char_type_of round-trip"
    ~count:200
    QCheck2.Gen.(pair gen_printable_char gen_char_type)
    (fun (c, ct) ->
      let rt = Readtable.empty |> Readtable.set_char_type c ct in
      Char_type.equal ct (Readtable.char_type_of rt c))

let readtable_update_tests =
  [ Alcotest.test_case "set_char_type" `Quick test_readtable_set_char_type
  ; Alcotest.test_case "set_macro" `Quick test_readtable_set_macro
  ; Alcotest.test_case "set_dispatch_macro" `Quick test_readtable_set_dispatch_macro
  ; Alcotest.test_case "with_fold_case" `Quick test_readtable_with_fold_case
  ; Alcotest.test_case "original unchanged" `Quick test_readtable_original_unchanged
  ; QCheck_alcotest.to_alcotest qcheck_set_get_roundtrip
  ]

(* -- Default table tests -- *)

let rt_default = Readtable.default

let test_default_whitespace () =
  List.iter
    (fun c ->
      check_char_type (Printf.sprintf "%C is whitespace" c) Char_type.Whitespace
        (Readtable.char_type_of rt_default c))
    [' '; '\t'; '\n'; '\r']

let test_default_terminating_macro () =
  List.iter
    (fun c ->
      check_char_type (Printf.sprintf "%C is terminating" c) Char_type.Terminating_macro
        (Readtable.char_type_of rt_default c))
    ['('; ')'; '"'; ';'; '\''; '`'; ',']

let test_default_non_terminating_macro () =
  check_char_type "# is non-terminating" Char_type.Non_terminating_macro
    (Readtable.char_type_of rt_default '#')

let test_default_multiple_escape () =
  check_char_type "| is multiple-escape" Char_type.Multiple_escape
    (Readtable.char_type_of rt_default '|')

let test_default_constituent () =
  List.iter
    (fun c ->
      check_char_type (Printf.sprintf "%C is constituent" c) Char_type.Constituent
        (Readtable.char_type_of rt_default c))
    ['a'; 'z'; 'A'; 'Z'; '0'; '9'; '+'; '-'; '.'; '*'; '/'; '!';
     '?'; '<'; '>'; '='; '_'; '['; ']'; '{'; '}']

let test_default_macros_present () =
  List.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "macro for %C" c)
        true
        (Option.is_some (Readtable.macro_of rt_default c)))
    ['('; ')'; '"'; ';'; '\''; '`'; ','; '#']

let test_default_dispatch_entries () =
  let dispatch_chars =
    ['t'; 'T'; 'f'; 'F'; '\\'; '('; 'u'; 'U'; ';'; '|'; '!';
     'b'; 'B'; 'o'; 'O'; 'd'; 'D'; 'x'; 'X'; 'e'; 'E'; 'i'; 'I']
  in
  List.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "dispatch for %C" c)
        true
        (Option.is_some (Readtable.dispatch_macro_of rt_default c)))
    dispatch_chars

let test_default_is_delimiter () =
  List.iter
    (fun c ->
      Alcotest.(check bool) (Printf.sprintf "%C is delimiter" c) true
        (Readtable.is_delimiter rt_default c))
    [' '; '\t'; '\n'; '\r'; '('; ')'; '"'; ';'];
  List.iter
    (fun c ->
      Alcotest.(check bool) (Printf.sprintf "%C is not delimiter" c) false
        (Readtable.is_delimiter rt_default c))
    ['a'; '1'; '+'; '#'; '|']

let test_default_is_whitespace () =
  List.iter
    (fun c ->
      Alcotest.(check bool) (Printf.sprintf "%C is whitespace" c) true
        (Readtable.is_whitespace rt_default c))
    [' '; '\t'; '\n'; '\r'];
  List.iter
    (fun c ->
      Alcotest.(check bool) (Printf.sprintf "%C is not whitespace" c) false
        (Readtable.is_whitespace rt_default c))
    ['a'; '('; '#']

let qcheck_delimiter_consistency =
  QCheck2.Test.make
    ~name:"is_delimiter iff whitespace or terminating-macro"
    ~count:200
    gen_printable_char
    (fun c ->
      let ct = Readtable.char_type_of rt_default c in
      let is_del = Readtable.is_delimiter rt_default c in
      let expected =
        Char_type.equal ct Char_type.Whitespace
        || Char_type.equal ct Char_type.Terminating_macro
      in
      is_del = expected)

let qcheck_dispatch_case_insensitive =
  QCheck2.Test.make
    ~name:"dispatch entries exist for both cases"
    ~count:50
    QCheck2.Gen.(oneof_list
      ['t'; 'f'; 'u'; 'b'; 'o'; 'd'; 'x'; 'e'; 'i'])
    (fun c ->
      let upper = Char.uppercase_ascii c in
      let lower = Char.lowercase_ascii c in
      Option.is_some (Readtable.dispatch_macro_of rt_default upper)
      && Option.is_some (Readtable.dispatch_macro_of rt_default lower))

let readtable_default_tests =
  [ Alcotest.test_case "whitespace chars" `Quick test_default_whitespace
  ; Alcotest.test_case "terminating macro chars" `Quick test_default_terminating_macro
  ; Alcotest.test_case "# is non-terminating" `Quick test_default_non_terminating_macro
  ; Alcotest.test_case "| is multiple-escape" `Quick test_default_multiple_escape
  ; Alcotest.test_case "constituent chars" `Quick test_default_constituent
  ; Alcotest.test_case "macros present" `Quick test_default_macros_present
  ; Alcotest.test_case "dispatch entries" `Quick test_default_dispatch_entries
  ; Alcotest.test_case "is_delimiter" `Quick test_default_is_delimiter
  ; Alcotest.test_case "is_whitespace" `Quick test_default_is_whitespace
  ; QCheck_alcotest.to_alcotest qcheck_delimiter_consistency
  ; QCheck_alcotest.to_alcotest qcheck_dispatch_case_insensitive
  ]

(* -- Immutability confidence tests -- *)

let test_immutability_copy_update () =
  let original = Readtable.default in
  let modified = original
    |> Readtable.set_char_type 'a' Char_type.Whitespace
    |> Readtable.set_char_type '(' Char_type.Constituent
    |> Readtable.with_fold_case true
  in
  (* original is unchanged *)
  check_char_type "original a" Char_type.Constituent
    (Readtable.char_type_of original 'a');
  check_char_type "original (" Char_type.Terminating_macro
    (Readtable.char_type_of original '(');
  Alcotest.(check bool) "original fold_case" false
    (Readtable.fold_case original);
  (* modified has changes *)
  check_char_type "modified a" Char_type.Whitespace
    (Readtable.char_type_of modified 'a');
  check_char_type "modified (" Char_type.Constituent
    (Readtable.char_type_of modified '(');
  Alcotest.(check bool) "modified fold_case" true
    (Readtable.fold_case modified)

let test_immutability_fold_case_independence () =
  let rt1 = Readtable.default |> Readtable.with_fold_case true in
  let rt2 = rt1 |> Readtable.with_fold_case false in
  Alcotest.(check bool) "rt1 still true" true (Readtable.fold_case rt1);
  Alcotest.(check bool) "rt2 is false" false (Readtable.fold_case rt2)

let test_immutability_dispatch_update () =
  let original = Readtable.default in
  let stub _c = Datum.Eof in
  let modified = Readtable.set_dispatch_macro 'z' stub original in
  Alcotest.(check bool) "original no z dispatch" true
    (Option.is_none (Readtable.dispatch_macro_of original 'z'));
  Alcotest.(check bool) "modified has z dispatch" true
    (Option.is_some (Readtable.dispatch_macro_of modified 'z'))

type mod_op =
  | Set_ct of char * Char_type.t
  | Set_fc of bool

let gen_mod_op =
  let open QCheck2.Gen in
  oneof [
    map2 (fun c ct -> Set_ct (c, ct)) gen_printable_char gen_char_type;
    map (fun b -> Set_fc b) bool;
  ]

let apply_mod rt = function
  | Set_ct (c, ct) -> Readtable.set_char_type c ct rt
  | Set_fc b -> Readtable.with_fold_case b rt

let qcheck_modifications_preserve_original =
  QCheck2.Test.make
    ~name:"arbitrary modifications preserve original"
    ~count:100
    QCheck2.Gen.(list_size (int_range 1 20) gen_mod_op)
    (fun ops ->
      let original = Readtable.default in
      let _modified = List.fold_left apply_mod original ops in
      (* Spot-check that original is unchanged *)
      Char_type.equal Char_type.Whitespace (Readtable.char_type_of original ' ')
      && Char_type.equal Char_type.Terminating_macro (Readtable.char_type_of original '(')
      && Char_type.equal Char_type.Constituent (Readtable.char_type_of original 'a')
      && not (Readtable.fold_case original))

let readtable_immutability_tests =
  [ Alcotest.test_case "copy-update doesn't affect original" `Quick test_immutability_copy_update
  ; Alcotest.test_case "fold_case toggle independence" `Quick test_immutability_fold_case_independence
  ; Alcotest.test_case "dispatch update independence" `Quick test_immutability_dispatch_update
  ; QCheck_alcotest.to_alcotest qcheck_modifications_preserve_original
  ]

(* -- Run -- *)

let () =
  Alcotest.run "Readtable"
    [ ("Readtable.empty", readtable_empty_tests)
    ; ("Readtable.update", readtable_update_tests)
    ; ("Readtable.default", readtable_default_tests)
    ; ("Readtable.immutability", readtable_immutability_tests)
    ]
