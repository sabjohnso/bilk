open Bilk

let rt = Readtable.default

let ctx_testable =
  Alcotest.testable
    (fun fmt ctx ->
       match ctx with
       | Completion_context.Identifier (prefix, off) ->
         Format.fprintf fmt "Identifier(%S, %d)" prefix off
       | Completion_context.String_literal (content, off) ->
         Format.fprintf fmt "String_literal(%S, %d)" content off
       | Completion_context.Repl_command_arg (cmd, arg, off) ->
         Format.fprintf fmt "Repl_command_arg(%S, %S, %d)" cmd arg off
       | Completion_context.Import_library (parts, prefix, off) ->
         Format.fprintf fmt "Import_library([%s], %S, %d)"
           (String.concat "; " (List.map (fun s -> "\"" ^ s ^ "\"") parts))
           prefix off
       | Completion_context.No_context ->
         Format.fprintf fmt "No_context")
    (=)

(* --- Identifier contexts --- *)

let test_plain_identifier () =
  let ctx = Completion_context.detect rt "(def" 4 in
  Alcotest.check ctx_testable "ident after paren"
    (Completion_context.Identifier ("def", 1)) ctx

let test_identifier_at_start () =
  let ctx = Completion_context.detect rt "define" 6 in
  Alcotest.check ctx_testable "ident at start"
    (Completion_context.Identifier ("define", 0)) ctx

let test_no_context_empty () =
  let ctx = Completion_context.detect rt "" 0 in
  Alcotest.check ctx_testable "empty" Completion_context.No_context ctx

let test_no_context_after_space () =
  let ctx = Completion_context.detect rt "(foo " 5 in
  Alcotest.check ctx_testable "after space" Completion_context.No_context ctx

let test_no_context_after_close_paren () =
  let ctx = Completion_context.detect rt "(foo)" 5 in
  Alcotest.check ctx_testable "after close paren"
    Completion_context.No_context ctx

(* --- String literal contexts --- *)

let test_inside_string () =
  let ctx = Completion_context.detect rt "(load \"./sr" 11 in
  Alcotest.check ctx_testable "inside string"
    (Completion_context.String_literal ("./sr", 7)) ctx

let test_inside_empty_string () =
  let ctx = Completion_context.detect rt "\"" 1 in
  Alcotest.check ctx_testable "empty string"
    (Completion_context.String_literal ("", 1)) ctx

(* --- REPL command arg contexts --- *)

let test_repl_load_arg () =
  let ctx = Completion_context.detect rt ",load foo" 9 in
  Alcotest.check ctx_testable "load arg"
    (Completion_context.Repl_command_arg (",load", "foo", 6)) ctx

let test_repl_load_empty_arg () =
  let ctx = Completion_context.detect rt ",load " 6 in
  Alcotest.check ctx_testable "load empty arg"
    (Completion_context.Repl_command_arg (",load", "", 6)) ctx

let test_repl_theme_arg () =
  let ctx = Completion_context.detect rt ",theme da" 9 in
  Alcotest.check ctx_testable "theme arg"
    (Completion_context.Repl_command_arg (",theme", "da", 7)) ctx

(* --- Import library contexts --- *)

let test_import_library () =
  let ctx = Completion_context.detect rt "(import (scheme b" 17 in
  Alcotest.check ctx_testable "import library"
    (Completion_context.Import_library (["scheme"], "b", 16)) ctx

let test_import_library_first_part () =
  let ctx = Completion_context.detect rt "(import (sch" 12 in
  Alcotest.check ctx_testable "import first part"
    (Completion_context.Import_library ([], "sch", 9)) ctx

let test_import_library_empty_prefix () =
  let ctx = Completion_context.detect rt "(import (scheme " 16 in
  Alcotest.check ctx_testable "import empty prefix"
    (Completion_context.Import_library (["scheme"], "", 16)) ctx

(* --- Properties --- *)

let prop_detect_never_raises =
  QCheck2.Test.make ~count:300
    ~name:"detect never raises"
    QCheck2.Gen.(pair
      (string_size (int_range 0 60))
      (int_range 0 60))
    (fun (text, cursor) ->
       let cursor = min cursor (String.length text) in
       let _ctx = Completion_context.detect rt text cursor in
       true)

let () =
  Alcotest.run "Completion_context"
    [ ("identifier",
       [ Alcotest.test_case "plain" `Quick test_plain_identifier
       ; Alcotest.test_case "at start" `Quick test_identifier_at_start
       ; Alcotest.test_case "no context empty" `Quick test_no_context_empty
       ; Alcotest.test_case "no context space" `Quick test_no_context_after_space
       ; Alcotest.test_case "no context paren" `Quick test_no_context_after_close_paren
       ])
    ; ("string_literal",
       [ Alcotest.test_case "inside string" `Quick test_inside_string
       ; Alcotest.test_case "empty string" `Quick test_inside_empty_string
       ])
    ; ("repl_command_arg",
       [ Alcotest.test_case "load arg" `Quick test_repl_load_arg
       ; Alcotest.test_case "load empty" `Quick test_repl_load_empty_arg
       ; Alcotest.test_case "theme arg" `Quick test_repl_theme_arg
       ])
    ; ("import_library",
       [ Alcotest.test_case "scheme b" `Quick test_import_library
       ; Alcotest.test_case "first part" `Quick test_import_library_first_part
       ; Alcotest.test_case "empty prefix" `Quick test_import_library_empty_prefix
       ])
    ; ("properties",
       List.map QCheck_alcotest.to_alcotest
         [ prop_detect_never_raises ])
    ]
