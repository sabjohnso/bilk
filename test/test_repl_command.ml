open Bilk

(* --- classify: ,quit → Client_local Quit --- *)

let test_classify_quit () =
  Alcotest.(check bool) ",quit" true
    (Repl_command.classify ",quit" = Client_local Quit)

(* --- classify: ,help → Client_local Help --- *)

let test_classify_help () =
  Alcotest.(check bool) ",help" true
    (Repl_command.classify ",help" = Client_local Help)

(* --- classify: ,paredit → Client_local Paredit --- *)

let test_classify_paredit () =
  Alcotest.(check bool) ",paredit" true
    (Repl_command.classify ",paredit" = Client_local Paredit)

(* --- classify: ,theme dark → Client_local (Theme "dark") --- *)

let test_classify_theme () =
  Alcotest.(check bool) ",theme dark" true
    (Repl_command.classify ",theme dark" = Client_local (Theme "dark"))

(* --- classify: ,clear → Client_local Clear --- *)

let test_classify_clear () =
  Alcotest.(check bool) ",clear" true
    (Repl_command.classify ",clear" = Client_local Clear)

(* --- classify: ,checkpoint foo → Server_side --- *)

let test_classify_checkpoint () =
  Alcotest.(check bool) ",checkpoint foo" true
    (Repl_command.classify ",checkpoint foo" = Server_side)

(* --- classify: (+ 1 2) → Scheme_input --- *)

let test_classify_scheme () =
  Alcotest.(check bool) "(+ 1 2)" true
    (Repl_command.classify "(+ 1 2)" = Scheme_input)

(* --- QCheck2 property: comma-prefixed non-client-local → Server_side --- *)

let client_local_words =
  ["quit"; "q"; "help"; "h"; "paredit"; "clear"]

let client_local_prefixes =
  ["theme"]

let test_classify_server_side_property =
  let open QCheck2 in
  Test.make ~name:"comma non-client-local → Server_side"
    ~count:200
    (Gen.string_size (Gen.int_range 1 30))
    (fun word ->
       let is_client_local =
         List.mem word client_local_words
         || List.exists (fun p ->
              String.length word >= String.length p
              && String.sub word 0 (String.length p) = p
            ) client_local_prefixes
       in
       if is_client_local then true  (* skip client-local, vacuously true *)
       else
         Repl_command.classify ("," ^ word) = Server_side)

let () =
  Alcotest.run "Repl_command"
    [ ("classify",
       [ Alcotest.test_case ",quit" `Quick test_classify_quit
       ; Alcotest.test_case ",help" `Quick test_classify_help
       ; Alcotest.test_case ",paredit" `Quick test_classify_paredit
       ; Alcotest.test_case ",theme dark" `Quick test_classify_theme
       ; Alcotest.test_case ",clear" `Quick test_classify_clear
       ; Alcotest.test_case ",checkpoint → server" `Quick
           test_classify_checkpoint
       ; Alcotest.test_case "scheme input" `Quick test_classify_scheme
       ; QCheck_alcotest.to_alcotest test_classify_server_side_property
       ])
    ]
