open Wile

let rt = Readtable.default
let theme = Highlight.dark_theme

let test_empty () =
  let result = Highlight.highlight_line theme rt "" (-1) in
  Alcotest.(check string) "empty" "" result

let test_plain_symbol () =
  let result = Highlight.highlight_line theme rt "hello" (-1) in
  (* Should contain ANSI codes *)
  Alcotest.(check bool) "has ansi" true (String.length result > 5);
  (* Strip ANSI should give back original *)
  Alcotest.(check string) "stripped" "hello" (Highlight.strip_ansi result)

let contains_substring s sub =
  let slen = String.length s in
  let sublen = String.length sub in
  if sublen > slen then false
  else
    let rec check i =
      if i + sublen > slen then false
      else if String.sub s i sublen = sub then true
      else check (i + 1)
    in
    check 0

let test_keyword_colored () =
  let result = Highlight.highlight_line theme rt "define" (-1) in
  let stripped = Highlight.strip_ansi result in
  Alcotest.(check string) "stripped" "define" stripped;
  (* Should contain ANSI escape *)
  Alcotest.(check bool) "has ansi" true (contains_substring result "\x1b[")

let test_strip_ansi_roundtrip () =
  let text = "(define (f x) (+ x 1))" in
  let highlighted = Highlight.highlight_line theme rt text (-1) in
  Alcotest.(check string) "roundtrip"
    text (Highlight.strip_ansi highlighted)

let test_rainbow_parens () =
  let text = "(((x)))" in
  let result = Highlight.highlight_line theme rt text (-1) in
  let stripped = Highlight.strip_ansi result in
  Alcotest.(check string) "stripped" text stripped;
  (* Different depths should have different colors *)
  Alcotest.(check bool) "highlighted longer" true
    (String.length result > String.length text)

let test_style_to_ansi_empty () =
  let s = Highlight.style_to_ansi Highlight.default_style in
  Alcotest.(check string) "empty style" "" s

let test_style_to_ansi_fg () =
  let s = Highlight.style_to_ansi (Highlight.fg 196) in
  Alcotest.(check string) "fg 196" "\x1b[38;5;196m" s

let test_style_to_ansi_bold () =
  let s = Highlight.style_to_ansi (Highlight.fg_bold 69) in
  (* Order: fg first, then bold *)
  Alcotest.(check string) "bold fg" "\x1b[38;5;69;1m" s

let test_strip_ansi () =
  Alcotest.(check string) "no ansi" "hello" (Highlight.strip_ansi "hello");
  Alcotest.(check string) "with color"
    "hello" (Highlight.strip_ansi "\x1b[38;5;196mhello\x1b[0m");
  Alcotest.(check string) "mixed"
    "ab" (Highlight.strip_ansi "\x1b[1ma\x1b[0m\x1b[38;5;69mb\x1b[0m")

let test_light_theme () =
  let result = Highlight.highlight_line Highlight.light_theme rt
    "(define x 42)" (-1) in
  Alcotest.(check string) "stripped"
    "(define x 42)" (Highlight.strip_ansi result)

let test_theme_load () =
  let tmp = Filename.temp_file "wile_theme" ".scm" in
  Fun.protect ~finally:(fun () -> Sys.remove tmp) (fun () ->
    let oc = open_out tmp in
    output_string oc {|(theme "test"
  (keyword (fg 198) (bold #t))
  (string (fg 114))
  (number (fg 208))
  (comment (fg 245) (italic #t))
  (paren (fg 196) (fg 208) (fg 226)))|};
    close_out oc;
    let t = Highlight.load_theme tmp in
    Alcotest.(check string) "name" "test" t.name;
    Alcotest.(check (option int)) "keyword fg" (Some 198) t.keyword_style.fg;
    Alcotest.(check bool) "keyword bold" true t.keyword_style.bold;
    Alcotest.(check (option int)) "string fg" (Some 114) t.string_style.fg;
    Alcotest.(check int) "paren count" 3 (Array.length t.paren))

let () =
  Alcotest.run "Highlight" [
    "highlighting", [
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "plain symbol" `Quick test_plain_symbol;
      Alcotest.test_case "keyword colored" `Quick test_keyword_colored;
      Alcotest.test_case "strip roundtrip" `Quick test_strip_ansi_roundtrip;
      Alcotest.test_case "rainbow parens" `Quick test_rainbow_parens;
      Alcotest.test_case "light theme" `Quick test_light_theme;
    ];
    "ansi helpers", [
      Alcotest.test_case "empty style" `Quick test_style_to_ansi_empty;
      Alcotest.test_case "fg style" `Quick test_style_to_ansi_fg;
      Alcotest.test_case "bold style" `Quick test_style_to_ansi_bold;
      Alcotest.test_case "strip ansi" `Quick test_strip_ansi;
    ];
    "theme loading", [
      Alcotest.test_case "load theme" `Quick test_theme_load;
    ];
  ]
