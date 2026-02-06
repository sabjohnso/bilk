open Wile

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let check_datum = Alcotest.check datum_testable

let read_one s =
  Reader.read Readtable.default (Port.of_string s)

let read_syntax_one s =
  Reader.read_syntax Readtable.default (Port.of_string s)

(* Atoms *)

let test_reader_eof () =
  check_datum "empty" Datum.Eof (read_one "");
  check_datum "whitespace only" Datum.Eof (read_one "   \n\t  ")

let test_reader_whitespace () =
  check_datum "spaces" (Datum.Fixnum 42) (read_one "   42   ");
  check_datum "newlines" (Datum.Symbol "foo") (read_one "\n\n foo \n")

let test_reader_booleans () =
  check_datum "#t" (Datum.Bool true) (read_one "#t");
  check_datum "#T" (Datum.Bool true) (read_one "#T");
  check_datum "#f" (Datum.Bool false) (read_one "#f");
  check_datum "#F" (Datum.Bool false) (read_one "#F");
  check_datum "#true" (Datum.Bool true) (read_one "#true");
  check_datum "#TRUE" (Datum.Bool true) (read_one "#TRUE");
  check_datum "#false" (Datum.Bool false) (read_one "#false");
  check_datum "#False" (Datum.Bool false) (read_one "#False")

let test_reader_integers () =
  check_datum "0" (Datum.Fixnum 0) (read_one "0");
  check_datum "42" (Datum.Fixnum 42) (read_one "42");
  check_datum "+42" (Datum.Fixnum 42) (read_one "+42");
  check_datum "-42" (Datum.Fixnum (-42)) (read_one "-42");
  check_datum "large" (Datum.Fixnum 1234567890) (read_one "1234567890")

let test_reader_radix () =
  check_datum "#b101" (Datum.Fixnum 5) (read_one "#b101");
  check_datum "#B1010" (Datum.Fixnum 10) (read_one "#B1010");
  check_datum "#o77" (Datum.Fixnum 63) (read_one "#o77");
  check_datum "#O10" (Datum.Fixnum 8) (read_one "#O10");
  check_datum "#d99" (Datum.Fixnum 99) (read_one "#d99");
  check_datum "#xff" (Datum.Fixnum 255) (read_one "#xff");
  check_datum "#XFF" (Datum.Fixnum 255) (read_one "#XFF");
  check_datum "#xDEAD" (Datum.Fixnum 57005) (read_one "#xDEAD")

let test_reader_exactness () =
  check_datum "#e42" (Datum.Fixnum 42) (read_one "#e42");
  check_datum "#i42" (Datum.Flonum 42.0) (read_one "#i42");
  check_datum "#e3.14" (Datum.Fixnum 3) (read_one "#e3.14");
  (* Combined prefixes *)
  check_datum "#x#e10" (Datum.Fixnum 16) (read_one "#x#e10");
  check_datum "#e#x10" (Datum.Fixnum 16) (read_one "#e#x10");
  check_datum "#i#b101" (Datum.Flonum 5.0) (read_one "#i#b101")

let test_reader_floats () =
  check_datum "3.14" (Datum.Flonum 3.14) (read_one "3.14");
  check_datum ".5" (Datum.Flonum 0.5) (read_one ".5");
  check_datum "1." (Datum.Flonum 1.0) (read_one "1.");
  check_datum "1e10" (Datum.Flonum 1e10) (read_one "1e10");
  check_datum "1E10" (Datum.Flonum 1e10) (read_one "1E10");
  check_datum "1.5e2" (Datum.Flonum 150.0) (read_one "1.5e2");
  check_datum "-3.14" (Datum.Flonum (-3.14)) (read_one "-3.14");
  check_datum "+inf.0" (Datum.Flonum Float.infinity) (read_one "+inf.0");
  check_datum "-inf.0" (Datum.Flonum Float.neg_infinity) (read_one "-inf.0");
  Alcotest.(check bool) "+nan.0 is nan" true
    (Float.is_nan (match read_one "+nan.0" with Datum.Flonum f -> f | _ -> 0.0));
  Alcotest.(check bool) "-nan.0 is nan" true
    (Float.is_nan (match read_one "-nan.0" with Datum.Flonum f -> f | _ -> 0.0))

let test_reader_symbols () =
  check_datum "foo" (Datum.Symbol "foo") (read_one "foo");
  check_datum "+" (Datum.Symbol "+") (read_one "+");
  check_datum "-" (Datum.Symbol "-") (read_one "-");
  check_datum "..." (Datum.Symbol "...") (read_one "...");
  check_datum "->string" (Datum.Symbol "->string") (read_one "->string");
  check_datum "list->vector" (Datum.Symbol "list->vector") (read_one "list->vector");
  check_datum "+soup+" (Datum.Symbol "+soup+") (read_one "+soup+");
  check_datum "<=?" (Datum.Symbol "<=?") (read_one "<=?");
  check_datum "$caml" (Datum.Symbol "$caml") (read_one "$caml");
  check_datum "@prefix" (Datum.Symbol "@prefix") (read_one "@prefix")

let test_reader_symbols_escaped () =
  check_datum "|hello world|" (Datum.Symbol "hello world") (read_one "|hello world|");
  check_datum "|a\\nb|" (Datum.Symbol "a\nb") (read_one "|a\\nb|");
  check_datum "|\\x41;|" (Datum.Symbol "A") (read_one "|\\x41;|");
  check_datum "||" (Datum.Symbol "") (read_one "||");
  (* | acts as delimiter: foo|bar| is two tokens *)
  let p = Port.of_string "foo|bar|" in
  let rt = Readtable.default in
  check_datum "foo before |" (Datum.Symbol "foo") (Reader.read rt p);
  check_datum "bar in |...|" (Datum.Symbol "bar") (Reader.read rt p)

let test_reader_fold_case () =
  let rt = Readtable.with_fold_case true Readtable.default in
  let read s = Reader.read rt (Port.of_string s) in
  check_datum "FOO -> foo" (Datum.Symbol "foo") (read "FOO");
  check_datum "Hello -> hello" (Datum.Symbol "hello") (read "Hello")

let test_reader_characters () =
  check_datum "#\\a" (Datum.Char (Uchar.of_char 'a')) (read_one "#\\a");
  check_datum "#\\A" (Datum.Char (Uchar.of_char 'A')) (read_one "#\\A");
  check_datum "#\\space" (Datum.Char (Uchar.of_int 0x20)) (read_one "#\\space");
  check_datum "#\\newline" (Datum.Char (Uchar.of_int 0x0A)) (read_one "#\\newline");
  check_datum "#\\tab" (Datum.Char (Uchar.of_int 0x09)) (read_one "#\\tab");
  check_datum "#\\alarm" (Datum.Char (Uchar.of_int 0x07)) (read_one "#\\alarm");
  check_datum "#\\backspace" (Datum.Char (Uchar.of_int 0x08)) (read_one "#\\backspace");
  check_datum "#\\delete" (Datum.Char (Uchar.of_int 0x7F)) (read_one "#\\delete");
  check_datum "#\\escape" (Datum.Char (Uchar.of_int 0x1B)) (read_one "#\\escape");
  check_datum "#\\null" (Datum.Char (Uchar.of_int 0x00)) (read_one "#\\null");
  check_datum "#\\return" (Datum.Char (Uchar.of_int 0x0D)) (read_one "#\\return");
  check_datum "#\\x41" (Datum.Char (Uchar.of_char 'A')) (read_one "#\\x41");
  check_datum "#\\x03BB" (Datum.Char (Uchar.of_int 0x03BB)) (read_one "#\\x03BB");
  (* Delimiter characters after #\ *)
  check_datum "#\\ (space)" (Datum.Char (Uchar.of_char ' ')) (read_one "#\\ ");
  check_datum "#\\( (lparen)" (Datum.Char (Uchar.of_char '(')) (read_one "#\\(");
  check_datum "#\\) (rparen)" (Datum.Char (Uchar.of_char ')')) (read_one "#\\)")

let test_reader_strings () =
  check_datum "simple" (Datum.Str "hello") (read_one "\"hello\"");
  check_datum "empty" (Datum.Str "") (read_one "\"\"");
  check_datum "spaces" (Datum.Str "hello world") (read_one "\"hello world\"");
  check_datum "\\n" (Datum.Str "a\nb") (read_one "\"a\\nb\"");
  check_datum "\\t" (Datum.Str "a\tb") (read_one "\"a\\tb\"");
  check_datum "\\r" (Datum.Str "a\rb") (read_one "\"a\\rb\"");
  check_datum "\\\"" (Datum.Str "a\"b") (read_one "\"a\\\"b\"");
  check_datum "\\\\" (Datum.Str "a\\b") (read_one "\"a\\\\b\"");
  check_datum "\\a" (Datum.Str "a\007b") (read_one "\"a\\ab\"");
  check_datum "\\b" (Datum.Str "a\008b") (read_one "\"a\\bb\"");
  check_datum "\\x41;" (Datum.Str "A") (read_one "\"\\x41;\"");
  check_datum "\\x3BB;" (Datum.Str "\xCE\xBB") (read_one "\"\\x3BB;\"");
  (* Line continuation *)
  check_datum "line cont" (Datum.Str "ab") (read_one "\"a\\\n   b\"")

(* Compound data *)

let test_reader_empty_list () =
  check_datum "()" Datum.Nil (read_one "()");
  check_datum "( )" Datum.Nil (read_one "( )");
  check_datum "(\n)" Datum.Nil (read_one "(\n)")

let test_reader_proper_list () =
  let expected = Datum.Pair (Datum.Fixnum 1,
    Datum.Pair (Datum.Fixnum 2,
      Datum.Pair (Datum.Fixnum 3, Datum.Nil))) in
  check_datum "(1 2 3)" expected (read_one "(1 2 3)");
  check_datum "( 1  2  3 )" expected (read_one "( 1  2  3 )")

let test_reader_nested_list () =
  let inner = Datum.Pair (Datum.Fixnum 2, Datum.Pair (Datum.Fixnum 3, Datum.Nil)) in
  let expected = Datum.Pair (Datum.Fixnum 1, Datum.Pair (inner, Datum.Nil)) in
  check_datum "(1 (2 3))" expected (read_one "(1 (2 3))")

let test_reader_dotted_pair () =
  check_datum "(1 . 2)" (Datum.Pair (Datum.Fixnum 1, Datum.Fixnum 2))
    (read_one "(1 . 2)")

let test_reader_dotted_list () =
  let expected = Datum.Pair (Datum.Fixnum 1,
    Datum.Pair (Datum.Fixnum 2, Datum.Fixnum 3)) in
  check_datum "(1 2 . 3)" expected (read_one "(1 2 . 3)")

let test_reader_vector () =
  check_datum "#()" (Datum.Vector [||]) (read_one "#()");
  check_datum "#(1 2 3)" (Datum.Vector [| Datum.Fixnum 1; Datum.Fixnum 2; Datum.Fixnum 3 |])
    (read_one "#(1 2 3)")

let test_reader_bytevector () =
  check_datum "#u8()" (Datum.Bytevector (Bytes.of_string "")) (read_one "#u8()");
  check_datum "#u8(1 2 3)" (Datum.Bytevector (Bytes.of_string "\x01\x02\x03"))
    (read_one "#u8(1 2 3)");
  check_datum "#u8(0 255)" (Datum.Bytevector (Bytes.of_string "\x00\xff"))
    (read_one "#u8(0 255)")

let test_reader_quote_shorthands () =
  (* 'x -> (quote x) *)
  let expected_quote = Datum.Pair (Datum.Symbol "quote",
    Datum.Pair (Datum.Symbol "x", Datum.Nil)) in
  check_datum "'x" expected_quote (read_one "'x");

  (* `x -> (quasiquote x) *)
  let expected_quasi = Datum.Pair (Datum.Symbol "quasiquote",
    Datum.Pair (Datum.Symbol "x", Datum.Nil)) in
  check_datum "`x" expected_quasi (read_one "`x");

  (* ,x -> (unquote x) *)
  let expected_unquote = Datum.Pair (Datum.Symbol "unquote",
    Datum.Pair (Datum.Symbol "x", Datum.Nil)) in
  check_datum ",x" expected_unquote (read_one ",x");

  (* ,@x -> (unquote-splicing x) *)
  let expected_splice = Datum.Pair (Datum.Symbol "unquote-splicing",
    Datum.Pair (Datum.Symbol "x", Datum.Nil)) in
  check_datum ",@x" expected_splice (read_one ",@x")

(* Comments *)

let test_reader_line_comment () =
  check_datum "; comment\n42" (Datum.Fixnum 42) (read_one "; comment\n42");
  check_datum "42 ; comment" (Datum.Fixnum 42) (read_one "42 ; comment")

let test_reader_block_comment () =
  check_datum "#| |# 42" (Datum.Fixnum 42) (read_one "#| comment |# 42");
  check_datum "nested" (Datum.Fixnum 42) (read_one "#| #| nested |# |# 42")

let test_reader_datum_comment () =
  check_datum "#;1 2" (Datum.Fixnum 2) (read_one "#;1 2");
  check_datum "#;(1 2) 3" (Datum.Fixnum 3) (read_one "#;(1 2) 3");
  check_datum "nested #;" (Datum.Fixnum 3) (read_one "#;#;1 2 3");
  (* Inside list *)
  let expected = Datum.Pair (Datum.Fixnum 1, Datum.Pair (Datum.Fixnum 3, Datum.Nil)) in
  check_datum "(1 #;2 3)" expected (read_one "(1 #;2 3)")

let test_reader_fold_case_directive () =
  let p = Port.of_string "#!fold-case FOO #!no-fold-case BAR" in
  let rt = Readtable.default in
  let d1 = Reader.read rt p in
  let d2 = Reader.read rt p in
  check_datum "FOO -> foo" (Datum.Symbol "foo") d1;
  check_datum "BAR stays BAR" (Datum.Symbol "BAR") d2;
  (* fold-case persists across multiple read calls on same port *)
  let p2 = Port.of_string "#!fold-case FOO BAR BAZ" in
  let d3 = Reader.read rt p2 in
  let d4 = Reader.read rt p2 in
  let d5 = Reader.read rt p2 in
  check_datum "FOO -> foo (persist)" (Datum.Symbol "foo") d3;
  check_datum "BAR -> bar (persist)" (Datum.Symbol "bar") d4;
  check_datum "BAZ -> baz (persist)" (Datum.Symbol "baz") d5

(* Datum labels *)

let test_reader_datum_labels () =
  check_datum "#0=42 -> 42" (Datum.Fixnum 42) (read_one "#0=42");
  (* Define and reference within same datum *)
  let expected = Datum.Pair (Datum.Fixnum 42, Datum.Pair (Datum.Fixnum 42, Datum.Nil)) in
  check_datum "(#0=42 #0#)" expected (read_one "(#0=42 #0#)");
  (* Multiple labels *)
  let expected2 = Datum.Pair (Datum.Symbol "a",
    Datum.Pair (Datum.Symbol "b",
      Datum.Pair (Datum.Symbol "a", Datum.Nil))) in
  check_datum "(#0=a #1=b #0#)" expected2 (read_one "(#0=a #1=b #0#)")

(* Error cases *)

let test_reader_errors () =
  let check_error name input =
    try
      ignore (read_one input);
      Alcotest.fail (Printf.sprintf "%s should have raised error" name)
    with Reader.Read_error _ -> ()
  in
  check_error "unterminated string" "\"hello";
  check_error "unterminated list" "(1 2";
  check_error "unexpected close" ")";
  check_error "unexpected dot" ".";
  check_error "bad #" "#z";
  check_error "empty char" "#\\";
  check_error "unterminated block comment" "#| oops";
  check_error "bytevector range" "#u8(256)";
  check_error "undefined label" "#99#"

(* Location tracking *)

let test_reader_location () =
  let s = read_syntax_one "  foo" in
  Alcotest.(check int) "col" 3 s.loc.col;

  let p = Port.of_string "\n\n  bar" in
  let s2 = Reader.read_syntax Readtable.default p in
  Alcotest.(check int) "line" 3 s2.loc.line;
  Alcotest.(check int) "col" 3 s2.loc.col

(* Integration tests *)

let test_reader_multiple_reads () =
  let p = Port.of_string "1 2 3" in
  let rt = Readtable.default in
  check_datum "first" (Datum.Fixnum 1) (Reader.read rt p);
  check_datum "second" (Datum.Fixnum 2) (Reader.read rt p);
  check_datum "third" (Datum.Fixnum 3) (Reader.read rt p);
  check_datum "eof" Datum.Eof (Reader.read rt p)

let test_reader_complex_expr () =
  let code = "(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))" in
  let d = read_one code in
  (* Just check it parses and has the right structure *)
  match d with
  | Datum.Pair (Datum.Symbol "define", _) -> ()
  | _ -> Alcotest.fail "expected (define ...)"

let test_reader_all_types () =
  let code = "(#t #f 42 3.14 #\\a \"str\" sym () #(1) #u8(1))" in
  let d = read_one code in
  match d with
  | Datum.Pair _ -> () (* It's a list, that's all we check *)
  | _ -> Alcotest.fail "expected list"

(* QCheck round-trip tests *)

let gen_simple_datum =
  QCheck2.Gen.(oneof [
    map (fun b -> Datum.Bool b) bool;
    map (fun n -> Datum.Fixnum n) (int_range (-1000) 1000);
    map (fun s -> Datum.Symbol s) (
      let* first = oneof_list ['a'; 'b'; 'c'; 'x'; 'y'; 'z'] in
      let* rest = string_size ~gen:(oneof_list ['a';'b';'c';'0';'1';'-']) (int_range 0 5) in
      return (String.make 1 first ^ rest)
    );
  ])

let qcheck_roundtrip =
  QCheck2.Test.make ~name:"datum roundtrip" ~count:100
    gen_simple_datum
    (fun d ->
      let s = Datum.to_string d in
      let d' = read_one s in
      Datum.equal d d')

let () =
  Alcotest.run "Reader"
    [ ("Atoms",
       [ Alcotest.test_case "eof" `Quick test_reader_eof
       ; Alcotest.test_case "whitespace" `Quick test_reader_whitespace
       ; Alcotest.test_case "booleans" `Quick test_reader_booleans
       ; Alcotest.test_case "integers" `Quick test_reader_integers
       ; Alcotest.test_case "radix prefixes" `Quick test_reader_radix
       ; Alcotest.test_case "exactness prefixes" `Quick test_reader_exactness
       ; Alcotest.test_case "floats" `Quick test_reader_floats
       ; Alcotest.test_case "symbols" `Quick test_reader_symbols
       ; Alcotest.test_case "escaped symbols" `Quick test_reader_symbols_escaped
       ; Alcotest.test_case "fold-case" `Quick test_reader_fold_case
       ; Alcotest.test_case "characters" `Quick test_reader_characters
       ; Alcotest.test_case "strings" `Quick test_reader_strings
       ])
    ; ("Compound",
       [ Alcotest.test_case "empty list" `Quick test_reader_empty_list
       ; Alcotest.test_case "proper list" `Quick test_reader_proper_list
       ; Alcotest.test_case "nested list" `Quick test_reader_nested_list
       ; Alcotest.test_case "dotted pair" `Quick test_reader_dotted_pair
       ; Alcotest.test_case "dotted list" `Quick test_reader_dotted_list
       ; Alcotest.test_case "vector" `Quick test_reader_vector
       ; Alcotest.test_case "bytevector" `Quick test_reader_bytevector
       ; Alcotest.test_case "quote shorthands" `Quick test_reader_quote_shorthands
       ])
    ; ("Comments",
       [ Alcotest.test_case "line comment" `Quick test_reader_line_comment
       ; Alcotest.test_case "block comment" `Quick test_reader_block_comment
       ; Alcotest.test_case "datum comment" `Quick test_reader_datum_comment
       ; Alcotest.test_case "fold-case directive" `Quick test_reader_fold_case_directive
       ])
    ; ("Labels",
       [ Alcotest.test_case "datum labels" `Quick test_reader_datum_labels
       ])
    ; ("Errors",
       [ Alcotest.test_case "error cases" `Quick test_reader_errors
       ])
    ; ("Integration",
       [ Alcotest.test_case "location" `Quick test_reader_location
       ; Alcotest.test_case "multiple reads" `Quick test_reader_multiple_reads
       ; Alcotest.test_case "complex expression" `Quick test_reader_complex_expr
       ; Alcotest.test_case "all types" `Quick test_reader_all_types
       ; QCheck_alcotest.to_alcotest qcheck_roundtrip
       ])
    ]
