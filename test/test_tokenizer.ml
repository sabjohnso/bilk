open Wile

let rt = Readtable.default

let kinds text =
  List.map (fun (t : Tokenizer.token) -> t.kind) (Tokenizer.tokenize rt text)

let check_kinds msg expected text =
  let actual = kinds text in
  let pp_kind fmt k =
    Format.pp_print_string fmt (match k with
      | Tokenizer.Paren_open -> "Paren_open"
      | Tokenizer.Paren_close -> "Paren_close"
      | Tokenizer.String_lit -> "String_lit"
      | Tokenizer.Number_lit -> "Number_lit"
      | Tokenizer.Boolean_lit -> "Boolean_lit"
      | Tokenizer.Char_lit -> "Char_lit"
      | Tokenizer.Symbol -> "Symbol"
      | Tokenizer.Keyword -> "Keyword"
      | Tokenizer.Quote_shorthand -> "Quote_shorthand"
      | Tokenizer.Comment -> "Comment"
      | Tokenizer.Datum_comment -> "Datum_comment"
      | Tokenizer.Whitespace -> "Whitespace"
      | Tokenizer.Hash_prefix -> "Hash_prefix"
      | Tokenizer.Error -> "Error")
  in
  let kind_t = Alcotest.testable pp_kind ( = ) in
  Alcotest.(check (list kind_t)) msg expected actual

(* --- Complete coverage --- *)

let test_empty () =
  check_kinds "empty" [] ""

let test_whitespace () =
  check_kinds "spaces" [Tokenizer.Whitespace] "   ";
  check_kinds "tabs" [Tokenizer.Whitespace] "\t\t";
  check_kinds "newlines" [Tokenizer.Whitespace] "\n\n"

let test_parens () =
  check_kinds "open/close" [Paren_open; Paren_close] "()";
  check_kinds "brackets" [Paren_open; Paren_close] "[]"

let test_string () =
  check_kinds "simple string" [String_lit] {|"hello"|};
  check_kinds "escaped" [String_lit] {|"he\"llo"|};
  check_kinds "with escapes" [String_lit] {|"a\nb"|}

let test_unterminated_string () =
  check_kinds "unterminated" [String_lit] {|"hello|}

let test_number () =
  check_kinds "integer" [Number_lit] "42";
  check_kinds "float" [Number_lit] "3.14";
  check_kinds "negative" [Number_lit] "-17";
  check_kinds "positive" [Number_lit] "+5"

let test_boolean () =
  check_kinds "#t" [Boolean_lit] "#t";
  check_kinds "#f" [Boolean_lit] "#f";
  check_kinds "#true" [Boolean_lit] "#true";
  check_kinds "#false" [Boolean_lit] "#false"

let test_char_lit () =
  check_kinds "char a" [Char_lit] "#\\a";
  check_kinds "char space" [Char_lit] "#\\space";
  check_kinds "char newline" [Char_lit] "#\\newline"

let test_symbol () =
  check_kinds "symbol" [Symbol] "hello";
  check_kinds "symbol +" [Symbol] "+";
  check_kinds "symbol ..." [Symbol] "..."

let test_keyword () =
  check_kinds "define" [Keyword] "define";
  check_kinds "lambda" [Keyword] "lambda";
  check_kinds "if" [Keyword] "if";
  check_kinds "let" [Keyword] "let";
  check_kinds "cond" [Keyword] "cond"

let test_quote () =
  check_kinds "quote" [Quote_shorthand] "'";
  check_kinds "backquote" [Quote_shorthand] "`";
  check_kinds "unquote" [Quote_shorthand] ",";
  check_kinds "unquote-splicing" [Quote_shorthand] ",@"

let test_comment () =
  check_kinds "line comment" [Comment] "; hello"

let test_block_comment () =
  check_kinds "block comment" [Comment] "#| nested |#";
  check_kinds "nested block" [Comment] "#| #| inner |# outer |#"

let test_datum_comment () =
  check_kinds "datum comment" [Datum_comment] "#;"

let test_vector () =
  check_kinds "#(" [Paren_open] "#("

let test_bytevector () =
  check_kinds "#u8(" [Paren_open] "#u8("

let test_number_prefix () =
  check_kinds "#x" [Number_lit] "#xFF";
  check_kinds "#b" [Number_lit] "#b101";
  check_kinds "#e" [Number_lit] "#e1.5"

let test_pipe_identifier () =
  check_kinds "pipe id" [Symbol] "|hello world|"

let test_directive () =
  check_kinds "fold-case" [Comment] "#!fold-case"

(* --- Complete expression --- *)

let test_full_expression () =
  check_kinds "(define x 42)"
    [Paren_open; Keyword; Whitespace; Symbol; Whitespace; Number_lit; Paren_close]
    "(define x 42)"

let test_nested_expression () =
  check_kinds "(+ 1 (- 2 3))"
    [Paren_open; Symbol; Whitespace; Number_lit; Whitespace;
     Paren_open; Symbol; Whitespace; Number_lit; Whitespace; Number_lit;
     Paren_close; Paren_close]
    "(+ 1 (- 2 3))"

let test_quoted () =
  check_kinds "'(1 2)"
    [Quote_shorthand; Paren_open; Number_lit; Whitespace; Number_lit; Paren_close]
    "'(1 2)"

let test_string_in_expr () =
  check_kinds "(display \"hello\")"
    [Paren_open; Symbol; Whitespace; String_lit; Paren_close]
    "(display \"hello\")"

(* --- Span coverage --- *)

let test_spans_cover_input () =
  let text = "(define (f x) (+ x 1))" in
  let tokens = Tokenizer.tokenize rt text in
  let total = List.fold_left (fun acc (t : Tokenizer.token) ->
    acc + t.span.stop - t.span.start
  ) 0 tokens in
  Alcotest.(check int) "total coverage" (String.length text) total

let test_spans_contiguous () =
  let text = "(define (f x) (+ x 1))" in
  let tokens = Tokenizer.tokenize rt text in
  let rec check = function
    | [] | [_] -> ()
    | a :: (b :: _ as rest) ->
      Alcotest.(check int) "contiguous"
        (a : Tokenizer.token).span.stop (b : Tokenizer.token).span.start;
      check rest
  in
  check tokens

(* --- Keyword detection --- *)

let test_is_keyword () =
  Alcotest.(check bool) "define" true (Tokenizer.is_keyword "define");
  Alcotest.(check bool) "lambda" true (Tokenizer.is_keyword "lambda");
  Alcotest.(check bool) "foo" false (Tokenizer.is_keyword "foo");
  Alcotest.(check bool) "+" false (Tokenizer.is_keyword "+")

let () =
  Alcotest.run "Tokenizer" [
    "basic", [
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "whitespace" `Quick test_whitespace;
      Alcotest.test_case "parens" `Quick test_parens;
      Alcotest.test_case "string" `Quick test_string;
      Alcotest.test_case "unterminated string" `Quick test_unterminated_string;
      Alcotest.test_case "number" `Quick test_number;
      Alcotest.test_case "boolean" `Quick test_boolean;
      Alcotest.test_case "char literal" `Quick test_char_lit;
      Alcotest.test_case "symbol" `Quick test_symbol;
      Alcotest.test_case "keyword" `Quick test_keyword;
      Alcotest.test_case "quote" `Quick test_quote;
      Alcotest.test_case "comment" `Quick test_comment;
      Alcotest.test_case "block comment" `Quick test_block_comment;
      Alcotest.test_case "datum comment" `Quick test_datum_comment;
      Alcotest.test_case "vector" `Quick test_vector;
      Alcotest.test_case "bytevector" `Quick test_bytevector;
      Alcotest.test_case "number prefix" `Quick test_number_prefix;
      Alcotest.test_case "pipe identifier" `Quick test_pipe_identifier;
      Alcotest.test_case "directive" `Quick test_directive;
    ];
    "expressions", [
      Alcotest.test_case "full expression" `Quick test_full_expression;
      Alcotest.test_case "nested" `Quick test_nested_expression;
      Alcotest.test_case "quoted" `Quick test_quoted;
      Alcotest.test_case "string in expr" `Quick test_string_in_expr;
    ];
    "spans", [
      Alcotest.test_case "coverage" `Quick test_spans_cover_input;
      Alcotest.test_case "contiguous" `Quick test_spans_contiguous;
    ];
    "keywords", [
      Alcotest.test_case "is_keyword" `Quick test_is_keyword;
    ];
  ]
