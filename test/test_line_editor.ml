open Wile

let check_int msg expected actual =
  Alcotest.(check int) msg expected actual

(* --- cursor_row --- *)

let test_cursor_row_empty () =
  check_int "empty" 0 (Line_editor.cursor_row "" 0)

let test_cursor_row_single_line () =
  check_int "mid single" 0 (Line_editor.cursor_row "hello" 3);
  check_int "end single" 0 (Line_editor.cursor_row "hello" 5)

let test_cursor_row_multi_line () =
  let text = "abc\ndef\nghi" in
  check_int "start" 0 (Line_editor.cursor_row text 0);
  check_int "before newline" 0 (Line_editor.cursor_row text 3);
  check_int "after first newline" 1 (Line_editor.cursor_row text 4);
  check_int "second line" 1 (Line_editor.cursor_row text 6);
  check_int "after second newline" 2 (Line_editor.cursor_row text 8);
  check_int "end" 2 (Line_editor.cursor_row text 11)

(* --- cursor_col --- *)

let test_cursor_col_empty () =
  check_int "empty" 0 (Line_editor.cursor_col "" 0)

let test_cursor_col_single_line () =
  check_int "start" 0 (Line_editor.cursor_col "hello" 0);
  check_int "mid" 3 (Line_editor.cursor_col "hello" 3);
  check_int "end" 5 (Line_editor.cursor_col "hello" 5)

let test_cursor_col_multi_line () =
  let text = "abc\ndef\nghi" in
  check_int "start of first" 0 (Line_editor.cursor_col text 0);
  check_int "end of first" 3 (Line_editor.cursor_col text 3);
  check_int "start of second" 0 (Line_editor.cursor_col text 4);
  check_int "mid of second" 2 (Line_editor.cursor_col text 6);
  check_int "start of third" 0 (Line_editor.cursor_col text 8);
  check_int "end of third" 3 (Line_editor.cursor_col text 11)

(* --- num_lines --- *)

let test_num_lines () =
  check_int "empty" 1 (Line_editor.num_lines "");
  check_int "single" 1 (Line_editor.num_lines "hello");
  check_int "two" 2 (Line_editor.num_lines "a\nb");
  check_int "three" 3 (Line_editor.num_lines "a\nb\nc");
  check_int "trailing newline" 2 (Line_editor.num_lines "hello\n")

(* --- row_start --- *)

let test_row_start () =
  let text = "abc\ndef\nghi" in
  check_int "row 0" 0 (Line_editor.row_start text 0);
  check_int "row 1" 4 (Line_editor.row_start text 1);
  check_int "row 2" 8 (Line_editor.row_start text 2)

let test_row_start_varying_lengths () =
  let text = "a\nbc\ndef" in
  check_int "row 0" 0 (Line_editor.row_start text 0);
  check_int "row 1" 2 (Line_editor.row_start text 1);
  check_int "row 2" 5 (Line_editor.row_start text 2)

(* --- row_length --- *)

let test_row_length () =
  let text = "abc\ndef\nghi" in
  check_int "row 0" 3 (Line_editor.row_length text 0);
  check_int "row 1" 3 (Line_editor.row_length text 1);
  check_int "row 2" 3 (Line_editor.row_length text 2)

let test_row_length_varying () =
  let text = "a\nbcde\nfg" in
  check_int "row 0" 1 (Line_editor.row_length text 0);
  check_int "row 1" 4 (Line_editor.row_length text 1);
  check_int "row 2" 2 (Line_editor.row_length text 2)

let test_row_length_empty_line () =
  let text = "a\n\nb" in
  check_int "empty middle" 0 (Line_editor.row_length text 1)

(* --- pos_of_row_col --- *)

let test_pos_of_row_col () =
  let text = "abc\ndef\nghi" in
  check_int "(0,0)" 0 (Line_editor.pos_of_row_col text 0 0);
  check_int "(0,2)" 2 (Line_editor.pos_of_row_col text 0 2);
  check_int "(1,1)" 5 (Line_editor.pos_of_row_col text 1 1);
  check_int "(2,3)" 11 (Line_editor.pos_of_row_col text 2 3)

let test_pos_of_row_col_clamp () =
  let text = "a\nbcde\nf" in
  (* Row 0 has length 1, requesting col 5 → clamped to 1: pos = 0 + 1 = 1 *)
  check_int "clamp row 0" 1 (Line_editor.pos_of_row_col text 0 5);
  (* Row 2 starts at 7, has length 1, requesting col 3 → clamped: pos = 7 + 1 = 8 *)
  check_int "clamp row 2" 8 (Line_editor.pos_of_row_col text 2 3)

(* --- word_forward --- *)

let test_word_forward () =
  let text = "hello world" in
  check_int "from 0" 6 (Line_editor.word_forward text 0);
  check_int "from 6" 11 (Line_editor.word_forward text 6);
  check_int "at end" 11 (Line_editor.word_forward text 11)

let test_word_forward_scheme () =
  let text = "(define (f x) (+ x 1))" in
  (* From 0: '(' is non-word, skip word (none), skip non-word '(' → 1 *)
  check_int "past open paren" 1 (Line_editor.word_forward text 0);
  (* From 1: skip word 'define' → 7, skip non-word ' (' → 9 (at 'f') *)
  check_int "past define" 9 (Line_editor.word_forward text 1)

(* --- word_backward --- *)

let test_word_backward () =
  let text = "hello world" in
  check_int "from end" 6 (Line_editor.word_backward text 11);
  check_int "from 6" 0 (Line_editor.word_backward text 6);
  check_int "at start" 0 (Line_editor.word_backward text 0)

let test_word_backward_scheme () =
  let text = "(+ x 1)" in
  (* From 7 (end): skip non-word ')' at 6 → 5, skip word '1' at 5 → 4, result 5 *)
  check_int "from end to 1" 5 (Line_editor.word_backward text 7);
  (* From 5 (at '1'): skip non-word ' ' at 4 → 3, skip word 'x' at 3 → 2, result 3 *)
  check_int "from 5 to x" 3 (Line_editor.word_backward text 5)

(* --- Cross-line cursor movement via pos_of_row_col --- *)

let test_cursor_move_up () =
  let text = "abcdef\nghi" in
  (* Cursor at (1, 2) = pos 9, move up: (0, 2) = pos 2 *)
  let row = Line_editor.cursor_row text 9 in
  let col = Line_editor.cursor_col text 9 in
  check_int "row" 1 row;
  check_int "col" 2 col;
  let new_pos = Line_editor.pos_of_row_col text (row - 1) col in
  check_int "up" 2 new_pos

let test_cursor_move_down () =
  let text = "ab\ncdef" in
  (* Cursor at (0, 1) = pos 1, move down: (1, 1) = pos 4 *)
  let row = Line_editor.cursor_row text 1 in
  let col = Line_editor.cursor_col text 1 in
  check_int "row" 0 row;
  check_int "col" 1 col;
  let new_pos = Line_editor.pos_of_row_col text (row + 1) col in
  check_int "down" 4 new_pos

let test_cursor_move_up_clamp () =
  let text = "ab\ncdef" in
  (* Cursor at (1, 3) = pos 6, move up: (0, 3) → clamped to (0, 2) = pos 2 *)
  let row = Line_editor.cursor_row text 6 in
  let col = Line_editor.cursor_col text 6 in
  check_int "row" 1 row;
  check_int "col" 3 col;
  let new_pos = Line_editor.pos_of_row_col text (row - 1) col in
  check_int "up clamp" 2 new_pos

let () =
  Alcotest.run "Line_editor" [
    "cursor_row", [
      Alcotest.test_case "empty" `Quick test_cursor_row_empty;
      Alcotest.test_case "single line" `Quick test_cursor_row_single_line;
      Alcotest.test_case "multi line" `Quick test_cursor_row_multi_line;
    ];
    "cursor_col", [
      Alcotest.test_case "empty" `Quick test_cursor_col_empty;
      Alcotest.test_case "single line" `Quick test_cursor_col_single_line;
      Alcotest.test_case "multi line" `Quick test_cursor_col_multi_line;
    ];
    "num_lines", [
      Alcotest.test_case "basic" `Quick test_num_lines;
    ];
    "row_start", [
      Alcotest.test_case "equal lengths" `Quick test_row_start;
      Alcotest.test_case "varying lengths" `Quick test_row_start_varying_lengths;
    ];
    "row_length", [
      Alcotest.test_case "equal lengths" `Quick test_row_length;
      Alcotest.test_case "varying lengths" `Quick test_row_length_varying;
      Alcotest.test_case "empty line" `Quick test_row_length_empty_line;
    ];
    "pos_of_row_col", [
      Alcotest.test_case "basic" `Quick test_pos_of_row_col;
      Alcotest.test_case "clamp" `Quick test_pos_of_row_col_clamp;
    ];
    "word_forward", [
      Alcotest.test_case "basic" `Quick test_word_forward;
      Alcotest.test_case "scheme" `Quick test_word_forward_scheme;
    ];
    "word_backward", [
      Alcotest.test_case "basic" `Quick test_word_backward;
      Alcotest.test_case "scheme" `Quick test_word_backward_scheme;
    ];
    "cross-line cursor", [
      Alcotest.test_case "move up" `Quick test_cursor_move_up;
      Alcotest.test_case "move down" `Quick test_cursor_move_down;
      Alcotest.test_case "move up clamp" `Quick test_cursor_move_up_clamp;
    ];
  ]
