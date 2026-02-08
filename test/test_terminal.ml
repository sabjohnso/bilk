open Wile

let parse bytes_list =
  let len = List.length bytes_list in
  let buf = Bytes.create len in
  List.iteri (fun i b -> Bytes.set buf i (Char.chr b)) bytes_list;
  Terminal.parse_key_bytes buf len

(* --- Key parsing tests --- *)

let test_printable_char () =
  Alcotest.(check bool) "letter a" true
    (parse [0x61] = Terminal.Char 'a');
  Alcotest.(check bool) "letter Z" true
    (parse [0x5A] = Terminal.Char 'Z');
  Alcotest.(check bool) "space" true
    (parse [0x20] = Terminal.Char ' ');
  Alcotest.(check bool) "tilde" true
    (parse [0x7E] = Terminal.Char '~')

let test_enter () =
  Alcotest.(check bool) "CR" true (parse [0x0D] = Terminal.Enter);
  Alcotest.(check bool) "LF" true (parse [0x0A] = Terminal.Enter)

let test_backspace () =
  Alcotest.(check bool) "DEL" true (parse [0x7F] = Terminal.Backspace);
  Alcotest.(check bool) "BS" true (parse [0x08] = Terminal.Backspace)

let test_tab () =
  Alcotest.(check bool) "tab" true (parse [0x09] = Terminal.Tab)

let test_ctrl_keys () =
  Alcotest.(check bool) "Ctrl-A" true (parse [0x01] = Terminal.Ctrl_a);
  Alcotest.(check bool) "Ctrl-B" true (parse [0x02] = Terminal.Ctrl_b);
  Alcotest.(check bool) "Ctrl-C" true (parse [0x03] = Terminal.Ctrl_c);
  Alcotest.(check bool) "Ctrl-D" true (parse [0x04] = Terminal.Ctrl_d);
  Alcotest.(check bool) "Ctrl-E" true (parse [0x05] = Terminal.Ctrl_e);
  Alcotest.(check bool) "Ctrl-F" true (parse [0x06] = Terminal.Ctrl_f);
  Alcotest.(check bool) "Ctrl-K" true (parse [0x0B] = Terminal.Ctrl_k);
  Alcotest.(check bool) "Ctrl-L" true (parse [0x0C] = Terminal.Ctrl_l);
  Alcotest.(check bool) "Ctrl-N" true (parse [0x0E] = Terminal.Ctrl_n);
  Alcotest.(check bool) "Ctrl-P" true (parse [0x10] = Terminal.Ctrl_p);
  Alcotest.(check bool) "Ctrl-U" true (parse [0x15] = Terminal.Ctrl_u);
  Alcotest.(check bool) "Ctrl-W" true (parse [0x17] = Terminal.Ctrl_w)

let test_arrow_keys () =
  (* ESC [ A/B/C/D *)
  Alcotest.(check bool) "Up" true
    (parse [0x1B; 0x5B; Char.code 'A'] = Terminal.Up);
  Alcotest.(check bool) "Down" true
    (parse [0x1B; 0x5B; Char.code 'B'] = Terminal.Down);
  Alcotest.(check bool) "Right" true
    (parse [0x1B; 0x5B; Char.code 'C'] = Terminal.Right);
  Alcotest.(check bool) "Left" true
    (parse [0x1B; 0x5B; Char.code 'D'] = Terminal.Left)

let test_home_end () =
  Alcotest.(check bool) "Home (H)" true
    (parse [0x1B; 0x5B; Char.code 'H'] = Terminal.Home);
  Alcotest.(check bool) "End (F)" true
    (parse [0x1B; 0x5B; Char.code 'F'] = Terminal.End_key);
  Alcotest.(check bool) "Home (1~)" true
    (parse [0x1B; 0x5B; Char.code '1'; Char.code '~'] = Terminal.Home);
  Alcotest.(check bool) "End (4~)" true
    (parse [0x1B; 0x5B; Char.code '4'; Char.code '~'] = Terminal.End_key)

let test_delete_key () =
  Alcotest.(check bool) "Delete (3~)" true
    (parse [0x1B; 0x5B; Char.code '3'; Char.code '~'] = Terminal.Delete)

let test_alt_arrows () =
  (* ESC [ 1;3C / ESC [ 1;3D *)
  Alcotest.(check bool) "Alt-Right" true
    (parse [0x1B; 0x5B; Char.code '1'; Char.code ';'; Char.code '3'; Char.code 'C']
     = Terminal.Alt_right);
  Alcotest.(check bool) "Alt-Left" true
    (parse [0x1B; 0x5B; Char.code '1'; Char.code ';'; Char.code '3'; Char.code 'D']
     = Terminal.Alt_left)

let test_alt_bf () =
  (* ESC b / ESC f (common Alt-Left/Right encoding) *)
  Alcotest.(check bool) "Alt-b (backward word)" true
    (parse [0x1B; Char.code 'b'] = Terminal.Alt_left);
  Alcotest.(check bool) "Alt-f (forward word)" true
    (parse [0x1B; Char.code 'f'] = Terminal.Alt_right)

let test_alt_backspace () =
  (* ESC DEL *)
  Alcotest.(check bool) "Alt-Backspace (ESC DEL)" true
    (parse [0x1B; 0x7F] = Terminal.Alt_backspace);
  Alcotest.(check bool) "Alt-Backspace (ESC BS)" true
    (parse [0x1B; 0x08] = Terminal.Alt_backspace)

let test_alt_enter () =
  (* ESC CR *)
  Alcotest.(check bool) "Alt-Enter (ESC CR)" true
    (parse [0x1B; 0x0D] = Terminal.Alt_enter);
  (* ESC LF *)
  Alcotest.(check bool) "Alt-Enter (ESC LF)" true
    (parse [0x1B; 0x0A] = Terminal.Alt_enter)

let test_ctrl_arrows () =
  (* ESC [ 1;5C / ESC [ 1;5D *)
  Alcotest.(check bool) "Ctrl-Right" true
    (parse [0x1B; 0x5B; Char.code '1'; Char.code ';'; Char.code '5'; Char.code 'C']
     = Terminal.Ctrl_right);
  Alcotest.(check bool) "Ctrl-Left" true
    (parse [0x1B; 0x5B; Char.code '1'; Char.code ';'; Char.code '5'; Char.code 'D']
     = Terminal.Ctrl_left)

let test_alt_paredit_keys () =
  Alcotest.(check bool) "Alt-s" true
    (parse [0x1B; Char.code 's'] = Terminal.Alt_s);
  Alcotest.(check bool) "Alt-r" true
    (parse [0x1B; Char.code 'r'] = Terminal.Alt_r);
  Alcotest.(check bool) "Alt-(" true
    (parse [0x1B; Char.code '('] = Terminal.Alt_open_paren);
  Alcotest.(check bool) "Alt-9" true
    (parse [0x1B; Char.code '9'] = Terminal.Alt_9)

let test_bare_escape () =
  Alcotest.(check bool) "bare ESC" true
    (parse [0x1B] = Terminal.Unknown)

let test_empty_input () =
  Alcotest.(check bool) "empty" true
    (Terminal.parse_key_bytes (Bytes.create 0) 0 = Terminal.Unknown)

let () =
  Alcotest.run "Terminal" [
    "key parsing", [
      Alcotest.test_case "printable characters" `Quick test_printable_char;
      Alcotest.test_case "enter" `Quick test_enter;
      Alcotest.test_case "backspace" `Quick test_backspace;
      Alcotest.test_case "tab" `Quick test_tab;
      Alcotest.test_case "ctrl keys" `Quick test_ctrl_keys;
      Alcotest.test_case "arrow keys" `Quick test_arrow_keys;
      Alcotest.test_case "home/end" `Quick test_home_end;
      Alcotest.test_case "delete" `Quick test_delete_key;
      Alcotest.test_case "alt arrows (CSI)" `Quick test_alt_arrows;
      Alcotest.test_case "alt b/f" `Quick test_alt_bf;
      Alcotest.test_case "alt backspace" `Quick test_alt_backspace;
      Alcotest.test_case "alt enter" `Quick test_alt_enter;
      Alcotest.test_case "ctrl arrows" `Quick test_ctrl_arrows;
      Alcotest.test_case "alt paredit keys" `Quick test_alt_paredit_keys;
      Alcotest.test_case "bare escape" `Quick test_bare_escape;
      Alcotest.test_case "empty input" `Quick test_empty_input;
    ];
  ]
