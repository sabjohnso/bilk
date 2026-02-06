open Wile

let test_construct () =
  (* Ensure all variants can be constructed *)
  let ops = [
    Opcode.Halt; Opcode.Const 0; Opcode.Lookup 1;
    Opcode.Define 2; Opcode.SetBang 3; Opcode.Push;
    Opcode.Jump 10; Opcode.JumpFalse 20;
    Opcode.Call 3; Opcode.TailCall 3;
    Opcode.Return; Opcode.MakeClosure 0;
  ] in
  Alcotest.(check int) "12 opcodes" 12 (List.length ops)

let test_to_string_no_arg () =
  Alcotest.(check string) "halt" "HALT" (Opcode.to_string Halt);
  Alcotest.(check string) "push" "PUSH" (Opcode.to_string Push);
  Alcotest.(check string) "return" "RETURN" (Opcode.to_string Return)

let test_to_string_with_arg () =
  Alcotest.(check string) "const" "CONST 5" (Opcode.to_string (Const 5));
  Alcotest.(check string) "lookup" "LOOKUP 0" (Opcode.to_string (Lookup 0));
  Alcotest.(check string) "define" "DEFINE 3" (Opcode.to_string (Define 3));
  Alcotest.(check string) "set!" "SET! 1" (Opcode.to_string (SetBang 1));
  Alcotest.(check string) "jump" "JUMP 10" (Opcode.to_string (Jump 10));
  Alcotest.(check string) "jump_false" "JUMP_FALSE 20" (Opcode.to_string (JumpFalse 20));
  Alcotest.(check string) "call" "CALL 2" (Opcode.to_string (Call 2));
  Alcotest.(check string) "tail_call" "TAIL_CALL 2" (Opcode.to_string (TailCall 2));
  Alcotest.(check string) "make_closure" "MAKE_CLOSURE 0" (Opcode.to_string (MakeClosure 0))

let test_pp () =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  Opcode.pp fmt (Opcode.Call 3);
  Format.pp_print_flush fmt ();
  Alcotest.(check string) "pp call" "CALL 3" (Buffer.contents buf)

let () =
  Alcotest.run "Opcode"
    [ ("Opcode",
       [ Alcotest.test_case "construct all variants" `Quick test_construct
       ; Alcotest.test_case "to_string no arg" `Quick test_to_string_no_arg
       ; Alcotest.test_case "to_string with arg" `Quick test_to_string_with_arg
       ; Alcotest.test_case "pp" `Quick test_pp
       ])
    ]
