open Bilk

(* --- Unit tests --- *)

let test_basic_append () =
  let sb = Scrollback.create ~max_bytes:100 in
  Scrollback.append sb "hello";
  Alcotest.(check string) "contents" "hello" (Scrollback.contents sb)

let test_multiple_appends () =
  let sb = Scrollback.create ~max_bytes:100 in
  Scrollback.append sb "hello ";
  Scrollback.append sb "world";
  Alcotest.(check string) "contents" "hello world" (Scrollback.contents sb)

let test_overflow_drops_oldest () =
  let sb = Scrollback.create ~max_bytes:10 in
  Scrollback.append sb "12345";
  Scrollback.append sb "67890";
  Scrollback.append sb "abc";
  let c = Scrollback.contents sb in
  Alcotest.(check bool) "no longer has 12345"
    false (String.length c >= 5 &&
           String.sub c 0 5 = "12345");
  Alcotest.(check bool) "ends with abc" true
    (let clen = String.length c in
     clen >= 3 && String.sub c (clen - 3) 3 = "abc")

let test_clear () =
  let sb = Scrollback.create ~max_bytes:100 in
  Scrollback.append sb "hello";
  Scrollback.clear sb;
  Alcotest.(check string) "empty after clear" "" (Scrollback.contents sb);
  Alcotest.(check int) "zero bytes" 0 (Scrollback.byte_count sb)

let test_byte_count () =
  let sb = Scrollback.create ~max_bytes:100 in
  Scrollback.append sb "hello";
  Alcotest.(check int) "5 bytes" 5 (Scrollback.byte_count sb);
  Scrollback.append sb " world";
  Alcotest.(check int) "11 bytes" 11 (Scrollback.byte_count sb)

let test_empty () =
  let sb = Scrollback.create ~max_bytes:100 in
  Alcotest.(check string) "empty" "" (Scrollback.contents sb);
  Alcotest.(check int) "zero" 0 (Scrollback.byte_count sb)

let test_single_large_append () =
  let sb = Scrollback.create ~max_bytes:5 in
  Scrollback.append sb "1234567890";
  (* Should keep the last 5 bytes *)
  let c = Scrollback.contents sb in
  Alcotest.(check bool) "at most max_bytes" true
    (String.length c <= 5)

(* --- Properties --- *)

let prop_byte_count_bounded =
  QCheck2.Test.make ~count:200
    ~name:"byte count never exceeds max_bytes"
    QCheck2.Gen.(pair
      (int_range 1 100)
      (list_size (int_range 0 20)
         (string_size (int_range 0 50))))
    (fun (max_bytes, chunks) ->
       let sb = Scrollback.create ~max_bytes in
       List.iter (Scrollback.append sb) chunks;
       Scrollback.byte_count sb <= max_bytes)

let prop_contents_length_eq_byte_count =
  QCheck2.Test.make ~count:200
    ~name:"contents length equals byte_count"
    QCheck2.Gen.(pair
      (int_range 1 100)
      (list_size (int_range 0 10)
         (string_size (int_range 0 30))))
    (fun (max_bytes, chunks) ->
       let sb = Scrollback.create ~max_bytes in
       List.iter (Scrollback.append sb) chunks;
       String.length (Scrollback.contents sb) = Scrollback.byte_count sb)

let () =
  Alcotest.run "Scrollback"
    [ ("basic",
       [ Alcotest.test_case "append" `Quick test_basic_append
       ; Alcotest.test_case "multiple" `Quick test_multiple_appends
       ; Alcotest.test_case "overflow" `Quick test_overflow_drops_oldest
       ; Alcotest.test_case "clear" `Quick test_clear
       ; Alcotest.test_case "byte_count" `Quick test_byte_count
       ; Alcotest.test_case "empty" `Quick test_empty
       ; Alcotest.test_case "large append" `Quick test_single_large_append
       ])
    ; ("properties",
       List.map QCheck_alcotest.to_alcotest
         [ prop_byte_count_bounded
         ; prop_contents_length_eq_byte_count
         ])
    ]
