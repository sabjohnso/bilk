(* embedding_basics.ml — Basic Wile embedding demo.

   Demonstrates:
   - Instance.create      — create a Scheme instance
   - Instance.eval_string — evaluate Scheme expressions
   - Instance.lookup      — retrieve values from Scheme
   - Instance.call        — invoke a Scheme procedure from OCaml
   - Instance.eval_datum  — evaluate programmatically-built Datum trees
   - Datum helpers        — is_true, to_list, list_of, to_string
*)

open Wile

let () =
  (* 1. Create a fresh Scheme instance *)
  let inst = Instance.create () in
  Printf.printf "=== 1. Instance.create ===\n";
  Printf.printf "Created a fresh Scheme instance.\n\n";

  (* 2. Evaluate Scheme expressions *)
  Printf.printf "=== 2. Instance.eval_string ===\n";

  let result = Instance.eval_string inst "(+ 2 3)" in
  Printf.printf "(+ 2 3) => %s\n" (Datum.to_string result);

  let result = Instance.eval_string inst "(string-append \"hello\" \" \" \"world\")" in
  Printf.printf "(string-append \"hello\" \" \" \"world\") => %s\n" (Datum.to_string result);

  let _ = Instance.eval_string inst "(define factorial (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))" in
  let result = Instance.eval_string inst "(factorial 10)" in
  Printf.printf "(factorial 10) => %s\n\n" (Datum.to_string result);

  (* 3. Look up Scheme values from OCaml *)
  Printf.printf "=== 3. Instance.lookup ===\n";
  (match Instance.lookup inst "factorial" with
   | Some proc -> Printf.printf "Found 'factorial': %s\n" (Datum.to_string proc)
   | None -> Printf.printf "Not found!\n");
  (match Instance.lookup inst "nonexistent" with
   | Some _ -> Printf.printf "Unexpectedly found 'nonexistent'!\n"
   | None -> Printf.printf "Lookup 'nonexistent': None (as expected)\n");
  Printf.printf "\n";

  (* 4. Call a Scheme procedure from OCaml *)
  Printf.printf "=== 4. Instance.call ===\n";
  let factorial_proc = Option.get (Instance.lookup inst "factorial") in
  let result = Instance.call inst factorial_proc [Datum.Fixnum 12] in
  Printf.printf "call factorial 12 => %s\n" (Datum.to_string result);

  let cons_proc = Option.get (Instance.lookup inst "cons") in
  let result = Instance.call inst cons_proc [Datum.Fixnum 1; Datum.Fixnum 2] in
  Printf.printf "call cons 1 2 => %s\n\n" (Datum.to_string result);

  (* 5. Evaluate programmatically-built Datum trees *)
  Printf.printf "=== 5. Instance.eval_datum ===\n";

  (* Self-evaluating values return themselves *)
  let result = Instance.eval_datum inst (Datum.Fixnum 42) in
  Printf.printf "eval_datum 42 => %s\n" (Datum.to_string result);

  (* Build (+ 10 20) as a Datum tree *)
  let expr = Datum.list_of [Datum.Symbol "+"; Datum.Fixnum 10; Datum.Fixnum 20] in
  Printf.printf "eval_datum %s => " (Datum.to_string expr);
  let result = Instance.eval_datum inst expr in
  Printf.printf "%s\n\n" (Datum.to_string result);

  (* 6. Datum helper functions *)
  Printf.printf "=== 6. Datum helpers ===\n";

  (* is_true: only #f is false *)
  Printf.printf "is_true #t => %b\n" (Datum.is_true (Datum.Bool true));
  Printf.printf "is_true #f => %b\n" (Datum.is_true (Datum.Bool false));
  Printf.printf "is_true 0 => %b\n" (Datum.is_true (Datum.Fixnum 0));
  Printf.printf "is_true () => %b\n" (Datum.is_true Datum.Nil);

  (* list_of / to_list round-trip *)
  let lst = Datum.list_of [Datum.Fixnum 1; Datum.Fixnum 2; Datum.Fixnum 3] in
  Printf.printf "list_of [1; 2; 3] => %s\n" (Datum.to_string lst);
  (match Datum.to_list lst with
   | Some elts ->
     Printf.printf "to_list => [%s]\n"
       (String.concat "; " (List.map Datum.to_string elts))
   | None -> Printf.printf "to_list => None\n");

  (* to_list on a non-list *)
  Printf.printf "to_list 42 => %s\n"
    (match Datum.to_list (Datum.Fixnum 42) with
     | Some _ -> "Some ..."
     | None -> "None");

  Printf.printf "\nDone!\n"
