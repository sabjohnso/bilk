(* custom_primitives.ml — Extending Scheme with OCaml functions.

   Demonstrates:
   - Instance.define_primitive — register OCaml functions as Scheme primitives
   - Calling custom primitives from Scheme
   - Instance.load_file       — load a Scheme file that uses custom primitives
   - Looking up and calling Scheme procedures defined in loaded files
*)

open Wile

let () =
  let inst = Instance.create () in

  (* 1. Register custom primitives *)
  Printf.printf "=== 1. Instance.define_primitive ===\n";

  (* A thunk that returns the current Unix timestamp *)
  Instance.define_primitive inst "timestamp" (fun _args ->
    Datum.Flonum (Unix.gettimeofday ()));
  Printf.printf "Registered 'timestamp' primitive.\n";

  (* A function that repeats a string n times *)
  Instance.define_primitive inst "string-repeat" (fun args ->
    match args with
    | [Datum.Str s; Datum.Fixnum n] ->
      let s = Bytes.to_string s in
      let buf = Buffer.create (String.length s * n) in
      for _ = 1 to n do Buffer.add_string buf s done;
      Datum.Str (Bytes.of_string (Buffer.contents buf))
    | _ -> failwith "string-repeat: expected (string, integer)");
  Printf.printf "Registered 'string-repeat' primitive.\n\n";

  (* 2. Call custom primitives from Scheme *)
  Printf.printf "=== 2. Use custom primitives from Scheme ===\n";

  let result = Instance.eval_string inst "(timestamp)" in
  Printf.printf "(timestamp) => %s\n" (Datum.to_string result);

  let result = Instance.eval_string inst "(string-repeat \"ab\" 4)" in
  Printf.printf "(string-repeat \"ab\" 4) => %s\n\n" (Datum.to_string result);

  (* 3. Load a Scheme file that uses our custom primitives *)
  Printf.printf "=== 3. Instance.load_file ===\n";

  let helpers_path =
    Filename.concat (Filename.dirname Sys.argv.(0)) "helpers.scm"
  in
  (* Try the build-relative path first; fall back to examples/ *)
  let path =
    if Sys.file_exists helpers_path then helpers_path
    else "examples/helpers.scm"
  in
  Printf.printf "Loading %s...\n" path;
  Instance.load_file inst path;
  Printf.printf "Loaded successfully.\n\n";

  (* 4. Look up and call the procedure defined in helpers.scm *)
  Printf.printf "=== 4. Call a procedure from the loaded file ===\n";

  let greeting = Option.get (Instance.lookup inst "greeting") in
  let result = Instance.call inst greeting [Datum.Str (Bytes.of_string "OCaml")] in
  Printf.printf "greeting \"OCaml\" =>\n%s\n" (Datum.to_display_string result);

  Printf.printf "\nDone!\n"
