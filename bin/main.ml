open Wile

let version = "0.1.0"

let history_file =
  match Sys.getenv_opt "HOME" with
  | Some home -> Some (Filename.concat home ".wile_history")
  | None -> None

(* --- Error formatting --- *)

let format_loc_error loc msg =
  Printf.eprintf "Error: %s: %s\n%!" (Loc.to_string loc) msg

let format_error msg =
  Printf.eprintf "Error: %s\n%!" msg

let handle_errors f =
  try f (); 0
  with
  | Reader.Read_error (loc, msg) -> format_loc_error loc msg; 1
  | Compiler.Compile_error (loc, msg) -> format_loc_error loc msg; 1
  | Vm.Runtime_error msg -> format_error msg; 1
  | Sys_error msg -> format_error msg; 1

(* --- Instance setup --- *)

let make_instance () =
  let inst = Instance.create () in
  inst.fasl_cache := true;
  inst

(* --- Expression mode (-e) --- *)

let run_expr expr =
  let inst = make_instance () in
  inst.search_paths := [Sys.getcwd ()];
  handle_errors (fun () ->
    let port = Port.of_string expr in
    let result = Instance.eval_port inst port in
    match result with
    | Datum.Void -> ()
    | v -> print_endline (Datum.to_string v))

(* --- File mode --- *)

let run_file path =
  let inst = make_instance () in
  let dir = Filename.dirname (
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  ) in
  inst.search_paths := [dir];
  handle_errors (fun () ->
    let port = Port.of_file path in
    ignore (Instance.eval_port inst port))

(* --- REPL commands --- *)

let repl_help () =
  print_endline "REPL commands:";
  print_endline "  ,help  ,h    Show this help";
  print_endline "  ,quit  ,q    Exit the REPL";
  print_endline "  ,load <file> Load and evaluate a Scheme file";
  print_endline "  ,env         List bound names in the global environment"

let repl_env inst =
  let syms = Symbol.all inst.Instance.symbols in
  let bound = List.filter_map (fun sym ->
    match Env.lookup inst.Instance.global_env sym with
    | Some _ -> Some (Symbol.name sym)
    | None -> None
  ) syms in
  let sorted = List.sort String.compare bound in
  List.iter (fun name -> print_string name; print_char ' ') sorted;
  print_newline ()

let repl_load inst path =
  try
    let port = Port.of_file path in
    ignore (Instance.eval_port inst port);
    Printf.printf "Loaded %s\n%!" path
  with
  | Reader.Read_error (loc, msg) -> format_loc_error loc msg
  | Compiler.Compile_error (loc, msg) -> format_loc_error loc msg
  | Vm.Runtime_error msg -> format_error msg
  | Sys_error msg -> format_error msg

let handle_repl_command inst line =
  let line = String.trim line in
  match line with
  | ",quit" | ",q" -> exit 0
  | ",help" | ",h" -> repl_help ()
  | ",env" -> repl_env inst
  | ",load" ->
    Printf.eprintf "Usage: ,load <file>\n%!"
  | _ ->
    if String.length line > 6 && String.sub line 0 6 = ",load " then
      let path = String.trim (String.sub line 6 (String.length line - 6)) in
      if path = "" then Printf.eprintf "Usage: ,load <file>\n%!"
      else repl_load inst path
    else
      Printf.eprintf "Unknown command: %s\nType ,help for available commands.\n%!" line

(* --- REPL --- *)

let is_unterminated msg =
  let prefix = "unterminated" in
  let len = String.length prefix in
  String.length msg >= len && String.sub msg 0 len = prefix

let save_history () =
  Option.iter (fun f -> ignore (LNoise.history_save ~filename:f)) history_file

let run_repl () =
  let inst = make_instance () in
  inst.search_paths := [Sys.getcwd ()];
  Printf.printf "Wile Scheme %s\nType ,help for REPL commands, Ctrl-D to exit.\n%!" version;
  Option.iter (fun f -> ignore (LNoise.history_load ~filename:f)) history_file;
  ignore (LNoise.history_set ~max_length:1000);
  LNoise.catch_break true;
  at_exit save_history;
  let buf = Buffer.create 256 in
  let continuation = ref false in
  let print_result v =
    match v with
    | Datum.Void -> ()
    | _ -> print_endline (Datum.to_string v)
  in
  let rec loop () =
    let prompt = if !continuation then "  ... " else "wile> " in
    match LNoise.linenoise prompt with
    | exception Sys.Break ->
      Buffer.clear buf;
      continuation := false;
      print_endline "Interrupted.";
      loop ()
    | None ->
      if !continuation then begin
        (* Ctrl-D during continuation: abandon partial input *)
        Buffer.clear buf;
        continuation := false;
        print_newline ();
        loop ()
      end else begin
        print_newline ()
      end
    | Some line ->
      if not !continuation && String.length line > 0 && line.[0] = ',' then begin
        handle_repl_command inst line;
        loop ()
      end else begin
        if Buffer.length buf > 0 then Buffer.add_char buf '\n';
        Buffer.add_string buf line;
        let input = Buffer.contents buf in
        let port = Port.of_string input in
        (* Evaluate all complete expressions from the port *)
        let rec eval_loop () =
          let save_pos = Port.position port in
          try
            let expr = Reader.read_syntax inst.readtable port in
            match expr with
            | { Syntax.datum = Syntax.Eof; _ } ->
              (* All expressions consumed successfully *)
              ignore (LNoise.history_add input);
              Buffer.clear buf;
              continuation := false
            | _ ->
              begin try
                print_result (Instance.eval_syntax inst expr)
              with
              | Reader.Read_error (loc, msg) -> format_loc_error loc msg
              | Compiler.Compile_error (loc, msg) -> format_loc_error loc msg
              | Vm.Runtime_error msg -> format_error msg
              end;
              eval_loop ()
          with
          | Reader.Read_error (_, msg) when is_unterminated msg ->
            (* Keep only the unconsumed portion in the buffer *)
            let remaining = String.sub input save_pos (String.length input - save_pos) in
            Buffer.clear buf;
            Buffer.add_string buf remaining;
            continuation := true
          | Reader.Read_error (loc, msg) ->
            format_loc_error loc msg;
            Buffer.clear buf;
            continuation := false
        in
        eval_loop ();
        loop ()
      end
  in
  loop ()

(* --- Cmdliner CLI --- *)

let () =
  let open Cmdliner in
  let expr_opt =
    Arg.(value & opt (some string) None &
         info ["e"] ~docv:"EXPR" ~doc:"Evaluate expression and print result.")
  in
  let file_arg =
    Arg.(value & pos 0 (some string) None &
         info [] ~docv:"FILE" ~doc:"Scheme source file to execute.")
  in
  let main_cmd expr file =
    match expr, file with
    | Some e, _ -> exit (run_expr e)
    | _, Some f -> exit (run_file f)
    | None, None -> run_repl ()
  in
  let term = Term.(const main_cmd $ expr_opt $ file_arg) in
  let info =
    Cmd.info "wile" ~version
      ~doc:"Wile Scheme — an R7RS implementation"
      ~man:[`S "DESCRIPTION";
            `P "Run Scheme code interactively, from a file, or from a command-line expression."]
  in
  exit (Cmd.eval (Cmd.v info term))
