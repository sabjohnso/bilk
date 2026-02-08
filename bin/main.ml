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
  | Fasl.Fasl_error msg -> format_error msg; 1
  | Package.Package_error msg -> format_error msg; 1
  | Pkg_manager.Pkg_error msg -> format_error msg; 1
  | Venv.Venv_error msg -> format_error msg; 1
  | Sys_error msg -> format_error msg; 1
  | Failure msg -> format_error msg; 1

(* --- Instance setup --- *)

let make_instance () =
  let inst = Instance.create () in
  inst.fasl_cache := true;
  inst

let dir_for_path path =
  Filename.dirname (
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  )

let chop_extension path =
  match Filename.chop_suffix_opt ~suffix:".scm" path with
  | Some base -> base
  | None ->
    try Filename.chop_extension path
    with Invalid_argument _ -> path

(* --- Package auto-detection --- *)

let setup_package inst start_dir =
  match Package.find_package_file start_dir with
  | None -> ()
  | Some pkg_path ->
    let pkg = Package.parse inst.Instance.readtable pkg_path in
    let pkg_dir = Filename.dirname pkg_path in
    let pkg_src = Filename.concat pkg_dir "src" in
    let registry_root = Pkg_manager.default_registry_root () in
    Instance.setup_package_paths inst ~registry_root pkg;
    (* Prepend the package's own src/ directory *)
    if Sys.file_exists pkg_src && Sys.is_directory pkg_src then
      inst.search_paths := pkg_src :: !(inst.search_paths)

(* --- Expression mode (-e) --- *)

let run_expr expr =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[Sys.getcwd ()];
  setup_package inst (Sys.getcwd ());
  handle_errors (fun () ->
    let port = Port.of_string expr in
    let result = Instance.eval_port inst port in
    match result with
    | Datum.Void -> ()
    | v -> print_endline (Datum.to_string v))

(* --- File mode --- *)

let run_file path =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[dir_for_path path];
  setup_package inst (dir_for_path path);
  handle_errors (fun () ->
    let port = Port.of_file path in
    ignore (Instance.eval_port inst port))

(* --- Compile subcommand --- *)

(* Escape a raw byte string for embedding as an OCaml string literal.
   All non-printable-ASCII, backslash, and double-quote characters are
   hex-escaped (\xHH) to safely embed arbitrary binary FASL data. *)
let escape_string_literal s =
  let buf = Buffer.create (String.length s * 4) in
  String.iter (fun c ->
    let code = Char.code c in
    if code < 32 || code > 126 || c = '\\' || c = '"' then
      Buffer.add_string buf (Printf.sprintf "\\x%02x" code)
    else
      Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let remove_if_exists path =
  try Sys.remove path with Sys_error _ -> ()

let generate_executable prog output_path =
  let fasl_bytes = Fasl.write_program_bytes prog in
  let escaped = escape_string_literal (Bytes.to_string fasl_bytes) in
  let ocaml_src = Printf.sprintf
    "let fasl_data = \"%s\"\n\
     let () =\n\
     \  try\n\
     \    let inst = Wile.Instance.create () in\n\
     \    inst.Wile.Instance.fasl_cache := true;\n\
     \    inst.Wile.Instance.search_paths :=\n\
     \      Wile.Search_path.resolve ~base_dirs:[Sys.getcwd ()];\n\
     \    let prog = Wile.Fasl.read_program_bytes\n\
     \      inst.Wile.Instance.symbols (Bytes.of_string fasl_data) in\n\
     \    ignore (Wile.Instance.run_program inst prog)\n\
     \  with\n\
     \  | Wile.Vm.Runtime_error msg ->\n\
     \    Printf.eprintf \"Error: %%s\\n%%!\" msg; exit 1\n\
     \  | Wile.Fasl.Fasl_error msg ->\n\
     \    Printf.eprintf \"Error: %%s\\n%%!\" msg; exit 1\n\
     \  | Failure msg ->\n\
     \    Printf.eprintf \"Error: %%s\\n%%!\" msg; exit 1\n"
    escaped
  in
  let tmp_ml = Filename.temp_file "wile_aot_" ".ml" in
  let tmp_base = Filename.chop_extension tmp_ml in
  Fun.protect ~finally:(fun () ->
    remove_if_exists tmp_ml;
    remove_if_exists (tmp_base ^ ".cmi");
    remove_if_exists (tmp_base ^ ".cmx");
    remove_if_exists (tmp_base ^ ".o"))
    (fun () ->
      let oc = open_out tmp_ml in
      Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
        output_string oc ocaml_src);
      let cmd = Printf.sprintf
        "ocamlfind ocamlopt -package wile -linkpkg %s -o %s 2>&1"
        (Filename.quote tmp_ml) (Filename.quote output_path)
      in
      let exit_code = Sys.command cmd in
      if exit_code <> 0 then
        failwith (Printf.sprintf
          "ocamlfind ocamlopt failed (exit %d). \
           Ensure wile is installed: opam install ."
          exit_code))

let compile_file path output exe =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[dir_for_path path];
  setup_package inst (dir_for_path path);
  handle_errors (fun () ->
    let port = Port.of_file path in
    let prog = Instance.compile_port inst port in
    if exe then begin
      let out = match output with
        | Some o -> o
        | None -> chop_extension path
      in
      generate_executable prog out
    end else begin
      let out = match output with
        | Some o -> o
        | None -> chop_extension path ^ ".fasl"
      in
      Fasl.write_program_fasl out prog
    end)

(* --- Run FASL subcommand --- *)

let run_fasl path =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[dir_for_path path];
  setup_package inst (dir_for_path path);
  handle_errors (fun () ->
    let prog = Fasl.read_program_fasl inst.symbols path in
    let result = Instance.run_program inst prog in
    match result with
    | Datum.Void -> ()
    | v -> print_endline (Datum.to_string v))

(* --- REPL commands --- *)

let repl_help () =
  print_endline "REPL commands:";
  print_endline "  ,help  ,h       Show this help";
  print_endline "  ,quit  ,q       Exit the REPL";
  print_endline "  ,load <file>    Load and evaluate a Scheme file";
  print_endline "  ,env            List bound names in the global environment";
  print_endline "  ,theme <name>   Switch theme (dark, light, none, or file path)";
  print_endline "  ,paredit        Toggle paredit mode (structural editing)"

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
  | Fasl.Fasl_error msg -> format_error msg
  | Package.Package_error msg -> format_error msg
  | Pkg_manager.Pkg_error msg -> format_error msg
  | Failure msg -> format_error msg
  | Sys_error msg -> format_error msg

let resolve_theme name =
  match name with
  | "dark" -> Some Highlight.dark_theme
  | "light" -> Some Highlight.light_theme
  | "none" | "off" -> None
  | path ->
    if Sys.file_exists path then
      Some (Highlight.load_theme path)
    else begin
      Printf.eprintf "Theme not found: %s\n%!" path;
      None
    end

let handle_repl_command inst theme_ref paredit_ref line =
  let line = String.trim line in
  match line with
  | ",quit" | ",q" -> exit 0
  | ",help" | ",h" -> repl_help ()
  | ",env" -> repl_env inst
  | ",paredit" ->
    paredit_ref := not !(paredit_ref);
    if !(paredit_ref) then
      Printf.printf "Paredit mode enabled.\n%!"
    else
      Printf.printf "Paredit mode disabled.\n%!"
  | ",load" ->
    Printf.eprintf "Usage: ,load <file>\n%!"
  | ",theme" ->
    let name = match !theme_ref with
      | Some t -> (t : Highlight.theme).name
      | None -> "none"
    in
    Printf.printf "Current theme: %s\nUsage: ,theme <dark|light|none|path>\n%!" name
  | _ ->
    if String.length line > 6 && String.sub line 0 6 = ",load " then begin
      let path = String.trim (String.sub line 6 (String.length line - 6)) in
      if path = "" then Printf.eprintf "Usage: ,load <file>\n%!"
      else repl_load inst path
    end else if String.length line > 7 && String.sub line 0 7 = ",theme " then begin
      let name = String.trim (String.sub line 7 (String.length line - 7)) in
      if name = "" then Printf.eprintf "Usage: ,theme <dark|light|none|path>\n%!"
      else begin
        let theme = resolve_theme name in
        theme_ref := theme;
        match theme with
        | Some t -> Printf.printf "Switched to theme: %s\n%!" t.Highlight.name
        | None -> Printf.printf "Highlighting disabled.\n%!"
      end
    end else
      Printf.eprintf "Unknown command: %s\nType ,help for available commands.\n%!" line

(* --- REPL --- *)

let is_unterminated msg =
  let prefix = "unterminated" in
  let len = String.length prefix in
  String.length msg >= len && String.sub msg 0 len = prefix

let is_complete inst text =
  let port = Port.of_string text in
  let rec check () =
    try
      match Reader.read_syntax inst.Instance.readtable port with
      | { Syntax.datum = Syntax.Eof; _ } -> true
      | _ -> check ()
    with Reader.Read_error (_, msg) ->
      if is_unterminated msg then false else true
  in
  check ()

let run_repl theme_name =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[Sys.getcwd ()];
  setup_package inst (Sys.getcwd ());
  Printf.printf "Wile Scheme %s\nType ,help for REPL commands, Ctrl-D to exit.\n%!" version;
  let initial_theme = match theme_name with
    | Some name -> resolve_theme name
    | None ->
      match Sys.getenv_opt "WILE_THEME" with
      | Some name -> resolve_theme name
      | None -> Some Highlight.dark_theme
  in
  let theme_ref = ref initial_theme in
  let paredit_ref = ref true in
  let highlight_fn text cursor =
    match !theme_ref with
    | None -> text
    | Some theme -> Highlight.highlight_line theme inst.readtable text cursor
  in
  let editor = Line_editor.create {
    prompt = "wile> ";
    continuation_prompt = "  ... ";
    history_file;
    max_history = 1000;
    is_complete = Some (is_complete inst);
    highlight = Some highlight_fn;
    paredit = Some paredit_ref;
    readtable = Some inst.readtable;
  } in
  at_exit (fun () -> Line_editor.destroy editor);
  let print_result v =
    match v with
    | Datum.Void -> ()
    | _ -> print_endline (Datum.to_string v)
  in
  let eval_input input =
    let port = Port.of_string input in
    let rec eval_loop () =
      try
        let expr = Reader.read_syntax inst.readtable port in
        match expr with
        | { Syntax.datum = Syntax.Eof; _ } -> ()
        | _ ->
          begin try
            print_result (Instance.eval_syntax inst expr)
          with
          | Reader.Read_error (loc, msg) -> format_loc_error loc msg
          | Compiler.Compile_error (loc, msg) -> format_loc_error loc msg
          | Vm.Runtime_error msg -> format_error msg
          | Fasl.Fasl_error msg -> format_error msg
          | Package.Package_error msg -> format_error msg
          | Pkg_manager.Pkg_error msg -> format_error msg
          | Failure msg -> format_error msg
          end;
          eval_loop ()
      with
      | Reader.Read_error (loc, msg) ->
        format_loc_error loc msg
    in
    eval_loop ()
  in
  let rec loop () =
    match Line_editor.read_input editor with
    | Line_editor.Interrupted ->
      print_endline "Interrupted.";
      loop ()
    | Line_editor.Eof -> ()
    | Line_editor.Input input ->
      let trimmed = String.trim input in
      if trimmed = "" then
        loop ()
      else if trimmed.[0] = ',' then begin
        handle_repl_command inst theme_ref paredit_ref trimmed;
        loop ()
      end else begin
        Line_editor.history_add editor input;
        eval_input input;
        loop ()
      end
  in
  loop ()

(* --- Cmdliner CLI --- *)

let make_default_cmd () =
  let open Cmdliner in
  let expr_opt =
    Arg.(value & opt (some string) None &
         info ["e"] ~docv:"EXPR" ~doc:"Evaluate expression and print result.")
  in
  let file_arg =
    Arg.(value & pos 0 (some string) None &
         info [] ~docv:"FILE" ~doc:"Scheme source file to execute.")
  in
  let theme_opt =
    Arg.(value & opt (some string) None &
         info ["theme"] ~docv:"THEME"
           ~doc:"Color theme: $(b,dark), $(b,light), $(b,none), or a file path.")
  in
  let default_cmd expr file theme =
    match expr, file with
    | Some e, _ -> exit (run_expr e)
    | _, Some f -> exit (run_file f)
    | None, None -> run_repl theme
  in
  let term = Term.(const default_cmd $ expr_opt $ file_arg $ theme_opt) in
  let info =
    Cmd.info "wile" ~version
      ~doc:"Wile Scheme — an R7RS implementation"
      ~man:[`S "DESCRIPTION";
            `P "Run Scheme code interactively, from a file, or from a \
                command-line expression.";
            `S "SUBCOMMANDS";
            `P "Use $(b,wile compile) to ahead-of-time compile Scheme source.";
            `P "Use $(b,wile run) to execute a compiled program FASL.";
            `P "Use $(b,wile pkg) to manage local packages.";
            `P "Use $(b,wile venv) to create a virtual environment.";
            `S "ENVIRONMENT";
            `P "$(b,WILE_VENV) — path to active virtual environment \
                (its $(b,lib/) directory is searched for libraries).";
            `P "$(b,WILE_PATH) — colon-separated list of additional \
                library search directories.";
            `P "$(b,WILE_HOME) — override for the Wile home directory \
                (default: $(b,~/.wile/))."]
  in
  Cmd.v info term

let make_compile_cmd () =
  let open Cmdliner in
  let compile_output =
    Arg.(value & opt (some string) None &
         info ["o"] ~docv:"OUTPUT" ~doc:"Output file path.")
  in
  let compile_exe =
    Arg.(value & flag &
         info ["exe"] ~doc:"Generate a standalone native executable \
           (requires wile to be installed as an opam package).")
  in
  let compile_file_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"FILE" ~doc:"Scheme source file to compile.")
  in
  let compile_cmd_fn file output exe =
    exit (compile_file file output exe)
  in
  let term = Term.(const compile_cmd_fn $ compile_file_arg $ compile_output $ compile_exe) in
  let info =
    Cmd.info "compile" ~version
      ~doc:"Compile a Scheme source file to a program FASL"
      ~man:[`S "DESCRIPTION";
            `P "Reads a Scheme source file, processes it through Reader, \
                Expander, and Compiler, and writes a program FASL file. \
                Use $(b,--exe) to generate a standalone native executable."]
  in
  Cmd.v info term

let make_run_cmd () =
  let open Cmdliner in
  let run_file_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"FILE" ~doc:"Program FASL file to execute.")
  in
  let run_cmd_fn file =
    exit (run_fasl file)
  in
  let term = Term.(const run_cmd_fn $ run_file_arg) in
  let info =
    Cmd.info "run" ~version
      ~doc:"Execute a program FASL file"
      ~man:[`S "DESCRIPTION";
            `P "Loads and executes a program FASL file produced by \
                $(b,wile compile)."]
  in
  Cmd.v info term

(* --- Package subcommands --- *)

let pkg_install path =
  handle_errors (fun () ->
    let src_dir = match path with
      | Some p -> p
      | None -> Sys.getcwd ()
    in
    let registry_root = Pkg_manager.default_registry_root () in
    Pkg_manager.install ~registry_root ~src_dir;
    let pkg = Package.parse Readtable.default
      (Filename.concat src_dir "package.scm") in
    Printf.printf "Installed %s %s\n%!" pkg.name (Semver.to_string pkg.version))

let pkg_list () =
  handle_errors (fun () ->
    let registry_root = Pkg_manager.default_registry_root () in
    let pkgs = Pkg_manager.list_packages ~registry_root in
    if pkgs = [] then
      print_endline "No packages installed."
    else
      List.iter (fun (name, versions) ->
        Printf.printf "%s: %s\n" name
          (String.concat ", " (List.map Semver.to_string versions))
      ) pkgs)

let pkg_remove name ver =
  handle_errors (fun () ->
    let registry_root = Pkg_manager.default_registry_root () in
    Pkg_manager.remove ~registry_root ~name ~version:ver;
    Printf.printf "Removed %s %s\n%!" name ver)

let pkg_info name version =
  handle_errors (fun () ->
    let registry_root = Pkg_manager.default_registry_root () in
    match version with
    | Some ver ->
      let pkg = Pkg_manager.package_info ~registry_root ~name ~version:ver in
      Printf.printf "Name:        %s\n" pkg.name;
      Printf.printf "Version:     %s\n" (Semver.to_string pkg.version);
      Printf.printf "Description: %s\n" pkg.description;
      Printf.printf "License:     %s\n" pkg.license;
      if pkg.depends <> [] then begin
        Printf.printf "Depends:     ";
        List.iter (fun (dep : Package.dependency) ->
          Printf.printf "%s " dep.dep_name
        ) pkg.depends;
        print_newline ()
      end;
      if pkg.libraries <> [] then begin
        Printf.printf "Libraries:   ";
        List.iter (fun lib ->
          Printf.printf "(%s) " (String.concat " " lib)
        ) pkg.libraries;
        print_newline ()
      end
    | None ->
      let pkgs = Pkg_manager.list_packages ~registry_root in
      match List.assoc_opt name pkgs with
      | None -> Printf.eprintf "Package not found: %s\n%!" name; exit 1
      | Some versions ->
        List.iter (fun ver ->
          let pkg = Pkg_manager.package_info ~registry_root ~name
              ~version:(Semver.to_string ver) in
          Printf.printf "%s %s — %s\n" pkg.name
            (Semver.to_string pkg.version) pkg.description
        ) versions)

let make_pkg_install_cmd () =
  let open Cmdliner in
  let path_arg =
    Arg.(value & pos 0 (some string) None &
         info [] ~docv:"PATH" ~doc:"Package directory (default: current directory).")
  in
  let cmd path = exit (pkg_install path) in
  let term = Term.(const cmd $ path_arg) in
  let info =
    Cmd.info "install" ~version
      ~doc:"Install a package from a local directory"
  in
  Cmd.v info term

let make_pkg_list_cmd () =
  let open Cmdliner in
  let cmd () = exit (pkg_list ()) in
  let term = Term.(const cmd $ const ()) in
  let info =
    Cmd.info "list" ~version
      ~doc:"List installed packages"
  in
  Cmd.v info term

let make_pkg_remove_cmd () =
  let open Cmdliner in
  let name_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"NAME" ~doc:"Package name.")
  in
  let version_arg =
    Arg.(required & pos 1 (some string) None &
         info [] ~docv:"VERSION" ~doc:"Package version.")
  in
  let cmd name ver = exit (pkg_remove name ver) in
  let term = Term.(const cmd $ name_arg $ version_arg) in
  let info =
    Cmd.info "remove" ~version
      ~doc:"Remove an installed package version"
  in
  Cmd.v info term

let make_pkg_info_cmd () =
  let open Cmdliner in
  let name_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"NAME" ~doc:"Package name.")
  in
  let version_arg =
    Arg.(value & pos 1 (some string) None &
         info [] ~docv:"VERSION" ~doc:"Package version (optional, shows all if omitted).")
  in
  let cmd name ver = exit (pkg_info name ver) in
  let term = Term.(const cmd $ name_arg $ version_arg) in
  let info =
    Cmd.info "info" ~version
      ~doc:"Show package details"
  in
  Cmd.v info term

(* --- Venv subcommand --- *)

let make_venv_cmd () =
  let open Cmdliner in
  let dir_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"DIR" ~doc:"Directory for the new virtual environment.")
  in
  let cmd dir =
    exit (handle_errors (fun () ->
      Venv.create ~wile_version:version dir;
      let abs_dir =
        if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir
        else dir
      in
      Printf.printf "Created virtual environment in %s\n%!" dir;
      Printf.printf "Activate with: export WILE_VENV=%s\n%!" abs_dir))
  in
  let term = Term.(const cmd $ dir_arg) in
  let info =
    Cmd.info "venv" ~version
      ~doc:"Create a virtual environment"
      ~man:[`S "DESCRIPTION";
            `P "Creates a new virtual environment directory with a $(b,lib/) \
                subdirectory for library files and a $(b,wile-venv.cfg) marker. \
                Activate by setting $(b,WILE_VENV) to the directory path."]
  in
  Cmd.v info term

let make_pkg_cmd () =
  let open Cmdliner in
  let info =
    Cmd.info "pkg" ~version
      ~doc:"Package management commands"
      ~man:[`S "DESCRIPTION";
            `P "Manage local packages. Use $(b,wile pkg install), \
                $(b,wile pkg list), $(b,wile pkg remove), or \
                $(b,wile pkg info)."]
  in
  Cmd.group info [
    make_pkg_install_cmd ();
    make_pkg_list_cmd ();
    make_pkg_remove_cmd ();
    make_pkg_info_cmd ();
  ]

(* Manual subcommand dispatch to avoid Cmd.group intercepting positional
   file arguments (e.g. "wile file.scm") as unknown subcommand names. *)
let () =
  let open Cmdliner in
  let argc = Array.length Sys.argv in
  if argc >= 2 then begin
    match Sys.argv.(1) with
    | "compile" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_compile_cmd ()))
    | "run" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_run_cmd ()))
    | "pkg" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_pkg_cmd ()))
    | "venv" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_venv_cmd ()))
    | _ -> exit (Cmd.eval (make_default_cmd ()))
  end else
    exit (Cmd.eval (make_default_cmd ()))
