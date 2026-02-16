open Bilk

let version = "0.1.0"

let history_file =
  match Sys.getenv_opt "HOME" with
  | Some home -> Some (Filename.concat home ".bilk_history")
  | None -> None

(* --- Persistent REPL settings --- *)

let config_dir () =
  match Sys.getenv_opt "BILK_HOME" with
  | Some d when d <> "" -> Some d
  | _ ->
    match Sys.getenv_opt "HOME" with
    | Some home -> Some (Filename.concat home ".bilk")
    | None -> None

let config_file () =
  match config_dir () with
  | Some dir -> Some (Filename.concat dir "config")
  | None -> None

(** Load config as an alist of [Datum.t] pairs from a Scheme s-expression file.
    Returns an empty list on missing file or parse error. *)
let load_config () : (string * Datum.t) list =
  match config_file () with
  | None -> []
  | Some path ->
    if Sys.file_exists path then
      try
        let port = Port.of_file path in
        let rt = Readtable.default in
        let sexp = Reader.read_syntax rt port in
        let datum = Syntax.to_datum sexp in
        (* Expect an alist: ((key . value) ...) *)
        match Datum.to_list datum with
        | Some pairs ->
          List.filter_map (fun pair ->
            match pair with
            | Datum.Pair { car = Datum.Symbol key; cdr = value } ->
              Some (key, value)
            | _ -> None
          ) pairs
        | None -> []
      with _ -> []
    else []

(** Save config as an alist s-expression.
    Each entry is [(key . value)] where value is a [Datum.t]. *)
let save_config (pairs : (string * Datum.t) list) =
  match config_file () with
  | None -> ()
  | Some path ->
    (match config_dir () with
     | Some dir ->
       (try
          if not (Sys.file_exists dir) then Sys.mkdir dir 0o755
        with Sys_error _ -> ())
     | None -> ());
    try
      let alist = Datum.list_of (List.map (fun (k, v) ->
        Datum.Pair { car = Datum.Symbol k; cdr = v }
      ) pairs) in
      let oc = open_out path in
      Printf.fprintf oc "%s\n" (Datum.to_string alist);
      close_out oc
    with Sys_error _ -> ()

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
  | Build.Build_error (name, msg) ->
    format_error (Printf.sprintf "building %s: %s"
                    (Library.name_to_string name) msg); 1
  | Dep_graph.Cycle_error cycles ->
    let cycle_strs = List.map (fun cycle ->
      String.concat " -> "
        (List.map Library.name_to_string cycle)
    ) cycles in
    format_error ("circular dependencies:\n  " ^
                  String.concat "\n  " cycle_strs); 1
  | Dep_graph.Resolve_error (name, msg) ->
    format_error (Printf.sprintf "resolving %s: %s"
                    (Library.name_to_string name) msg); 1
  | Extension.Extension_error msg -> format_error msg; 1
  | Debug_server.Debug_error msg -> format_error msg; 1
  | Dap.Dap_error msg -> format_error msg; 1
  | Lsp.Lsp_error msg -> format_error msg; 1
  | Language_server.Lsp_server_error msg -> format_error msg; 1
  | Profiler.Profiler_error msg -> format_error msg; 1
  | Repository.Repository_error msg -> format_error msg; 1
  | Lockfile.Lockfile_error msg -> format_error msg; 1
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

(** Registry root for install/fetch: always local when in a project. *)
let install_registry_root () =
  match Package.find_package_file (Sys.getcwd ()) with
  | Some pkg_path ->
    Pkg_manager.local_registry_root (Filename.dirname pkg_path)
  | None -> Pkg_manager.default_registry_root ()

let setup_package inst start_dir =
  match Package.find_package_file start_dir with
  | None -> None
  | Some pkg_path ->
    let pkg = Package.parse inst.Instance.readtable pkg_path in
    let pkg_dir = Filename.dirname pkg_path in
    let pkg_src = Filename.concat pkg_dir "src" in
    let registry_root = Pkg_manager.effective_registry_root start_dir in
    Instance.setup_package_paths inst ~registry_root pkg;
    (* Prepend the package's own src/ directory *)
    if Sys.file_exists pkg_src && Sys.is_directory pkg_src then
      inst.search_paths := pkg_src :: !(inst.search_paths);
    Some (pkg, pkg_dir)

(* --- Auto-build helpers --- *)

let report_auto_build result =
  let n = List.length result.Build.actions_taken in
  if n > 0 then
    Printf.eprintf "Auto-built %d librar%s.\n%!" n
      (if n = 1 then "y" else "ies")

let auto_build_for_scm inst pkg_info scm_path =
  let rt = inst.Instance.readtable in
  let builtins = Build.builtin_library_names inst in
  let search_paths = !(inst.search_paths) in
  let scm_imports =
    try Dep_graph.imports_of_scm rt scm_path
    with
    | Sys_error _ | Reader.Read_error _ | Failure _ -> []
  in
  let pkg_roots = match pkg_info with
    | Some (pkg, pkg_dir) ->
      Build.collect_roots ~readtable:rt ~pkg_dir pkg
    | None -> []
  in
  (* Merge: pkg_roots first, then scm imports not already present *)
  let roots = pkg_roots @ List.filter (fun name ->
    not (List.mem name pkg_roots)
  ) scm_imports in
  if roots <> [] then
    report_auto_build
      (Build.auto_build ~search_paths ~builtins ~readtable:rt roots)

let auto_build_from_package inst pkg_info =
  match pkg_info with
  | None -> ()
  | Some (pkg, pkg_dir) ->
    let rt = inst.Instance.readtable in
    let roots = Build.collect_roots ~readtable:rt ~pkg_dir pkg in
    if roots <> [] then begin
      let builtins = Build.builtin_library_names inst in
      let search_paths = !(inst.search_paths) in
      report_auto_build
        (Build.auto_build ~search_paths ~builtins ~readtable:rt roots)
    end

(* --- Expression mode (-e) --- *)

let run_expr expr =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[Sys.getcwd ()];
  let pkg_info = setup_package inst (Sys.getcwd ()) in
  handle_errors (fun () ->
    auto_build_from_package inst pkg_info;
    let port = Port.of_string expr in
    let result = Instance.eval_port inst port in
    match result with
    | Datum.Void -> ()
    | v -> print_endline (Datum.to_string v))

(* --- File mode --- *)

let run_file path script_args =
  let inst = make_instance () in
  inst.command_line := path :: script_args;
  inst.search_paths := Search_path.resolve ~base_dirs:[dir_for_path path];
  let pkg_info = setup_package inst (dir_for_path path) in
  handle_errors (fun () ->
    auto_build_for_scm inst pkg_info path;
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
     \    let inst = Bilk.Instance.create () in\n\
     \    inst.Bilk.Instance.fasl_cache := true;\n\
     \    inst.Bilk.Instance.search_paths :=\n\
     \      Bilk.Search_path.resolve ~base_dirs:[Sys.getcwd ()];\n\
     \    let prog = Bilk.Fasl.read_program_bytes\n\
     \      inst.Bilk.Instance.symbols (Bytes.of_string fasl_data) in\n\
     \    ignore (Bilk.Instance.run_program inst prog)\n\
     \  with\n\
     \  | Bilk.Vm.Runtime_error msg ->\n\
     \    Printf.eprintf \"Error: %%s\\n%%!\" msg; exit 1\n\
     \  | Bilk.Fasl.Fasl_error msg ->\n\
     \    Printf.eprintf \"Error: %%s\\n%%!\" msg; exit 1\n\
     \  | Failure msg ->\n\
     \    Printf.eprintf \"Error: %%s\\n%%!\" msg; exit 1\n"
    escaped
  in
  let tmp_ml = Filename.temp_file "bilk_aot_" ".ml" in
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
        "ocamlfind ocamlopt -package bilk -linkpkg %s -o %s 2>&1"
        (Filename.quote tmp_ml) (Filename.quote output_path)
      in
      let exit_code = Sys.command cmd in
      if exit_code <> 0 then
        failwith (Printf.sprintf
          "ocamlfind ocamlopt failed (exit %d). \
           Ensure bilk is installed: opam install ."
          exit_code))

let compile_file path output exe =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[dir_for_path path];
  let pkg_info = setup_package inst (dir_for_path path) in
  handle_errors (fun () ->
    auto_build_for_scm inst pkg_info path;
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
  let pkg_info = setup_package inst (dir_for_path path) in
  handle_errors (fun () ->
    auto_build_from_package inst pkg_info;
    let prog = Fasl.read_program_fasl inst.symbols path in
    let result = Instance.run_program inst prog in
    match result with
    | Datum.Void -> ()
    | v -> print_endline (Datum.to_string v))

(* --- REPL commands --- *)

let repl_help () =
  print_endline "REPL commands:";
  print_endline "  ,help  ,h            Show this help";
  print_endline "  ,quit  ,q            Exit the REPL";
  print_endline "  ,load <file>         Load and evaluate a Scheme file";
  print_endline "  ,env                 List bound names in the global environment";
  print_endline "  ,libs                List loaded/registered libraries";
  print_endline "  ,available           List all discoverable libraries on disk";
  print_endline "  ,exports <lib>       Show exports of a library (e.g. ,exports (scheme base))";
  print_endline "  ,build               Build/rebuild stale libraries";
  print_endline "  ,deps <lib>          Show dependency tree (e.g. ,deps (srfi 1))";
  print_endline "  ,reload <lib>        Reload a library from source (e.g. ,reload (my lib))";
  print_endline "  ,theme <name>        Switch theme (dark, light, none, or file path)";
  print_endline "  ,paredit             Toggle paredit mode (structural editing)";
  print_endline "  ,checkpoint <name>   Snapshot current state as a named checkpoint";
  print_endline "  ,revert <name>       Restore state from a named checkpoint";
  print_endline "  ,checkpoints         List all checkpoints";
  print_endline "  ,save-session <file> Save all checkpoints to a binary file";
  print_endline "  ,load-session <file> Load checkpoints from a binary file";
  print_endline "  ,clear               Clear terminal screen"

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

let repl_libs inst =
  let libs = Library.list_all inst.Instance.libraries in
  let names = List.map (fun (l : Library.t) -> Library.name_to_string l.name) libs in
  let sorted = List.sort String.compare names in
  List.iter print_endline sorted

let repl_available inst =
  let names = Instance.discover_available_libraries !(inst.Instance.search_paths) in
  let strs = List.map Library.name_to_string names in
  let sorted = List.sort String.compare strs in
  List.iter print_endline sorted

let repl_exports inst arg =
  try
    let port = Port.of_string arg in
    let syntax = Reader.read_syntax inst.Instance.readtable port in
    let lib_name = Library.parse_library_name syntax in
    match Instance.ensure_library inst lib_name with
    | None ->
      Printf.eprintf "Library not found: %s\n%!" (Library.name_to_string lib_name)
    | Some lib ->
      let (rt, syn) = Library.export_names lib in
      let rt_sorted = List.sort String.compare rt in
      let syn_sorted = List.sort String.compare syn in
      if syn_sorted <> [] then begin
        Printf.printf "Syntax:\n";
        List.iter (fun name -> Printf.printf "  %s\n" name) syn_sorted
      end;
      if rt_sorted <> [] then begin
        Printf.printf "Runtime:\n";
        List.iter (fun name -> Printf.printf "  %s\n" name) rt_sorted
      end;
      Printf.printf "%!"
  with
  | Reader.Read_error (_, msg) ->
    Printf.eprintf "Error parsing library name: %s\n%!" msg
  | Compiler.Compile_error (_, msg) ->
    Printf.eprintf "Error parsing library name: %s\n%!" msg
  | Failure msg -> format_error msg

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
  | Extension.Extension_error msg -> format_error msg
  | Failure msg -> format_error msg
  | Sys_error msg -> format_error msg

let repl_build inst pkg_info =
  let rt = inst.Instance.readtable in
  let builtins = Build.builtin_library_names inst in
  let search_paths = !(inst.search_paths) in
  let roots = match pkg_info with
    | Some (pkg, pkg_dir) ->
      Build.collect_roots ~readtable:rt ~pkg_dir pkg
    | None ->
      Instance.discover_available_libraries search_paths
  in
  if roots = [] then
    Printf.printf "No libraries found.\n%!"
  else begin
    let result = Build.auto_build ~search_paths ~builtins ~readtable:rt roots in
    report_auto_build result;
    if result.actions_taken = [] then
      Printf.printf "Nothing to build (all libraries up to date).\n%!"
  end

let repl_deps inst arg =
  try
    let port = Port.of_string arg in
    let syntax = Reader.read_syntax inst.Instance.readtable port in
    let lib_name = Library.parse_library_name syntax in
    let builtins = Build.builtin_library_names inst in
    let nodes = Dep_graph.build_graph ~builtins
        ~search_paths:!(inst.Instance.search_paths) inst.readtable [lib_name] in
    print_string (Dep_graph.format_tree nodes lib_name)
  with
  | Reader.Read_error (_, msg) ->
    Printf.eprintf "Error parsing library name: %s\n%!" msg
  | Compiler.Compile_error (_, msg) ->
    Printf.eprintf "Error parsing library name: %s\n%!" msg
  | Dep_graph.Resolve_error (name, msg) ->
    Printf.eprintf "Error: resolving %s: %s\n%!"
      (Library.name_to_string name) msg
  | Dep_graph.Cycle_error cycles ->
    let cycle_strs = List.map (fun cycle ->
      String.concat " -> " (List.map Library.name_to_string cycle)
    ) cycles in
    Printf.eprintf "Error: circular dependencies:\n  %s\n%!"
      (String.concat "\n  " cycle_strs)
  | Failure msg -> format_error msg

let repl_reload inst arg =
  try
    let port = Port.of_string arg in
    let syntax = Reader.read_syntax inst.Instance.readtable port in
    let lib_name = Library.parse_library_name syntax in
    Instance.reload_library inst lib_name;
    Printf.printf "Reloaded %s\n%!" (Library.name_to_string lib_name)
  with
  | Reader.Read_error (_, msg) ->
    Printf.eprintf "Error parsing library name: %s\n%!" msg
  | Compiler.Compile_error (_, msg) ->
    Printf.eprintf "Error: %s\n%!" msg
  | Vm.Runtime_error msg -> format_error msg
  | Failure msg -> format_error msg

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

let save_repl_settings theme_ref paredit_ref =
  let theme_val = match !theme_ref with
    | Some (t : Highlight.theme) -> Datum.Str (Bytes.of_string t.name)
    | None -> Datum.Bool false
  in
  save_config [
    ("theme", theme_val);
    ("paredit", Datum.Bool !paredit_ref);
  ]

let handle_repl_command inst pkg_info theme_ref paredit_ref ~on_clear line =
  let line = String.trim line in
  match line with
  | ",quit" | ",q" -> exit 0
  | ",help" | ",h" -> repl_help ()
  | ",env" -> repl_env inst
  | ",libs" -> repl_libs inst
  | ",available" -> repl_available inst
  | ",build" -> repl_build inst pkg_info
  | ",exports" ->
    Printf.eprintf "Usage: ,exports (library name)\n%!"
  | ",deps" ->
    Printf.eprintf "Usage: ,deps (library name)\n%!"
  | ",reload" ->
    Printf.eprintf "Usage: ,reload (library name)\n%!"
  | ",clear" ->
    Printf.printf "\x1b[2J\x1b[H%!";
    on_clear ()
  | ",paredit" ->
    paredit_ref := not !(paredit_ref);
    save_repl_settings theme_ref paredit_ref;
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
    end else if String.length line > 9 && String.sub line 0 9 = ",exports " then begin
      let arg = String.trim (String.sub line 9 (String.length line - 9)) in
      if arg = "" then Printf.eprintf "Usage: ,exports (library name)\n%!"
      else repl_exports inst arg
    end else if String.length line > 6 && String.sub line 0 6 = ",deps " then begin
      let arg = String.trim (String.sub line 6 (String.length line - 6)) in
      if arg = "" then Printf.eprintf "Usage: ,deps (library name)\n%!"
      else repl_deps inst arg
    end else if String.length line > 8 && String.sub line 0 8 = ",reload " then begin
      let arg = String.trim (String.sub line 8 (String.length line - 8)) in
      if arg = "" then Printf.eprintf "Usage: ,reload (library name)\n%!"
      else repl_reload inst arg
    end else if String.length line > 7 && String.sub line 0 7 = ",theme " then begin
      let name = String.trim (String.sub line 7 (String.length line - 7)) in
      if name = "" then Printf.eprintf "Usage: ,theme <dark|light|none|path>\n%!"
      else begin
        let theme = resolve_theme name in
        theme_ref := theme;
        save_repl_settings theme_ref paredit_ref;
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
  let inst_ref = ref (make_instance ()) in
  (!inst_ref).search_paths := Search_path.resolve ~base_dirs:[Sys.getcwd ()];
  let pkg_info = setup_package !inst_ref (Sys.getcwd ()) in
  (match pkg_info with
   | Some (pkg, _) ->
     Printf.printf "Project: %s %s\n%!" pkg.Package.name
       (Semver.to_string pkg.version)
   | None -> ());
  auto_build_from_package !inst_ref pkg_info;
  let session_ref = ref (Session.create ()) in
  Printf.printf "Bilk Scheme %s\nType ,help for REPL commands, Ctrl-D to exit.\n%!" version;
  let saved = load_config () in
  let initial_theme = match theme_name with
    | Some name -> resolve_theme name
    | None ->
      match Sys.getenv_opt "BILK_THEME" with
      | Some name -> resolve_theme name
      | None ->
        match List.assoc_opt "theme" saved with
        | Some (Datum.Str b) -> resolve_theme (Bytes.to_string b)
        | Some (Datum.Bool false) -> None
        | _ -> Some Highlight.dark_theme
  in
  let initial_paredit = match List.assoc_opt "paredit" saved with
    | Some (Datum.Bool b) -> b
    | _ -> true
  in
  let theme_ref = ref initial_theme in
  let paredit_ref = ref initial_paredit in
  let highlight_fn text cursor =
    match !theme_ref with
    | None -> text
    | Some theme -> Highlight.highlight_line theme (!inst_ref).readtable text cursor
  in
  let repl_commands =
    [",help"; ",h"; ",quit"; ",q"; ",load"; ",env"; ",libs";
     ",available"; ",exports"; ",build"; ",deps"; ",reload";
     ",theme"; ",paredit"; ",checkpoint"; ",revert"; ",checkpoints";
     ",save-session"; ",load-session"; ",clear"]
  in
  let cached_candidates = ref [] in
  let cache_gen = ref (-1) in
  let get_scheme_candidates () =
    let inst = !inst_ref in
    let gen = List.length (Symbol.all inst.Instance.symbols) in
    if gen <> !cache_gen then begin
      let syms = Symbol.all inst.Instance.symbols in
      let bound = List.filter_map (fun sym ->
        match Env.lookup inst.Instance.global_env sym with
        | Some _ -> Some (Symbol.name sym)
        | None -> None
      ) syms in
      let syntax_names = Expander.binding_names inst.Instance.syn_env in
      let all = bound @ syntax_names in
      cached_candidates := List.sort_uniq String.compare all;
      cache_gen := gen
    end;
    !cached_candidates
  in
  let get_library_names () =
    let inst = !inst_ref in
    let loaded = Library.list_all inst.Instance.libraries in
    List.map (fun (l : Library.t) -> l.name) loaded
  in
  let apply_matches ~width:_ text cursor start matches =
    match matches with
    | [] -> Line_editor.No_completions
    | _ ->
      let cp = Completion.common_prefix matches in
      let before = String.sub text 0 start in
      let after = String.sub text cursor (String.length text - cursor) in
      Line_editor.Menu {
        text = before ^ cp ^ after;
        cursor = start + String.length cp;
        candidates = matches;
        start;
      }
  in
  let complete_fn text cursor ~width =
    let rt = (!inst_ref).Instance.readtable in
    match Completion_context.detect rt text cursor with
    | Completion_context.No_context ->
      Line_editor.No_completions
    | Completion_context.String_literal (content, start) ->
      let matches = Completion.complete_path content in
      apply_matches ~width text cursor start matches
    | Completion_context.Repl_command_arg (cmd, arg, start) ->
      let matches = match cmd with
        | ",load" | ",save-session" | ",load-session" ->
          Completion.complete_path arg
        | ",theme" ->
          Completion.find_matches arg ["dark"; "light"; "none"]
        | ",reload" | ",exports" | ",deps" ->
          let lib_names = get_library_names () in
          let formatted = List.map Completion.format_library_name lib_names in
          Completion.find_matches arg formatted
        | ",revert" | ",checkpoint" ->
          let names = Session.list_checkpoints !session_ref in
          Completion.find_matches arg names
        | _ -> []
      in
      apply_matches ~width text cursor start matches
    | Completion_context.Import_library (parts, prefix, start) ->
      let lib_names = get_library_names () in
      let matching = Completion.match_library_name parts prefix lib_names in
      let next_parts = List.filter_map (fun name ->
        let n = List.length parts in
        if List.length name > n then Some (List.nth name n)
        else None
      ) matching in
      let unique = List.sort_uniq String.compare next_parts in
      apply_matches ~width text cursor start unique
    | Completion_context.Identifier (prefix, start) ->
      let candidates =
        if String.length prefix > 0 && prefix.[0] = ',' then
          repl_commands
        else
          get_scheme_candidates ()
      in
      let matches = Completion.find_matches prefix candidates in
      apply_matches ~width text cursor start matches
  in
  let on_idle = match pkg_info with
    | None -> None
    | Some _ ->
      let w = Watch.create () in
      at_exit (fun () -> Watch.destroy w);
      List.iter (Watch.add_directory w) !((!inst_ref).search_paths);
      let rt = (!inst_ref).Instance.readtable in
      let builtins = Build.builtin_library_names !inst_ref in
      Some (fun () ->
        let inst = !inst_ref in
        let events = Watch.poll w in
        if events <> [] then begin
          let search_paths = !(inst.search_paths) in
          let roots = match pkg_info with
            | Some (pkg, pkg_dir) ->
              Build.collect_roots ~readtable:rt ~pkg_dir pkg
            | None -> []
          in
          (try
             let result = Build.auto_build ~search_paths ~builtins
                 ~readtable:rt roots in
             let rebuilt = result.Build.actions_taken in
             if rebuilt <> [] then begin
               (* Reload any rebuilt libraries that are currently loaded *)
               let loaded = Library.list_all inst.Instance.libraries in
               let loaded_names = List.map (fun (l : Library.t) -> l.name) loaded in
               let reloaded = ref 0 in
               List.iter (fun (br : Build.build_result) ->
                 if List.mem br.name loaded_names then begin
                   (try
                      Instance.reload_library inst br.name;
                      incr reloaded
                    with exn ->
                      Printf.eprintf "\r\x1b[K[watch] Reload error in %s: %s\n%!"
                        (Library.name_to_string br.name)
                        (Printexc.to_string exn))
                 end
               ) rebuilt;
               Printf.eprintf "\r\x1b[K[watch] Rebuilt %d librar%s, reloaded %d.\n%!"
                 (List.length rebuilt)
                 (if List.length rebuilt = 1 then "y" else "ies")
                 !reloaded
             end
           with exn ->
             Printf.eprintf "\r\x1b[K[watch] Build error: %s\n%!"
               (Printexc.to_string exn))
        end)
  in
  let editor = Line_editor.create {
    prompt = "\x1b[1mbilk>\x1b[0m ";
    continuation_prompt = "  ... ";
    history_file;
    max_history = 1000;
    is_complete = Some (fun text -> is_complete !inst_ref text);
    highlight = Some highlight_fn;
    paredit = Some paredit_ref;
    readtable = Some (!inst_ref).readtable;
    complete = Some complete_fn;
    on_idle;
  } in
  at_exit (fun () -> Line_editor.destroy editor);
  let print_result v =
    match v with
    | Datum.Void -> ()
    | _ -> print_endline (Datum.to_string v)
  in
  let eval_input input =
    let inst = !inst_ref in
    let port = Port.of_string input in
    let rec eval_loop () =
      try
        let expr = Reader.read_syntax inst.readtable port in
        match expr with
        | { Syntax.datum = Syntax.Eof; _ } -> ()
        | _ ->
          begin try
            print_result (Instance.eval_syntax inst expr);
            Port.flush !(inst.current_output);
            Port.flush !(inst.current_error)
          with
          | Reader.Read_error (loc, msg) -> format_loc_error loc msg
          | Compiler.Compile_error (loc, msg) -> format_loc_error loc msg
          | Vm.Runtime_error msg -> format_error msg
          | Fasl.Fasl_error msg -> format_error msg
          | Package.Package_error msg -> format_error msg
          | Pkg_manager.Pkg_error msg -> format_error msg
          | Extension.Extension_error msg -> format_error msg
          | Failure msg -> format_error msg
          end;
          eval_loop ()
      with
      | Reader.Read_error (loc, msg) ->
        format_loc_error loc msg
    in
    eval_loop ()
  in
  let setup_instance inst =
    inst.Instance.search_paths := Search_path.resolve ~base_dirs:[Sys.getcwd ()];
    (match pkg_info with
     | Some (_pkg, pkg_dir) ->
       let registry_root = Filename.concat pkg_dir ".bilk_packages" in
       if Sys.file_exists registry_root then
         inst.search_paths := registry_root :: !(inst.search_paths)
     | None -> ())
  in
  let revert_checkpoint session name =
    try
      let new_inst = Session.revert session name in
      setup_instance new_inst;
      new_inst.Instance.fasl_cache := true;
      inst_ref := new_inst;
      Printf.printf "Reverted to checkpoint: %s\n%!" name
    with
    | Session.Session_error msg -> Printf.eprintf "Error: %s\n%!" msg
    | Checkpoint.Checkpoint_error msg -> Printf.eprintf "Error: %s\n%!" msg
  in
  let handle_session_command trimmed =
    if trimmed = ",checkpoints" then begin
      let cps = Session.list_checkpoints !session_ref in
      if cps = [] then
        Printf.printf "No checkpoints.\n%!"
      else
        List.iter (fun name ->
          Printf.printf "  %s\n%!" name
        ) cps;
      true
    end else if String.length trimmed > 12
                && String.sub trimmed 0 12 = ",checkpoint " then begin
      let name = String.trim (String.sub trimmed 12 (String.length trimmed - 12)) in
      if name = "" then
        Printf.eprintf "Usage: ,checkpoint <name>\n%!"
      else begin
        try
          Session.checkpoint !session_ref name !inst_ref;
          Printf.printf "Checkpoint saved: %s\n%!" name
        with Checkpoint.Checkpoint_error msg ->
          Printf.eprintf "Error: %s\n%!" msg
      end;
      true
    end else if trimmed = ",checkpoint" then begin
      Printf.eprintf "Usage: ,checkpoint <name>\n%!";
      true
    end else if String.length trimmed > 8
                && String.sub trimmed 0 8 = ",revert " then begin
      let name = String.trim (String.sub trimmed 8 (String.length trimmed - 8)) in
      if name = "" then
        Printf.eprintf "Usage: ,revert <name>\n%!"
      else
        revert_checkpoint !session_ref name;
      true
    end else if trimmed = ",revert" then begin
      Printf.eprintf "Usage: ,revert <name>\n%!";
      true
    end else if String.length trimmed > 14
                && String.sub trimmed 0 14 = ",save-session " then begin
      let path = String.trim (String.sub trimmed 14 (String.length trimmed - 14)) in
      if path = "" then
        Printf.eprintf "Usage: ,save-session <file>\n%!"
      else begin
        try
          Session.save !session_ref path;
          let nc = List.length (Session.list_checkpoints !session_ref) in
          Printf.printf "Session saved: %s (%d checkpoint%s)\n%!"
            path nc (if nc = 1 then "" else "s")
        with Sys_error msg ->
          Printf.eprintf "Error: %s\n%!" msg
      end;
      true
    end else if trimmed = ",save-session" then begin
      Printf.eprintf "Usage: ,save-session <file>\n%!";
      true
    end else if String.length trimmed > 14
                && String.sub trimmed 0 14 = ",load-session " then begin
      let path = String.trim (String.sub trimmed 14 (String.length trimmed - 14)) in
      if path = "" then
        Printf.eprintf "Usage: ,load-session <file>\n%!"
      else begin
        try
          let loaded = Session.load path in
          let cps = Session.list_checkpoints loaded in
          let nc = List.length cps in
          Printf.printf "Session loaded: %s (%d checkpoint%s)\n%!"
            path nc (if nc = 1 then "" else "s");
          (* Replace current session *)
          session_ref := loaded;
          (* Revert to latest checkpoint if any *)
          match List.rev cps with
          | latest :: _ -> revert_checkpoint !session_ref latest
          | [] -> ()
        with
        | Session.Session_error msg -> Printf.eprintf "Error: %s\n%!" msg
        | Sys_error msg -> Printf.eprintf "Error: %s\n%!" msg
      end;
      true
    end else if trimmed = ",load-session" then begin
      Printf.eprintf "Usage: ,load-session <file>\n%!";
      true
    end else
      false
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
        if not (handle_session_command trimmed) then begin
          if String.length trimmed > 6 && String.sub trimmed 0 6 = ",load " then begin
            let path = String.trim (String.sub trimmed 6 (String.length trimmed - 6)) in
            if path <> "" then
              repl_load !inst_ref path
            else
              Printf.eprintf "Usage: ,load <file>\n%!"
          end else
            handle_repl_command !inst_ref pkg_info theme_ref paredit_ref
              ~on_clear:(fun () -> ()) trimmed
        end;
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
    | _, Some f -> exit (run_file f [])
    | None, None -> run_repl theme
  in
  let term = Term.(const default_cmd $ expr_opt $ file_arg $ theme_opt) in
  let info =
    Cmd.info "bilk" ~version
      ~doc:"Bilk Scheme — an R7RS implementation"
      ~man:[`S "DESCRIPTION";
            `P "Run Scheme code interactively, from a file, or from a \
                command-line expression.";
            `S "SUBCOMMANDS";
            `P "Use $(b,bilk compile) to ahead-of-time compile Scheme source.";
            `P "Use $(b,bilk run) to execute a compiled program FASL.";
            `P "Use $(b,bilk pkg) to manage local packages.";
            `P "Use $(b,bilk venv) to create a virtual environment.";
            `P "Use $(b,bilk ext) to manage native extensions.";
            `P "Use $(b,bilk build) to build libraries from a package.";
            `P "Use $(b,bilk test) to discover and run Scheme test files.";
            `P "Use $(b,bilk debug) to debug a Scheme program via DAP.";
            `P "Use $(b,bilk lsp) to start the Language Server Protocol server.";
            `P "Use $(b,bilk profile) to profile a Scheme program.";
            `S "ENVIRONMENT";
            `P "$(b,BILK_VENV) — path to active virtual environment \
                (its $(b,lib/) directory is searched for libraries).";
            `P "$(b,BILK_PATH) — colon-separated list of additional \
                library search directories.";
            `P "$(b,BILK_HOME) — override for the Bilk home directory \
                (default: $(b,~/.bilk/))."]
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
           (requires bilk to be installed as an opam package).")
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
                $(b,bilk compile)."]
  in
  Cmd.v info term

(* --- Package subcommands --- *)

let pkg_install path =
  handle_errors (fun () ->
    let src_dir = match path with
      | Some p -> p
      | None -> Sys.getcwd ()
    in
    let registry_root = install_registry_root () in
    let lockpath = Lockfile.lockfile_path src_dir in
    if Sys.file_exists lockpath then begin
      let lock = Lockfile.parse Readtable.default lockpath in
      let mismatches = Lockfile.verify ~registry_root lock in
      if mismatches = [] then
        Printf.printf "Lockfile verified: %d packages OK\n%!"
          (List.length lock.packages)
      else begin
        List.iter (fun m ->
          match m with
          | Lockfile.Hash_mismatch { name; expected; actual } ->
            Printf.eprintf "Hash mismatch: %s (expected %s, got %s)\n%!"
              name expected actual
          | Lockfile.Not_installed { name; version } ->
            Printf.eprintf "Not installed: %s %s\n%!" name version
        ) mismatches;
        raise (Lockfile.Lockfile_error
          (Printf.sprintf "%d integrity check(s) failed"
             (List.length mismatches)))
      end
    end else begin
      Pkg_manager.install ~registry_root ~src_dir;
      let pkg = Package.parse Readtable.default
        (Filename.concat src_dir "package.scm") in
      Printf.printf "Installed %s %s\n%!" pkg.name
        (Semver.to_string pkg.version)
    end)

let pkg_lock () =
  handle_errors (fun () ->
    let cwd = Sys.getcwd () in
    let pkg_file = match Package.find_package_file cwd with
      | Some p -> p
      | None -> raise (Lockfile.Lockfile_error "no package.scm found")
    in
    let project_dir = Filename.dirname pkg_file in
    let pkg = Package.parse Readtable.default pkg_file in
    let registry_root = Pkg_manager.effective_registry_root cwd in
    let lock = Lockfile.create ~registry_root pkg.depends in
    let path = Lockfile.lockfile_path project_dir in
    Lockfile.write path lock;
    Printf.printf "Wrote %s (%d packages)\n%!" path
      (List.length lock.packages))

let pkg_list () =
  handle_errors (fun () ->
    let registry_root = Pkg_manager.effective_registry_root (Sys.getcwd ()) in
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
    let registry_root = Pkg_manager.effective_registry_root (Sys.getcwd ()) in
    Pkg_manager.remove ~registry_root ~name ~version:ver;
    Printf.printf "Removed %s %s\n%!" name ver)

let pkg_info name version =
  handle_errors (fun () ->
    let registry_root = Pkg_manager.effective_registry_root (Sys.getcwd ()) in
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

let make_pkg_lock_cmd () =
  let open Cmdliner in
  let cmd () = exit (pkg_lock ()) in
  let term = Term.(const cmd $ const ()) in
  let info =
    Cmd.info "lock" ~version
      ~doc:"Resolve dependencies and write bilk.lock"
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

let pkg_why name =
  handle_errors (fun () ->
    let cwd = Sys.getcwd () in
    let pkg_file = match Package.find_package_file cwd with
      | Some p -> p
      | None ->
        Printf.eprintf "Error: no package.scm found\n%!";
        exit 1
    in
    let pkg = Package.parse Readtable.default pkg_file in
    let registry_root = Pkg_manager.effective_registry_root cwd in
    let reasons = Pkg_manager.why ~registry_root pkg.depends name in
    if reasons = [] then
      Printf.printf "%s is not in the dependency tree\n%!" name
    else
      List.iter (fun reason ->
        Printf.printf "%s\n%!" reason
      ) reasons)

let make_pkg_why_cmd () =
  let open Cmdliner in
  let name_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"NAME" ~doc:"Package name to explain.")
  in
  let cmd name = exit (pkg_why name) in
  let term = Term.(const cmd $ name_arg) in
  let info =
    Cmd.info "why" ~version
      ~doc:"Explain why a package is in the dependency tree"
  in
  Cmd.v info term

let pkg_init name =
  handle_errors (fun () ->
    let dir = Sys.getcwd () in
    let name = match name with
      | Some n -> n
      | None -> Filename.basename dir
    in
    Pkg_manager.init_project ~dir ~name;
    Printf.printf "Created package.scm and _packages/ in %s\n%!" dir)

let make_pkg_init_cmd () =
  let open Cmdliner in
  let name_arg =
    Arg.(value & pos 0 (some string) None &
         info [] ~docv:"NAME"
           ~doc:"Package name (default: current directory name).")
  in
  let cmd name = exit (pkg_init name) in
  let term = Term.(const cmd $ name_arg) in
  let info =
    Cmd.info "init" ~version
      ~doc:"Initialize a new project with package.scm and _packages/"
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
      Venv.create ~bilk_version:version dir;
      let abs_dir =
        if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir
        else dir
      in
      Printf.printf "Created virtual environment in %s\n%!" dir;
      Printf.printf "Activate with: export BILK_VENV=%s\n%!" abs_dir))
  in
  let term = Term.(const cmd $ dir_arg) in
  let info =
    Cmd.info "venv" ~version
      ~doc:"Create a virtual environment"
      ~man:[`S "DESCRIPTION";
            `P "Creates a new virtual environment directory with a $(b,lib/) \
                subdirectory for library files and a $(b,bilk-venv.cfg) marker. \
                Activate by setting $(b,BILK_VENV) to the directory path."]
  in
  Cmd.v info term

(* --- Repository subcommands --- *)

let bilk_home () =
  match config_dir () with
  | Some d -> d
  | None ->
    Printf.eprintf "Error: cannot determine bilk home directory\n%!";
    exit 1

let pkg_fetch name version =
  handle_errors (fun () ->
    let bilk_home = bilk_home () in
    let registry_root = install_registry_root () in
    let repos = Repository.load_repos bilk_home in
    if repos = [] then
      raise (Repository.Repository_error "no repositories configured; use 'bilk pkg repo add'");
    let version = match version with
      | Some v -> v
      | None ->
        (* Find latest version across all repos *)
        let all_versions = List.concat_map (fun repo ->
          Repository.scan_versions bilk_home repo name
        ) repos in
        if all_versions = [] then
          raise (Repository.Repository_error
            (Printf.sprintf "package %s not found in any repository" name));
        Semver.to_string (List.hd (List.rev
          (List.sort Semver.compare all_versions)))
    in
    (* Find which repo has the package *)
    let repo = List.find_opt (fun repo ->
      Repository.has_package bilk_home repo ~name ~version
    ) repos in
    match repo with
    | None ->
      raise (Repository.Repository_error
        (Printf.sprintf "package %s %s not found in any repository" name version))
    | Some repo ->
      Repository.fetch_package ~bilk_home ~registry_root repo ~name ~version;
      Printf.printf "Fetched %s %s from %s\n%!" name version repo.name)

let pkg_search query =
  handle_errors (fun () ->
    let bilk_home = bilk_home () in
    let repos = Repository.load_repos bilk_home in
    let results = Repository.search_all bilk_home repos query in
    if results = [] then
      print_endline "No packages found."
    else
      List.iter (fun (repo, entry) ->
        Printf.printf "%s (%s): %s\n" entry.Repository.pkg_name repo.Repository.name
          (String.concat ", " (List.map Semver.to_string entry.versions))
      ) results)

let repo_add name url =
  handle_errors (fun () ->
    let bilk_home = bilk_home () in
    let repos = Repository.load_repos bilk_home in
    if List.exists (fun r -> r.Repository.name = name) repos then
      raise (Repository.Repository_error
        (Printf.sprintf "repository %S already exists" name));
    let repo : Repository.repo = { name; url } in
    Repository.sync bilk_home repo;
    Repository.save_repos bilk_home (repos @ [repo]);
    Printf.printf "Added repository %s (%s)\n%!" name url)

let repo_list () =
  handle_errors (fun () ->
    let bilk_home = bilk_home () in
    let repos = Repository.load_repos bilk_home in
    if repos = [] then
      print_endline "No repositories configured."
    else
      List.iter (fun r ->
        Printf.printf "%s  %s\n" r.Repository.name r.Repository.url
      ) repos)

let repo_remove name =
  handle_errors (fun () ->
    let bilk_home = bilk_home () in
    let repos = Repository.load_repos bilk_home in
    let repo = List.find_opt (fun r -> r.Repository.name = name) repos in
    match repo with
    | None ->
      raise (Repository.Repository_error
        (Printf.sprintf "repository %S not found" name))
    | Some repo ->
      let repos' = List.filter (fun r -> r.Repository.name <> name) repos in
      Repository.save_repos bilk_home repos';
      (* Clean up cached clone *)
      let clone = Repository.clone_dir bilk_home repo in
      if Sys.file_exists clone then begin
        let rec rm_rf path =
          if Sys.is_directory path then begin
            Array.iter (fun f -> rm_rf (Filename.concat path f))
              (Sys.readdir path);
            Sys.rmdir path
          end else
            Sys.remove path
        in
        (try rm_rf clone with _ -> ())
      end;
      Printf.printf "Removed repository %s\n%!" name)

let repo_update name =
  handle_errors (fun () ->
    let bilk_home = bilk_home () in
    let repos = Repository.load_repos bilk_home in
    let to_update = match name with
      | None -> repos
      | Some n ->
        (match List.filter (fun r -> r.Repository.name = n) repos with
         | [] -> raise (Repository.Repository_error
                   (Printf.sprintf "repository %S not found" n))
         | filtered -> filtered)
    in
    List.iter (fun repo ->
      Printf.printf "Updating %s...\n%!" repo.Repository.name;
      Repository.sync bilk_home repo
    ) to_update;
    print_endline "Done.")

let make_pkg_fetch_cmd () =
  let open Cmdliner in
  let name_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"NAME" ~doc:"Package name.")
  in
  let version_arg =
    Arg.(value & pos 1 (some string) None &
         info [] ~docv:"VERSION" ~doc:"Package version (optional, fetches latest if omitted).")
  in
  let cmd name ver = exit (pkg_fetch name ver) in
  let term = Term.(const cmd $ name_arg $ version_arg) in
  let info =
    Cmd.info "fetch" ~version
      ~doc:"Fetch a package from a remote repository"
  in
  Cmd.v info term

let make_pkg_search_cmd () =
  let open Cmdliner in
  let query_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"QUERY" ~doc:"Search query.")
  in
  let cmd query = exit (pkg_search query) in
  let term = Term.(const cmd $ query_arg) in
  let info =
    Cmd.info "search" ~version
      ~doc:"Search remote repositories for packages"
  in
  Cmd.v info term

let make_repo_add_cmd () =
  let open Cmdliner in
  let name_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"NAME" ~doc:"Repository name.")
  in
  let url_arg =
    Arg.(required & pos 1 (some string) None &
         info [] ~docv:"URL" ~doc:"Repository URL.")
  in
  let cmd name url = exit (repo_add name url) in
  let term = Term.(const cmd $ name_arg $ url_arg) in
  let info = Cmd.info "add" ~version ~doc:"Add a repository" in
  Cmd.v info term

let make_repo_list_cmd () =
  let open Cmdliner in
  let cmd () = exit (repo_list ()) in
  let term = Term.(const cmd $ const ()) in
  let info = Cmd.info "list" ~version ~doc:"List configured repositories" in
  Cmd.v info term

let make_repo_remove_cmd () =
  let open Cmdliner in
  let name_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"NAME" ~doc:"Repository name.")
  in
  let cmd name = exit (repo_remove name) in
  let term = Term.(const cmd $ name_arg) in
  let info = Cmd.info "remove" ~version ~doc:"Remove a repository" in
  Cmd.v info term

let make_repo_update_cmd () =
  let open Cmdliner in
  let name_arg =
    Arg.(value & pos 0 (some string) None &
         info [] ~docv:"NAME" ~doc:"Repository name (optional, updates all if omitted).")
  in
  let cmd name = exit (repo_update name) in
  let term = Term.(const cmd $ name_arg) in
  let info = Cmd.info "update" ~version ~doc:"Update repository clones" in
  Cmd.v info term

let make_repo_cmd () =
  let open Cmdliner in
  let info =
    Cmd.info "repo" ~version
      ~doc:"Manage package repositories"
  in
  Cmd.group info [
    make_repo_add_cmd ();
    make_repo_list_cmd ();
    make_repo_remove_cmd ();
    make_repo_update_cmd ();
  ]

let make_pkg_cmd () =
  let open Cmdliner in
  let info =
    Cmd.info "pkg" ~version
      ~doc:"Package management commands"
      ~man:[`S "DESCRIPTION";
            `P "Manage packages. Use $(b,bilk pkg init), \
                $(b,bilk pkg install), \
                $(b,bilk pkg lock), $(b,bilk pkg list), \
                $(b,bilk pkg remove), $(b,bilk pkg info), \
                $(b,bilk pkg why), $(b,bilk pkg fetch), \
                $(b,bilk pkg search), or $(b,bilk pkg repo)."]
  in
  Cmd.group info [
    make_pkg_init_cmd ();
    make_pkg_install_cmd ();
    make_pkg_lock_cmd ();
    make_pkg_list_cmd ();
    make_pkg_remove_cmd ();
    make_pkg_info_cmd ();
    make_pkg_why_cmd ();
    make_pkg_fetch_cmd ();
    make_pkg_search_cmd ();
    make_repo_cmd ();
  ]

(* --- Extension scaffolding --- *)

let ensure_dir path =
  if not (Sys.file_exists path) then Sys.mkdir path 0o755

let ext_init_ocaml name dir =
  handle_errors (fun () ->
    let project_dir = Filename.concat dir ("bilk-" ^ name) in
    ensure_dir project_dir;
    ensure_dir (Filename.concat project_dir "lib");
    ensure_dir (Filename.concat project_dir "scheme");
    ensure_dir (Filename.concat project_dir (Filename.concat "scheme" name));
    (* dune-project *)
    let oc = open_out (Filename.concat project_dir "dune-project") in
    Printf.fprintf oc "(lang dune 3.0)\n(name bilk_%s)\n" name;
    close_out oc;
    (* lib/dune *)
    let oc = open_out (Filename.concat project_dir "lib/dune") in
    Printf.fprintf oc
      "(library\n (name bilk_%s)\n (public_name bilk_%s)\n (libraries bilk))\n"
      name name;
    close_out oc;
    (* lib/bilk_<name>.ml *)
    let oc = open_out (Filename.concat project_dir
      (Printf.sprintf "lib/bilk_%s.ml" name)) in
    Printf.fprintf oc
      "let init inst =\n\
      \  Bilk.Instance.define_primitive inst \"%s-hello\" (fun _args ->\n\
      \    Bilk.Datum.Str (Bytes.of_string \"hello from %s!\"))\n\
       \n\
       let () = Bilk.Extension.register_static \"%s\" init\n"
      name name name;
    close_out oc;
    (* lib/bilk_<name>.mli *)
    let oc = open_out (Filename.concat project_dir
      (Printf.sprintf "lib/bilk_%s.mli" name)) in
    Printf.fprintf oc
      "(** %s extension for Bilk. *)\n\
       \n\
       val init : Bilk.Instance.t -> unit\n\
       (** [init inst] registers the extension's primitives. *)\n"
      name;
    close_out oc;
    (* scheme/<name>/core.sld *)
    let oc = open_out (Filename.concat project_dir
      (Printf.sprintf "scheme/%s/core.sld" name)) in
    Printf.fprintf oc
      "(define-library (%s core)\n\
      \  (export %s-hello)\n\
      \  (include-shared \"%s\"))\n"
      name name name;
    close_out oc;
    Printf.printf "Created OCaml extension project: %s\n%!"
      project_dir)

let ext_init_c name dir =
  handle_errors (fun () ->
    let project_dir = Filename.concat dir ("bilk-" ^ name) in
    ensure_dir project_dir;
    ensure_dir (Filename.concat project_dir "src");
    ensure_dir (Filename.concat project_dir "scheme");
    ensure_dir (Filename.concat project_dir (Filename.concat "scheme" name));
    (* Makefile *)
    let oc = open_out (Filename.concat project_dir "Makefile") in
    Printf.fprintf oc
      "CC ?= cc\n\
       CFLAGS ?= -fPIC -Wall -Wextra\n\
       \n\
       .PHONY: all clean\n\
       \n\
       all: bilk_%s.so\n\
       \n\
       bilk_%s.so: src/bilk_%s.c\n\
       \t$(CC) -shared $(CFLAGS) -o $@ $<\n\
       \n\
       clean:\n\
       \trm -f bilk_%s.so\n"
      name name name name;
    close_out oc;
    (* src/bilk_<name>.c *)
    let oc = open_out (Filename.concat project_dir
      (Printf.sprintf "src/bilk_%s.c" name)) in
    Printf.fprintf oc
      "#include \"bilk.h\"\n\
       \n\
       static bilk_val_t my_hello(bilk_inst_t inst, int argc,\n\
       \                            const bilk_val_t *argv, void *data) {\n\
       \    (void)argc; (void)argv; (void)data;\n\
       \    return bilk_string(inst, \"hello from %s!\");\n\
       }\n\
       \n\
       BILK_EXT_INIT {\n\
       \    bilk_define_primitive(inst, \"%s-hello\", my_hello, NULL);\n\
       }\n"
      name name;
    close_out oc;
    (* scheme/<name>/core.sld *)
    let oc = open_out (Filename.concat project_dir
      (Printf.sprintf "scheme/%s/core.sld" name)) in
    Printf.fprintf oc
      "(define-library (%s core)\n\
      \  (export %s-hello)\n\
      \  (include-shared \"%s\"))\n"
      name name name;
    close_out oc;
    Printf.printf "Created C extension project: %s\n%!"
      project_dir)

let make_ext_init_cmd () =
  let open Cmdliner in
  let name_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"NAME" ~doc:"Extension name.")
  in
  let lang_opt =
    Arg.(value & opt string "ocaml" &
         info ["lang"] ~docv:"LANG"
           ~doc:"Language: $(b,ocaml) (default) or $(b,c).")
  in
  let dir_opt =
    Arg.(value & opt string "." &
         info ["dir"] ~docv:"DIR"
           ~doc:"Parent directory for the project (default: current directory).")
  in
  let cmd name lang dir =
    match lang with
    | "ocaml" -> exit (ext_init_ocaml name dir)
    | "c" -> exit (ext_init_c name dir)
    | _ ->
      Printf.eprintf "Unknown language: %s (use ocaml or c)\n%!" lang;
      exit 1
  in
  let term = Term.(const cmd $ name_arg $ lang_opt $ dir_opt) in
  let info =
    Cmd.info "init" ~version
      ~doc:"Initialize a new extension project"
      ~man:[`S "DESCRIPTION";
            `P "Creates a new extension project directory with boilerplate \
                for an OCaml or C extension."]
  in
  Cmd.v info term

let make_ext_cmd () =
  let open Cmdliner in
  let info =
    Cmd.info "ext" ~version
      ~doc:"Extension management commands"
      ~man:[`S "DESCRIPTION";
            `P "Manage native extensions. Use $(b,bilk ext init) to \
                create a new extension project."]
  in
  Cmd.group info [
    make_ext_init_cmd ();
  ]

(* --- Debug subcommand --- *)

let run_debug path port =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[dir_for_path path];
  ignore (setup_package inst (dir_for_path path));
  handle_errors (fun () ->
    let ds = Debug_server.create inst in
    match port with
    | None ->
      Debug_server.run_session ds stdin stdout path []
    | Some p ->
      let sock = Unix.socket PF_INET SOCK_STREAM 0 in
      Unix.setsockopt sock SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, p));
      Unix.listen sock 1;
      Printf.eprintf "Listening on port %d...\n%!" p;
      let (client, _addr) = Unix.accept sock in
      Fun.protect ~finally:(fun () ->
        Unix.close client;
        Unix.close sock)
        (fun () ->
          let ic = Unix.in_channel_of_descr client in
          let oc = Unix.out_channel_of_descr client in
          Debug_server.run_session ds ic oc path []))

let make_debug_cmd () =
  let open Cmdliner in
  let file_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"FILE" ~doc:"Scheme source file to debug.")
  in
  let port_opt =
    Arg.(value & opt (some int) None &
         info ["port"] ~docv:"PORT"
           ~doc:"Listen on a TCP port instead of using stdin/stdout.")
  in
  let cmd file port = exit (run_debug file port) in
  let term = Term.(const cmd $ file_arg $ port_opt) in
  let info =
    Cmd.info "debug" ~version
      ~doc:"Debug a Scheme program via DAP"
      ~man:[`S "DESCRIPTION";
            `P "Launches a Debug Adapter Protocol (DAP) server that \
                communicates over stdin/stdout (default) or over a TCP \
                socket when $(b,--port) is given. Connect a DAP client \
                (e.g. VS Code) to interactively debug the Scheme program."]
  in
  Cmd.v info term

(* --- LSP subcommand --- *)

let run_lsp port =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[Sys.getcwd ()];
  ignore (setup_package inst (Sys.getcwd ()));
  handle_errors (fun () ->
    let ls = Language_server.create inst in
    match port with
    | None ->
      Language_server.run_session ls stdin stdout
    | Some p ->
      let sock = Unix.socket PF_INET SOCK_STREAM 0 in
      Unix.setsockopt sock SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, p));
      Unix.listen sock 1;
      Printf.eprintf "LSP server listening on port %d...\n%!" p;
      let (client, _addr) = Unix.accept sock in
      Fun.protect ~finally:(fun () ->
        Unix.close client;
        Unix.close sock)
        (fun () ->
          let ic = Unix.in_channel_of_descr client in
          let oc = Unix.out_channel_of_descr client in
          Language_server.run_session ls ic oc))

let make_lsp_cmd () =
  let open Cmdliner in
  let port_opt =
    Arg.(value & opt (some int) None &
         info ["port"] ~docv:"PORT"
           ~doc:"Listen on a TCP port instead of using stdin/stdout.")
  in
  let cmd port = exit (run_lsp port) in
  let term = Term.(const cmd $ port_opt) in
  let info =
    Cmd.info "lsp" ~version
      ~doc:"Start the Language Server Protocol server"
      ~man:[`S "DESCRIPTION";
            `P "Launches an LSP server that communicates over \
                stdin/stdout (default) or over a TCP socket when \
                $(b,--port) is given. Connect an LSP client (e.g. \
                VS Code, Emacs, Neovim) for IDE features like \
                diagnostics, hover, completion, go-to-definition, \
                document symbols, and semantic tokens."]
  in
  Cmd.v info term

(* --- Test subcommand --- *)

let discover_test_files pkg_dir =
  let test_dir = Filename.concat pkg_dir "test" in
  if not (Sys.file_exists test_dir && Sys.is_directory test_dir) then []
  else
    let entries = Sys.readdir test_dir in
    let matches name =
      Filename.check_suffix name ".scm"
      && (let base = Filename.chop_suffix name ".scm" in
          (String.length base > 5 && String.sub base 0 5 = "test-")
          || (String.length base > 5
              && String.sub base (String.length base - 5) 5 = "-test"))
    in
    Array.to_list entries
    |> List.filter matches
    |> List.sort String.compare
    |> List.map (Filename.concat test_dir)

let run_test_file ~verbose path =
  if verbose then Printf.printf "Running %s ...\n%!" path;
  let dev_null = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let pid =
    Fun.protect ~finally:(fun () -> Unix.close dev_null) (fun () ->
      Unix.create_process
        Sys.executable_name
        [| Sys.executable_name; path |]
        dev_null Unix.stdout Unix.stderr)
  in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED _ -> 128
  | Unix.WSTOPPED _ -> 128

let run_test ~verbose file =
  let inst = make_instance () in
  let cwd = Sys.getcwd () in
  inst.search_paths := Search_path.resolve ~base_dirs:[cwd];
  let pkg_info = setup_package inst cwd in
  let build_code = handle_errors (fun () ->
    auto_build_from_package inst pkg_info)
  in
  if build_code <> 0 then build_code
  else
    let (files, pkg_dir) = match file with
      | Some path -> ([path], cwd)
      | None ->
        let pkg_dir = match pkg_info with
          | Some (_, dir) -> dir
          | None -> cwd
        in
        (discover_test_files pkg_dir, pkg_dir)
    in
    if files = [] then begin
      Printf.printf "No test files found.\n%!";
      Printf.printf "Place test files in test/ matching test-*.scm or *-test.scm.\n%!";
      0
    end else begin
      let t0 = Unix.gettimeofday () in
      let results = List.map (fun path ->
        let file_t0 = Unix.gettimeofday () in
        let code = run_test_file ~verbose path in
        let elapsed = Unix.gettimeofday () -. file_t0 in
        let rel_path =
          let prefix = pkg_dir ^ Filename.dir_sep in
          let plen = String.length prefix in
          if String.length path >= plen
             && String.sub path 0 plen = prefix then
            String.sub path plen (String.length path - plen)
          else path
        in
        let status_str = if code = 0 then "PASS" else "FAIL" in
        Printf.printf "%s ... %s  (%.2fs)\n%!" rel_path status_str elapsed;
        code
      ) files in
      let total = Unix.gettimeofday () -. t0 in
      let passed = List.length (List.filter (fun c -> c = 0) results) in
      let failed = List.length results - passed in
      Printf.printf "\n%d passed, %d failed (%.2fs)\n%!" passed failed total;
      if failed > 0 then 1 else 0
    end

let make_test_cmd () =
  let open Cmdliner in
  let verbose_flag =
    Arg.(value & flag &
         info ["verbose"; "v"] ~doc:"Print each test file name before running.")
  in
  let file_arg =
    Arg.(value & pos 0 (some string) None &
         info [] ~docv:"FILE" ~doc:"Run only this test file instead of discovering all.")
  in
  let cmd verbose file =
    exit (run_test ~verbose file)
  in
  let term = Term.(const cmd $ verbose_flag $ file_arg) in
  let info =
    Cmd.info "test" ~version
      ~doc:"Run Scheme test files"
      ~man:[`S "DESCRIPTION";
            `P "Discovers test files in the $(b,test/) directory (matching \
                $(b,test-*.scm) or $(b,*-test.scm)), auto-builds project \
                libraries, then runs each test as a subprocess. Exit code \
                is 0 if all tests pass, 1 if any fail."]
  in
  Cmd.v info term

(* --- Build subcommand --- *)

let do_build ~verbose ~search_paths ~builtins ~rt roots =
  let nodes = Dep_graph.build_graph ~builtins ~search_paths rt roots in
  let actions = Build.plan nodes in
  if actions = [] then begin
    Printf.printf "Everything is up to date.\n%!";
    0
  end else begin
    let t0 = Unix.gettimeofday () in
    let results = Build.execute ~verbose ~search_paths actions in
    let t1 = Unix.gettimeofday () in
    Printf.printf "Compiled %d librar%s in %.2fs.\n%!"
      (List.length results)
      (if List.length results = 1 then "y" else "ies")
      (t1 -. t0);
    List.length results
  end

let run_build ~graph ~clean_flag ~dry_run ~verbose ~watch target =
  handle_errors (fun () ->
    let base_search_paths = Search_path.resolve ~base_dirs:[Sys.getcwd ()] in
    let inst = Instance.create () in
    let builtins = Build.builtin_library_names inst in
    let rt = Readtable.default in
    let (search_paths, roots) = match target with
      | Some name_str ->
        let parts = String.split_on_char ' ' name_str in
        (base_search_paths, [parts])
      | None ->
        (match Package.find_package_file (Sys.getcwd ()) with
         | Some pkg_path ->
           let pkg = Package.parse rt pkg_path in
           let pkg_dir = Filename.dirname pkg_path in
           let registry_root = Pkg_manager.effective_registry_root (Sys.getcwd ()) in
           Instance.setup_package_paths inst ~registry_root pkg;
           let pkg_src = Filename.concat pkg_dir "src" in
           if Sys.file_exists pkg_src && Sys.is_directory pkg_src then
             inst.search_paths := pkg_src :: !(inst.search_paths);
           let paths = !(inst.search_paths) in
           let roots = Build.collect_roots ~readtable:rt ~pkg_dir pkg in
           (paths, roots)
         | None ->
           (base_search_paths,
            Instance.discover_available_libraries base_search_paths))
    in
    if graph || clean_flag || dry_run then begin
      let nodes = Dep_graph.build_graph ~builtins ~search_paths rt roots in
      if graph then
        print_string (Dep_graph.to_dot nodes)
      else if clean_flag then begin
        let count = Build.clean nodes in
        Printf.printf "Cleaned %d .fasl file%s.\n" count
          (if count = 1 then "" else "s")
      end else begin
        let actions = Build.plan nodes in
        if actions = [] then
          Printf.printf "Everything is up to date.\n"
        else begin
          Printf.printf "Would compile %d librar%s:\n"
            (List.length actions)
            (if List.length actions = 1 then "y" else "ies");
          List.iter (fun (a : Build.build_action) ->
            let reason_str = match a.reason with
              | Build.No_fasl -> "no cache"
              | Build.Source_newer -> "source changed"
              | Build.Dep_newer dep ->
                Printf.sprintf "dependency %s changed"
                  (Library.name_to_string dep)
            in
            Printf.printf "  %s  (%s)\n"
              (Library.name_to_string a.node.name) reason_str
          ) actions
        end
      end
    end else begin
      ignore (do_build ~verbose ~search_paths ~builtins ~rt roots);
      if watch then begin
        Printf.printf "[watch] Watching for changes... (Ctrl-C to stop)\n%!";
        let w = Watch.create () in
        Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
          List.iter (Watch.add_directory w) search_paths;
          let rec watch_loop () =
            let _events = Watch.wait w in
            Printf.printf "\x1b[2J\x1b[H%!";  (* clear screen *)
            (try
               ignore (do_build ~verbose ~search_paths ~builtins ~rt roots)
             with
             | Build.Build_error (name, msg) ->
               Printf.eprintf "[watch] Build error in %s: %s\n%!"
                 (Library.name_to_string name) msg
             | Dep_graph.Resolve_error (name, msg) ->
               Printf.eprintf "[watch] Resolve error for %s: %s\n%!"
                 (Library.name_to_string name) msg
             | exn ->
               Printf.eprintf "[watch] Error: %s\n%!" (Printexc.to_string exn));
            Printf.printf "[watch] Watching for changes... (Ctrl-C to stop)\n%!";
            watch_loop ()
          in
          watch_loop ())
      end
    end)

let make_build_cmd () =
  let open Cmdliner in
  let graph_flag =
    Arg.(value & flag &
         info ["graph"] ~doc:"Print the dependency graph in Graphviz DOT format.")
  in
  let clean_flag =
    Arg.(value & flag &
         info ["clean"] ~doc:"Remove all .fasl cache files.")
  in
  let dry_run_flag =
    Arg.(value & flag &
         info ["dry-run"] ~doc:"Show what would be compiled without compiling.")
  in
  let verbose_flag =
    Arg.(value & flag &
         info ["verbose"; "v"] ~doc:"Print each library as it is compiled.")
  in
  let watch_flag =
    Arg.(value & flag &
         info ["watch"; "w"] ~doc:"Watch for file changes and rebuild automatically.")
  in
  let target_arg =
    Arg.(value & pos 0 (some string) None &
         info [] ~docv:"LIBRARY" ~doc:"Library name to build (e.g. \"srfi 1\"). \
           If omitted, discovers and builds all project libraries.")
  in
  let cmd graph clean_flag dry_run verbose watch target =
    exit (run_build ~graph ~clean_flag ~dry_run ~verbose ~watch target)
  in
  let term = Term.(const cmd $ graph_flag $ clean_flag $ dry_run_flag
                   $ verbose_flag $ watch_flag $ target_arg) in
  let info =
    Cmd.info "build" ~version
      ~doc:"Incrementally compile project libraries to FASL cache"
      ~man:[`S "DESCRIPTION";
            `P "Discovers project libraries (or takes a specific library name), \
                computes the dependency graph, and compiles only those libraries \
                whose source or dependencies have changed. Compiled code is \
                cached as .fasl files next to the .sld source files."]
  in
  Cmd.v info term

(* --- Profile subcommand --- *)

let run_profile path format_str =
  let inst = make_instance () in
  inst.search_paths := Search_path.resolve ~base_dirs:[dir_for_path path];
  ignore (setup_package inst (dir_for_path path));
  handle_errors (fun () ->
    let prof = Profiler.create () in
    Profiler.install prof inst;
    let port = Port.of_file path in
    (try ignore (Instance.eval_port inst port)
     with exn ->
       Profiler.uninstall inst;
       Profiler.finalize prof;
       raise exn);
    Profiler.uninstall inst;
    Profiler.finalize prof;
    match format_str with
    | "text" ->
      Printf.eprintf "%s%!" (Profile_report.text_report prof)
    | "flamegraph" ->
      print_string (Profile_report.flamegraph_svg prof)
    | "trace" ->
      print_string (Profile_report.trace_json prof)
    | _ ->
      raise (Profiler.Profiler_error
        (Printf.sprintf "unknown format: %s (expected text, flamegraph, or trace)"
           format_str)))

let make_profile_cmd () =
  let open Cmdliner in
  let file_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"FILE" ~doc:"Scheme source file to profile.")
  in
  let format_opt =
    Arg.(value & opt string "text" &
         info ["format"] ~docv:"FORMAT"
           ~doc:"Output format: $(b,text) (default, to stderr), \
                 $(b,flamegraph) (SVG to stdout), or \
                 $(b,trace) (Chrome Trace JSON to stdout).")
  in
  let cmd file format_str = exit (run_profile file format_str) in
  let term = Term.(const cmd $ file_arg $ format_opt) in
  let info =
    Cmd.info "profile" ~version
      ~doc:"Profile a Scheme program"
      ~man:[`S "DESCRIPTION";
            `P "Executes a Scheme source file with profiling enabled and \
                produces a performance report. Three output formats are \
                available: a flat text table (default, to stderr), a flame \
                graph SVG (to stdout), or a Chrome Trace Event JSON file \
                (to stdout, loadable in chrome://tracing or Perfetto)."]
  in
  Cmd.v info term

(* --- Remote REPL: serve / attach --- *)

let run_serve port scrollback_size auto_checkpoint name =
  let config = {
    Repl_server.port;
    scrollback_size;
    auto_checkpoint;
    name;
  } in
  let server = Repl_server.create config in
  Printf.printf "Bilk REPL server '%s' listening on port %d\n%!" name port;
  (try Repl_server.run server
   with e ->
     Repl_server.shutdown server;
     raise e);
  0

let make_serve_cmd () =
  let open Cmdliner in
  let port_opt =
    Arg.(value & opt int 7890 &
         info ["port"; "p"] ~docv:"PORT"
           ~doc:"TCP port to listen on (default 7890).")
  in
  let scrollback_opt =
    Arg.(value & opt int 65536 &
         info ["scrollback"] ~docv:"BYTES"
           ~doc:"Scrollback buffer size in bytes (default 64KB).")
  in
  let auto_checkpoint_opt =
    Arg.(value & flag &
         info ["auto-checkpoint"]
           ~doc:"Automatically checkpoint on client disconnect.")
  in
  let name_opt =
    Arg.(value & opt string "default" &
         info ["name"] ~docv:"NAME"
           ~doc:"Session name (default 'default').")
  in
  let cmd port scrollback auto_checkpoint name =
    exit (run_serve port scrollback auto_checkpoint name)
  in
  let term = Term.(const cmd $ port_opt $ scrollback_opt
                   $ auto_checkpoint_opt $ name_opt) in
  let info =
    Cmd.info "serve" ~version
      ~doc:"Start a remote REPL server"
      ~man:[`S "DESCRIPTION";
            `P "Starts a persistent REPL server that clients can connect \
                to with $(b,bilk attach). The server holds the Scheme \
                instance, line editor, and scrollback buffer. Clients \
                that disconnect can reconnect and see previous output."]
  in
  Cmd.v info term

let run_attach host port =
  let config = { Repl_client.host; port } in
  (try Repl_client.connect config
   with
   | Unix.Unix_error (err, _, _) ->
     Printf.eprintf "Connection failed: %s\n%!" (Unix.error_message err);
     exit 1
   | Repl_protocol.Protocol_error msg ->
     Printf.eprintf "Protocol error: %s\n%!" msg;
     exit 1);
  0

let make_attach_cmd () =
  let open Cmdliner in
  let target_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"HOST:PORT"
           ~doc:"Server address in HOST:PORT format \
                 (e.g., localhost:7890).")
  in
  let cmd target =
    match String.split_on_char ':' target with
    | [host; port_str] ->
      (match int_of_string_opt port_str with
       | Some port -> exit (run_attach host port)
       | None ->
         Printf.eprintf "Invalid port: %s\n%!" port_str;
         exit 1)
    | _ ->
      Printf.eprintf "Expected HOST:PORT format (e.g., localhost:7890)\n%!";
      exit 1
  in
  let term = Term.(const cmd $ target_arg) in
  let info =
    Cmd.info "attach" ~version
      ~doc:"Connect to a remote REPL server"
      ~man:[`S "DESCRIPTION";
            `P "Connects to a running REPL server started with \
                $(b,bilk serve). Enters raw terminal mode and relays \
                keystrokes to the server. On reconnect, previous \
                output is replayed from the server's scrollback buffer."]
  in
  Cmd.v info term

(* Manual subcommand dispatch to avoid Cmd.group intercepting positional
   file arguments (e.g. "bilk file.scm") as unknown subcommand names. *)
let () =
  let open Cmdliner in
  let argc = Array.length Sys.argv in
  if argc >= 2 then begin
    match Sys.argv.(1) with
    | "test" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_test_cmd ()))
    | "build" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_build_cmd ()))
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
    | "ext" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_ext_cmd ()))
    | "debug" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_debug_cmd ()))
    | "lsp" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_lsp_cmd ()))
    | "profile" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_profile_cmd ()))
    | "serve" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_serve_cmd ()))
    | "attach" ->
      let argv = Array.concat [
        [| Sys.argv.(0) |];
        Array.sub Sys.argv 2 (argc - 2)
      ] in
      exit (Cmd.eval ~argv (make_attach_cmd ()))
    | arg when String.length arg > 0 && arg.[0] <> '-' ->
      let script_args = Array.to_list (Array.sub Sys.argv 2 (argc - 2)) in
      exit (run_file arg script_args)
    | _ -> exit (Cmd.eval (make_default_cmd ()))
  end else
    exit (Cmd.eval (make_default_cmd ()))
