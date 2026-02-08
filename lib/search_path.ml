let wile_home () =
  match Sys.getenv_opt "WILE_HOME" with
  | Some dir when dir <> "" -> dir
  | _ ->
    match Sys.getenv_opt "HOME" with
    | Some home -> Filename.concat home ".wile"
    | None -> ".wile"

let site_lib () =
  Filename.concat (wile_home ()) "lib"

let env_paths () =
  match Sys.getenv_opt "WILE_PATH" with
  | None | Some "" -> []
  | Some s ->
    String.split_on_char ':' s
    |> List.filter (fun s -> s <> "")

let venv_lib_path () =
  match Sys.getenv_opt "WILE_VENV" with
  | None | Some "" -> None
  | Some dir ->
    if Venv.is_venv dir then
      Some (Venv.lib_path dir)
    else
      None

let dir_exists path =
  Sys.file_exists path && Sys.is_directory path

let resolve ~base_dirs =
  let venv = match venv_lib_path () with
    | Some p -> [p]
    | None -> []
  in
  let paths =
    base_dirs
    @ venv
    @ env_paths ()
    @ [site_lib ()]
  in
  List.filter dir_exists paths
