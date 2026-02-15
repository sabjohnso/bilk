exception Repository_error of string

type repo = { name : string; url : string }

type index_entry = { pkg_name : string; versions : Semver.t list }
type index = { repo_name : string; entries : index_entry list }

let error msg = raise (Repository_error msg)
let errorf fmt = Printf.ksprintf error fmt

(* --- Filesystem helpers --- *)

let mkdir_p path =
  let rec aux dir =
    if Sys.file_exists dir then begin
      if not (Sys.is_directory dir) then
        errorf "not a directory: %s" dir
    end else begin
      aux (Filename.dirname dir);
      Sys.mkdir dir 0o755
    end
  in
  aux path

(* --- Configuration --- *)

let repos_dir bilk_home =
  Filename.concat bilk_home "repos"

let repos_config_path bilk_home =
  Filename.concat bilk_home "repositories"

let load_repos bilk_home =
  let path = repos_config_path bilk_home in
  if not (Sys.file_exists path) then []
  else
    let port = Port.of_file path in
    let sexp = Reader.read_syntax Readtable.default port in
    let datum = Syntax.to_datum sexp in
    match Datum.to_list datum with
    | Some pairs ->
      List.map (fun pair ->
        match pair with
        | Datum.Pair { car = Datum.Symbol name; cdr = Datum.Str url } ->
          { name; url = Bytes.to_string url }
        | _ -> error "malformed repository config"
      ) pairs
    | None -> error "malformed repository config"

let save_repos bilk_home repos =
  let path = repos_config_path bilk_home in
  mkdir_p bilk_home;
  let alist = Datum.list_of (List.map (fun r ->
    Datum.Pair { car = Datum.Symbol r.name;
                 cdr = Datum.Str (Bytes.of_string r.url) }
  ) repos) in
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () -> Printf.fprintf oc "%s\n" (Datum.to_string alist))

(* --- Clone management --- *)

let clone_dir bilk_home repo =
  Filename.concat (repos_dir bilk_home) repo.name

let sync bilk_home repo =
  let dest = clone_dir bilk_home repo in
  if Sys.file_exists dest && Sys.file_exists (Filename.concat dest ".git") then begin
    (* Pull latest *)
    let cmd = Printf.sprintf "git -C %s pull --ff-only >/dev/null"
      (Filename.quote dest) in
    let rc = Sys.command cmd in
    if rc <> 0 then
      errorf "failed to update repository %S" repo.name
  end else begin
    (* Remove corrupted clone if .git is missing *)
    if Sys.file_exists dest then begin
      let rec rm_rf path =
        if Sys.is_directory path then begin
          Array.iter (fun f -> rm_rf (Filename.concat path f))
            (Sys.readdir path);
          Sys.rmdir path
        end else
          Sys.remove path
      in
      rm_rf dest
    end;
    (* Fresh clone *)
    mkdir_p (repos_dir bilk_home);
    let cmd = Printf.sprintf "git clone --depth 1 %s %s >/dev/null"
      (Filename.quote repo.url) (Filename.quote dest) in
    let rc = Sys.command cmd in
    if rc <> 0 then
      errorf "failed to clone repository %S from %s" repo.name repo.url
  end

(* --- Index --- *)


let parse_index readtable content =
  let port = Port.of_string content in
  let sexp = Reader.read_syntax readtable port in
  match Syntax.to_proper_list sexp with
  | Some ({ Syntax.datum = Syntax.Symbol "repository"; _ } :: clauses) ->
    let name = ref None in
    let entries = ref [] in
    List.iter (fun clause ->
      match Syntax.to_proper_list clause with
      | Some ({ datum = Syntax.Symbol key; _ } :: vals) ->
        (match key with
         | "name" ->
           (match vals with
            | [{ datum = Syntax.Str n; _ }] -> name := Some n
            | _ -> error "index: name: expected a single string")
         | "packages" ->
           entries := List.map (fun pkg_syn ->
             match Syntax.to_proper_list pkg_syn with
             | Some ({ datum = Syntax.Symbol pkg_name; _ } :: ver_syns) ->
               let versions = List.map (fun vs ->
                 match vs.Syntax.datum with
                 | Syntax.Str ver_str ->
                   (try Semver.parse ver_str
                    with Semver.Parse_error _ ->
                      errorf "index: invalid version %S" ver_str)
                 | _ -> error "index: version must be a string"
               ) ver_syns in
               { pkg_name; versions }
             | _ -> error "index: expected (package-name version ...)"
           ) vals
         | _ -> error (Printf.sprintf "index: unknown clause: %s" key))
      | _ -> error "index: clause: expected (key value ...)"
    ) clauses;
    let repo_name = match !name with
      | Some n -> n
      | None -> error "index: missing required field: name"
    in
    { repo_name; entries = !entries }
  | _ -> error "index: expected (repository ...)"

let load_index bilk_home repo =
  let idx_path = Filename.concat (clone_dir bilk_home repo) "index.scm" in
  if Sys.file_exists idx_path then
    let ic = open_in idx_path in
    let content = Fun.protect ~finally:(fun () -> close_in ic)
      (fun () -> In_channel.input_all ic) in
    Some (parse_index Readtable.default content)
  else
    None

(* --- Package access --- *)

let package_dir bilk_home repo ~name ~version =
  Filename.concat
    (Filename.concat
      (Filename.concat (clone_dir bilk_home repo) "packages")
      name)
    version

let has_package bilk_home repo ~name ~version =
  Sys.file_exists (package_dir bilk_home repo ~name ~version)

let scan_versions bilk_home repo name =
  let pkg_dir = Filename.concat
    (Filename.concat (clone_dir bilk_home repo) "packages") name in
  if not (Sys.file_exists pkg_dir) then []
  else
    Sys.readdir pkg_dir
    |> Array.to_list
    |> List.filter_map (fun ver_str ->
         try Some (Semver.parse ver_str)
         with Semver.Parse_error _ -> None)
    |> List.sort Semver.compare

let fetch_package ~bilk_home ~registry_root repo ~name ~version =
  let src_dir = package_dir bilk_home repo ~name ~version in
  if not (Sys.file_exists src_dir) then
    errorf "package %s %s not found in repository %S" name version repo.name;
  Pkg_manager.install ~registry_root ~src_dir

(* --- Search --- *)

let search_index idx query =
  let q = String.lowercase_ascii query in
  List.filter (fun entry ->
    let name = String.lowercase_ascii entry.pkg_name in
    let rec contains_at i =
      if i + String.length q > String.length name then false
      else if String.sub name i (String.length q) = q then true
      else contains_at (i + 1)
    in
    contains_at 0
  ) idx.entries

let search_all bilk_home repos query =
  List.concat_map (fun repo ->
    match load_index bilk_home repo with
    | None -> []
    | Some idx ->
      List.map (fun entry -> (repo, entry)) (search_index idx query)
  ) repos
