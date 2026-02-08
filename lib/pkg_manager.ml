exception Pkg_error of string

let error msg = raise (Pkg_error msg)
let errorf fmt = Printf.ksprintf error fmt

let default_registry_root () =
  match Sys.getenv_opt "HOME" with
  | Some home -> Filename.concat (Filename.concat home ".wile") "packages"
  | None -> error "HOME environment variable not set"

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

let rec copy_tree src dst =
  if Sys.is_directory src then begin
    mkdir_p dst;
    Array.iter (fun name ->
      copy_tree
        (Filename.concat src name)
        (Filename.concat dst name)
    ) (Sys.readdir src)
  end else begin
    let ic = open_in_bin src in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      let oc = open_out_bin dst in
      Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
        let buf = Bytes.create 4096 in
        let rec loop () =
          let n = input ic buf 0 4096 in
          if n > 0 then begin
            output oc buf 0 n;
            loop ()
          end
        in
        loop ()))
  end

let rec rm_rf path =
  if Sys.is_directory path then begin
    Array.iter (fun f -> rm_rf (Filename.concat path f))
      (Sys.readdir path);
    Sys.rmdir path
  end else
    Sys.remove path

(* --- Path helpers --- *)

let version_dir ~registry_root ~name ~version =
  Filename.concat
    (Filename.concat registry_root name)
    version

let src_dir ~registry_root ~name ~version =
  Filename.concat
    (version_dir ~registry_root ~name ~version)
    "src"

(* --- Install --- *)

let install ~registry_root ~src_dir:source_dir =
  let pkg_file = Filename.concat source_dir "package.scm" in
  if not (Sys.file_exists pkg_file) then
    errorf "no package.scm found in %s" source_dir;
  let pkg = Package.parse Readtable.default pkg_file in
  let ver_str = Semver.to_string pkg.version in
  let dest = version_dir ~registry_root ~name:pkg.name ~version:ver_str in
  if Sys.file_exists dest then
    errorf "package %s %s is already installed" pkg.name ver_str;
  (* Copy to temp dir first, then rename for atomicity *)
  let parent = Filename.dirname dest in
  mkdir_p parent;
  let tmp = Filename.temp_dir
    (Printf.sprintf "wile_install_%s_" pkg.name) "" in
  Fun.protect ~finally:(fun () ->
    if Sys.file_exists tmp then (try rm_rf tmp with _ -> ()))
    (fun () ->
      (* Copy package.scm *)
      copy_tree pkg_file (Filename.concat tmp "package.scm");
      (* Copy src/ if it exists *)
      let src_subdir = Filename.concat source_dir "src" in
      if Sys.file_exists src_subdir && Sys.is_directory src_subdir then
        copy_tree src_subdir (Filename.concat tmp "src");
      (* Move into place *)
      Sys.rename tmp dest)

(* --- Remove --- *)

let remove ~registry_root ~name ~version =
  let dest = version_dir ~registry_root ~name ~version in
  if not (Sys.file_exists dest) then
    errorf "package %s %s is not installed" name version;
  rm_rf dest;
  (* Clean up empty package directory *)
  let pkg_dir = Filename.concat registry_root name in
  if Sys.file_exists pkg_dir then begin
    let entries = Sys.readdir pkg_dir in
    if Array.length entries = 0 then
      (try Sys.rmdir pkg_dir with _ -> ())
  end

(* --- List --- *)

let list_packages ~registry_root =
  if not (Sys.file_exists registry_root) then []
  else begin
    let entries = Sys.readdir registry_root in
    let packages = Array.to_list entries |> List.filter_map (fun name ->
      let pkg_dir = Filename.concat registry_root name in
      if Sys.is_directory pkg_dir then begin
        let versions = Sys.readdir pkg_dir
          |> Array.to_list
          |> List.filter_map (fun ver_str ->
               try Some (Semver.parse ver_str)
               with Semver.Parse_error _ -> None)
          |> List.sort Semver.compare
        in
        if versions = [] then None
        else Some (name, versions)
      end else None
    ) in
    List.sort (fun (a, _) (b, _) -> String.compare a b) packages
  end

(* --- Package info --- *)

let package_info ~registry_root ~name ~version =
  let dest = version_dir ~registry_root ~name ~version in
  let pkg_file = Filename.concat dest "package.scm" in
  if not (Sys.file_exists pkg_file) then
    errorf "package %s %s is not installed" name version;
  Package.parse Readtable.default pkg_file

(* --- Dependency resolution --- *)

let installed_versions ~registry_root name =
  let pkg_dir = Filename.concat registry_root name in
  if not (Sys.file_exists pkg_dir) then []
  else
    Sys.readdir pkg_dir
    |> Array.to_list
    |> List.filter_map (fun ver_str ->
         try Some (Semver.parse ver_str)
         with Semver.Parse_error _ -> None)

let resolve ~registry_root deps =
  let resolved : (string, Semver.t) Hashtbl.t = Hashtbl.create 16 in
  let visiting : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let rec process dep =
    let name = dep.Package.dep_name in
    let constraints = dep.dep_constraints in
    (* Check for circular dependency *)
    if Hashtbl.mem visiting name then
      errorf "circular dependency: %s" name;
    (* Check if already resolved *)
    (match Hashtbl.find_opt resolved name with
     | Some existing_ver ->
       (* Verify existing resolution satisfies new constraints *)
       if not (Semver.satisfies existing_ver constraints) then
         errorf "version conflict for %s: %s does not satisfy new constraints"
           name (Semver.to_string existing_ver)
     | None ->
       (* Find latest satisfying version *)
       let versions = installed_versions ~registry_root name in
       (match Semver.latest_satisfying versions constraints with
        | None ->
          if versions = [] then
            errorf "package not found: %s" name
          else
            errorf "no installed version of %s satisfies constraints" name
        | Some ver ->
          Hashtbl.replace resolved name ver;
          (* Resolve transitive dependencies *)
          Hashtbl.replace visiting name ();
          let pkg = package_info ~registry_root ~name
              ~version:(Semver.to_string ver) in
          List.iter process pkg.depends;
          Hashtbl.remove visiting name))
  in
  List.iter process deps;
  Hashtbl.fold (fun name ver acc -> (name, ver) :: acc) resolved []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

(* --- Search paths --- *)

let search_paths_for ~registry_root resolved =
  List.map (fun (name, ver) ->
    src_dir ~registry_root ~name ~version:(Semver.to_string ver)
  ) resolved
