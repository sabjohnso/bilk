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

let constraint_op_to_string = function
  | Semver.Eq -> "=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="

let format_constraints cs =
  String.concat ", " (List.map (fun (op, v) ->
    constraint_op_to_string op ^ " " ^ Semver.to_string v
  ) cs)

(* Provenance: for each dep name, which (requester, constraints) pairs exist *)
type provenance = (string, (string * Semver.constraint_set) list) Hashtbl.t

let record_provenance (prov : provenance) ~dep_name ~requester ~constraints =
  let existing = match Hashtbl.find_opt prov dep_name with
    | Some l -> l | None -> []
  in
  Hashtbl.replace prov dep_name ((requester, constraints) :: existing)

let dedup_provenance entries =
  List.fold_left (fun acc entry ->
    if List.exists (fun (r, cs) ->
      fst entry = r && snd entry = cs) acc
    then acc
    else entry :: acc
  ) [] entries |> List.rev

let format_conflict_error ~dep_name ~(prov : provenance) ~registry_root =
  let buf = Buffer.create 128 in
  Buffer.add_string buf (Printf.sprintf "version conflict for %s:\n" dep_name);
  (match Hashtbl.find_opt prov dep_name with
   | Some entries ->
     let unique = dedup_provenance (List.rev entries) in
     List.iter (fun (requester, cs) ->
       if cs = [] then
         Buffer.add_string buf
           (Printf.sprintf "  %s requires %s (any version)\n" requester dep_name)
       else
         Buffer.add_string buf
           (Printf.sprintf "  %s requires %s %s\n" requester dep_name
              (format_constraints cs))
     ) unique
   | None -> ());
  let versions = installed_versions ~registry_root dep_name in
  let sorted = List.sort Semver.compare versions in
  Buffer.add_string buf
    (Printf.sprintf "  available: %s"
       (String.concat ", " (List.map Semver.to_string sorted)));
  Buffer.contents buf

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let max_backtracks = 1000

type resolve_state = {
  resolved : Semver.t StringMap.t;
  visiting : StringSet.t;
}

let resolve ~registry_root deps =
  let prov : provenance = Hashtbl.create 16 in
  let backtracks = ref 0 in
  (* Resolve a single dep, then call continuation k on success *)
  let rec resolve_one dep ~k state =
    let name = dep.Package.dep_name in
    let constraints = dep.Package.dep_constraints in
    if StringSet.mem name state.visiting then
      errorf "circular dependency: %s" name;
    match StringMap.find_opt name state.resolved with
    | Some existing_ver ->
      if Semver.satisfies existing_ver constraints then
        k state
      else
        None (* backtrack *)
    | None ->
      let versions = installed_versions ~registry_root name in
      if versions = [] then
        errorf "package not found: %s" name;
      let candidates =
        List.filter (fun v -> Semver.satisfies v constraints) versions
        |> List.sort (fun a b -> Semver.compare b a) (* newest first *)
      in
      try_versions ~name candidates ~k state
  (* Try each candidate version for a package *)
  and try_versions ~name candidates ~k state =
    match candidates with
    | [] -> None (* no version works, backtrack *)
    | ver :: rest ->
      incr backtracks;
      if !backtracks > max_backtracks then
        errorf "dependency resolution exceeded %d backtracks" max_backtracks;
      let state' = {
        resolved = StringMap.add name ver state.resolved;
        visiting = StringSet.add name state.visiting;
      } in
      let pkg = package_info ~registry_root ~name
          ~version:(Semver.to_string ver) in
      let req = Printf.sprintf "%s %s" name (Semver.to_string ver) in
      let k' s =
        k { s with visiting = StringSet.remove name s.visiting }
      in
      begin match resolve_all ~requester:req pkg.depends ~k:k' state' with
      | Some final -> Some final
      | None -> try_versions ~name rest ~k state
      end
  (* Resolve a list of deps; record provenance then chain via continuations *)
  and resolve_all ~requester dep_list ~k state =
    match dep_list with
    | [] -> k state
    | dep :: rest ->
      record_provenance prov ~dep_name:dep.Package.dep_name
        ~requester ~constraints:dep.Package.dep_constraints;
      resolve_one dep ~k:(fun state' ->
        resolve_all ~requester rest ~k state'
      ) state
  in
  let init = { resolved = StringMap.empty; visiting = StringSet.empty } in
  match resolve_all ~requester:"(root)" deps ~k:Option.some init with
  | Some final ->
    StringMap.bindings final.resolved
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  | None ->
    (* Find the most constrained dep to produce an informative error *)
    let failed_dep =
      Hashtbl.to_seq_keys prov
      |> Seq.find (fun name ->
        let versions = installed_versions ~registry_root name in
        let all_constraints =
          match Hashtbl.find_opt prov name with
          | Some entries -> List.concat_map snd entries
          | None -> []
        in
        not (List.exists (fun v -> Semver.satisfies v all_constraints) versions))
    in
    begin match failed_dep with
    | Some name ->
      error (format_conflict_error ~dep_name:name ~prov ~registry_root)
    | None ->
      error "dependency resolution failed: no valid combination found"
    end

(* --- Why --- *)

let why ~registry_root deps target =
  let resolved = resolve ~registry_root deps in
  if not (List.exists (fun (name, _) -> name = target) resolved) then
    []
  else begin
    let reasons = ref [] in
    (* Check if it's a direct dependency *)
    if List.exists (fun d -> d.Package.dep_name = target) deps then
      reasons := [Printf.sprintf "%s is a direct dependency" target];
    (* Check which resolved packages depend on target *)
    List.iter (fun (name, ver) ->
      if name <> target then begin
        let pkg = package_info ~registry_root ~name
            ~version:(Semver.to_string ver) in
        List.iter (fun d ->
          if d.Package.dep_name = target then
            reasons :=
              (Printf.sprintf "%s %s is required by %s %s"
                 target (Semver.to_string (List.assoc target resolved))
                 name (Semver.to_string ver))
              :: !reasons
        ) pkg.depends
      end
    ) resolved;
    List.rev !reasons
  end

(* --- Search paths --- *)

let search_paths_for ~registry_root resolved =
  List.map (fun (name, ver) ->
    src_dir ~registry_root ~name ~version:(Semver.to_string ver)
  ) resolved
