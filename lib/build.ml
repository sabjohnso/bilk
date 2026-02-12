(* Incremental FASL compilation. *)

type stale_reason =
  | No_fasl
  | Source_newer
  | Dep_newer of Library.library_name

type build_action = {
  node : Dep_graph.node;
  reason : stale_reason;
}

type build_result = {
  name : Library.library_name;
  elapsed : float;
}

exception Build_error of Library.library_name * string

let is_stale nodes node =
  let fasl_path = Fasl.fasl_path_for node.Dep_graph.sld_path in
  if not (Sys.file_exists fasl_path) then
    Some No_fasl
  else if not (Fasl.is_cache_valid
                 ~sld_path:node.sld_path ~fasl_path) then
    Some Source_newer
  else
    (* Check if any dep's fasl is newer than this node's fasl *)
    let this_mtime = (Unix.stat fasl_path).Unix.st_mtime in
    let dep_with_newer_fasl =
      List.find_map (fun dep_name ->
        match List.find_opt (fun (n : Dep_graph.node) ->
          n.name = dep_name) nodes with
        | None -> None
        | Some dep_node ->
          let dep_fasl = Fasl.fasl_path_for dep_node.sld_path in
          if Sys.file_exists dep_fasl then
            let dep_mtime = (Unix.stat dep_fasl).Unix.st_mtime in
            if dep_mtime > this_mtime then Some dep_name
            else None
          else None
      ) node.imports
    in
    match dep_with_newer_fasl with
    | Some name -> Some (Dep_newer name)
    | None -> None

let plan nodes =
  let stale_set = Hashtbl.create 16 in
  let actions = ref [] in
  List.iter (fun (node : Dep_graph.node) ->
    match is_stale nodes node with
    | Some reason ->
      Hashtbl.replace stale_set node.name true;
      actions := { node; reason } :: !actions
    | None ->
      (* Check if any import is in the stale set (propagation) *)
      let stale_dep =
        List.find_opt (fun dep_name ->
          Hashtbl.mem stale_set dep_name
        ) node.imports
      in
      match stale_dep with
      | Some dep_name ->
        Hashtbl.replace stale_set node.name true;
        actions := { node; reason = Dep_newer dep_name } :: !actions
      | None -> ()
  ) nodes;
  List.rev !actions

let execute ?(verbose=false) ?on_compile ~search_paths actions =
  let inst = Instance.create () in
  inst.fasl_cache := true;
  inst.search_paths := search_paths;
  List.map (fun action ->
    let name = action.node.Dep_graph.name in
    if verbose then
      Printf.eprintf "  compiling %s\n%!" (Library.name_to_string name);
    (match on_compile with Some f -> f name | None -> ());
    (* Delete stale fasl to force recompilation from source *)
    let fasl_path = Fasl.fasl_path_for action.node.sld_path in
    (if Sys.file_exists fasl_path then Sys.remove fasl_path);
    let t0 = Unix.gettimeofday () in
    (try
       ignore (Instance.ensure_library inst name)
     with
     | Reader.Read_error (loc, msg) ->
       raise (Build_error (name,
         Printf.sprintf "%s: %s" (Loc.to_string loc) msg))
     | Compiler.Compile_error (loc, msg) ->
       raise (Build_error (name,
         Printf.sprintf "%s: %s" (Loc.to_string loc) msg))
     | Vm.Runtime_error msg -> raise (Build_error (name, msg))
     | Fasl.Fasl_error msg -> raise (Build_error (name, msg))
     | Failure msg -> raise (Build_error (name, msg))
     | exn ->
       raise (Build_error (name, Printexc.to_string exn)));
    let t1 = Unix.gettimeofday () in
    { name; elapsed = t1 -. t0 }
  ) actions

let clean nodes =
  List.fold_left (fun count (node : Dep_graph.node) ->
    let fasl_path = Fasl.fasl_path_for node.sld_path in
    if Sys.file_exists fasl_path then begin
      Sys.remove fasl_path;
      count + 1
    end else
      count
  ) 0 nodes

let builtin_library_names inst =
  List.map (fun (lib : Library.t) -> lib.name)
    (Library.list_all inst.Instance.libraries)
