(* Static dependency graph for R7RS libraries. *)

type node = {
  name : Library.library_name;
  sld_path : string;
  imports : Library.library_name list;
}

type sld_info = {
  declared_name : Library.library_name;
  imports : Library.library_name list;
}

exception Cycle_error of Library.library_name list list
exception Resolve_error of Library.library_name * string

(* --- Helpers --- *)

let rec syntax_to_proper_list (s : Syntax.t) =
  match s.datum with
  | Syntax.Nil -> Some []
  | Syntax.Pair (car, cdr) ->
    (match syntax_to_proper_list cdr with
     | Some rest -> Some (car :: rest)
     | None -> None)
  | _ -> None

(* --- Import extraction --- *)

let rec base_library_name = function
  | Library.Import_lib name -> name
  | Library.Import_only (inner, _) -> base_library_name inner
  | Library.Import_except (inner, _) -> base_library_name inner
  | Library.Import_prefix (inner, _) -> base_library_name inner
  | Library.Import_rename (inner, _) -> base_library_name inner

let extract_imports_from_import_sets import_sets =
  List.map (fun iset_syntax ->
    let iset = Library.parse_import_set iset_syntax in
    base_library_name iset
  ) import_sets

let rec extract_imports_from_decls decls =
  (* decls is a list of Syntax.t representing define-library declarations.
     Extract all (import ...) clauses and return the base library names.
     Handles (cond-expand ...) by extracting imports from ALL branches. *)
  List.concat_map extract_imports_from_decl decls

and extract_imports_from_decl (decl : Syntax.t) =
  match syntax_to_proper_list decl with
  | Some ({ datum = Syntax.Symbol "import"; _ } :: import_sets) ->
    extract_imports_from_import_sets import_sets
  | Some ({ datum = Syntax.Symbol "cond-expand"; _ } :: clauses) ->
    extract_imports_from_cond_expand clauses
  | _ -> []

and extract_imports_from_cond_expand clauses =
  (* For static analysis, extract imports from ALL branches.
     Each clause is (feature-req decl ...) or (else decl ...). *)
  List.concat_map (fun (clause : Syntax.t) ->
    match syntax_to_proper_list clause with
    | Some (_ :: decls) ->
      (* Skip the feature-requirement (first element), process the rest
         as declarations. The first element is either a feature-req
         like (library ...), an identifier like r7rs, or the keyword else. *)
      extract_imports_from_decls decls
    | _ -> []
  ) clauses

let parse_sld_file rt path =
  (* Parse a .sld file and return (declared_name, declarations). *)
  let port = Port.of_file path in
  let expr = Reader.read_syntax rt port in
  match syntax_to_proper_list expr with
  | Some ({ datum = Syntax.Symbol "define-library"; _ } :: name_syn :: decls) ->
    let name = Library.parse_library_name name_syn in
    (name, decls)
  | _ ->
    failwith (Printf.sprintf "%s: not a define-library form" path)

let parse_sld rt path =
  let (declared_name, decls) = parse_sld_file rt path in
  let imports = extract_imports_from_decls decls in
  { declared_name; imports }

let imports_of_sld rt path =
  (parse_sld rt path).imports

let imports_of_scm rt path =
  let port = Port.of_file path in
  let rec read_imports acc =
    let expr = Reader.read_syntax rt port in
    match expr.datum with
    | Syntax.Eof -> List.rev acc
    | _ ->
      match syntax_to_proper_list expr with
      | Some ({ datum = Syntax.Symbol "import"; _ } :: import_sets) ->
        let names = extract_imports_from_import_sets import_sets in
        read_imports (List.rev_append names acc)
      | _ ->
        (* Stop at first non-import top-level form *)
        List.rev acc
  in
  read_imports []

(* --- Path resolution --- *)

let lib_name_to_path dir name =
  let rec build d = function
    | [] -> failwith "empty library name"
    | [last] -> Filename.concat d (last ^ ".sld")
    | segment :: rest -> build (Filename.concat d segment) rest
  in
  build dir name

let resolve_sld ~search_paths name =
  List.find_map (fun dir ->
    let path = lib_name_to_path dir name in
    if Sys.file_exists path then Some path else None
  ) search_paths

(* --- Graph construction --- *)

let build_graph ?builtins ~search_paths rt roots =
  (* When builtins is provided, build a hashtable for O(1) lookup *)
  let builtin_set = match builtins with
    | None -> None
    | Some names ->
      let tbl = Hashtbl.create (List.length names) in
      List.iter (fun name -> Hashtbl.replace tbl name true) names;
      Some tbl
  in
  let is_builtin name = match builtin_set with
    | None -> false
    | Some tbl -> Hashtbl.mem tbl name
  in

  (* Visited: library name -> node (fully processed) *)
  let visited = Hashtbl.create 16 in
  (* In-progress: library name -> true (currently on the DFS stack) *)
  let in_progress = Hashtbl.create 16 in
  (* Track path for cycle reporting *)
  let cycles = ref [] in
  let result = ref [] in

  let rec visit path name =
    if Hashtbl.mem visited name then ()
    else if is_builtin name then ()
    else if Hashtbl.mem in_progress name then begin
      (* Found a cycle: path is the DFS stack with most recent first.
         Walk from the top of the path until we find [name] again,
         collecting the cycle members. *)
      let rec extract_cycle = function
        | [] -> []
        | n :: rest ->
          if n = name then [n]
          else n :: extract_cycle rest
      in
      let cycle = name :: extract_cycle path in
      cycles := cycle :: !cycles
    end else begin
      match resolve_sld ~search_paths name with
      | None ->
        if builtin_set = None then
          (* Legacy behaviour: silently skip *)
          ()
        else
          raise (Resolve_error (name,
            Printf.sprintf "library %s not found on search path"
              (Library.name_to_string name)))
      | Some sld_path ->
        Hashtbl.replace in_progress name true;
        let imports = imports_of_sld rt sld_path in
        List.iter (fun imp -> visit (name :: path) imp) imports;
        Hashtbl.remove in_progress name;
        let node = { name; sld_path; imports } in
        Hashtbl.replace visited name node;
        result := node :: !result
    end
  in

  List.iter (fun root -> visit [] root) roots;

  if !cycles <> [] then
    raise (Cycle_error (List.rev !cycles));

  List.rev !result

(* --- Tree display --- *)

let format_tree nodes root =
  let by_name = Hashtbl.create (List.length nodes) in
  List.iter (fun n -> Hashtbl.replace by_name n.name n) nodes;
  let buf = Buffer.create 128 in
  let visited = Hashtbl.create 16 in
  let rec go indent name =
    let prefix = String.make indent ' ' in
    let label = Library.name_to_string name in
    if Hashtbl.mem visited name then
      Buffer.add_string buf (prefix ^ label ^ " ...\n")
    else begin
      Hashtbl.replace visited name true;
      Buffer.add_string buf (prefix ^ label ^ "\n");
      match Hashtbl.find_opt by_name name with
      | None -> ()
      | Some node ->
        List.iter (fun imp -> go (indent + 2) imp) node.imports
    end
  in
  go 0 root;
  Buffer.contents buf

(* --- Graphviz export --- *)

let dot_id name =
  "\"" ^ String.concat " " name ^ "\""

let to_dot nodes =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "digraph dependencies {\n";
  Buffer.add_string buf "  rankdir=BT;\n";
  Buffer.add_string buf "  node [shape=box];\n";

  (* Build a set of node names for cycle detection *)
  let node_set = Hashtbl.create 16 in
  List.iter (fun n -> Hashtbl.replace node_set n.name true) nodes;

  (* Detect cycle edges: an edge A -> B is a cycle edge if B also
     imports A (directly or transitively).  Simple approach: check
     if both A->B and B->A edges exist. *)
  let import_set = Hashtbl.create 16 in
  List.iter (fun n ->
    List.iter (fun imp ->
      if Hashtbl.mem node_set imp then
        Hashtbl.replace import_set (n.name, imp) true
    ) n.imports
  ) nodes;

  let is_cycle_edge a b =
    Hashtbl.mem import_set (a, b) && Hashtbl.mem import_set (b, a)
  in

  (* Emit nodes *)
  List.iter (fun n ->
    Buffer.add_string buf
      (Printf.sprintf "  %s [label=\"(%s)\"];\n"
         (dot_id n.name) (String.concat " " n.name))
  ) nodes;

  (* Emit edges *)
  List.iter (fun n ->
    List.iter (fun imp ->
      if Hashtbl.mem node_set imp then begin
        let attrs =
          if is_cycle_edge n.name imp then " [color=red, penwidth=2]"
          else ""
        in
        Buffer.add_string buf
          (Printf.sprintf "  %s -> %s%s;\n"
             (dot_id n.name) (dot_id imp) attrs)
      end
    ) n.imports
  ) nodes;

  Buffer.add_string buf "}\n";
  Buffer.contents buf
