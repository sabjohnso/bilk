type library_name = string list

type export_spec =
  | Export_id of string
  | Export_rename of string * string

type import_set =
  | Import_lib of library_name
  | Import_only of import_set * string list
  | Import_except of import_set * string list
  | Import_prefix of import_set * string
  | Import_rename of import_set * (string * string) list

type t = {
  name : library_name;
  env : Env.t;
  exports : (string, int * Datum.t ref) Hashtbl.t;
  syntax_exports : (string, Expander.binding) Hashtbl.t;
}

type registry = (string, t) Hashtbl.t

let create_registry () : registry = Hashtbl.create 16

let name_to_string name =
  "(" ^ String.concat " " name ^ ")"

let name_key name = String.concat "\x00" name

let register (reg : registry) lib =
  Hashtbl.replace reg (name_key lib.name) lib

let lookup (reg : registry) name =
  Hashtbl.find_opt reg (name_key name)

let list_all (reg : registry) =
  Hashtbl.fold (fun _key lib acc -> lib :: acc) reg []

let export_names lib =
  let rt = Hashtbl.fold (fun name _v acc -> name :: acc) lib.exports [] in
  let syn = Hashtbl.fold (fun name _v acc -> name :: acc) lib.syntax_exports [] in
  (rt, syn)

(* --- Syntax helpers --- *)

let compile_error loc msg = raise (Compiler.Compile_error (loc, msg))

let rec syntax_to_proper_list s =
  match s.Syntax.datum with
  | Syntax.Nil -> Some []
  | Syntax.Pair (car, cdr) ->
    (match syntax_to_proper_list cdr with
     | Some rest -> Some (car :: rest)
     | None -> None)
  | _ -> None

(* --- Parsing --- *)

let parse_library_name s =
  match syntax_to_proper_list s with
  | None -> compile_error s.loc "library name: expected a proper list"
  | Some parts ->
    if parts = [] then compile_error s.loc "library name: empty";
    List.map (fun p ->
      match p.Syntax.datum with
      | Syntax.Symbol name -> name
      | Syntax.Fixnum n -> string_of_int n
      | Syntax.Bignum z -> Z.to_string z
      | _ -> compile_error p.loc "library name: expected identifier or integer"
    ) parts

let parse_export_spec s =
  match s.Syntax.datum with
  | Syntax.Symbol name -> Export_id name
  | Syntax.Pair _ ->
    (match syntax_to_proper_list s with
     | Some [ { datum = Syntax.Symbol "rename"; _ };
              { datum = Syntax.Symbol internal; _ };
              { datum = Syntax.Symbol external_; _ } ] ->
       Export_rename (internal, external_)
     | _ -> compile_error s.loc "export: malformed export spec")
  | _ -> compile_error s.loc "export: expected identifier or (rename ...)"

let rec parse_import_set s =
  match s.Syntax.datum with
  | Syntax.Pair _ ->
    (match syntax_to_proper_list s with
     | None -> compile_error s.loc "import set: expected proper list"
     | Some [] -> compile_error s.loc "import set: empty list"
     | Some (head :: rest) ->
       match head.datum with
       | Syntax.Symbol "only" ->
         (match rest with
          | inner :: (_ :: _ as names) ->
            let ids = List.map (fun n ->
              match n.Syntax.datum with
              | Syntax.Symbol name -> name
              | _ -> compile_error n.loc "only: expected identifier"
            ) names in
            Import_only (parse_import_set inner, ids)
          | _ -> compile_error s.loc "only: expected import set and identifiers")
       | Syntax.Symbol "except" ->
         (match rest with
          | inner :: (_ :: _ as names) ->
            let ids = List.map (fun n ->
              match n.Syntax.datum with
              | Syntax.Symbol name -> name
              | _ -> compile_error n.loc "except: expected identifier"
            ) names in
            Import_except (parse_import_set inner, ids)
          | _ -> compile_error s.loc "except: expected import set and identifiers")
       | Syntax.Symbol "prefix" ->
         (match rest with
          | [ inner; { datum = Syntax.Symbol prefix; _ } ] ->
            Import_prefix (parse_import_set inner, prefix)
          | _ -> compile_error s.loc "prefix: expected import set and prefix")
       | Syntax.Symbol "rename" ->
         (match rest with
          | inner :: (_ :: _ as pairs) ->
            let renames = List.map (fun p ->
              match syntax_to_proper_list p with
              | Some [ { datum = Syntax.Symbol from; _ };
                       { datum = Syntax.Symbol to_; _ } ] ->
                (from, to_)
              | _ -> compile_error p.loc "rename: expected (old new)"
            ) pairs in
            Import_rename (parse_import_set inner, renames)
          | _ -> compile_error s.loc "rename: expected import set and pairs")
       | _ ->
         (* Must be a library name *)
         Import_lib (parse_library_name s))
  | _ -> compile_error s.loc "import set: expected list"

(* --- Resolution --- *)

let resolve_import lookup_fn iset =
  let rec resolve = function
    | Import_lib name ->
      (match lookup_fn name with
       | None -> failwith ("unknown library: " ^ name_to_string name)
       | Some lib ->
         let rt = Hashtbl.fold (fun ext (id, slot) acc ->
           (ext, id, slot) :: acc
         ) lib.exports [] in
         let syn = Hashtbl.fold (fun ext b acc ->
           (ext, b) :: acc
         ) lib.syntax_exports [] in
         (rt, syn))
    | Import_only (inner, names) ->
      let (rt, syn) = resolve inner in
      let rt' = List.filter (fun (name, _, _) -> List.mem name names) rt in
      let syn' = List.filter (fun (name, _) -> List.mem name names) syn in
      (* Check that all requested names exist *)
      List.iter (fun name ->
        let in_rt = List.exists (fun (n, _, _) -> n = name) rt in
        let in_syn = List.exists (fun (n, _) -> n = name) syn in
        if not in_rt && not in_syn then
          failwith ("only: name not in export set: " ^ name)
      ) names;
      (rt', syn')
    | Import_except (inner, names) ->
      let (rt, syn) = resolve inner in
      (* Check that all excepted names exist *)
      List.iter (fun name ->
        let in_rt = List.exists (fun (n, _, _) -> n = name) rt in
        let in_syn = List.exists (fun (n, _) -> n = name) syn in
        if not in_rt && not in_syn then
          failwith ("except: name not in export set: " ^ name)
      ) names;
      let rt' = List.filter (fun (name, _, _) -> not (List.mem name names)) rt in
      let syn' = List.filter (fun (name, _) -> not (List.mem name names)) syn in
      (rt', syn')
    | Import_prefix (inner, prefix) ->
      let (rt, syn) = resolve inner in
      let rt' = List.map (fun (name, id, slot) -> (prefix ^ name, id, slot)) rt in
      let syn' = List.map (fun (name, b) -> (prefix ^ name, b)) syn in
      (rt', syn')
    | Import_rename (inner, renames) ->
      let (rt, syn) = resolve inner in
      (* Check that all "from" names exist in the export set *)
      List.iter (fun (from_name, _) ->
        let in_rt = List.exists (fun (n, _, _) -> n = from_name) rt in
        let in_syn = List.exists (fun (n, _) -> n = from_name) syn in
        if not in_rt && not in_syn then
          failwith ("rename: name not in export set: " ^ from_name)
      ) renames;
      (* Check for duplicate "from" names *)
      let from_names = List.map fst renames in
      List.iter (fun name ->
        if List.length (List.filter ((=) name) from_names) > 1 then
          failwith ("rename: duplicate source name: " ^ name)
      ) from_names;
      (* Check for "to" name collisions *)
      let to_names = List.map snd renames in
      List.iter (fun name ->
        if List.length (List.filter ((=) name) to_names) > 1 then
          failwith ("rename: duplicate target name: " ^ name)
      ) to_names;
      let rename_name name =
        match List.assoc_opt name renames with
        | Some new_name -> new_name
        | None -> name
      in
      let rt' = List.map (fun (name, id, slot) -> (rename_name name, id, slot)) rt in
      let syn' = List.map (fun (name, b) -> (rename_name name, b)) syn in
      (* Check for collisions between renamed and unrenamed exports *)
      let all_names =
        List.map (fun (n, _, _) -> n) rt' @
        List.map (fun (n, _) -> n) syn' in
      let seen = Hashtbl.create (List.length all_names) in
      List.iter (fun name ->
        if Hashtbl.mem seen name then
          failwith ("rename: produces duplicate export name: " ^ name);
        Hashtbl.replace seen name ()
      ) all_names;
      (rt', syn')
  in
  resolve iset
