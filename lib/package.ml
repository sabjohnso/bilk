type dependency = {
  dep_name : string;
  dep_constraints : Semver.constraint_set;
}

type t = {
  name : string;
  version : Semver.t;
  description : string;
  license : string;
  depends : dependency list;
  libraries : Library.library_name list;
}

exception Package_error of string

let error msg = raise (Package_error msg)

(* --- Syntax helpers --- *)

let rec syntax_to_proper_list s =
  match s.Syntax.datum with
  | Syntax.Nil -> Some []
  | Syntax.Pair (car, cdr) ->
    (match syntax_to_proper_list cdr with
     | Some rest -> Some (car :: rest)
     | None -> None)
  | _ -> None

let expect_symbol s =
  match s.Syntax.datum with
  | Syntax.Symbol name -> name
  | _ -> error "expected identifier"

let expect_string s =
  match s.Syntax.datum with
  | Syntax.Str v -> v
  | _ -> error "expected string"

(* --- Dependency parsing --- *)

let parse_constraint_pair s =
  match syntax_to_proper_list s with
  | Some [{ datum = Syntax.Symbol op_str; _ }; { datum = Syntax.Str ver_str; _ }] ->
    (try
       let op = Semver.parse_constraint op_str in
       let ver = Semver.parse ver_str in
       (op, ver)
     with
     | Semver.Parse_error msg -> error msg)
  | _ -> error "dependency constraint: expected (op \"version\")"

let parse_dependency s =
  match syntax_to_proper_list s with
  | Some [] -> error "dependency: empty"
  | Some (name_syn :: constraints) ->
    let dep_name = expect_symbol name_syn in
    let dep_constraints = List.map parse_constraint_pair constraints in
    { dep_name; dep_constraints }
  | None ->
    (* bare symbol = dep with no constraints *)
    (match s.Syntax.datum with
     | Syntax.Symbol name -> { dep_name = name; dep_constraints = [] }
     | _ -> error "dependency: expected package name")

(* --- Library name parsing --- *)

let parse_lib_name s =
  match syntax_to_proper_list s with
  | None -> error "library name: expected a proper list"
  | Some parts ->
    if parts = [] then error "library name: empty";
    List.map (fun p ->
      match p.Syntax.datum with
      | Syntax.Symbol name -> name
      | Syntax.Fixnum n -> string_of_int n
      | Syntax.Bignum z -> Z.to_string z
      | _ -> error "library name: expected identifier or integer"
    ) parts

(* --- Top-level parsing --- *)

let parse readtable path =
  let port = Port.of_file path in
  let expr =
    try Reader.read_syntax readtable port
    with Reader.Read_error (_, msg) -> error (Printf.sprintf "%s: %s" path msg)
  in
  match syntax_to_proper_list expr with
  | Some ({ datum = Syntax.Symbol "define-package"; _ } :: clauses) ->
    let name = ref None in
    let version = ref None in
    let description = ref None in
    let license = ref None in
    let depends = ref None in
    let libraries = ref None in
    List.iter (fun clause ->
      match syntax_to_proper_list clause with
      | Some ({ datum = Syntax.Symbol key; _ } :: vals) ->
        (match key with
         | "name" ->
           (match vals with
            | [v] -> name := Some (expect_symbol v)
            | _ -> error "name: expected a single identifier")
         | "version" ->
           (match vals with
            | [v] ->
              let s = expect_string v in
              (try version := Some (Semver.parse s)
               with Semver.Parse_error msg -> error msg)
            | _ -> error "version: expected a single string")
         | "description" ->
           (match vals with
            | [v] -> description := Some (expect_string v)
            | _ -> error "description: expected a single string")
         | "license" ->
           (match vals with
            | [v] -> license := Some (expect_string v)
            | _ -> error "license: expected a single string")
         | "depends" ->
           depends := Some (List.map parse_dependency vals)
         | "libraries" ->
           libraries := Some (List.map parse_lib_name vals)
         | _ -> error (Printf.sprintf "unknown clause: %s" key))
      | _ -> error "clause: expected (key value ...)"
    ) clauses;
    let get field_name opt =
      match !opt with
      | Some v -> v
      | None -> error (Printf.sprintf "missing required field: %s" field_name)
    in
    { name = get "name" name;
      version = get "version" version;
      description = get "description" description;
      license = get "license" license;
      depends = (match !depends with Some d -> d | None -> []);
      libraries = (match !libraries with Some l -> l | None -> []) }
  | _ -> error (Printf.sprintf "%s: expected (define-package ...)" path)

let find_package_file dir =
  let rec walk d =
    let candidate = Filename.concat d "package.scm" in
    if Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname d in
      if parent = d then None  (* reached root *)
      else walk parent
  in
  walk dir
