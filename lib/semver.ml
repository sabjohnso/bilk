type t = { major : int; minor : int; patch : int }

type constraint_op = Eq | Lt | Le | Gt | Ge

type constraint_ = constraint_op * t

type constraint_set = constraint_ list

exception Parse_error of string

let parse s =
  match String.split_on_char '.' s with
  | [maj; min; pat] ->
    (try
       let major = int_of_string maj in
       let minor = int_of_string min in
       let patch = int_of_string pat in
       if major < 0 || minor < 0 || patch < 0 then
         raise (Parse_error (Printf.sprintf "negative component in version: %s" s));
       { major; minor; patch }
     with Failure _ ->
       raise (Parse_error (Printf.sprintf "invalid version: %s" s)))
  | _ ->
    raise (Parse_error (Printf.sprintf "invalid version format: %s" s))

let parse_constraint s =
  match s with
  | "=" -> Eq
  | "<" -> Lt
  | "<=" -> Le
  | ">" -> Gt
  | ">=" -> Ge
  | _ -> raise (Parse_error (Printf.sprintf "invalid constraint operator: %s" s))

let to_string v =
  Printf.sprintf "%d.%d.%d" v.major v.minor v.patch

let compare a b =
  let c = Int.compare a.major b.major in
  if c <> 0 then c
  else
    let c = Int.compare a.minor b.minor in
    if c <> 0 then c
    else Int.compare a.patch b.patch

let equal a b = compare a b = 0

let satisfies_one v (op, target) =
  let c = compare v target in
  match op with
  | Eq -> c = 0
  | Lt -> c < 0
  | Le -> c <= 0
  | Gt -> c > 0
  | Ge -> c >= 0

let satisfies v cs =
  List.for_all (satisfies_one v) cs

let latest_satisfying versions cs =
  let matching = List.filter (fun v -> satisfies v cs) versions in
  match matching with
  | [] -> None
  | _ ->
    Some (List.fold_left (fun best v ->
      if compare v best > 0 then v else best
    ) (List.hd matching) matching)
