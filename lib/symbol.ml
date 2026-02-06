type t = {
  id : int;
  name : string;
}

type table = {
  tbl : (string, t) Hashtbl.t;
  mutable next_id : int;
}

let create_table () = { tbl = Hashtbl.create 64; next_id = 0 }

let intern table name =
  match Hashtbl.find_opt table.tbl name with
  | Some sym -> sym
  | None ->
    let sym = { id = table.next_id; name } in
    table.next_id <- table.next_id + 1;
    Hashtbl.replace table.tbl name sym;
    sym

let name sym = sym.name
let id sym = sym.id
let equal a b = Int.equal a.id b.id
let compare a b = Int.compare a.id b.id
let hash sym = sym.id
let pp fmt sym = Format.pp_print_string fmt sym.name
let to_string sym = sym.name
