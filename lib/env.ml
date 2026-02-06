exception Unbound_variable of Symbol.t

type frame = (int, Datum.t ref) Hashtbl.t

type t = frame list

let empty () = [ Hashtbl.create 16 ]

let extend env bindings =
  let frame = Hashtbl.create (max 16 (List.length bindings)) in
  List.iter (fun (sym, value) ->
    Hashtbl.replace frame (Symbol.id sym) (ref value)
  ) bindings;
  frame :: env

let lookup env sym =
  let id = Symbol.id sym in
  let rec search = function
    | [] -> None
    | frame :: rest ->
      match Hashtbl.find_opt frame id with
      | Some slot -> Some !slot
      | None -> search rest
  in
  search env

let set env sym value =
  let id = Symbol.id sym in
  let rec search = function
    | [] -> raise (Unbound_variable sym)
    | frame :: rest ->
      match Hashtbl.find_opt frame id with
      | Some slot -> slot := value
      | None -> search rest
  in
  search env

let define env sym value =
  match env with
  | [] -> failwith "Env.define: empty environment (impossible)"
  | frame :: _ ->
    match Hashtbl.find_opt frame (Symbol.id sym) with
    | Some slot -> slot := value
    | None -> Hashtbl.replace frame (Symbol.id sym) (ref value)
