module Char_map = Map.Make (Char)

type reader_macro = char -> Datum.t
type dispatch_macro = char -> Datum.t

type entry = { char_type : Char_type.t; macro : reader_macro option }

type t = {
  table : entry Char_map.t;
  dispatch_table : dispatch_macro Char_map.t;
  fold_case : bool;
}

let empty =
  { table = Char_map.empty;
    dispatch_table = Char_map.empty;
    fold_case = false;
  }

let char_type_of t c =
  match Char_map.find_opt c t.table with
  | Some e -> e.char_type
  | None -> Char_type.Constituent

let macro_of t c =
  match Char_map.find_opt c t.table with
  | Some { macro = Some f; _ } -> Some f
  | _ -> None

let dispatch_macro_of t c =
  Char_map.find_opt c t.dispatch_table

let fold_case t = t.fold_case

let set_char_type c ct t =
  let entry =
    match Char_map.find_opt c t.table with
    | Some e -> { e with char_type = ct }
    | None -> { char_type = ct; macro = None }
  in
  { t with table = Char_map.add c entry t.table }

let set_macro c ct macro t =
  let entry = { char_type = ct; macro = Some macro } in
  { t with table = Char_map.add c entry t.table }

let set_dispatch_macro c macro t =
  { t with dispatch_table = Char_map.add c macro t.dispatch_table }

let with_fold_case fc t =
  { t with fold_case = fc }

let is_whitespace t c =
  Char_type.equal (char_type_of t c) Char_type.Whitespace

let is_delimiter t c =
  match char_type_of t c with
  | Char_type.Whitespace | Char_type.Terminating_macro
  | Char_type.Multiple_escape -> true
  | _ -> false

let stub_macro _c = Datum.Nil
let stub_dispatch _c = Datum.Nil

let default =
  let add_ws c rt = set_char_type c Char_type.Whitespace rt in
  let add_tm c rt = set_macro c Char_type.Terminating_macro stub_macro rt in
  let add_ntm c rt = set_macro c Char_type.Non_terminating_macro stub_macro rt in
  let add_me c rt = set_char_type c Char_type.Multiple_escape rt in
  let add_disp c rt = set_dispatch_macro c stub_dispatch rt in
  empty
  (* Whitespace *)
  |> add_ws ' ' |> add_ws '\t' |> add_ws '\n' |> add_ws '\r'
  (* Terminating macros *)
  |> add_tm '(' |> add_tm ')' |> add_tm '"' |> add_tm ';'
  |> add_tm '\'' |> add_tm '`' |> add_tm ','
  (* Non-terminating macro *)
  |> add_ntm '#'
  (* Multiple escape *)
  |> add_me '|'
  (* Dispatch sub-table for # *)
  |> add_disp 't' |> add_disp 'T'
  |> add_disp 'f' |> add_disp 'F'
  |> add_disp '\\' |> add_disp '('
  |> add_disp 'u' |> add_disp 'U'
  |> add_disp ';'
  |> add_disp '|'
  |> add_disp '!'
  |> add_disp 'b' |> add_disp 'B'
  |> add_disp 'o' |> add_disp 'O'
  |> add_disp 'd' |> add_disp 'D'
  |> add_disp 'x' |> add_disp 'X'
  |> add_disp 'e' |> add_disp 'E'
  |> add_disp 'i' |> add_disp 'I'
