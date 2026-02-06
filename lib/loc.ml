type t = { file : string; line : int; col : int }

let make file line col = { file; line; col }

let none = { file = "<unknown>"; line = 0; col = 0 }

let equal a b =
  String.equal a.file b.file && a.line = b.line && a.col = b.col

let pp fmt t =
  Format.fprintf fmt "%s:%d:%d" t.file t.line t.col

let to_string t =
  Format.asprintf "%a" pp t
