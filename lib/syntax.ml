type t = { loc : Loc.t; datum : datum }
and datum =
  | Bool of bool
  | Fixnum of int
  | Flonum of float
  | Char of Uchar.t
  | Str of string
  | Symbol of string
  | Pair of t * t
  | Vector of t array
  | Bytevector of bytes
  | Nil
  | Eof

let make loc datum = { loc; datum }

let rec to_datum t =
  match t.datum with
  | Bool b -> Datum.Bool b
  | Fixnum n -> Datum.Fixnum n
  | Flonum f -> Datum.Flonum f
  | Char c -> Datum.Char c
  | Str s -> Datum.Str (Bytes.of_string s)
  | Symbol s -> Datum.Symbol s
  | Pair (car, cdr) -> Datum.Pair { car = to_datum car; cdr = to_datum cdr }
  | Vector elts -> Datum.Vector (Array.map to_datum elts)
  | Bytevector bv -> Datum.Bytevector bv
  | Nil -> Datum.Nil
  | Eof -> Datum.Eof

let rec equal_datum a b =
  match (a.datum, b.datum) with
  | Bool x, Bool y -> x = y
  | Fixnum x, Fixnum y -> x = y
  | Flonum x, Flonum y -> Float.equal x y
  | Char x, Char y -> Uchar.equal x y
  | Str x, Str y -> String.equal x y
  | Symbol x, Symbol y -> String.equal x y
  | Pair (a1, a2), Pair (b1, b2) -> equal_datum a1 b1 && equal_datum a2 b2
  | Vector xs, Vector ys ->
    Array.length xs = Array.length ys
    && Array.for_all2 equal_datum xs ys
  | Bytevector x, Bytevector y -> Bytes.equal x y
  | Nil, Nil -> true
  | Eof, Eof -> true
  | _ -> false

let equal a b =
  Loc.equal a.loc b.loc && equal_datum a b

let pp fmt t = Datum.pp fmt (to_datum t)

let to_string t =
  Format.asprintf "%a" pp t
