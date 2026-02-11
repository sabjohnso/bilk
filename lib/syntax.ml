type t = { loc : Loc.t; datum : datum }
and datum =
  | Bool of bool
  | Fixnum of int
  | Rational of int * int
  | Flonum of float
  | Complex of t * t
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
  | Rational (n, d) -> Datum.Rational (n, d)
  | Flonum f -> Datum.Flonum f
  | Complex (re, im) -> Datum.Complex (to_datum re, to_datum im)
  | Char c -> Datum.Char c
  | Str s -> Datum.Str (Bytes.of_string s)
  | Symbol s -> Datum.Symbol s
  | Pair (car, cdr) -> Datum.Pair { car = to_datum car; cdr = to_datum cdr }
  | Vector elts -> Datum.Vector (Array.map to_datum elts)
  | Bytevector bv -> Datum.Bytevector (Bytes.copy bv)
  | Nil -> Datum.Nil
  | Eof -> Datum.Eof

let rec from_datum loc (d : Datum.t) =
  let datum = match d with
    | Datum.Bool b -> Bool b
    | Datum.Fixnum n -> Fixnum n
    | Datum.Rational (n, d) -> Rational (n, d)
    | Datum.Flonum f -> Flonum f
    | Datum.Complex (re, im) -> Complex (from_datum loc re, from_datum loc im)
    | Datum.Char c -> Char c
    | Datum.Str s -> Str (Bytes.to_string s)
    | Datum.Symbol s -> Symbol s
    | Datum.Pair { car; cdr } -> Pair (from_datum loc car, from_datum loc cdr)
    | Datum.Vector elts -> Vector (Array.map (from_datum loc) elts)
    | Datum.Bytevector bv -> Bytevector (Bytes.copy bv)
    | Datum.Nil -> Nil
    | Datum.Eof -> Eof
    | Datum.Void -> Symbol "#<void>"
    | Datum.Primitive p -> Symbol (Printf.sprintf "#<primitive %s>" p.prim_name)
    | Datum.Closure c -> Symbol (Printf.sprintf "#<closure %s>" c.clos_name)
    | Datum.Continuation _ -> Symbol "#<continuation>"
    | Datum.Values _ -> Symbol "#<values>"
    | Datum.Error_object e -> Symbol (Printf.sprintf "#<error \"%s\">" e.err_message)
    | Datum.Port _ -> Symbol "#<port>"
    | Datum.Promise _ -> Symbol "#<promise>"
    | Datum.Hash_table _ -> Symbol "#<hash-table>"
    | Datum.Char_set _ -> Symbol "#<char-set>"
    | Datum.Regexp _ -> Symbol "#<regexp>"
  in
  { loc; datum }

let rec equal_datum a b =
  match (a.datum, b.datum) with
  | Bool x, Bool y -> x = y
  | Fixnum x, Fixnum y -> x = y
  | Rational (n1, d1), Rational (n2, d2) -> n1 = n2 && d1 = d2
  | Flonum x, Flonum y -> Float.equal x y
  | Complex (r1, i1), Complex (r2, i2) -> equal_datum r1 r2 && equal_datum i1 i2
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
