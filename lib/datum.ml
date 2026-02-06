type t =
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

let rec equal a b =
  match (a, b) with
  | Bool x, Bool y -> x = y
  | Fixnum x, Fixnum y -> x = y
  | Flonum x, Flonum y -> Float.equal x y
  | Char x, Char y -> Uchar.equal x y
  | Str x, Str y -> String.equal x y
  | Symbol x, Symbol y -> String.equal x y
  | Pair (a1, a2), Pair (b1, b2) -> equal a1 b1 && equal a2 b2
  | Vector xs, Vector ys ->
    Array.length xs = Array.length ys
    && Array.for_all2 equal xs ys
  | Bytevector x, Bytevector y -> Bytes.equal x y
  | Nil, Nil -> true
  | Eof, Eof -> true
  | _ -> false

let rec pp fmt = function
  | Bool true -> Format.fprintf fmt "#t"
  | Bool false -> Format.fprintf fmt "#f"
  | Fixnum n -> Format.fprintf fmt "%d" n
  | Flonum f ->
    if Float.is_nan f then Format.fprintf fmt "+nan.0"
    else if Float.is_infinite f then
      if f > 0.0 then Format.fprintf fmt "+inf.0"
      else Format.fprintf fmt "-inf.0"
    else
      let s = string_of_float f in
      Format.fprintf fmt "%s" s
  | Char c -> Format.fprintf fmt "#\\x%04X" (Uchar.to_int c)
  | Str s -> Format.fprintf fmt "%S" s
  | Symbol s -> Format.fprintf fmt "%s" s
  | Pair (car, cdr) ->
    Format.fprintf fmt "(";
    pp fmt car;
    pp_tail fmt cdr;
    Format.fprintf fmt ")"
  | Vector elts ->
    Format.fprintf fmt "#(";
    Array.iteri
      (fun i e ->
        if i > 0 then Format.fprintf fmt " ";
        pp fmt e)
      elts;
    Format.fprintf fmt ")"
  | Bytevector bv ->
    Format.fprintf fmt "#u8(";
    Bytes.iteri
      (fun i b ->
        if i > 0 then Format.fprintf fmt " ";
        Format.fprintf fmt "%d" (Char.code b))
      bv;
    Format.fprintf fmt ")"
  | Nil -> Format.fprintf fmt "()"
  | Eof -> Format.fprintf fmt "#<eof>"

and pp_tail fmt = function
  | Nil -> ()
  | Pair (car, cdr) ->
    Format.fprintf fmt " ";
    pp fmt car;
    pp_tail fmt cdr
  | other ->
    Format.fprintf fmt " . ";
    pp fmt other

let to_string d =
  Format.asprintf "%a" pp d
