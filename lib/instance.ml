type t = {
  symbols : Symbol.table;
  global_env : Env.t;
  readtable : Readtable.t;
  winds : Datum.wind list ref;
  handlers : Datum.t list ref;
}

(* --- Primitive helpers --- *)

let runtime_error msg = raise (Vm.Runtime_error msg)

let as_fixnum name = function
  | Datum.Fixnum n -> n
  | v -> runtime_error (Printf.sprintf "%s: expected integer, got %s" name (Datum.to_string v))

let as_flonum name = function
  | Datum.Flonum f -> f
  | Datum.Fixnum n -> Float.of_int n
  | v -> runtime_error (Printf.sprintf "%s: expected number, got %s" name (Datum.to_string v))

let is_numeric = function
  | Datum.Fixnum _ | Datum.Flonum _ -> true
  | _ -> false

let has_flonum args =
  List.exists (function Datum.Flonum _ -> true | _ -> false) args

let make_prim name fn : Datum.t =
  Datum.Primitive { prim_name = name; prim_fn = fn; prim_intrinsic = None }

let chain_compare name cmp args =
  match args with
  | [] | [_] -> runtime_error (Printf.sprintf "%s: expected at least 2 arguments" name)
  | _ ->
    let rec go = function
      | [] | [_] -> Datum.Bool true
      | a :: ((b :: _) as rest) ->
        if is_numeric a && is_numeric b then
          if cmp (as_flonum name a) (as_flonum name b) then go rest
          else Datum.Bool false
        else runtime_error (Printf.sprintf "%s: expected numeric arguments" name)
    in
    go args

(* --- Arithmetic primitives --- *)

let prim_add args =
  match args with
  | [] -> Datum.Fixnum 0
  | _ ->
  if has_flonum args then
    Datum.Flonum (List.fold_left (fun acc x -> acc +. as_flonum "+" x) 0.0 args)
  else
    Datum.Fixnum (List.fold_left (fun acc x -> acc + as_fixnum "+" x) 0 args)

let prim_sub args =
  match args with
  | [] -> runtime_error "-: expected at least 1 argument"
  | [x] ->
    if has_flonum [x] then Datum.Flonum (-. (as_flonum "-" x))
    else Datum.Fixnum (- (as_fixnum "-" x))
  | first :: rest ->
    if has_flonum args then
      Datum.Flonum (List.fold_left (fun acc x -> acc -. as_flonum "-" x) (as_flonum "-" first) rest)
    else
      Datum.Fixnum (List.fold_left (fun acc x -> acc - as_fixnum "-" x) (as_fixnum "-" first) rest)

let prim_mul args =
  match args with
  | [] -> Datum.Fixnum 1
  | _ ->
  if has_flonum args then
    Datum.Flonum (List.fold_left (fun acc x -> acc *. as_flonum "*" x) 1.0 args)
  else
    Datum.Fixnum (List.fold_left (fun acc x -> acc * as_fixnum "*" x) 1 args)

let prim_lt args = chain_compare "<" (<) args

let prim_num_eq args = chain_compare "=" Float.equal args

let prim_gt args = chain_compare ">" (>) args

(* --- List primitives --- *)

let prim_cons args =
  match args with
  | [a; b] -> Datum.Pair { car = a; cdr = b }
  | _ -> runtime_error (Printf.sprintf "cons: expected 2 arguments, got %d" (List.length args))

let prim_car args =
  match args with
  | [Datum.Pair { car = a; _ }] -> a
  | [v] -> runtime_error (Printf.sprintf "car: expected pair, got %s" (Datum.to_string v))
  | _ -> runtime_error (Printf.sprintf "car: expected 1 argument, got %d" (List.length args))

let prim_cdr args =
  match args with
  | [Datum.Pair { cdr = d; _ }] -> d
  | [v] -> runtime_error (Printf.sprintf "cdr: expected pair, got %s" (Datum.to_string v))
  | _ -> runtime_error (Printf.sprintf "cdr: expected 1 argument, got %d" (List.length args))

let prim_null args =
  match args with
  | [Datum.Nil] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "null?: expected 1 argument, got %d" (List.length args))

let prim_pair args =
  match args with
  | [Datum.Pair _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "pair?: expected 1 argument, got %d" (List.length args))

let prim_not args =
  match args with
  | [Datum.Bool false] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "not: expected 1 argument, got %d" (List.length args))

(* --- Equivalence primitives --- *)

let prim_eqv args =
  match args with
  | [a; b] ->
    let result = match (a, b) with
      | Datum.Bool x, Datum.Bool y -> x = y
      | Datum.Fixnum x, Datum.Fixnum y -> x = y
      | Datum.Flonum x, Datum.Flonum y -> Float.equal x y
      | Datum.Char x, Datum.Char y -> Uchar.equal x y
      | Datum.Symbol x, Datum.Symbol y -> String.equal x y
      | Datum.Nil, Datum.Nil -> true
      | _ -> false
    in
    Datum.Bool result
  | _ -> runtime_error (Printf.sprintf "eqv?: expected 2 arguments, got %d" (List.length args))

let prim_eq args = prim_eqv args

let prim_list args =
  List.fold_right (fun x acc -> Datum.Pair { car = x; cdr = acc }) args Datum.Nil

let prim_le args = chain_compare "<=" (<=) args

let prim_ge args = chain_compare ">=" (>=) args

(* --- I/O primitives --- *)

let prim_display args =
  match args with
  | [v] -> print_string (Datum.to_display_string v); Datum.Void
  | _ -> runtime_error (Printf.sprintf "display: expected 1 argument, got %d" (List.length args))

let prim_write args =
  match args with
  | [v] -> print_string (Datum.to_string v); Datum.Void
  | _ -> runtime_error (Printf.sprintf "write: expected 1 argument, got %d" (List.length args))

let prim_newline args =
  match args with
  | [] -> print_newline (); Datum.Void
  | _ -> runtime_error (Printf.sprintf "newline: expected 0 arguments, got %d" (List.length args))

(* --- Intrinsic dummy --- *)

let intrinsic_dummy _ =
  runtime_error "intrinsic: must be called via VM"

let make_intrinsic name id : Datum.t =
  Datum.Primitive { prim_name = name; prim_fn = intrinsic_dummy; prim_intrinsic = Some id }

(* --- values primitive --- *)

let prim_values args =
  match args with
  | [v] -> v
  | _ -> Datum.Values args

(* --- Numeric primitives --- *)

let prim_div args =
  match args with
  | [] -> runtime_error "/: expected at least 1 argument"
  | [x] ->
    let f = as_flonum "/" x in
    if f = 0.0 then runtime_error "/: division by zero"
    else Datum.Flonum (1.0 /. f)
  | first :: rest ->
    if has_flonum args then
      let result = List.fold_left (fun acc x ->
        let d = as_flonum "/" x in
        if d = 0.0 then runtime_error "/: division by zero";
        acc /. d) (as_flonum "/" first) rest in
      Datum.Flonum result
    else
      let n = as_fixnum "/" first in
      let (num, exact) = List.fold_left (fun (acc, exact) x ->
        let d = as_fixnum "/" x in
        if d = 0 then runtime_error "/: division by zero";
        if exact && acc mod d = 0 then (acc / d, true)
        else (acc, false)
      ) (n, true) rest in
      if exact then Datum.Fixnum num
      else
        let result = List.fold_left (fun acc x ->
          acc /. as_flonum "/" x) (as_flonum "/" first) rest in
        Datum.Flonum result

let prim_abs args =
  match args with
  | [Datum.Fixnum n] -> Datum.Fixnum (abs n)
  | [Datum.Flonum f] -> Datum.Flonum (Float.abs f)
  | [_] -> runtime_error "abs: expected number"
  | _ -> runtime_error (Printf.sprintf "abs: expected 1 argument, got %d" (List.length args))

let prim_min args =
  match args with
  | [] -> runtime_error "min: expected at least 1 argument"
  | first :: rest ->
    if has_flonum args then
      Datum.Flonum (List.fold_left (fun acc x -> Float.min acc (as_flonum "min" x))
        (as_flonum "min" first) rest)
    else
      Datum.Fixnum (List.fold_left (fun acc x -> min acc (as_fixnum "min" x))
        (as_fixnum "min" first) rest)

let prim_max args =
  match args with
  | [] -> runtime_error "max: expected at least 1 argument"
  | first :: rest ->
    if has_flonum args then
      Datum.Flonum (List.fold_left (fun acc x -> Float.max acc (as_flonum "max" x))
        (as_flonum "max" first) rest)
    else
      Datum.Fixnum (List.fold_left (fun acc x -> max acc (as_fixnum "max" x))
        (as_fixnum "max" first) rest)

let prim_quotient args =
  match args with
  | [a; b] ->
    let n = as_fixnum "quotient" a in
    let d = as_fixnum "quotient" b in
    if d = 0 then runtime_error "quotient: division by zero";
    Datum.Fixnum (Int.div n d)
  | _ -> runtime_error (Printf.sprintf "quotient: expected 2 arguments, got %d" (List.length args))

let prim_remainder args =
  match args with
  | [a; b] ->
    let n = as_fixnum "remainder" a in
    let d = as_fixnum "remainder" b in
    if d = 0 then runtime_error "remainder: division by zero";
    Datum.Fixnum (Int.rem n d)
  | _ -> runtime_error (Printf.sprintf "remainder: expected 2 arguments, got %d" (List.length args))

let prim_modulo args =
  match args with
  | [a; b] ->
    let n = as_fixnum "modulo" a in
    let d = as_fixnum "modulo" b in
    if d = 0 then runtime_error "modulo: division by zero";
    let r = Int.rem n d in
    if r = 0 then Datum.Fixnum 0
    else if (r > 0) = (d > 0) then Datum.Fixnum r
    else Datum.Fixnum (r + d)
  | _ -> runtime_error (Printf.sprintf "modulo: expected 2 arguments, got %d" (List.length args))

let prim_floor args =
  match args with
  | [Datum.Fixnum n] -> Datum.Fixnum n
  | [Datum.Flonum f] -> Datum.Flonum (floor f)
  | [_] -> runtime_error "floor: expected number"
  | _ -> runtime_error (Printf.sprintf "floor: expected 1 argument, got %d" (List.length args))

let prim_ceiling args =
  match args with
  | [Datum.Fixnum n] -> Datum.Fixnum n
  | [Datum.Flonum f] -> Datum.Flonum (ceil f)
  | [_] -> runtime_error "ceiling: expected number"
  | _ -> runtime_error (Printf.sprintf "ceiling: expected 1 argument, got %d" (List.length args))

let prim_truncate args =
  match args with
  | [Datum.Fixnum n] -> Datum.Fixnum n
  | [Datum.Flonum f] -> Datum.Flonum (Float.of_int (int_of_float f))
  | [_] -> runtime_error "truncate: expected number"
  | _ -> runtime_error (Printf.sprintf "truncate: expected 1 argument, got %d" (List.length args))

let prim_round args =
  match args with
  | [Datum.Fixnum n] -> Datum.Fixnum n
  | [Datum.Flonum f] ->
    (* R7RS: round to even (banker's rounding) *)
    let rounded =
      let fl = floor f in
      let frac = f -. fl in
      if frac = 0.5 then
        let ifl = int_of_float fl in
        if ifl mod 2 = 0 then fl else fl +. 1.0
      else if frac = -0.5 then
        let cl = ceil f in
        let icl = int_of_float cl in
        if icl mod 2 = 0 then cl else cl -. 1.0
      else Float.round f
    in
    Datum.Flonum rounded
  | [_] -> runtime_error "round: expected number"
  | _ -> runtime_error (Printf.sprintf "round: expected 1 argument, got %d" (List.length args))

let prim_floor_quotient args =
  match args with
  | [a; b] ->
    let fa = as_flonum "floor-quotient" a in
    let fb = as_flonum "floor-quotient" b in
    if fb = 0.0 then runtime_error "floor-quotient: division by zero";
    let q = Float.of_int (int_of_float (floor (fa /. fb))) in
    if not (has_flonum [a; b]) then Datum.Fixnum (Float.to_int q)
    else Datum.Flonum q
  | _ -> runtime_error (Printf.sprintf "floor-quotient: expected 2 arguments, got %d" (List.length args))

let prim_floor_remainder args =
  match args with
  | [a; b] ->
    let fa = as_flonum "floor-remainder" a in
    let fb = as_flonum "floor-remainder" b in
    if fb = 0.0 then runtime_error "floor-remainder: division by zero";
    let r = fa -. fb *. floor (fa /. fb) in
    if not (has_flonum [a; b]) then Datum.Fixnum (Float.to_int r)
    else Datum.Flonum r
  | _ -> runtime_error (Printf.sprintf "floor-remainder: expected 2 arguments, got %d" (List.length args))

let prim_truncate_quotient args =
  match args with
  | [a; b] ->
    let fa = as_flonum "truncate-quotient" a in
    let fb = as_flonum "truncate-quotient" b in
    if fb = 0.0 then runtime_error "truncate-quotient: division by zero";
    let q = Float.of_int (int_of_float (fa /. fb)) in
    if not (has_flonum [a; b]) then Datum.Fixnum (Float.to_int q)
    else Datum.Flonum q
  | _ -> runtime_error (Printf.sprintf "truncate-quotient: expected 2 arguments, got %d" (List.length args))

let prim_truncate_remainder args =
  match args with
  | [a; b] ->
    let fa = as_flonum "truncate-remainder" a in
    let fb = as_flonum "truncate-remainder" b in
    if fb = 0.0 then runtime_error "truncate-remainder: division by zero";
    let q = Float.of_int (int_of_float (fa /. fb)) in
    let r = fa -. fb *. q in
    if not (has_flonum [a; b]) then Datum.Fixnum (Float.to_int r)
    else Datum.Flonum r
  | _ -> runtime_error (Printf.sprintf "truncate-remainder: expected 2 arguments, got %d" (List.length args))

let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

let prim_gcd args =
  match args with
  | [] -> Datum.Fixnum 0
  | _ ->
    let ns = List.map (as_fixnum "gcd") args in
    Datum.Fixnum (List.fold_left gcd 0 ns)

let prim_lcm args =
  match args with
  | [] -> Datum.Fixnum 1
  | _ ->
    let ns = List.map (as_fixnum "lcm") args in
    let lcm a b = if a = 0 || b = 0 then 0 else abs (a * b / gcd a b) in
    Datum.Fixnum (List.fold_left lcm 1 ns)

let prim_exact_to_inexact args =
  match args with
  | [Datum.Fixnum n] -> Datum.Flonum (float_of_int n)
  | [Datum.Flonum f] -> Datum.Flonum f
  | [_] -> runtime_error "inexact: expected number"
  | _ -> runtime_error (Printf.sprintf "inexact: expected 1 argument, got %d" (List.length args))

let prim_inexact_to_exact args =
  match args with
  | [Datum.Fixnum n] -> Datum.Fixnum n
  | [Datum.Flonum f] ->
    if Float.is_integer f then Datum.Fixnum (Float.to_int f)
    else runtime_error (Printf.sprintf "exact: cannot convert %s to exact" (string_of_float f))
  | [_] -> runtime_error "exact: expected number"
  | _ -> runtime_error (Printf.sprintf "exact: expected 1 argument, got %d" (List.length args))

let prim_expt args =
  match args with
  | [a; b] ->
    if has_flonum [a; b] then
      Datum.Flonum (Float.pow (as_flonum "expt" a) (as_flonum "expt" b))
    else
      let base = as_fixnum "expt" a in
      let exp = as_fixnum "expt" b in
      if exp < 0 then
        Datum.Flonum (Float.pow (float_of_int base) (float_of_int exp))
      else
        let rec pow b e acc =
          if e = 0 then acc
          else if e mod 2 = 0 then pow (b * b) (e / 2) acc
          else pow b (e - 1) (acc * b)
        in
        Datum.Fixnum (pow base exp 1)
  | _ -> runtime_error (Printf.sprintf "expt: expected 2 arguments, got %d" (List.length args))

let prim_sqrt args =
  match args with
  | [v] ->
    let f = as_flonum "sqrt" v in
    let r = Float.sqrt f in
    (match v with
     | Datum.Fixnum _ when Float.is_integer r && r *. r = f ->
       Datum.Fixnum (Float.to_int r)
     | _ -> Datum.Flonum r)
  | _ -> runtime_error (Printf.sprintf "sqrt: expected 1 argument, got %d" (List.length args))

let prim_exact_integer_sqrt args =
  match args with
  | [Datum.Fixnum n] when n >= 0 ->
    let s = Float.to_int (Float.sqrt (float_of_int n)) in
    let s = if (s + 1) * (s + 1) <= n then s + 1 else s in
    Datum.Values [Datum.Fixnum s; Datum.Fixnum (n - s * s)]
  | [Datum.Fixnum _] -> runtime_error "exact-integer-sqrt: expected non-negative integer"
  | [_] -> runtime_error "exact-integer-sqrt: expected exact non-negative integer"
  | _ -> runtime_error (Printf.sprintf "exact-integer-sqrt: expected 1 argument, got %d" (List.length args))

let prim_number_to_string args =
  match args with
  | [v] ->
    let s = match v with
      | Datum.Fixnum n -> string_of_int n
      | Datum.Flonum f -> Datum.to_string v |> fun _ ->
        if Float.is_nan f then "+nan.0"
        else if Float.is_infinite f then (if f > 0.0 then "+inf.0" else "-inf.0")
        else string_of_float f
      | _ -> runtime_error "number->string: expected number"
    in
    Datum.Str (Bytes.of_string s)
  | [v; Datum.Fixnum radix] ->
    let n = as_fixnum "number->string" v in
    let s = match radix with
      | 2 ->
        if n = 0 then "0"
        else
          let buf = Buffer.create 32 in
          let neg = n < 0 in
          let n = abs n in
          let rec go n = if n > 0 then (go (n / 2); Buffer.add_char buf (if n mod 2 = 0 then '0' else '1')) in
          go n;
          if neg then "-" ^ Buffer.contents buf else Buffer.contents buf
      | 8 -> if n >= 0 then Printf.sprintf "%o" n else "-" ^ Printf.sprintf "%o" (abs n)
      | 10 -> string_of_int n
      | 16 -> if n >= 0 then Printf.sprintf "%x" n else "-" ^ Printf.sprintf "%x" (abs n)
      | _ -> runtime_error "number->string: radix must be 2, 8, 10, or 16"
    in
    Datum.Str (Bytes.of_string s)
  | _ -> runtime_error "number->string: expected 1 or 2 arguments"

let prim_string_to_number args =
  match args with
  | [Datum.Str s] ->
    let str = Bytes.to_string s in
    (try Datum.Fixnum (int_of_string str)
     with _ ->
       try Datum.Flonum (float_of_string str)
       with _ -> Datum.Bool false)
  | [Datum.Str s; Datum.Fixnum radix] ->
    let str = Bytes.to_string s in
    let prefix = match radix with
      | 2 -> "0b" | 8 -> "0o" | 10 -> "" | 16 -> "0x"
      | _ -> runtime_error "string->number: radix must be 2, 8, 10, or 16"
    in
    (try Datum.Fixnum (int_of_string (prefix ^ str))
     with _ -> Datum.Bool false)
  | _ -> runtime_error "string->number: expected 1 or 2 arguments"

(* --- Deep structural equal? --- *)

let rec scheme_equal a b =
  match (a, b) with
  | Datum.Bool x, Datum.Bool y -> x = y
  | Datum.Fixnum x, Datum.Fixnum y -> x = y
  | Datum.Flonum x, Datum.Flonum y -> Float.equal x y
  | Datum.Char x, Datum.Char y -> Uchar.equal x y
  | Datum.Str x, Datum.Str y -> Bytes.equal x y
  | Datum.Symbol x, Datum.Symbol y -> String.equal x y
  | Datum.Pair { car = a1; cdr = a2 }, Datum.Pair { car = b1; cdr = b2 } ->
    scheme_equal a1 b1 && scheme_equal a2 b2
  | Datum.Vector xs, Datum.Vector ys ->
    Array.length xs = Array.length ys
    && Array.for_all2 scheme_equal xs ys
  | Datum.Bytevector x, Datum.Bytevector y -> Bytes.equal x y
  | Datum.Nil, Datum.Nil -> true
  | Datum.Eof, Datum.Eof -> true
  | _ -> false

(* --- Pair & list primitives --- *)

let prim_set_car args =
  match args with
  | [Datum.Pair p; v] -> p.car <- v; Datum.Void
  | [x; _] -> runtime_error (Printf.sprintf "set-car!: expected pair, got %s" (Datum.to_string x))
  | _ -> runtime_error (Printf.sprintf "set-car!: expected 2 arguments, got %d" (List.length args))

let prim_set_cdr args =
  match args with
  | [Datum.Pair p; v] -> p.cdr <- v; Datum.Void
  | [x; _] -> runtime_error (Printf.sprintf "set-cdr!: expected pair, got %s" (Datum.to_string x))
  | _ -> runtime_error (Printf.sprintf "set-cdr!: expected 2 arguments, got %d" (List.length args))

let prim_caar args =
  match args with
  | [Datum.Pair { car = Datum.Pair { car = a; _ }; _ }] -> a
  | [_] -> runtime_error "caar: expected pair of pair"
  | _ -> runtime_error (Printf.sprintf "caar: expected 1 argument, got %d" (List.length args))

let prim_cadr args =
  match args with
  | [Datum.Pair { cdr = Datum.Pair { car = a; _ }; _ }] -> a
  | [_] -> runtime_error "cadr: expected pair"
  | _ -> runtime_error (Printf.sprintf "cadr: expected 1 argument, got %d" (List.length args))

let prim_cdar args =
  match args with
  | [Datum.Pair { car = Datum.Pair { cdr = d; _ }; _ }] -> d
  | [_] -> runtime_error "cdar: expected pair of pair"
  | _ -> runtime_error (Printf.sprintf "cdar: expected 1 argument, got %d" (List.length args))

let prim_cddr args =
  match args with
  | [Datum.Pair { cdr = Datum.Pair { cdr = d; _ }; _ }] -> d
  | [_] -> runtime_error "cddr: expected pair"
  | _ -> runtime_error (Printf.sprintf "cddr: expected 1 argument, got %d" (List.length args))

let prim_make_list args =
  match args with
  | [Datum.Fixnum n] ->
    let rec build k acc =
      if k <= 0 then acc
      else build (k - 1) (Datum.Pair { car = Datum.Fixnum 0; cdr = acc })
    in
    build n Datum.Nil
  | [Datum.Fixnum n; fill] ->
    let rec build k acc =
      if k <= 0 then acc
      else build (k - 1) (Datum.Pair { car = fill; cdr = acc })
    in
    build n Datum.Nil
  | _ -> runtime_error "make-list: expected (make-list k) or (make-list k fill)"

let prim_length args =
  match args with
  | [v] ->
    let rec len acc = function
      | Datum.Nil -> Datum.Fixnum acc
      | Datum.Pair { cdr; _ } -> len (acc + 1) cdr
      | _ -> runtime_error "length: expected proper list"
    in
    len 0 v
  | _ -> runtime_error (Printf.sprintf "length: expected 1 argument, got %d" (List.length args))

let prim_append args =
  match args with
  | [] -> Datum.Nil
  | _ ->
    let rec to_list = function
      | Datum.Nil -> []
      | Datum.Pair { car; cdr } -> car :: to_list cdr
      | _ -> runtime_error "append: expected proper list"
    in
    let rec go = function
      | [] -> Datum.Nil
      | [last] -> last
      | hd :: rest ->
        let elts = to_list hd in
        List.fold_right (fun x acc -> Datum.Pair { car = x; cdr = acc }) elts (go rest)
    in
    go args

let prim_reverse args =
  match args with
  | [v] ->
    let rec rev acc = function
      | Datum.Nil -> acc
      | Datum.Pair { car; cdr } -> rev (Datum.Pair { car; cdr = acc }) cdr
      | _ -> runtime_error "reverse: expected proper list"
    in
    rev Datum.Nil v
  | _ -> runtime_error (Printf.sprintf "reverse: expected 1 argument, got %d" (List.length args))

let prim_list_tail args =
  match args with
  | [v; Datum.Fixnum k] ->
    let rec drop n lst =
      if n <= 0 then lst
      else match lst with
        | Datum.Pair { cdr; _ } -> drop (n - 1) cdr
        | Datum.Nil when n > 0 -> runtime_error "list-tail: index out of range"
        | _ -> runtime_error "list-tail: expected proper list"
    in
    drop k v
  | _ -> runtime_error "list-tail: expected 2 arguments (list k)"

let prim_list_ref args =
  match args with
  | [v; Datum.Fixnum k] ->
    let rec nth n lst =
      match lst with
      | Datum.Pair { car; cdr } ->
        if n = 0 then car else nth (n - 1) cdr
      | _ -> runtime_error "list-ref: index out of range"
    in
    nth k v
  | _ -> runtime_error "list-ref: expected 2 arguments (list k)"

let prim_list_set args =
  match args with
  | [v; Datum.Fixnum k; obj] ->
    let rec nth n lst =
      match lst with
      | Datum.Pair p ->
        if n = 0 then (p.car <- obj; Datum.Void)
        else nth (n - 1) p.cdr
      | _ -> runtime_error "list-set!: index out of range"
    in
    nth k v
  | _ -> runtime_error "list-set!: expected 3 arguments (list k obj)"

let prim_list_copy args =
  match args with
  | [v] ->
    let rec copy = function
      | Datum.Nil -> Datum.Nil
      | Datum.Pair { car; cdr } -> Datum.Pair { car; cdr = copy cdr }
      | x -> x
    in
    copy v
  | _ -> runtime_error (Printf.sprintf "list-copy: expected 1 argument, got %d" (List.length args))

let prim_memq args =
  match args with
  | [obj; lst] ->
    let rec search = function
      | Datum.Nil -> Datum.Bool false
      | Datum.Pair ({ car; _ } as p) ->
        let eq = match (obj, car) with
          | Datum.Bool a, Datum.Bool b -> a = b
          | Datum.Fixnum a, Datum.Fixnum b -> a = b
          | Datum.Symbol a, Datum.Symbol b -> String.equal a b
          | Datum.Char a, Datum.Char b -> Uchar.equal a b
          | Datum.Nil, Datum.Nil -> true
          | _ -> false
        in
        if eq then Datum.Pair p else search p.cdr
      | _ -> runtime_error "memq: expected proper list"
    in
    search lst
  | _ -> runtime_error (Printf.sprintf "memq: expected 2 arguments, got %d" (List.length args))

let prim_memv args = prim_memq args

let prim_member args =
  match args with
  | [obj; lst] ->
    let rec search = function
      | Datum.Nil -> Datum.Bool false
      | Datum.Pair ({ car; _ } as p) ->
        if scheme_equal obj car then Datum.Pair p else search p.cdr
      | _ -> runtime_error "member: expected proper list"
    in
    search lst
  | _ -> runtime_error (Printf.sprintf "member: expected 2 arguments, got %d" (List.length args))

let prim_assq args =
  match args with
  | [obj; lst] ->
    let rec search = function
      | Datum.Nil -> Datum.Bool false
      | Datum.Pair { car = Datum.Pair ({ car = key; _ } as entry); cdr = rest } ->
        let eq = match (obj, key) with
          | Datum.Bool a, Datum.Bool b -> a = b
          | Datum.Fixnum a, Datum.Fixnum b -> a = b
          | Datum.Symbol a, Datum.Symbol b -> String.equal a b
          | Datum.Char a, Datum.Char b -> Uchar.equal a b
          | Datum.Nil, Datum.Nil -> true
          | _ -> false
        in
        if eq then Datum.Pair entry else search rest
      | _ -> runtime_error "assq: expected alist"
    in
    search lst
  | _ -> runtime_error (Printf.sprintf "assq: expected 2 arguments, got %d" (List.length args))

let prim_assv args = prim_assq args

let prim_assoc args =
  match args with
  | [obj; lst] ->
    let rec search = function
      | Datum.Nil -> Datum.Bool false
      | Datum.Pair { car = Datum.Pair ({ car = key; _ } as entry); cdr = rest } ->
        if scheme_equal obj key then Datum.Pair entry else search rest
      | _ -> runtime_error "assoc: expected alist"
    in
    search lst
  | _ -> runtime_error (Printf.sprintf "assoc: expected 2 arguments, got %d" (List.length args))

(* --- Character primitives --- *)

let as_char name = function
  | Datum.Char c -> c
  | v -> runtime_error (Printf.sprintf "%s: expected char, got %s" name (Datum.to_string v))

let char_chain_compare name cmp args =
  match args with
  | [] | [_] -> runtime_error (Printf.sprintf "%s: expected at least 2 arguments" name)
  | _ ->
    let rec go = function
      | [] | [_] -> Datum.Bool true
      | a :: ((b :: _) as rest) ->
        if cmp (Uchar.to_int (as_char name a)) (Uchar.to_int (as_char name b)) then go rest
        else Datum.Bool false
    in
    go args

let prim_char_eq args = char_chain_compare "char=?" (=) args
let prim_char_lt args = char_chain_compare "char<?" (<) args
let prim_char_gt args = char_chain_compare "char>?" (>) args
let prim_char_le args = char_chain_compare "char<=?" (<=) args
let prim_char_ge args = char_chain_compare "char>=?" (>=) args

let char_ci_chain_compare name cmp args =
  match args with
  | [] | [_] -> runtime_error (Printf.sprintf "%s: expected at least 2 arguments" name)
  | _ ->
    let fold c = Uchar.to_int (Uchar.of_char (Char.lowercase_ascii (Uchar.to_char c))) in
    let rec go = function
      | [] | [_] -> Datum.Bool true
      | a :: ((b :: _) as rest) ->
        if cmp (fold (as_char name a)) (fold (as_char name b)) then go rest
        else Datum.Bool false
    in
    go args

let prim_char_ci_eq args = char_ci_chain_compare "char-ci=?" (=) args
let prim_char_ci_lt args = char_ci_chain_compare "char-ci<?" (<) args
let prim_char_ci_gt args = char_ci_chain_compare "char-ci>?" (>) args
let prim_char_ci_le args = char_ci_chain_compare "char-ci<=?" (<=) args
let prim_char_ci_ge args = char_ci_chain_compare "char-ci>=?" (>=) args

let prim_char_to_integer args =
  match args with
  | [Datum.Char c] -> Datum.Fixnum (Uchar.to_int c)
  | [_] -> runtime_error "char->integer: expected char"
  | _ -> runtime_error (Printf.sprintf "char->integer: expected 1 argument, got %d" (List.length args))

let prim_integer_to_char args =
  match args with
  | [Datum.Fixnum n] -> Datum.Char (Uchar.of_int n)
  | [_] -> runtime_error "integer->char: expected integer"
  | _ -> runtime_error (Printf.sprintf "integer->char: expected 1 argument, got %d" (List.length args))

let prim_char_upcase args =
  match args with
  | [Datum.Char c] ->
    if Uchar.to_int c < 128 then
      Datum.Char (Uchar.of_char (Char.uppercase_ascii (Uchar.to_char c)))
    else Datum.Char c
  | [_] -> runtime_error "char-upcase: expected char"
  | _ -> runtime_error (Printf.sprintf "char-upcase: expected 1 argument, got %d" (List.length args))

let prim_char_downcase args =
  match args with
  | [Datum.Char c] ->
    if Uchar.to_int c < 128 then
      Datum.Char (Uchar.of_char (Char.lowercase_ascii (Uchar.to_char c)))
    else Datum.Char c
  | [_] -> runtime_error "char-downcase: expected char"
  | _ -> runtime_error (Printf.sprintf "char-downcase: expected 1 argument, got %d" (List.length args))

let prim_char_foldcase args = prim_char_downcase args

let prim_char_alphabetic args =
  match args with
  | [Datum.Char c] ->
    let n = Uchar.to_int c in
    Datum.Bool (n < 128 && Char.lowercase_ascii (Char.chr n) <> Char.uppercase_ascii (Char.chr n))
  | [_] -> runtime_error "char-alphabetic?: expected char"
  | _ -> runtime_error (Printf.sprintf "char-alphabetic?: expected 1 argument, got %d" (List.length args))

let prim_char_numeric args =
  match args with
  | [Datum.Char c] ->
    let n = Uchar.to_int c in
    Datum.Bool (n >= 0x30 && n <= 0x39)
  | [_] -> runtime_error "char-numeric?: expected char"
  | _ -> runtime_error (Printf.sprintf "char-numeric?: expected 1 argument, got %d" (List.length args))

let prim_char_whitespace args =
  match args with
  | [Datum.Char c] ->
    let n = Uchar.to_int c in
    Datum.Bool (n = 0x20 || n = 0x09 || n = 0x0A || n = 0x0D || n = 0x0C)
  | [_] -> runtime_error "char-whitespace?: expected char"
  | _ -> runtime_error (Printf.sprintf "char-whitespace?: expected 1 argument, got %d" (List.length args))

let prim_char_upper_case args =
  match args with
  | [Datum.Char c] ->
    let n = Uchar.to_int c in
    Datum.Bool (n >= 0x41 && n <= 0x5A)
  | [_] -> runtime_error "char-upper-case?: expected char"
  | _ -> runtime_error (Printf.sprintf "char-upper-case?: expected 1 argument, got %d" (List.length args))

let prim_char_lower_case args =
  match args with
  | [Datum.Char c] ->
    let n = Uchar.to_int c in
    Datum.Bool (n >= 0x61 && n <= 0x7A)
  | [_] -> runtime_error "char-lower-case?: expected char"
  | _ -> runtime_error (Printf.sprintf "char-lower-case?: expected 1 argument, got %d" (List.length args))

let prim_digit_value args =
  match args with
  | [Datum.Char c] ->
    let n = Uchar.to_int c in
    if n >= 0x30 && n <= 0x39 then Datum.Fixnum (n - 0x30)
    else Datum.Bool false
  | [_] -> runtime_error "digit-value: expected char"
  | _ -> runtime_error (Printf.sprintf "digit-value: expected 1 argument, got %d" (List.length args))

(* --- String primitives --- *)

let as_string name = function
  | Datum.Str s -> s
  | v -> runtime_error (Printf.sprintf "%s: expected string, got %s" name (Datum.to_string v))

let prim_make_string args =
  match args with
  | [Datum.Fixnum n] -> Datum.Str (Bytes.make n (Char.chr 0))
  | [Datum.Fixnum n; Datum.Char c] ->
    if Uchar.to_int c < 256 then
      Datum.Str (Bytes.make n (Char.chr (Uchar.to_int c)))
    else runtime_error "make-string: char out of byte range"
  | _ -> runtime_error "make-string: expected (make-string k) or (make-string k char)"

let prim_string args =
  let buf = Bytes.create (List.length args) in
  List.iteri (fun i c ->
    let uc = as_char "string" c in
    let n = Uchar.to_int uc in
    if n < 256 then Bytes.set buf i (Char.chr n)
    else runtime_error "string: char out of byte range"
  ) args;
  Datum.Str buf

let prim_string_length args =
  match args with
  | [Datum.Str s] -> Datum.Fixnum (Bytes.length s)
  | [_] -> runtime_error "string-length: expected string"
  | _ -> runtime_error (Printf.sprintf "string-length: expected 1 argument, got %d" (List.length args))

let prim_string_ref args =
  match args with
  | [Datum.Str s; Datum.Fixnum k] ->
    if k >= 0 && k < Bytes.length s then
      Datum.Char (Uchar.of_char (Bytes.get s k))
    else runtime_error "string-ref: index out of range"
  | _ -> runtime_error "string-ref: expected 2 arguments (string k)"

let prim_string_set args =
  match args with
  | [Datum.Str s; Datum.Fixnum k; Datum.Char c] ->
    let n = Uchar.to_int c in
    if k >= 0 && k < Bytes.length s && n < 256 then
      (Bytes.set s k (Char.chr n); Datum.Void)
    else runtime_error "string-set!: index out of range or char out of byte range"
  | _ -> runtime_error "string-set!: expected 3 arguments (string k char)"

let str_chain_compare name cmp args =
  match args with
  | [] | [_] -> runtime_error (Printf.sprintf "%s: expected at least 2 arguments" name)
  | _ ->
    let rec go = function
      | [] | [_] -> Datum.Bool true
      | a :: ((b :: _) as rest) ->
        if cmp (Bytes.to_string (as_string name a)) (Bytes.to_string (as_string name b)) then go rest
        else Datum.Bool false
    in
    go args

let prim_string_eq args = str_chain_compare "string=?" (=) args
let prim_string_lt args = str_chain_compare "string<?" (<) args
let prim_string_gt args = str_chain_compare "string>?" (>) args
let prim_string_le args = str_chain_compare "string<=?" (<=) args
let prim_string_ge args = str_chain_compare "string>=?" (>=) args

let str_ci_chain_compare name cmp args =
  match args with
  | [] | [_] -> runtime_error (Printf.sprintf "%s: expected at least 2 arguments" name)
  | _ ->
    let rec go = function
      | [] | [_] -> Datum.Bool true
      | a :: ((b :: _) as rest) ->
        if cmp (String.lowercase_ascii (Bytes.to_string (as_string name a)))
               (String.lowercase_ascii (Bytes.to_string (as_string name b))) then go rest
        else Datum.Bool false
    in
    go args

let prim_string_ci_eq args = str_ci_chain_compare "string-ci=?" (=) args
let prim_string_ci_lt args = str_ci_chain_compare "string-ci<?" (<) args
let prim_string_ci_gt args = str_ci_chain_compare "string-ci>?" (>) args
let prim_string_ci_le args = str_ci_chain_compare "string-ci<=?" (<=) args
let prim_string_ci_ge args = str_ci_chain_compare "string-ci>=?" (>=) args

let prim_substring args =
  match args with
  | [Datum.Str s; Datum.Fixnum start; Datum.Fixnum stop] ->
    if start >= 0 && stop >= start && stop <= Bytes.length s then
      Datum.Str (Bytes.sub s start (stop - start))
    else runtime_error "substring: index out of range"
  | _ -> runtime_error "substring: expected 3 arguments (string start end)"

let prim_string_append args =
  let bufs = List.map (as_string "string-append") args in
  Datum.Str (Bytes.concat Bytes.empty bufs)

let prim_string_to_list args =
  match args with
  | [Datum.Str s] ->
    let len = Bytes.length s in
    let rec build i acc =
      if i < 0 then acc
      else build (i - 1) (Datum.Pair { car = Datum.Char (Uchar.of_char (Bytes.get s i)); cdr = acc })
    in
    build (len - 1) Datum.Nil
  | _ -> runtime_error (Printf.sprintf "string->list: expected 1 argument, got %d" (List.length args))

let prim_list_to_string args =
  match args with
  | [lst] ->
    let rec collect acc = function
      | Datum.Nil -> List.rev acc
      | Datum.Pair { car = Datum.Char c; cdr } ->
        collect (Char.chr (Uchar.to_int c) :: acc) cdr
      | _ -> runtime_error "list->string: expected list of chars"
    in
    let chars = collect [] lst in
    let buf = Bytes.create (List.length chars) in
    List.iteri (fun i c -> Bytes.set buf i c) chars;
    Datum.Str buf
  | _ -> runtime_error (Printf.sprintf "list->string: expected 1 argument, got %d" (List.length args))

let prim_string_copy args =
  match args with
  | [Datum.Str s] -> Datum.Str (Bytes.copy s)
  | _ -> runtime_error (Printf.sprintf "string-copy: expected 1 argument, got %d" (List.length args))

let prim_string_copy_to args =
  match args with
  | [Datum.Str to_; Datum.Fixnum at; Datum.Str from] ->
    Bytes.blit from 0 to_ at (Bytes.length from); Datum.Void
  | [Datum.Str to_; Datum.Fixnum at; Datum.Str from; Datum.Fixnum start] ->
    Bytes.blit from start to_ at (Bytes.length from - start); Datum.Void
  | [Datum.Str to_; Datum.Fixnum at; Datum.Str from; Datum.Fixnum start; Datum.Fixnum stop] ->
    Bytes.blit from start to_ at (stop - start); Datum.Void
  | _ -> runtime_error "string-copy!: expected 3-5 arguments"

let prim_string_fill args =
  match args with
  | [Datum.Str s; Datum.Char c] ->
    Bytes.fill s 0 (Bytes.length s) (Char.chr (Uchar.to_int c)); Datum.Void
  | _ -> runtime_error "string-fill!: expected 2 arguments (string char)"

let prim_string_upcase args =
  match args with
  | [Datum.Str s] -> Datum.Str (Bytes.of_string (String.uppercase_ascii (Bytes.to_string s)))
  | [_] -> runtime_error "string-upcase: expected string"
  | _ -> runtime_error (Printf.sprintf "string-upcase: expected 1 argument, got %d" (List.length args))

let prim_string_downcase args =
  match args with
  | [Datum.Str s] -> Datum.Str (Bytes.of_string (String.lowercase_ascii (Bytes.to_string s)))
  | [_] -> runtime_error "string-downcase: expected string"
  | _ -> runtime_error (Printf.sprintf "string-downcase: expected 1 argument, got %d" (List.length args))

let prim_string_foldcase args = prim_string_downcase args

(* --- Vector primitives --- *)

let prim_make_vector args =
  match args with
  | [Datum.Fixnum n] -> Datum.Vector (Array.make n Datum.Void)
  | [Datum.Fixnum n; fill] -> Datum.Vector (Array.make n fill)
  | _ -> runtime_error "make-vector: expected (make-vector k) or (make-vector k fill)"

let prim_vector args = Datum.Vector (Array.of_list args)

let prim_vector_length args =
  match args with
  | [Datum.Vector v] -> Datum.Fixnum (Array.length v)
  | [_] -> runtime_error "vector-length: expected vector"
  | _ -> runtime_error (Printf.sprintf "vector-length: expected 1 argument, got %d" (List.length args))

let prim_vector_ref args =
  match args with
  | [Datum.Vector v; Datum.Fixnum k] ->
    if k >= 0 && k < Array.length v then v.(k)
    else runtime_error "vector-ref: index out of range"
  | _ -> runtime_error "vector-ref: expected 2 arguments (vector k)"

let prim_vector_set args =
  match args with
  | [Datum.Vector v; Datum.Fixnum k; obj] ->
    if k >= 0 && k < Array.length v then (v.(k) <- obj; Datum.Void)
    else runtime_error "vector-set!: index out of range"
  | _ -> runtime_error "vector-set!: expected 3 arguments (vector k obj)"

let prim_vector_to_list args =
  match args with
  | [Datum.Vector v] ->
    Array.fold_right (fun x acc -> Datum.Pair { car = x; cdr = acc }) v Datum.Nil
  | _ -> runtime_error (Printf.sprintf "vector->list: expected 1 argument, got %d" (List.length args))

let prim_list_to_vector args =
  match args with
  | [lst] ->
    let rec collect acc = function
      | Datum.Nil -> List.rev acc
      | Datum.Pair { car; cdr } -> collect (car :: acc) cdr
      | _ -> runtime_error "list->vector: expected proper list"
    in
    Datum.Vector (Array.of_list (collect [] lst))
  | _ -> runtime_error (Printf.sprintf "list->vector: expected 1 argument, got %d" (List.length args))

let prim_vector_copy args =
  match args with
  | [Datum.Vector v] -> Datum.Vector (Array.copy v)
  | _ -> runtime_error (Printf.sprintf "vector-copy: expected 1 argument, got %d" (List.length args))

let prim_vector_copy_to args =
  match args with
  | [Datum.Vector to_; Datum.Fixnum at; Datum.Vector from] ->
    Array.blit from 0 to_ at (Array.length from); Datum.Void
  | [Datum.Vector to_; Datum.Fixnum at; Datum.Vector from; Datum.Fixnum start] ->
    Array.blit from start to_ at (Array.length from - start); Datum.Void
  | [Datum.Vector to_; Datum.Fixnum at; Datum.Vector from; Datum.Fixnum start; Datum.Fixnum stop] ->
    Array.blit from start to_ at (stop - start); Datum.Void
  | _ -> runtime_error "vector-copy!: expected 3-5 arguments"

let prim_vector_append args =
  let vecs = List.map (function
    | Datum.Vector v -> v
    | _ -> runtime_error "vector-append: expected vectors"
  ) args in
  Datum.Vector (Array.concat vecs)

let prim_vector_fill args =
  match args with
  | [Datum.Vector v; fill] ->
    Array.fill v 0 (Array.length v) fill; Datum.Void
  | _ -> runtime_error "vector-fill!: expected 2 arguments (vector fill)"

let prim_vector_to_string args =
  match args with
  | [Datum.Vector v] ->
    let buf = Bytes.create (Array.length v) in
    Array.iteri (fun i c ->
      match c with
      | Datum.Char uc ->
        let n = Uchar.to_int uc in
        if n < 256 then Bytes.set buf i (Char.chr n)
        else runtime_error "vector->string: char out of byte range"
      | _ -> runtime_error "vector->string: expected chars"
    ) v;
    Datum.Str buf
  | _ -> runtime_error (Printf.sprintf "vector->string: expected 1 argument, got %d" (List.length args))

let prim_string_to_vector args =
  match args with
  | [Datum.Str s] ->
    Datum.Vector (Array.init (Bytes.length s) (fun i ->
      Datum.Char (Uchar.of_char (Bytes.get s i))))
  | _ -> runtime_error (Printf.sprintf "string->vector: expected 1 argument, got %d" (List.length args))

(* --- Bytevector primitives --- *)

let prim_make_bytevector args =
  match args with
  | [Datum.Fixnum n] -> Datum.Bytevector (Bytes.make n (Char.chr 0))
  | [Datum.Fixnum n; Datum.Fixnum byte] ->
    if byte >= 0 && byte <= 255 then
      Datum.Bytevector (Bytes.make n (Char.chr byte))
    else runtime_error "make-bytevector: byte out of range"
  | _ -> runtime_error "make-bytevector: expected (make-bytevector k) or (make-bytevector k byte)"

let prim_bytevector args =
  let buf = Bytes.create (List.length args) in
  List.iteri (fun i v ->
    let n = as_fixnum "bytevector" v in
    if n >= 0 && n <= 255 then Bytes.set buf i (Char.chr n)
    else runtime_error "bytevector: byte out of range"
  ) args;
  Datum.Bytevector buf

let prim_bytevector_length args =
  match args with
  | [Datum.Bytevector bv] -> Datum.Fixnum (Bytes.length bv)
  | [_] -> runtime_error "bytevector-length: expected bytevector"
  | _ -> runtime_error (Printf.sprintf "bytevector-length: expected 1 argument, got %d" (List.length args))

let prim_bytevector_u8_ref args =
  match args with
  | [Datum.Bytevector bv; Datum.Fixnum k] ->
    if k >= 0 && k < Bytes.length bv then
      Datum.Fixnum (Char.code (Bytes.get bv k))
    else runtime_error "bytevector-u8-ref: index out of range"
  | _ -> runtime_error "bytevector-u8-ref: expected 2 arguments (bytevector k)"

let prim_bytevector_u8_set args =
  match args with
  | [Datum.Bytevector bv; Datum.Fixnum k; Datum.Fixnum byte] ->
    if k >= 0 && k < Bytes.length bv && byte >= 0 && byte <= 255 then
      (Bytes.set bv k (Char.chr byte); Datum.Void)
    else runtime_error "bytevector-u8-set!: index out of range or byte out of range"
  | _ -> runtime_error "bytevector-u8-set!: expected 3 arguments (bytevector k byte)"

let prim_bytevector_copy args =
  match args with
  | [Datum.Bytevector bv] -> Datum.Bytevector (Bytes.copy bv)
  | _ -> runtime_error (Printf.sprintf "bytevector-copy: expected 1 argument, got %d" (List.length args))

let prim_bytevector_copy_to args =
  match args with
  | [Datum.Bytevector to_; Datum.Fixnum at; Datum.Bytevector from] ->
    Bytes.blit from 0 to_ at (Bytes.length from); Datum.Void
  | [Datum.Bytevector to_; Datum.Fixnum at; Datum.Bytevector from; Datum.Fixnum start] ->
    Bytes.blit from start to_ at (Bytes.length from - start); Datum.Void
  | [Datum.Bytevector to_; Datum.Fixnum at; Datum.Bytevector from; Datum.Fixnum start; Datum.Fixnum stop] ->
    Bytes.blit from start to_ at (stop - start); Datum.Void
  | _ -> runtime_error "bytevector-copy!: expected 3-5 arguments"

let prim_bytevector_append args =
  let bvs = List.map (function
    | Datum.Bytevector bv -> bv
    | _ -> runtime_error "bytevector-append: expected bytevectors"
  ) args in
  Datum.Bytevector (Bytes.concat Bytes.empty bvs)

let prim_utf8_to_string args =
  match args with
  | [Datum.Bytevector bv] -> Datum.Str (Bytes.copy bv)
  | _ -> runtime_error (Printf.sprintf "utf8->string: expected 1 argument, got %d" (List.length args))

let prim_string_to_utf8 args =
  match args with
  | [Datum.Str s] -> Datum.Bytevector (Bytes.copy s)
  | _ -> runtime_error (Printf.sprintf "string->utf8: expected 1 argument, got %d" (List.length args))

let prim_equal args =
  match args with
  | [a; b] -> Datum.Bool (scheme_equal a b)
  | _ -> runtime_error (Printf.sprintf "equal?: expected 2 arguments, got %d" (List.length args))

(* --- Type predicates --- *)

let prim_boolean args =
  match args with
  | [Datum.Bool _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "boolean?: expected 1 argument, got %d" (List.length args))

let prim_boolean_eq args =
  match args with
  | Datum.Bool _ :: _ :: _ ->
    let rec go = function
      | [] | [_] -> Datum.Bool true
      | Datum.Bool a :: ((Datum.Bool b :: _) as rest) ->
        if a = b then go rest else Datum.Bool false
      | _ -> runtime_error "boolean=?: expected boolean arguments"
    in
    go args
  | _ -> runtime_error "boolean=?: expected at least 2 boolean arguments"

let prim_number args =
  match args with
  | [Datum.Fixnum _ | Datum.Flonum _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "number?: expected 1 argument, got %d" (List.length args))

let prim_integer args =
  match args with
  | [Datum.Fixnum _] -> Datum.Bool true
  | [Datum.Flonum f] -> Datum.Bool (Float.is_integer f)
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "integer?: expected 1 argument, got %d" (List.length args))

let prim_exact args =
  match args with
  | [Datum.Fixnum _] -> Datum.Bool true
  | [Datum.Flonum _] -> Datum.Bool false
  | [_] -> runtime_error "exact?: expected number"
  | _ -> runtime_error (Printf.sprintf "exact?: expected 1 argument, got %d" (List.length args))

let prim_inexact_pred args =
  match args with
  | [Datum.Fixnum _] -> Datum.Bool false
  | [Datum.Flonum _] -> Datum.Bool true
  | [_] -> runtime_error "inexact?: expected number"
  | _ -> runtime_error (Printf.sprintf "inexact?: expected 1 argument, got %d" (List.length args))

let prim_exact_integer args =
  match args with
  | [Datum.Fixnum _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "exact-integer?: expected 1 argument, got %d" (List.length args))

let prim_zero args =
  match args with
  | [Datum.Fixnum 0] -> Datum.Bool true
  | [Datum.Flonum f] -> Datum.Bool (f = 0.0)
  | [Datum.Fixnum _] -> Datum.Bool false
  | [_] -> runtime_error "zero?: expected number"
  | _ -> runtime_error (Printf.sprintf "zero?: expected 1 argument, got %d" (List.length args))

let prim_positive args =
  match args with
  | [Datum.Fixnum n] -> Datum.Bool (n > 0)
  | [Datum.Flonum f] -> Datum.Bool (f > 0.0)
  | [_] -> runtime_error "positive?: expected number"
  | _ -> runtime_error (Printf.sprintf "positive?: expected 1 argument, got %d" (List.length args))

let prim_negative args =
  match args with
  | [Datum.Fixnum n] -> Datum.Bool (n < 0)
  | [Datum.Flonum f] -> Datum.Bool (f < 0.0)
  | [_] -> runtime_error "negative?: expected number"
  | _ -> runtime_error (Printf.sprintf "negative?: expected 1 argument, got %d" (List.length args))

let prim_odd args =
  match args with
  | [Datum.Fixnum n] -> Datum.Bool (n mod 2 <> 0)
  | [_] -> runtime_error "odd?: expected integer"
  | _ -> runtime_error (Printf.sprintf "odd?: expected 1 argument, got %d" (List.length args))

let prim_even args =
  match args with
  | [Datum.Fixnum n] -> Datum.Bool (n mod 2 = 0)
  | [_] -> runtime_error "even?: expected integer"
  | _ -> runtime_error (Printf.sprintf "even?: expected 1 argument, got %d" (List.length args))

let prim_symbol args =
  match args with
  | [Datum.Symbol _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "symbol?: expected 1 argument, got %d" (List.length args))

let prim_symbol_eq args =
  match args with
  | Datum.Symbol _ :: _ :: _ ->
    let rec go = function
      | [] | [_] -> Datum.Bool true
      | Datum.Symbol a :: ((Datum.Symbol b :: _) as rest) ->
        if String.equal a b then go rest else Datum.Bool false
      | _ -> runtime_error "symbol=?: expected symbol arguments"
    in
    go args
  | _ -> runtime_error "symbol=?: expected at least 2 symbol arguments"

let prim_symbol_to_string args =
  match args with
  | [Datum.Symbol s] -> Datum.Str (Bytes.of_string s)
  | [_] -> runtime_error "symbol->string: expected symbol"
  | _ -> runtime_error (Printf.sprintf "symbol->string: expected 1 argument, got %d" (List.length args))

let prim_string_to_symbol sym_table args =
  match args with
  | [Datum.Str s] ->
    let name = Bytes.to_string s in
    let sym = Symbol.intern sym_table name in
    Datum.Symbol (Symbol.name sym)
  | [_] -> runtime_error "string->symbol: expected string"
  | _ -> runtime_error (Printf.sprintf "string->symbol: expected 1 argument, got %d" (List.length args))

let prim_char_pred args =
  match args with
  | [Datum.Char _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "char?: expected 1 argument, got %d" (List.length args))

let prim_string_pred args =
  match args with
  | [Datum.Str _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "string?: expected 1 argument, got %d" (List.length args))

let prim_vector_pred args =
  match args with
  | [Datum.Vector _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "vector?: expected 1 argument, got %d" (List.length args))

let prim_bytevector_pred args =
  match args with
  | [Datum.Bytevector _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "bytevector?: expected 1 argument, got %d" (List.length args))

let prim_procedure_pred args =
  match args with
  | [Datum.Primitive _] | [Datum.Closure _] | [Datum.Continuation _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "procedure?: expected 1 argument, got %d" (List.length args))

let prim_list_pred args =
  match args with
  | [v] ->
    let rec is_list = function
      | Datum.Nil -> true
      | Datum.Pair { cdr; _ } -> is_list cdr
      | _ -> false
    in
    Datum.Bool (is_list v)
  | _ -> runtime_error (Printf.sprintf "list?: expected 1 argument, got %d" (List.length args))

let prim_eof_object_pred args =
  match args with
  | [Datum.Eof] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "eof-object?: expected 1 argument, got %d" (List.length args))

let prim_eof_object args =
  match args with
  | [] -> Datum.Eof
  | _ -> runtime_error (Printf.sprintf "eof-object: expected 0 arguments, got %d" (List.length args))

(* --- Exception primitives --- *)

let prim_push_handler handlers args =
  match args with
  | [handler] -> handlers := handler :: !handlers; Datum.Void
  | _ -> runtime_error "%push-handler!: expected 1 argument"

let prim_pop_handler handlers args =
  match args with
  | [] ->
    (match !handlers with
     | h :: rest -> handlers := rest; h
     | [] -> runtime_error "%pop-handler!: handler stack empty")
  | _ -> runtime_error "%pop-handler!: expected 0 arguments"

let prim_handler_stack_empty handlers args =
  match args with
  | [] -> Datum.Bool (!handlers = [])
  | _ -> runtime_error "%handler-stack-empty?: expected 0 arguments"

let prim_make_error args =
  match args with
  | Datum.Str msg :: irritants ->
    Datum.Error_object {
      err_message = Bytes.to_string msg;
      err_irritants = irritants;
      err_tag = Datum.General_error;
    }
  | _ -> runtime_error "%make-error: expected string message"

let prim_fatal_error _args =
  match _args with
  | [obj] ->
    runtime_error (Printf.sprintf "unhandled exception: %s" (Datum.to_string obj))
  | _ -> runtime_error "%fatal-error: expected 1 argument"

let prim_error_object_pred args =
  match args with
  | [Datum.Error_object _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "error-object?: expected 1 argument, got %d" (List.length args))

let prim_error_object_message args =
  match args with
  | [Datum.Error_object e] -> Datum.Str (Bytes.of_string e.err_message)
  | [_] -> runtime_error "error-object-message: expected error object"
  | _ -> runtime_error (Printf.sprintf "error-object-message: expected 1 argument, got %d" (List.length args))

let prim_error_object_irritants args =
  match args with
  | [Datum.Error_object e] ->
    List.fold_right (fun x acc -> Datum.Pair { car = x; cdr = acc }) e.err_irritants Datum.Nil
  | [_] -> runtime_error "error-object-irritants: expected error object"
  | _ -> runtime_error (Printf.sprintf "error-object-irritants: expected 1 argument, got %d" (List.length args))

let prim_read_error_pred args =
  match args with
  | [Datum.Error_object e] -> Datum.Bool (e.err_tag = Datum.Read_error)
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "read-error?: expected 1 argument, got %d" (List.length args))

let prim_file_error_pred args =
  match args with
  | [Datum.Error_object e] -> Datum.Bool (e.err_tag = Datum.File_error)
  | [_] -> Datum.Bool false
  | _ -> runtime_error (Printf.sprintf "file-error?: expected 1 argument, got %d" (List.length args))

let prim_error_object_type_pred args =
  match args with
  | [Datum.Str _] -> Datum.Bool true
  | [_] -> Datum.Bool false
  | _ -> runtime_error "error-object-type?: expected 1 argument"

(* --- Primitive registration --- *)

let register_primitives symbols env handlers =
  let register name fn =
    let sym = Symbol.intern symbols name in
    Env.define env sym (make_prim name fn)
  in
  let register_intrinsic name id =
    let sym = Symbol.intern symbols name in
    Env.define env sym (make_intrinsic name id)
  in
  register "+" prim_add;
  register "-" prim_sub;
  register "*" prim_mul;
  register "<" prim_lt;
  register "=" prim_num_eq;
  register ">" prim_gt;
  register "cons" prim_cons;
  register "car" prim_car;
  register "cdr" prim_cdr;
  register "null?" prim_null;
  register "pair?" prim_pair;
  register "not" prim_not;
  register "display" prim_display;
  register "write" prim_write;
  register "newline" prim_newline;
  register "eqv?" prim_eqv;
  register "eq?" prim_eq;
  register "list" prim_list;
  register "<=" prim_le;
  register ">=" prim_ge;
  register_intrinsic "apply" Datum.Intrinsic_apply;
  register_intrinsic "call/cc" Datum.Intrinsic_call_cc;
  register_intrinsic "call-with-current-continuation" Datum.Intrinsic_call_cc;
  register_intrinsic "call-with-values" Datum.Intrinsic_call_with_values;
  register_intrinsic "dynamic-wind" Datum.Intrinsic_dynamic_wind;
  register "values" prim_values;
  (* Numeric *)
  register "/" prim_div;
  register "abs" prim_abs;
  register "min" prim_min;
  register "max" prim_max;
  register "quotient" prim_quotient;
  register "remainder" prim_remainder;
  register "modulo" prim_modulo;
  register "floor" prim_floor;
  register "ceiling" prim_ceiling;
  register "truncate" prim_truncate;
  register "round" prim_round;
  register "floor-quotient" prim_floor_quotient;
  register "floor-remainder" prim_floor_remainder;
  register "truncate-quotient" prim_truncate_quotient;
  register "truncate-remainder" prim_truncate_remainder;
  register "gcd" prim_gcd;
  register "lcm" prim_lcm;
  register "exact->inexact" prim_exact_to_inexact;
  register "inexact->exact" prim_inexact_to_exact;
  register "exact" prim_inexact_to_exact;
  register "inexact" prim_exact_to_inexact;
  register "expt" prim_expt;
  register "sqrt" prim_sqrt;
  register "exact-integer-sqrt" prim_exact_integer_sqrt;
  register "number->string" prim_number_to_string;
  register "string->number" prim_string_to_number;
  (* Pair & list *)
  register "set-car!" prim_set_car;
  register "set-cdr!" prim_set_cdr;
  register "caar" prim_caar;
  register "cadr" prim_cadr;
  register "cdar" prim_cdar;
  register "cddr" prim_cddr;
  register "make-list" prim_make_list;
  register "length" prim_length;
  register "append" prim_append;
  register "reverse" prim_reverse;
  register "list-tail" prim_list_tail;
  register "list-ref" prim_list_ref;
  register "list-set!" prim_list_set;
  register "list-copy" prim_list_copy;
  register "memq" prim_memq;
  register "memv" prim_memv;
  register "member" prim_member;
  register "assq" prim_assq;
  register "assv" prim_assv;
  register "assoc" prim_assoc;
  (* Character *)
  register "char=?" prim_char_eq;
  register "char<?" prim_char_lt;
  register "char>?" prim_char_gt;
  register "char<=?" prim_char_le;
  register "char>=?" prim_char_ge;
  register "char-ci=?" prim_char_ci_eq;
  register "char-ci<?" prim_char_ci_lt;
  register "char-ci>?" prim_char_ci_gt;
  register "char-ci<=?" prim_char_ci_le;
  register "char-ci>=?" prim_char_ci_ge;
  register "char->integer" prim_char_to_integer;
  register "integer->char" prim_integer_to_char;
  register "char-upcase" prim_char_upcase;
  register "char-downcase" prim_char_downcase;
  register "char-foldcase" prim_char_foldcase;
  register "char-alphabetic?" prim_char_alphabetic;
  register "char-numeric?" prim_char_numeric;
  register "char-whitespace?" prim_char_whitespace;
  register "char-upper-case?" prim_char_upper_case;
  register "char-lower-case?" prim_char_lower_case;
  register "digit-value" prim_digit_value;
  (* String *)
  register "make-string" prim_make_string;
  register "string" prim_string;
  register "string-length" prim_string_length;
  register "string-ref" prim_string_ref;
  register "string-set!" prim_string_set;
  register "string=?" prim_string_eq;
  register "string<?" prim_string_lt;
  register "string>?" prim_string_gt;
  register "string<=?" prim_string_le;
  register "string>=?" prim_string_ge;
  register "string-ci=?" prim_string_ci_eq;
  register "string-ci<?" prim_string_ci_lt;
  register "string-ci>?" prim_string_ci_gt;
  register "string-ci<=?" prim_string_ci_le;
  register "string-ci>=?" prim_string_ci_ge;
  register "substring" prim_substring;
  register "string-append" prim_string_append;
  register "string->list" prim_string_to_list;
  register "list->string" prim_list_to_string;
  register "string-copy" prim_string_copy;
  register "string-copy!" prim_string_copy_to;
  register "string-fill!" prim_string_fill;
  register "string-upcase" prim_string_upcase;
  register "string-downcase" prim_string_downcase;
  register "string-foldcase" prim_string_foldcase;
  (* Vector *)
  register "make-vector" prim_make_vector;
  register "vector" prim_vector;
  register "vector-length" prim_vector_length;
  register "vector-ref" prim_vector_ref;
  register "vector-set!" prim_vector_set;
  register "vector->list" prim_vector_to_list;
  register "list->vector" prim_list_to_vector;
  register "vector-copy" prim_vector_copy;
  register "vector-copy!" prim_vector_copy_to;
  register "vector-append" prim_vector_append;
  register "vector-fill!" prim_vector_fill;
  register "vector->string" prim_vector_to_string;
  register "string->vector" prim_string_to_vector;
  (* Bytevector *)
  register "make-bytevector" prim_make_bytevector;
  register "bytevector" prim_bytevector;
  register "bytevector-length" prim_bytevector_length;
  register "bytevector-u8-ref" prim_bytevector_u8_ref;
  register "bytevector-u8-set!" prim_bytevector_u8_set;
  register "bytevector-copy" prim_bytevector_copy;
  register "bytevector-copy!" prim_bytevector_copy_to;
  register "bytevector-append" prim_bytevector_append;
  register "utf8->string" prim_utf8_to_string;
  register "string->utf8" prim_string_to_utf8;
  (* Type predicates *)
  register "equal?" prim_equal;
  register "boolean?" prim_boolean;
  register "boolean=?" prim_boolean_eq;
  register "number?" prim_number;
  register "complex?" prim_number;
  register "real?" prim_number;
  register "rational?" prim_number;
  register "integer?" prim_integer;
  register "exact?" prim_exact;
  register "inexact?" prim_inexact_pred;
  register "exact-integer?" prim_exact_integer;
  register "zero?" prim_zero;
  register "positive?" prim_positive;
  register "negative?" prim_negative;
  register "odd?" prim_odd;
  register "even?" prim_even;
  register "symbol?" prim_symbol;
  register "symbol=?" prim_symbol_eq;
  register "symbol->string" prim_symbol_to_string;
  register "string->symbol" (prim_string_to_symbol symbols);
  register "char?" prim_char_pred;
  register "string?" prim_string_pred;
  register "vector?" prim_vector_pred;
  register "bytevector?" prim_bytevector_pred;
  register "procedure?" prim_procedure_pred;
  register "list?" prim_list_pred;
  register "eof-object?" prim_eof_object_pred;
  register "eof-object" prim_eof_object;
  (* Exception *)
  register "%push-handler!" (prim_push_handler handlers);
  register "%pop-handler!" (prim_pop_handler handlers);
  register "%handler-stack-empty?" (prim_handler_stack_empty handlers);
  register "%make-error" prim_make_error;
  register "%fatal-error" prim_fatal_error;
  register "error-object?" prim_error_object_pred;
  register "error-object-message" prim_error_object_message;
  register "error-object-irritants" prim_error_object_irritants;
  register "read-error?" prim_read_error_pred;
  register "file-error?" prim_file_error_pred;
  register "error-object-type?" prim_error_object_type_pred

(* --- Instance creation --- *)

let boot_definitions = [
  (* with-exception-handler: push handler, run thunk, pop handler *)
  "(define (with-exception-handler handler thunk) \
     (%push-handler! handler) \
     (let ((result (thunk))) \
       (%pop-handler!) \
       result))";

  (* raise: pop handler and call it; if it returns, raise again *)
  "(define (raise obj) \
     (if (%handler-stack-empty?) \
       (%fatal-error obj) \
       (let ((handler (%pop-handler!))) \
         (handler obj) \
         (raise (%make-error \"handler returned\" (list obj))))))";

  (* raise-continuable: pop handler and call it; return value is result *)
  "(define (raise-continuable obj) \
     (if (%handler-stack-empty?) \
       (%fatal-error obj) \
       (let ((handler (%pop-handler!))) \
         (%push-handler! handler) \
         (let ((result (handler obj))) \
           result))))";

  (* error — create error object and raise it *)
  "(define (error message . irritants) \
     (raise (apply %make-error message irritants)))";

  (* map — single-list for now *)
  "(define (map f lst) \
     (if (null? lst) '() \
       (cons (f (car lst)) (map f (cdr lst)))))";

  (* for-each — single-list *)
  "(define (for-each f lst) \
     (if (null? lst) (begin) \
       (begin (f (car lst)) (for-each f (cdr lst)))))";

  (* string-map *)
  "(define (string-map f s) \
     (list->string (map f (string->list s))))";

  (* string-for-each *)
  "(define (string-for-each f s) \
     (for-each f (string->list s)))";

  (* vector-map *)
  "(define (vector-map f v) \
     (list->vector (map f (vector->list v))))";

  (* vector-for-each *)
  "(define (vector-for-each f v) \
     (for-each f (vector->list v)))";
]

let eval_boot inst src =
  let port = Port.of_string src in
  let expr = Reader.read_syntax inst.readtable port in
  let code = Compiler.compile inst.symbols expr in
  ignore (Vm.execute ~winds:inst.winds inst.global_env code)

let create ?(readtable = Readtable.default) () =
  let symbols = Symbol.create_table () in
  let global_env = Env.empty () in
  let handlers = ref [] in
  register_primitives symbols global_env handlers;
  let inst = { symbols; global_env; readtable; winds = ref []; handlers } in
  List.iter (eval_boot inst) boot_definitions;
  inst

let intern inst name = Symbol.intern inst.symbols name

(* --- Evaluation --- *)

let eval_syntax inst expr =
  let code = Compiler.compile inst.symbols expr in
  Vm.execute ~winds:inst.winds inst.global_env code

let eval_string inst src =
  let port = Port.of_string src in
  let expr = Reader.read_syntax inst.readtable port in
  eval_syntax inst expr
