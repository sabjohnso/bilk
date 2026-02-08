type t = {
  symbols : Symbol.table;
  global_env : Env.t;
  readtable : Readtable.t;
  winds : Datum.wind list ref;
  handlers : Datum.t list ref;
  syn_env : Expander.syn_env;
  gensym_counter : int ref;
  libraries : Library.registry;
  search_paths : string list ref;
  features : string list;
  loading_libs : string list list ref;
  fasl_cache : bool ref;
  current_input : Port.t ref;
  current_output : Port.t ref;
  current_error : Port.t ref;
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

let prim_display current_output args =
  match args with
  | [v] -> Port.write_string !current_output (Datum.to_display_string v); Datum.Void
  | [v; Datum.Port p] -> Port.write_string p (Datum.to_display_string v); Datum.Void
  | [_; _] -> runtime_error "display: second argument must be an output port"
  | _ -> runtime_error (Printf.sprintf "display: expected 1 or 2 arguments, got %d" (List.length args))

let prim_write_val current_output args =
  match args with
  | [v] -> Port.write_string !current_output (Datum.to_string v); Datum.Void
  | [v; Datum.Port p] -> Port.write_string p (Datum.to_string v); Datum.Void
  | [_; _] -> runtime_error "write: second argument must be an output port"
  | _ -> runtime_error (Printf.sprintf "write: expected 1 or 2 arguments, got %d" (List.length args))

let prim_newline current_output args =
  match args with
  | [] -> Port.write_char !current_output '\n'; Datum.Void
  | [Datum.Port p] -> Port.write_char p '\n'; Datum.Void
  | [_] -> runtime_error "newline: argument must be an output port"
  | _ -> runtime_error (Printf.sprintf "newline: expected 0 or 1 arguments, got %d" (List.length args))

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
    (* R7RS: round to even (banker's rounding).
       f -. floor f is always in [0, 1), so only frac = 0.5 needs special
       handling — the negative-half case is covered because floor(-2.5) = -3
       gives frac = 0.5 and -3 is odd, so we round up to -2. *)
    let rounded =
      let fl = floor f in
      let frac = f -. fl in
      if frac = 0.5 then
        let ifl = int_of_float fl in
        if ifl mod 2 = 0 then fl else fl +. 1.0
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
      | Datum.Flonum f ->
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
    let fold c =
      let n = Uchar.to_int c in
      if n < 128 then Char.code (Char.lowercase_ascii (Char.chr n))
      else n
    in
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
  | [Datum.Fixnum n] ->
    if not (Uchar.is_valid n) then
      runtime_error (Printf.sprintf "integer->char: invalid Unicode scalar value %d" n);
    Datum.Char (Uchar.of_int n)
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
        let n = Uchar.to_int c in
        if n >= 256 then runtime_error "list->string: char out of byte range";
        collect (Char.chr n :: acc) cdr
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
    let n = Uchar.to_int c in
    if n >= 256 then runtime_error "string-fill!: char out of byte range";
    Bytes.fill s 0 (Bytes.length s) (Char.chr n); Datum.Void
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

let prim_handler_depth handlers args =
  match args with
  | [] -> Datum.Fixnum (List.length !handlers)
  | _ -> runtime_error "%handler-depth: expected 0 arguments"

let prim_handler_truncate handlers args =
  match args with
  | [Datum.Fixnum target] ->
    let rec truncate lst n =
      if n <= target then lst
      else match lst with
        | _ :: rest -> truncate rest (n - 1)
        | [] -> []
    in
    handlers := truncate !handlers (List.length !handlers);
    Datum.Void
  | _ -> runtime_error "%handler-truncate!: expected 1 argument (depth)"

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

let register_primitives symbols env handlers
    ~current_input ~current_output ~current_error =
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
  register "display" (prim_display current_output);
  register "write" (prim_write_val current_output);
  register "write-shared" (prim_write_val current_output);
  register "write-simple" (prim_write_val current_output);
  register "newline" (prim_newline current_output);
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
  register "%handler-depth" (prim_handler_depth handlers);
  register "%handler-truncate!" (prim_handler_truncate handlers);
  register "%make-error" prim_make_error;
  register "%fatal-error" prim_fatal_error;
  register "error-object?" prim_error_object_pred;
  register "error-object-message" prim_error_object_message;
  register "error-object-irritants" prim_error_object_irritants;
  register "read-error?" prim_read_error_pred;
  register "file-error?" prim_file_error_pred;
  register "error-object-type?" prim_error_object_type_pred;
  (* Port predicates *)
  register "port?" (fun args -> match args with
    | [Datum.Port _] -> Datum.Bool true
    | [_] -> Datum.Bool false
    | _ -> runtime_error (Printf.sprintf "port?: expected 1 argument, got %d" (List.length args)));
  register "input-port?" (fun args -> match args with
    | [Datum.Port p] -> Datum.Bool (Port.is_input p)
    | [_] -> Datum.Bool false
    | _ -> runtime_error (Printf.sprintf "input-port?: expected 1 argument, got %d" (List.length args)));
  register "output-port?" (fun args -> match args with
    | [Datum.Port p] -> Datum.Bool (Port.is_output p)
    | [_] -> Datum.Bool false
    | _ -> runtime_error (Printf.sprintf "output-port?: expected 1 argument, got %d" (List.length args)));
  register "input-port-open?" (fun args -> match args with
    | [Datum.Port p] -> Datum.Bool (Port.is_input p && Port.is_open p)
    | [_] -> runtime_error "input-port-open?: expected input port"
    | _ -> runtime_error (Printf.sprintf "input-port-open?: expected 1 argument, got %d" (List.length args)));
  register "output-port-open?" (fun args -> match args with
    | [Datum.Port p] -> Datum.Bool (Port.is_output p && Port.is_open p)
    | [_] -> runtime_error "output-port-open?: expected output port"
    | _ -> runtime_error (Printf.sprintf "output-port-open?: expected 1 argument, got %d" (List.length args)));
  register "textual-port?" (fun args -> match args with
    | [Datum.Port _] -> Datum.Bool true
    | [_] -> Datum.Bool false
    | _ -> runtime_error (Printf.sprintf "textual-port?: expected 1 argument, got %d" (List.length args)));
  register "binary-port?" (fun args -> match args with
    | [Datum.Port _] -> Datum.Bool false
    | [_] -> Datum.Bool false
    | _ -> runtime_error (Printf.sprintf "binary-port?: expected 1 argument, got %d" (List.length args)));
  (* Current port procedures *)
  register "current-input-port" (fun args -> match args with
    | [] -> Datum.Port !current_input
    | _ -> runtime_error (Printf.sprintf "current-input-port: expected 0 arguments, got %d" (List.length args)));
  register "current-output-port" (fun args -> match args with
    | [] -> Datum.Port !current_output
    | _ -> runtime_error (Printf.sprintf "current-output-port: expected 0 arguments, got %d" (List.length args)));
  register "current-error-port" (fun args -> match args with
    | [] -> Datum.Port !current_error
    | _ -> runtime_error (Printf.sprintf "current-error-port: expected 0 arguments, got %d" (List.length args)));
  (* String port constructors *)
  register "open-input-string" (fun args -> match args with
    | [Datum.Str s] -> Datum.Port (Port.of_string (Bytes.to_string s))
    | [_] -> runtime_error "open-input-string: expected string"
    | _ -> runtime_error (Printf.sprintf "open-input-string: expected 1 argument, got %d" (List.length args)));
  register "open-output-string" (fun args -> match args with
    | [] -> Datum.Port (Port.open_output_string ())
    | _ -> runtime_error (Printf.sprintf "open-output-string: expected 0 arguments, got %d" (List.length args)));
  register "get-output-string" (fun args -> match args with
    | [Datum.Port p] -> Datum.Str (Bytes.of_string (Port.get_output_string p))
    | [_] -> runtime_error "get-output-string: expected output string port"
    | _ -> runtime_error (Printf.sprintf "get-output-string: expected 1 argument, got %d" (List.length args)));
  (* Write primitives *)
  register "write-char" (fun args -> match args with
    | [Datum.Char c] ->
      let buf = Buffer.create 4 in
      Buffer.add_utf_8_uchar buf c;
      Port.write_string !current_output (Buffer.contents buf); Datum.Void
    | [Datum.Char c; Datum.Port p] ->
      Port.write_uchar p c; Datum.Void
    | [_] -> runtime_error "write-char: expected character"
    | [_; _] -> runtime_error "write-char: expected character and optional port"
    | _ -> runtime_error (Printf.sprintf "write-char: expected 1 or 2 arguments, got %d" (List.length args)));
  register "write-string" (fun args -> match args with
    | [Datum.Str s] ->
      Port.write_string !current_output (Bytes.to_string s); Datum.Void
    | [Datum.Str s; Datum.Port p] ->
      Port.write_string p (Bytes.to_string s); Datum.Void
    | [_] -> runtime_error "write-string: expected string"
    | [_; _] -> runtime_error "write-string: expected string and optional port"
    | _ -> runtime_error (Printf.sprintf "write-string: expected 1 or 2 arguments, got %d" (List.length args)));
  register "write-u8" (fun args -> match args with
    | [Datum.Fixnum n] ->
      Port.write_u8 !current_output n; Datum.Void
    | [Datum.Fixnum n; Datum.Port p] ->
      Port.write_u8 p n; Datum.Void
    | [_] -> runtime_error "write-u8: expected integer"
    | [_; _] -> runtime_error "write-u8: expected integer and optional port"
    | _ -> runtime_error (Printf.sprintf "write-u8: expected 1 or 2 arguments, got %d" (List.length args)));
  register "write-bytevector" (fun args -> match args with
    | [Datum.Bytevector bv] ->
      Port.write_bytes !current_output bv 0 (Bytes.length bv); Datum.Void
    | [Datum.Bytevector bv; Datum.Port p] ->
      Port.write_bytes p bv 0 (Bytes.length bv); Datum.Void
    | [_] -> runtime_error "write-bytevector: expected bytevector"
    | [_; _] -> runtime_error "write-bytevector: expected bytevector and optional port"
    | _ -> runtime_error (Printf.sprintf "write-bytevector: expected 1 or 2 arguments, got %d" (List.length args)));
  register "flush-output-port" (fun args -> match args with
    | [] -> Port.flush !current_output; Datum.Void
    | [Datum.Port p] -> Port.flush p; Datum.Void
    | [_] -> runtime_error "flush-output-port: expected output port"
    | _ -> runtime_error (Printf.sprintf "flush-output-port: expected 0 or 1 arguments, got %d" (List.length args)));
  (* Read primitives *)
  register "read-char" (fun args -> match args with
    | [] -> (match Port.read_char !current_input with
             | Some c -> Datum.Char (Uchar.of_char c)
             | None -> Datum.Eof)
    | [Datum.Port p] -> (match Port.read_char p with
                         | Some c -> Datum.Char (Uchar.of_char c)
                         | None -> Datum.Eof)
    | [_] -> runtime_error "read-char: expected input port"
    | _ -> runtime_error (Printf.sprintf "read-char: expected 0 or 1 arguments, got %d" (List.length args)));
  register "peek-char" (fun args -> match args with
    | [] -> (match Port.peek_char !current_input with
             | Some c -> Datum.Char (Uchar.of_char c)
             | None -> Datum.Eof)
    | [Datum.Port p] -> (match Port.peek_char p with
                         | Some c -> Datum.Char (Uchar.of_char c)
                         | None -> Datum.Eof)
    | [_] -> runtime_error "peek-char: expected input port"
    | _ -> runtime_error (Printf.sprintf "peek-char: expected 0 or 1 arguments, got %d" (List.length args)));
  register "read-line" (fun args -> match args with
    | [] -> (match Port.read_line !current_input with
             | Some s -> Datum.Str (Bytes.of_string s)
             | None -> Datum.Eof)
    | [Datum.Port p] -> (match Port.read_line p with
                         | Some s -> Datum.Str (Bytes.of_string s)
                         | None -> Datum.Eof)
    | [_] -> runtime_error "read-line: expected input port"
    | _ -> runtime_error (Printf.sprintf "read-line: expected 0 or 1 arguments, got %d" (List.length args)));
  register "read-string" (fun args -> match args with
    | [Datum.Fixnum n] ->
      let buf = Buffer.create n in
      let rec loop i =
        if i >= n then ()
        else match Port.read_char !current_input with
          | None -> ()
          | Some c -> Buffer.add_char buf c; loop (i + 1)
      in loop 0;
      if Buffer.length buf = 0 then Datum.Eof
      else Datum.Str (Bytes.of_string (Buffer.contents buf))
    | [Datum.Fixnum n; Datum.Port p] ->
      let buf = Buffer.create n in
      let rec loop i =
        if i >= n then ()
        else match Port.read_char p with
          | None -> ()
          | Some c -> Buffer.add_char buf c; loop (i + 1)
      in loop 0;
      if Buffer.length buf = 0 then Datum.Eof
      else Datum.Str (Bytes.of_string (Buffer.contents buf))
    | [_] -> runtime_error "read-string: expected integer"
    | [_; _] -> runtime_error "read-string: expected integer and optional port"
    | _ -> runtime_error (Printf.sprintf "read-string: expected 1 or 2 arguments, got %d" (List.length args)));
  register "read-u8" (fun args -> match args with
    | [] -> (match Port.read_u8 !current_input with
             | Some b -> Datum.Fixnum b
             | None -> Datum.Eof)
    | [Datum.Port p] -> (match Port.read_u8 p with
                         | Some b -> Datum.Fixnum b
                         | None -> Datum.Eof)
    | [_] -> runtime_error "read-u8: expected input port"
    | _ -> runtime_error (Printf.sprintf "read-u8: expected 0 or 1 arguments, got %d" (List.length args)));
  register "peek-u8" (fun args -> match args with
    | [] -> (match Port.peek_u8 !current_input with
             | Some b -> Datum.Fixnum b
             | None -> Datum.Eof)
    | [Datum.Port p] -> (match Port.peek_u8 p with
                         | Some b -> Datum.Fixnum b
                         | None -> Datum.Eof)
    | [_] -> runtime_error "peek-u8: expected input port"
    | _ -> runtime_error (Printf.sprintf "peek-u8: expected 0 or 1 arguments, got %d" (List.length args)));
  register "read-bytevector" (fun args -> match args with
    | [Datum.Fixnum n] ->
      let buf = Bytes.create n in
      let rec loop i =
        if i >= n then i
        else match Port.read_u8 !current_input with
          | None -> i
          | Some b -> Bytes.set buf i (Char.chr b); loop (i + 1)
      in let read = loop 0 in
      if read = 0 then Datum.Eof
      else Datum.Bytevector (Bytes.sub buf 0 read)
    | [Datum.Fixnum n; Datum.Port p] ->
      let buf = Bytes.create n in
      let rec loop i =
        if i >= n then i
        else match Port.read_u8 p with
          | None -> i
          | Some b -> Bytes.set buf i (Char.chr b); loop (i + 1)
      in let read = loop 0 in
      if read = 0 then Datum.Eof
      else Datum.Bytevector (Bytes.sub buf 0 read)
    | [_] -> runtime_error "read-bytevector: expected integer"
    | [_; _] -> runtime_error "read-bytevector: expected integer and optional port"
    | _ -> runtime_error (Printf.sprintf "read-bytevector: expected 1 or 2 arguments, got %d" (List.length args)));
  register "char-ready?" (fun args -> match args with
    | [] -> Datum.Bool true  (* string ports are always ready *)
    | [Datum.Port _] -> Datum.Bool true
    | [_] -> runtime_error "char-ready?: expected input port"
    | _ -> runtime_error (Printf.sprintf "char-ready?: expected 0 or 1 arguments, got %d" (List.length args)));
  (* File I/O *)
  register "open-input-file" (fun args -> match args with
    | [Datum.Str s] -> Datum.Port (Port.open_input_file (Bytes.to_string s))
    | [_] -> runtime_error "open-input-file: expected string"
    | _ -> runtime_error (Printf.sprintf "open-input-file: expected 1 argument, got %d" (List.length args)));
  register "open-output-file" (fun args -> match args with
    | [Datum.Str s] -> Datum.Port (Port.open_output_file (Bytes.to_string s))
    | [_] -> runtime_error "open-output-file: expected string"
    | _ -> runtime_error (Printf.sprintf "open-output-file: expected 1 argument, got %d" (List.length args)));
  register "close-input-port" (fun args -> match args with
    | [Datum.Port p] -> Port.close p; Datum.Void
    | [_] -> runtime_error "close-input-port: expected input port"
    | _ -> runtime_error (Printf.sprintf "close-input-port: expected 1 argument, got %d" (List.length args)));
  register "close-output-port" (fun args -> match args with
    | [Datum.Port p] -> Port.close p; Datum.Void
    | [_] -> runtime_error "close-output-port: expected output port"
    | _ -> runtime_error (Printf.sprintf "close-output-port: expected 1 argument, got %d" (List.length args)));
  register "close-port" (fun args -> match args with
    | [Datum.Port p] -> Port.close p; Datum.Void
    | [_] -> runtime_error "close-port: expected port"
    | _ -> runtime_error (Printf.sprintf "close-port: expected 1 argument, got %d" (List.length args)));
  (* File system *)
  register "file-exists?" (fun args -> match args with
    | [Datum.Str s] -> Datum.Bool (Sys.file_exists (Bytes.to_string s))
    | [_] -> runtime_error "file-exists?: expected string"
    | _ -> runtime_error (Printf.sprintf "file-exists?: expected 1 argument, got %d" (List.length args)));
  register "delete-file" (fun args -> match args with
    | [Datum.Str s] -> Sys.remove (Bytes.to_string s); Datum.Void
    | [_] -> runtime_error "delete-file: expected string"
    | _ -> runtime_error (Printf.sprintf "delete-file: expected 1 argument, got %d" (List.length args)))

(* --- Instance creation --- *)

let boot_definitions = [
  (* with-exception-handler: use dynamic-wind + depth-based truncation
     so that cleanup is safe even if raise already popped the handler *)
  "(define (with-exception-handler handler thunk) \
     (let ((depth (%handler-depth))) \
       (dynamic-wind \
         (lambda () (%push-handler! handler)) \
         thunk \
         (lambda () (%handler-truncate! depth)))))";

  (* raise: pop handler and call it; if it returns, raise again *)
  "(define (raise obj) \
     (if (%handler-stack-empty?) \
       (%fatal-error obj) \
       (let ((handler (%pop-handler!))) \
         (handler obj) \
         (raise (%make-error \"handler returned\" (list obj))))))";

  (* raise-continuable: pop handler, call it, re-push after it returns *)
  "(define (raise-continuable obj) \
     (if (%handler-stack-empty?) \
       (%fatal-error obj) \
       (let ((handler (%pop-handler!))) \
         (let ((result (handler obj))) \
           (%push-handler! handler) \
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

let make_gensym inst () =
  let n = !(inst.gensym_counter) in
  inst.gensym_counter := n + 1;
  Printf.sprintf "%%g%d" n

(* --- Platform features --- *)

let detect_features () =
  let os = match Sys.os_type with
    | "Unix" -> "linux"
    | "Win32" -> "windows"
    | "Cygwin" -> "cygwin"
    | s -> String.lowercase_ascii s
  in
  ["r7rs"; "wile"; os]

(* --- R7RS export names --- *)

let scheme_base_runtime_names = [
  (* Arithmetic *)
  "+"; "-"; "*"; "/"; "="; "<"; ">"; "<="; ">=";
  "abs"; "min"; "max"; "quotient"; "remainder"; "modulo";
  "floor"; "ceiling"; "truncate"; "round";
  "floor-quotient"; "floor-remainder";
  "truncate-quotient"; "truncate-remainder";
  "gcd"; "lcm"; "expt"; "sqrt"; "exact-integer-sqrt";
  "number->string"; "string->number";
  "exact"; "inexact"; "exact->inexact"; "inexact->exact";
  (* Type predicates *)
  "boolean?"; "boolean=?"; "number?"; "complex?"; "real?"; "rational?";
  "integer?"; "exact?"; "inexact?"; "exact-integer?";
  "zero?"; "positive?"; "negative?"; "odd?"; "even?";
  "symbol?"; "symbol=?"; "char?"; "string?"; "vector?";
  "bytevector?"; "procedure?"; "list?"; "pair?"; "null?";
  "eof-object?"; "eof-object"; "error-object?";
  "error-object-message"; "error-object-irritants";
  "read-error?"; "file-error?";
  (* Equivalence *)
  "eq?"; "eqv?"; "equal?"; "not";
  (* Pairs & lists *)
  "cons"; "car"; "cdr"; "set-car!"; "set-cdr!";
  "caar"; "cadr"; "cdar"; "cddr";
  "list"; "make-list"; "length"; "append"; "reverse";
  "list-tail"; "list-ref"; "list-set!"; "list-copy";
  "memq"; "memv"; "member"; "assq"; "assv"; "assoc";
  (* Symbols *)
  "symbol->string"; "string->symbol";
  (* Characters *)
  "char=?"; "char<?"; "char>?"; "char<=?"; "char>=?";
  "char->integer"; "integer->char";
  "char-upcase"; "char-downcase"; "char-foldcase";
  "char-alphabetic?"; "char-numeric?"; "char-whitespace?";
  "char-upper-case?"; "char-lower-case?"; "digit-value";
  (* Strings *)
  "make-string"; "string"; "string-length"; "string-ref"; "string-set!";
  "string=?"; "string<?"; "string>?"; "string<=?"; "string>=?";
  "substring"; "string-append"; "string->list"; "list->string";
  "string-copy"; "string-copy!"; "string-fill!";
  "string-upcase"; "string-downcase"; "string-foldcase";
  (* Vectors *)
  "make-vector"; "vector"; "vector-length"; "vector-ref"; "vector-set!";
  "vector->list"; "list->vector"; "vector-copy"; "vector-copy!";
  "vector-append"; "vector-fill!"; "vector->string"; "string->vector";
  (* Bytevectors *)
  "make-bytevector"; "bytevector"; "bytevector-length";
  "bytevector-u8-ref"; "bytevector-u8-set!";
  "bytevector-copy"; "bytevector-copy!"; "bytevector-append";
  "utf8->string"; "string->utf8";
  (* I/O *)
  "display"; "write"; "newline";
  (* Ports *)
  "port?"; "input-port?"; "output-port?";
  "input-port-open?"; "output-port-open?";
  "textual-port?"; "binary-port?";
  "current-input-port"; "current-output-port"; "current-error-port";
  "open-input-string"; "open-output-string"; "get-output-string";
  "write-char"; "write-string"; "write-u8"; "write-bytevector";
  "flush-output-port";
  "read-char"; "peek-char"; "read-line"; "read-string";
  "read-u8"; "peek-u8"; "read-bytevector"; "char-ready?";
  "open-input-file"; "open-output-file";
  "close-input-port"; "close-output-port"; "close-port";
  "file-exists?"; "delete-file";
  "call-with-port";
  "call-with-input-file"; "call-with-output-file";
  "with-input-from-file"; "with-output-to-file";
  "read";
  (* Control *)
  "apply"; "call/cc"; "call-with-current-continuation";
  "call-with-values"; "values"; "dynamic-wind";
  (* Exceptions (boot-defined) *)
  "with-exception-handler"; "raise"; "raise-continuable"; "error";
  (* Higher-order (boot-defined) *)
  "map"; "for-each"; "string-map"; "string-for-each";
  "vector-map"; "vector-for-each";
]

let scheme_base_syntax_names = [
  "if"; "lambda"; "define"; "set!"; "begin"; "quote";
  "let"; "let*"; "letrec"; "letrec*";
  "cond"; "case"; "do"; "and"; "or"; "when"; "unless";
  "define-syntax"; "let-syntax"; "letrec-syntax";
  "quasiquote"; "guard"; "define-record-type"; "syntax-error";
]

let scheme_char_names = [
  "char-ci=?"; "char-ci<?"; "char-ci>?"; "char-ci<=?"; "char-ci>=?";
  "char-alphabetic?"; "char-numeric?"; "char-whitespace?";
  "char-upper-case?"; "char-lower-case?";
  "char-upcase"; "char-downcase"; "char-foldcase";
  "digit-value";
]

let scheme_write_names = [
  "display"; "write"; "newline";
  "write-shared"; "write-simple";
]

let scheme_cxr_names = [ "caar"; "cadr"; "cdar"; "cddr" ]

let scheme_file_names = [
  "open-input-file"; "open-output-file";
  "close-input-port"; "close-output-port"; "close-port";
  "call-with-input-file"; "call-with-output-file";
  "with-input-from-file"; "with-output-to-file";
  "file-exists?"; "delete-file";
]

let scheme_read_names = [ "read" ]

let build_library inst name runtime_names syntax_names =
  let exports = Hashtbl.create (List.length runtime_names) in
  List.iter (fun rname ->
    let sym = Symbol.intern inst.symbols rname in
    match Env.lookup_slot inst.global_env sym with
    | Some slot -> Hashtbl.replace exports rname (Symbol.id sym, slot)
    | None -> ()  (* silently skip unimplemented *)
  ) runtime_names;
  let syntax_exports = Hashtbl.create (List.length syntax_names) in
  List.iter (fun sname ->
    match Expander.lookup_binding inst.syn_env sname with
    | Some b -> Hashtbl.replace syntax_exports sname b
    | None -> ()  (* silently skip *)
  ) syntax_names;
  let lib : Library.t = {
    name;
    env = inst.global_env;
    exports;
    syntax_exports;
  } in
  Library.register inst.libraries lib

let register_builtin_libraries inst =
  build_library inst ["scheme"; "base"]
    scheme_base_runtime_names scheme_base_syntax_names;
  build_library inst ["scheme"; "char"]
    scheme_char_names [];
  build_library inst ["scheme"; "write"]
    scheme_write_names [];
  build_library inst ["scheme"; "cxr"]
    scheme_cxr_names [];
  build_library inst ["scheme"; "file"]
    scheme_file_names [];
  build_library inst ["scheme"; "read"]
    scheme_read_names []

(* --- Expander callbacks --- *)

(* Forward reference for library lookup that supports auto-loading from .sld files.
   Initialized to basic registry lookup; updated to lookup_or_load after it's defined. *)
let lib_lookup_ref : (t -> string list -> Library.t option) ref =
  ref (fun inst name -> Library.lookup inst.libraries name)

let make_expander_callbacks inst =
  let features = inst.features in
  let has_library name = !lib_lookup_ref inst name <> None in
  let read_include ~fold_case filename =
    let port = Port.of_file filename in
    let rt = if fold_case then Readtable.with_fold_case true inst.readtable
             else inst.readtable in
    let rec read_all acc =
      let s = Reader.read_syntax rt port in
      if s.datum = Syntax.Eof then List.rev acc
      else read_all (s :: acc)
    in
    read_all []
  in
  (features, has_library, read_include)

let expand_with_callbacks inst expr =
  let (features, has_library, read_include) = make_expander_callbacks inst in
  Expander.expand ~syn_env:inst.syn_env ~gensym:(make_gensym inst)
    ~features ~has_library ~read_include expr

(* --- Evaluation --- *)

let eval_boot inst src =
  let port = Port.of_string src in
  let expr = Reader.read_syntax inst.readtable port in
  let expanded = expand_with_callbacks inst expr in
  let code = Compiler.compile inst.symbols expanded in
  ignore (Vm.execute ~winds:inst.winds inst.global_env code)

let call inst proc args =
  let n = List.length args in
  let constants = Array.of_list (args @ [proc]) in
  let instrs =
    let buf = Array.make (2 * n + 3) Opcode.Halt in
    List.iteri (fun i _ ->
      buf.(2 * i) <- Opcode.Const i;
      buf.(2 * i + 1) <- Opcode.Push) args;
    buf.(2 * n) <- Opcode.Const n;
    buf.(2 * n + 1) <- Opcode.Call n;
    buf
  in
  let code : Datum.code = {
    instructions = instrs; constants; symbols = [||]; children = [||];
    params = [||]; variadic = false; name = "<call>";
  } in
  Vm.execute ~winds:inst.winds inst.global_env code

let create ?(readtable = Readtable.default) () =
  let symbols = Symbol.create_table () in
  let global_env = Env.empty () in
  let handlers = ref [] in
  let current_input = ref (Port.of_in_channel ~file:"<stdin>" stdin) in
  let current_output = ref (Port.of_out_channel ~file:"<stdout>" stdout) in
  let current_error = ref (Port.of_out_channel ~file:"<stderr>" stderr) in
  register_primitives symbols global_env handlers
    ~current_input ~current_output ~current_error;
  let syn_env = Expander.core_env () in
  let gensym_counter = ref 0 in
  let libraries = Library.create_registry () in
  let features = detect_features () in
  let inst = { symbols; global_env; readtable; winds = ref []; handlers;
               syn_env; gensym_counter; libraries;
               search_paths = ref []; features;
               loading_libs = ref []; fasl_cache = ref false;
               current_input; current_output; current_error } in
  List.iter (eval_boot inst) boot_definitions;
  (* Register higher-order port procedures that need the instance *)
  let register_late name fn =
    let sym = Symbol.intern inst.symbols name in
    Env.define inst.global_env sym (make_prim name fn);
    Expander.define_binding inst.syn_env name Expander.var_binding
  in
  register_late "call-with-port" (fun args -> match args with
    | [Datum.Port p; proc] ->
      let result = call inst proc [Datum.Port p] in
      Port.close p;
      result
    | _ -> runtime_error "call-with-port: expected port and procedure");
  register_late "call-with-input-file" (fun args -> match args with
    | [Datum.Str s; proc] ->
      let p = Port.open_input_file (Bytes.to_string s) in
      Fun.protect ~finally:(fun () -> Port.close p) (fun () ->
        call inst proc [Datum.Port p])
    | _ -> runtime_error "call-with-input-file: expected string and procedure");
  register_late "call-with-output-file" (fun args -> match args with
    | [Datum.Str s; proc] ->
      let p = Port.open_output_file (Bytes.to_string s) in
      Fun.protect ~finally:(fun () -> Port.close p) (fun () ->
        call inst proc [Datum.Port p])
    | _ -> runtime_error "call-with-output-file: expected string and procedure");
  register_late "with-input-from-file" (fun args -> match args with
    | [Datum.Str s; proc] ->
      let p = Port.open_input_file (Bytes.to_string s) in
      let saved = !(inst.current_input) in
      inst.current_input := p;
      Fun.protect ~finally:(fun () ->
        inst.current_input := saved;
        Port.close p) (fun () ->
        call inst proc [])
    | _ -> runtime_error "with-input-from-file: expected string and procedure");
  register_late "with-output-to-file" (fun args -> match args with
    | [Datum.Str s; proc] ->
      let p = Port.open_output_file (Bytes.to_string s) in
      let saved = !(inst.current_output) in
      inst.current_output := p;
      Fun.protect ~finally:(fun () ->
        inst.current_output := saved;
        Port.close p) (fun () ->
        call inst proc [])
    | _ -> runtime_error "with-output-to-file: expected string and procedure");
  register_late "read" (fun args ->
    let port = match args with
      | [] -> !(inst.current_input)
      | [Datum.Port p] -> p
      | [_] -> runtime_error "read: expected input port"
      | _ -> runtime_error (Printf.sprintf "read: expected 0 or 1 arguments, got %d" (List.length args))
    in
    let s = Reader.read_syntax inst.readtable port in
    match s.Syntax.datum with
    | Syntax.Eof -> Datum.Eof
    | _ -> Syntax.to_datum s);
  register_builtin_libraries inst;
  inst

let intern inst name = Symbol.intern inst.symbols name

(* --- Import processing --- *)

let rec syntax_to_proper_list s =
  match s.Syntax.datum with
  | Syntax.Nil -> Some []
  | Syntax.Pair (car, cdr) ->
    (match syntax_to_proper_list cdr with
     | Some rest -> Some (car :: rest)
     | None -> None)
  | _ -> None


(* --- define-library processing --- *)

let rec syntax_list_to_list s =
  match s.Syntax.datum with
  | Syntax.Nil -> []
  | Syntax.Pair (car, cdr) -> car :: syntax_list_to_list cdr
  | _ -> [s]

let process_define_library ?sld_path inst name_syn decls =
  let loc = name_syn.Syntax.loc in
  let lib_name = Library.parse_library_name name_syn in
  (* Create a fresh env and syn_env for the library *)
  let lib_env = Env.empty () in
  let lib_syn_env = Expander.core_env () in
  let lib_gensym_counter = ref 0 in
  let make_lib_gensym () =
    let n = !lib_gensym_counter in
    lib_gensym_counter := n + 1;
    Printf.sprintf "%%lib%d" n
  in
  let expand_in_lib expr =
    let features = inst.features in
    let has_library name = !lib_lookup_ref inst name <> None in
    let read_include ~fold_case filename =
      let port = Port.of_file filename in
      let rt = if fold_case then Readtable.with_fold_case true inst.readtable
               else inst.readtable in
      let rec read_all acc =
        let s = Reader.read_syntax rt port in
        if s.datum = Syntax.Eof then List.rev acc
        else read_all (s :: acc)
      in
      read_all []
    in
    Expander.expand ~syn_env:lib_syn_env ~gensym:make_lib_gensym
      ~features ~has_library ~read_include expr
  in
  let tracking = sld_path <> None && !(inst.fasl_cache) in
  let fasl_decls = ref [] in
  let eval_in_lib expr =
    let expanded = expand_in_lib expr in
    let code = Compiler.compile inst.symbols expanded in
    if tracking then fasl_decls := Fasl.Lib_code code :: !fasl_decls;
    ignore (Vm.execute ~winds:inst.winds lib_env code)
  in
  let eval_forms_in_lib forms =
    List.iter eval_in_lib forms
  in
  (* Collect export specs and process declarations *)
  let export_specs = ref [] in
  let rec process_decl decl =
    let parts = syntax_list_to_list decl in
    match parts with
    | { datum = Syntax.Symbol "export"; _ } :: specs ->
      List.iter (fun spec ->
        export_specs := Library.parse_export_spec spec :: !export_specs
      ) specs
    | { datum = Syntax.Symbol "import"; _ } :: import_sets ->
      (* Import into the library env *)
      List.iter (fun iset_syntax ->
        let iset = Library.parse_import_set iset_syntax in
        if tracking then fasl_decls := Fasl.Lib_import iset :: !fasl_decls;
        let lookup_fn name = !lib_lookup_ref inst name in
        let (rt_bindings, syn_bindings) = Library.resolve_import lookup_fn iset in
        List.iter (fun (name, _id, slot) ->
          let sym = Symbol.intern inst.symbols name in
          Env.define_slot lib_env sym slot
        ) rt_bindings;
        List.iter (fun (name, binding) ->
          Expander.define_binding lib_syn_env name binding
        ) syn_bindings
      ) import_sets
    | { datum = Syntax.Symbol "begin"; _ } :: body ->
      eval_forms_in_lib body
    | { datum = Syntax.Symbol "include"; _ } :: filenames ->
      let forms = List.concat_map (fun fn ->
        match fn.Syntax.datum with
        | Syntax.Str filename ->
          let port = Port.of_file filename in
          let rec read_all acc =
            let s = Reader.read_syntax inst.readtable port in
            if s.datum = Syntax.Eof then List.rev acc
            else read_all (s :: acc)
          in
          read_all []
        | _ -> raise (Compiler.Compile_error (fn.Syntax.loc,
            "include: expected string filename"))
      ) filenames in
      eval_forms_in_lib forms
    | { datum = Syntax.Symbol "include-ci"; _ } :: filenames ->
      let rt = Readtable.with_fold_case true inst.readtable in
      let forms = List.concat_map (fun fn ->
        match fn.Syntax.datum with
        | Syntax.Str filename ->
          let port = Port.of_file filename in
          let rec read_all acc =
            let s = Reader.read_syntax rt port in
            if s.datum = Syntax.Eof then List.rev acc
            else read_all (s :: acc)
          in
          read_all []
        | _ -> raise (Compiler.Compile_error (fn.Syntax.loc,
            "include-ci: expected string filename"))
      ) filenames in
      eval_forms_in_lib forms
    | { datum = Syntax.Symbol "cond-expand"; _ } :: _ ->
      (* Expand cond-expand, then process each resulting declaration *)
      let expanded = expand_in_lib decl in
      (* The result should be (begin decl ...) *)
      let inner = syntax_list_to_list expanded in
      (match inner with
       | { datum = Syntax.Symbol "begin"; _ } :: rest ->
         List.iter process_decl rest
       | _ -> process_decl expanded)
    | _ ->
      raise (Compiler.Compile_error (loc,
        "define-library: unknown declaration"))
  in
  List.iter process_decl decls;
  (* Build export tables *)
  let exports = Hashtbl.create 16 in
  let syntax_exports = Hashtbl.create 16 in
  List.iter (fun spec ->
    let (internal_name, external_name) = match spec with
      | Library.Export_id name -> (name, name)
      | Library.Export_rename (internal, external_) -> (internal, external_)
    in
    let sym = Symbol.intern inst.symbols internal_name in
    (match Env.lookup_slot lib_env sym with
     | Some slot -> Hashtbl.replace exports external_name (Symbol.id sym, slot)
     | None ->
       (match Expander.lookup_binding lib_syn_env internal_name with
        | Some b -> Hashtbl.replace syntax_exports external_name b
        | None ->
          raise (Compiler.Compile_error (loc,
            Printf.sprintf "define-library: exported name not defined: %s"
              internal_name))))
  ) !export_specs;
  (* Register the library *)
  let lib : Library.t = {
    name = lib_name;
    env = lib_env;
    exports;
    syntax_exports;
  } in
  Library.register inst.libraries lib;
  (* Write FASL cache if loaded from .sld file and caching is enabled *)
  (match sld_path with
   | Some path when !(inst.fasl_cache) ->
     let has_syn = Hashtbl.length syntax_exports > 0 in
     let fasl : Fasl.lib_fasl = {
       lib_name;
       has_syntax_exports = has_syn;
       exports = List.rev !export_specs;
       declarations = List.rev !fasl_decls;
     } in
     let fasl_path = Fasl.fasl_path_for path in
     (try Fasl.write_lib_fasl fasl_path fasl with Fasl.Fasl_error _ -> ())
   | _ -> ())

(* --- Top-level form classification --- *)

type top_level_form =
  | Import of Syntax.t list
  | Define_library of Syntax.t * Syntax.t list
  | Expression

let classify_top_level (expr : Syntax.t) =
  match expr.datum with
  | Syntax.Pair ({ datum = Syntax.Symbol "import"; _ }, rest) ->
    (match syntax_to_proper_list rest with
     | Some sets when sets <> [] -> Import sets
     | _ -> Expression)
  | Syntax.Pair ({ datum = Syntax.Symbol "define-library"; _ }, rest) ->
    (match syntax_to_proper_list rest with
     | Some (name :: decls) -> Define_library (name, decls)
     | _ -> Expression)
  | _ -> Expression

(* --- Import processing with auto-loading --- *)

let lib_name_to_path search_dir name =
  Filename.concat search_dir
    (String.concat Filename.dir_sep name ^ ".sld")

let replay_lib_fasl inst (fasl : Fasl.lib_fasl) =
  let lib_env = Env.empty () in
  List.iter (fun decl ->
    match decl with
    | Fasl.Lib_import iset ->
      let lookup_fn name = !lib_lookup_ref inst name in
      let (rt_bindings, _syn_bindings) = Library.resolve_import lookup_fn iset in
      List.iter (fun (name, _id, slot) ->
        let sym = Symbol.intern inst.symbols name in
        Env.define_slot lib_env sym slot
      ) rt_bindings
    | Fasl.Lib_code code ->
      ignore (Vm.execute ~winds:inst.winds lib_env code)
  ) fasl.declarations;
  let exports = Hashtbl.create 16 in
  List.iter (fun spec ->
    let (internal_name, external_name) = match spec with
      | Library.Export_id name -> (name, name)
      | Library.Export_rename (internal, external_) -> (internal, external_)
    in
    let sym = Symbol.intern inst.symbols internal_name in
    match Env.lookup_slot lib_env sym with
    | Some slot -> Hashtbl.replace exports external_name (Symbol.id sym, slot)
    | None ->
      failwith (Printf.sprintf
        "FASL replay: exported name not defined: %s" internal_name)
  ) fasl.exports;
  let lib : Library.t = {
    name = fasl.lib_name;
    env = lib_env;
    exports;
    syntax_exports = Hashtbl.create 0;
  } in
  Library.register inst.libraries lib

let with_loading_guard inst name fn =
  inst.loading_libs := name :: !(inst.loading_libs);
  Fun.protect ~finally:(fun () ->
    inst.loading_libs :=
      List.filter (fun n -> n <> name) !(inst.loading_libs))
    fn

let load_from_source inst name sld_path =
  with_loading_guard inst name (fun () ->
    let port = Port.of_file sld_path in
    let expr = Reader.read_syntax inst.readtable port in
    match classify_top_level expr with
    | Define_library (name_syn, decls) ->
      process_define_library ~sld_path inst name_syn decls
    | _ ->
      failwith (Printf.sprintf
        "library file %s does not contain define-library" sld_path))

let try_load_library inst name =
  if List.mem name !(inst.loading_libs) then
    failwith ("circular library dependency: " ^ Library.name_to_string name);
  let paths = List.filter_map (fun dir ->
    let path = lib_name_to_path dir name in
    if Sys.file_exists path then Some path else None
  ) !(inst.search_paths) in
  match paths with
  | [] -> ()  (* no file found, will fail later at resolve_import *)
  | sld_path :: _ ->
    let fasl_path = Fasl.fasl_path_for sld_path in
    if !(inst.fasl_cache) && Fasl.is_cache_valid ~sld_path ~fasl_path then begin
      (* Try loading from FASL cache *)
      try
        let fasl = Fasl.read_lib_fasl inst.symbols fasl_path in
        if fasl.has_syntax_exports then
          load_from_source inst name sld_path
        else
          with_loading_guard inst name (fun () ->
            replay_lib_fasl inst fasl)
      with Fasl.Fasl_error _ ->
        load_from_source inst name sld_path
    end else
      load_from_source inst name sld_path

let lookup_or_load inst name =
  match Library.lookup inst.libraries name with
  | Some lib -> Some lib
  | None ->
    try_load_library inst name;
    Library.lookup inst.libraries name

let () = lib_lookup_ref := lookup_or_load

let process_import_set inst iset =
  let lookup_fn name = lookup_or_load inst name in
  let (rt_bindings, syn_bindings) = Library.resolve_import lookup_fn iset in
  List.iter (fun (name, _id, slot) ->
    let sym = Symbol.intern inst.symbols name in
    Env.define_slot inst.global_env sym slot
  ) rt_bindings;
  List.iter (fun (name, binding) ->
    Expander.define_binding inst.syn_env name binding
  ) syn_bindings

let process_imports inst import_sets =
  List.iter (fun iset_syntax ->
    let iset = Library.parse_import_set iset_syntax in
    process_import_set inst iset
  ) import_sets

(* --- Evaluation --- *)

let lookup inst name =
  let sym = Symbol.intern inst.symbols name in
  Env.lookup inst.global_env sym

let define_primitive inst name fn =
  let sym = Symbol.intern inst.symbols name in
  let prim = Datum.Primitive { prim_name = name; prim_fn = fn; prim_intrinsic = None } in
  Env.define inst.global_env sym prim;
  Expander.define_binding inst.syn_env name (Expander.var_binding)

let eval_syntax inst expr =
  match classify_top_level expr with
  | Import import_sets ->
    process_imports inst import_sets;
    Datum.Void
  | Define_library (name_syn, decls) ->
    process_define_library inst name_syn decls;
    Datum.Void
  | Expression ->
    let expanded = expand_with_callbacks inst expr in
    let code = Compiler.compile inst.symbols expanded in
    Vm.execute ~winds:inst.winds inst.global_env code

let eval_datum inst d =
  let expr = Syntax.from_datum Loc.none d in
  eval_syntax inst expr

let eval_string inst src =
  let port = Port.of_string src in
  let expr = Reader.read_syntax inst.readtable port in
  eval_syntax inst expr

let eval_port inst port =
  let rec loop last =
    let expr = Reader.read_syntax inst.readtable port in
    match expr with
    | { Syntax.datum = Syntax.Eof; _ } -> last
    | _ -> loop (eval_syntax inst expr)
  in
  loop Datum.Void

let load_file inst path =
  let port = Port.of_file path in
  ignore (eval_port inst port)

let load_fasl inst path =
  let code = Fasl.read_code_from_file inst.symbols path in
  ignore (Vm.execute ~winds:inst.winds inst.global_env code)

(* --- Ahead-of-time compilation --- *)

let compile_port inst port =
  let decls = ref [] in
  let rec loop () =
    let expr = Reader.read_syntax inst.readtable port in
    match expr with
    | { Syntax.datum = Syntax.Eof; _ } -> ()
    | _ ->
      (match classify_top_level expr with
       | Import import_sets ->
         List.iter (fun iset_syntax ->
           let iset = Library.parse_import_set iset_syntax in
           process_import_set inst iset;
           decls := Fasl.Lib_import iset :: !decls
         ) import_sets
       | Define_library (name_syn, lib_decls) ->
         process_define_library inst name_syn lib_decls
       | Expression ->
         let expanded = expand_with_callbacks inst expr in
         let code = Compiler.compile inst.symbols expanded in
         decls := Fasl.Lib_code code :: !decls);
      loop ()
  in
  loop ();
  { Fasl.declarations = List.rev !decls }

let run_program inst prog =
  let last = ref Datum.Void in
  List.iter (fun decl ->
    match decl with
    | Fasl.Lib_import iset ->
      process_import_set inst iset
    | Fasl.Lib_code code ->
      last := Vm.execute ~winds:inst.winds inst.global_env code
  ) prog.Fasl.declarations;
  !last

(* --- Package integration --- *)

let setup_package_paths inst ~registry_root pkg =
  let resolved = Pkg_manager.resolve ~registry_root pkg.Package.depends in
  let dep_paths = Pkg_manager.search_paths_for ~registry_root resolved in
  (* The package's own src/ dir is determined from its package.scm location,
     but since we receive the parsed Package.t, we compute it from the
     registry if the package is installed there, or the caller provides
     the appropriate src/ path via search_paths. *)
  inst.search_paths := dep_paths @ !(inst.search_paths)
