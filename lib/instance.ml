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
  command_line : string list ref;
  eval_envs : (int, Datum.env * Expander.syn_env) Hashtbl.t;
  eval_env_counter : int ref;
  extension_lib_env : (Env.t * Expander.syn_env) option ref;
  on_call : (Loc.t -> Datum.t -> Datum.t list -> unit) option ref;
  on_return : (Loc.t -> Datum.t -> unit) option ref;
  debug_state : Vm.debug_state option ref;
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

(* --- Inexact math primitives --- *)

let prim_exp args =
  match args with
  | [v] -> Datum.Flonum (Float.exp (as_flonum "exp" v))
  | _ -> runtime_error (Printf.sprintf "exp: expected 1 argument, got %d" (List.length args))

let prim_log args =
  match args with
  | [v] -> Datum.Flonum (Float.log (as_flonum "log" v))
  | [v; base] ->
    Datum.Flonum (Float.log (as_flonum "log" v) /. Float.log (as_flonum "log" base))
  | _ -> runtime_error (Printf.sprintf "log: expected 1 or 2 arguments, got %d" (List.length args))

let prim_sin args =
  match args with
  | [v] -> Datum.Flonum (Float.sin (as_flonum "sin" v))
  | _ -> runtime_error (Printf.sprintf "sin: expected 1 argument, got %d" (List.length args))

let prim_cos args =
  match args with
  | [v] -> Datum.Flonum (Float.cos (as_flonum "cos" v))
  | _ -> runtime_error (Printf.sprintf "cos: expected 1 argument, got %d" (List.length args))

let prim_tan args =
  match args with
  | [v] -> Datum.Flonum (Float.tan (as_flonum "tan" v))
  | _ -> runtime_error (Printf.sprintf "tan: expected 1 argument, got %d" (List.length args))

let prim_asin args =
  match args with
  | [v] -> Datum.Flonum (Float.asin (as_flonum "asin" v))
  | _ -> runtime_error (Printf.sprintf "asin: expected 1 argument, got %d" (List.length args))

let prim_acos args =
  match args with
  | [v] -> Datum.Flonum (Float.acos (as_flonum "acos" v))
  | _ -> runtime_error (Printf.sprintf "acos: expected 1 argument, got %d" (List.length args))

let prim_atan args =
  match args with
  | [v] -> Datum.Flonum (Float.atan (as_flonum "atan" v))
  | [y; x] -> Datum.Flonum (Float.atan2 (as_flonum "atan" y) (as_flonum "atan" x))
  | _ -> runtime_error (Printf.sprintf "atan: expected 1 or 2 arguments, got %d" (List.length args))

let prim_finite args =
  match args with
  | [Datum.Fixnum _] -> Datum.Bool true
  | [Datum.Flonum f] -> Datum.Bool (Float.is_finite f)
  | [_] -> runtime_error "finite?: expected number"
  | _ -> runtime_error (Printf.sprintf "finite?: expected 1 argument, got %d" (List.length args))

let prim_infinite args =
  match args with
  | [Datum.Fixnum _] -> Datum.Bool false
  | [Datum.Flonum f] -> Datum.Bool (Float.is_infinite f)
  | [_] -> runtime_error "infinite?: expected number"
  | _ -> runtime_error (Printf.sprintf "infinite?: expected 1 argument, got %d" (List.length args))

let prim_nan args =
  match args with
  | [Datum.Fixnum _] -> Datum.Bool false
  | [Datum.Flonum f] -> Datum.Bool (Float.is_nan f)
  | [_] -> runtime_error "nan?: expected number"
  | _ -> runtime_error (Printf.sprintf "nan?: expected 1 argument, got %d" (List.length args))

(* --- Complex stubs (reals only) --- *)

let prim_real_part args =
  match args with
  | [Datum.Fixnum _ as v] -> v
  | [Datum.Flonum _ as v] -> v
  | [_] -> runtime_error "real-part: expected number"
  | _ -> runtime_error (Printf.sprintf "real-part: expected 1 argument, got %d" (List.length args))

let prim_imag_part args =
  match args with
  | [Datum.Fixnum _] -> Datum.Fixnum 0
  | [Datum.Flonum _] -> Datum.Flonum 0.0
  | [_] -> runtime_error "imag-part: expected number"
  | _ -> runtime_error (Printf.sprintf "imag-part: expected 1 argument, got %d" (List.length args))

let prim_magnitude args =
  match args with
  | [Datum.Fixnum n] -> Datum.Fixnum (abs n)
  | [Datum.Flonum f] -> Datum.Flonum (Float.abs f)
  | [_] -> runtime_error "magnitude: expected number"
  | _ -> runtime_error (Printf.sprintf "magnitude: expected 1 argument, got %d" (List.length args))

let prim_angle args =
  match args with
  | [Datum.Fixnum n] -> Datum.Flonum (if n >= 0 then 0.0 else Float.pi)
  | [Datum.Flonum f] -> Datum.Flonum (if f >= 0.0 then 0.0 else Float.pi)
  | [_] -> runtime_error "angle: expected number"
  | _ -> runtime_error (Printf.sprintf "angle: expected 1 argument, got %d" (List.length args))

let prim_make_rectangular args =
  match args with
  | [real; imag] ->
    let i = as_flonum "make-rectangular" imag in
    if i = 0.0 then real
    else runtime_error "make-rectangular: complex numbers not supported"
  | _ -> runtime_error (Printf.sprintf "make-rectangular: expected 2 arguments, got %d" (List.length args))

let prim_make_polar args =
  match args with
  | [r; theta] ->
    let rf = as_flonum "make-polar" r in
    let tf = as_flonum "make-polar" theta in
    let real = rf *. Float.cos tf in
    let imag = rf *. Float.sin tf in
    if Float.abs imag < 1e-15 then
      Datum.Flonum real
    else runtime_error "make-polar: complex numbers not supported"
  | _ -> runtime_error (Printf.sprintf "make-polar: expected 2 arguments, got %d" (List.length args))

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
  (* Inexact math *)
  register "exp" prim_exp;
  register "log" prim_log;
  register "sin" prim_sin;
  register "cos" prim_cos;
  register "tan" prim_tan;
  register "asin" prim_asin;
  register "acos" prim_acos;
  register "atan" prim_atan;
  register "finite?" prim_finite;
  register "infinite?" prim_infinite;
  register "nan?" prim_nan;
  (* Complex stubs *)
  register "real-part" prim_real_part;
  register "imag-part" prim_imag_part;
  register "magnitude" prim_magnitude;
  register "angle" prim_angle;
  register "make-rectangular" prim_make_rectangular;
  register "make-polar" prim_make_polar;
  (* Lazy *)
  register "%make-promise" (fun args -> match args with
    | [Datum.Bool done_; v] ->
      Datum.Promise { promise_done = done_; promise_value = v }
    | _ -> runtime_error "%make-promise: expected 2 arguments");
  register "make-promise" (fun args -> match args with
    | [Datum.Promise _ as p] -> p
    | [v] -> Datum.Promise { promise_done = true; promise_value = v }
    | _ -> runtime_error (Printf.sprintf "make-promise: expected 1 argument, got %d" (List.length args)));
  register "promise?" (fun args -> match args with
    | [Datum.Promise _] -> Datum.Bool true
    | [_] -> Datum.Bool false
    | _ -> runtime_error (Printf.sprintf "promise?: expected 1 argument, got %d" (List.length args)));
  (* Process context *)
  register "exit" (fun args -> match args with
    | [] -> Stdlib.exit 0
    | [Datum.Bool true] -> Stdlib.exit 0
    | [Datum.Bool false] -> Stdlib.exit 1
    | [Datum.Fixnum n] -> Stdlib.exit n
    | [_] -> runtime_error "exit: expected boolean or integer"
    | _ -> runtime_error (Printf.sprintf "exit: expected 0 or 1 arguments, got %d" (List.length args)));
  register "emergency-exit" (fun args -> match args with
    | [] -> Stdlib.exit 0
    | [Datum.Bool true] -> Stdlib.exit 0
    | [Datum.Bool false] -> Stdlib.exit 1
    | [Datum.Fixnum n] -> Stdlib.exit n
    | [_] -> runtime_error "emergency-exit: expected boolean or integer"
    | _ -> runtime_error (Printf.sprintf "emergency-exit: expected 0 or 1 arguments, got %d" (List.length args)));
  register "get-environment-variable" (fun args -> match args with
    | [Datum.Str s] ->
      (match Sys.getenv_opt (Bytes.to_string s) with
       | Some v -> Datum.Str (Bytes.of_string v)
       | None -> Datum.Bool false)
    | [_] -> runtime_error "get-environment-variable: expected string"
    | _ -> runtime_error (Printf.sprintf "get-environment-variable: expected 1 argument, got %d" (List.length args)));
  register "get-environment-variables" (fun args -> match args with
    | [] ->
      let env = Unix.environment () in
      let pairs = Array.to_list (Array.map (fun s ->
        match String.split_on_char '=' s with
        | key :: rest ->
          Datum.Pair { car = Datum.Str (Bytes.of_string key);
                       cdr = Datum.Str (Bytes.of_string (String.concat "=" rest)) }
        | [] -> Datum.Pair { car = Datum.Str Bytes.empty; cdr = Datum.Str Bytes.empty }
      ) env) in
      Datum.list_of pairs
    | _ -> runtime_error (Printf.sprintf "get-environment-variables: expected 0 arguments, got %d" (List.length args)));
  (* Time *)
  register "current-second" (fun args -> match args with
    | [] -> Datum.Flonum (Unix.gettimeofday ())
    | _ -> runtime_error (Printf.sprintf "current-second: expected 0 arguments, got %d" (List.length args)));
  register "current-jiffy" (fun args -> match args with
    | [] -> Datum.Fixnum (int_of_float (Unix.gettimeofday () *. 1e6))
    | _ -> runtime_error (Printf.sprintf "current-jiffy: expected 0 arguments, got %d" (List.length args)));
  register "jiffies-per-second" (fun args -> match args with
    | [] -> Datum.Fixnum 1_000_000
    | _ -> runtime_error (Printf.sprintf "jiffies-per-second: expected 0 arguments, got %d" (List.length args)));
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
  ["r7rs"; "wile"; os] @ Srfi.bundled_features

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
  "let-values"; "let*-values";
]

let scheme_char_names = [
  "char-ci=?"; "char-ci<?"; "char-ci>?"; "char-ci<=?"; "char-ci>=?";
  "char-alphabetic?"; "char-numeric?"; "char-whitespace?";
  "char-upper-case?"; "char-lower-case?";
  "char-upcase"; "char-downcase"; "char-foldcase";
  "digit-value";
  "string-ci=?"; "string-ci<?"; "string-ci>?"; "string-ci<=?"; "string-ci>=?";
  "string-upcase"; "string-downcase"; "string-foldcase";
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

let scheme_inexact_names = [
  "exp"; "log"; "sin"; "cos"; "tan"; "asin"; "acos"; "atan";
  "sqrt"; "finite?"; "infinite?"; "nan?"
]

let scheme_complex_names = [
  "real-part"; "imag-part"; "magnitude"; "angle";
  "make-rectangular"; "make-polar"
]

let scheme_lazy_names = [ "force"; "make-promise"; "promise?"; "%make-promise" ]
let scheme_lazy_syntax_names = [ "delay"; "delay-force" ]

let scheme_case_lambda_syntax_names = [ "case-lambda" ]

let scheme_process_context_names = [
  "command-line"; "exit"; "emergency-exit";
  "get-environment-variable"; "get-environment-variables"
]

let scheme_time_names = [
  "current-second"; "current-jiffy"; "jiffies-per-second"
]

let scheme_eval_names = [ "environment"; "eval" ]
let scheme_load_names = [ "load" ]
let scheme_repl_names = [ "interaction-environment" ]

let scheme_r5rs_runtime_names = [
  (* Arithmetic *)
  "+"; "-"; "*"; "/"; "="; "<"; ">"; "<="; ">=";
  "abs"; "min"; "max"; "quotient"; "remainder"; "modulo";
  "floor"; "ceiling"; "truncate"; "round";
  "gcd"; "lcm"; "expt"; "sqrt";
  "number->string"; "string->number";
  "exact->inexact"; "inexact->exact";
  (* Inexact *)
  "exp"; "log"; "sin"; "cos"; "tan"; "asin"; "acos"; "atan";
  (* Complex *)
  "real-part"; "imag-part"; "magnitude"; "angle";
  "make-rectangular"; "make-polar";
  (* Type predicates *)
  "boolean?"; "number?"; "complex?"; "real?"; "rational?";
  "integer?"; "exact?"; "inexact?";
  "zero?"; "positive?"; "negative?"; "odd?"; "even?";
  "symbol?"; "char?"; "string?"; "vector?";
  "procedure?"; "pair?"; "null?";
  "eof-object?";
  (* Equivalence *)
  "eq?"; "eqv?"; "equal?"; "not";
  (* Pairs & lists *)
  "cons"; "car"; "cdr"; "set-car!"; "set-cdr!";
  "caar"; "cadr"; "cdar"; "cddr";
  "list"; "make-list"; "length"; "append"; "reverse";
  "list-tail"; "list-ref";
  "memq"; "memv"; "member"; "assq"; "assv"; "assoc";
  (* Symbols *)
  "symbol->string"; "string->symbol";
  (* Characters *)
  "char=?"; "char<?"; "char>?"; "char<=?"; "char>=?";
  "char-ci=?"; "char-ci<?"; "char-ci>?"; "char-ci<=?"; "char-ci>=?";
  "char->integer"; "integer->char";
  "char-upcase"; "char-downcase";
  "char-alphabetic?"; "char-numeric?"; "char-whitespace?";
  "char-upper-case?"; "char-lower-case?";
  (* Strings *)
  "make-string"; "string"; "string-length"; "string-ref"; "string-set!";
  "string=?"; "string<?"; "string>?"; "string<=?"; "string>=?";
  "string-ci=?"; "string-ci<?"; "string-ci>?"; "string-ci<=?"; "string-ci>=?";
  "substring"; "string-append"; "string->list"; "list->string";
  "string-copy"; "string-fill!";
  (* Vectors *)
  "make-vector"; "vector"; "vector-length"; "vector-ref"; "vector-set!";
  "vector->list"; "list->vector"; "vector-fill!";
  (* I/O *)
  "display"; "write"; "newline";
  "read";
  "open-input-file"; "open-output-file";
  "close-input-port"; "close-output-port";
  "call-with-input-file"; "call-with-output-file";
  "with-input-from-file"; "with-output-to-file";
  "input-port?"; "output-port?";
  "current-input-port"; "current-output-port";
  "read-char"; "peek-char"; "write-char";
  "eof-object";
  "char-ready?";
  (* Control *)
  "apply"; "call/cc"; "call-with-current-continuation";
  "call-with-values"; "values"; "dynamic-wind";
  "map"; "for-each";
  (* Eval / load *)
  "eval"; "load"; "interaction-environment";
  (* Lazy *)
  "force"; "make-promise"; "promise?";
  (* Process context *)
  "exit";
]

let scheme_r5rs_syntax_names = [
  "if"; "lambda"; "define"; "set!"; "begin"; "quote";
  "let"; "let*"; "letrec";
  "cond"; "case"; "do"; "and"; "or";
  "define-syntax"; "let-syntax"; "letrec-syntax";
  "quasiquote";
  "delay";
]

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
    scheme_read_names [];
  build_library inst ["scheme"; "inexact"]
    scheme_inexact_names [];
  build_library inst ["scheme"; "complex"]
    scheme_complex_names [];
  build_library inst ["scheme"; "lazy"]
    scheme_lazy_names scheme_lazy_syntax_names;
  build_library inst ["scheme"; "case-lambda"]
    [] scheme_case_lambda_syntax_names;
  build_library inst ["scheme"; "process-context"]
    scheme_process_context_names [];
  build_library inst ["scheme"; "time"]
    scheme_time_names [];
  build_library inst ["scheme"; "eval"]
    scheme_eval_names [];
  build_library inst ["scheme"; "load"]
    scheme_load_names [];
  build_library inst ["scheme"; "repl"]
    scheme_repl_names [];
  build_library inst ["scheme"; "r5rs"]
    scheme_r5rs_runtime_names scheme_r5rs_syntax_names;
  (* SRFI 151 — bitwise operations (OCaml primitives) *)
  let srfi_151_names = [
    "bitwise-not"; "bitwise-and"; "bitwise-ior"; "bitwise-xor";
    "arithmetic-shift"; "bit-count"; "integer-length"; "bitwise-if";
    "bit-set?"; "copy-bit"; "bit-swap";
    "any-bit-set?"; "every-bit-set?"; "first-set-bit";
    "bit-field"; "bit-field-any?"; "bit-field-every?";
    "bit-field-replace"; "bit-field-rotate"; "bit-field-reverse";
  ] in
  build_library inst ["srfi"; "151"] srfi_151_names [];
  (* SRFI 69 — basic hash tables (OCaml primitives) *)
  let srfi_69_names = [
    "make-hash-table"; "hash-table?"; "alist->hash-table";
    "hash-table-equivalence-function"; "hash-table-hash-function";
    "hash-table-ref"; "hash-table-ref/default";
    "hash-table-set!"; "hash-table-delete!"; "hash-table-exists?";
    "hash-table-update!"; "hash-table-update!/default";
    "hash-table-size"; "hash-table-keys"; "hash-table-values";
    "hash-table-walk"; "hash-table-fold";
    "hash-table->alist"; "hash-table-copy"; "hash-table-merge!";
    "hash"; "string-hash"; "string-ci-hash"; "hash-by-identity";
    "hash-table-clear!"; "hash-table-mutable?";
  ] in
  build_library inst ["srfi"; "69"] srfi_69_names []

(* --- Expander callbacks --- *)

(* Forward reference for library lookup that supports auto-loading from .sld files.
   Initialized to basic registry lookup; updated to lookup_or_load after it's defined. *)
let lib_lookup_ref : (t -> string list -> Library.t option) ref =
  ref (fun inst name -> Library.lookup inst.libraries name)

(* Forward reference for native extension loading.
   Filled in by Extension module at init time to break the dependency cycle. *)
let load_native_ref :
    (t -> search_dirs:string list -> sld_dir:string option -> string -> unit) ref =
  ref (fun _inst ~search_dirs:_ ~sld_dir:_ name ->
    failwith (Printf.sprintf
      "extension loading not available: cannot load %s" name))

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
  ignore (Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) inst.global_env code)

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
    instructions = instrs; source_map = Array.make (Array.length instrs) Loc.none;
    constants; symbols = [||]; children = [||];
    params = [||]; variadic = false; name = "<call>";
  } in
  Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) inst.global_env code

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
               search_paths = ref (Search_path.stdlib_dirs ()); features;
               loading_libs = ref []; fasl_cache = ref false;
               current_input; current_output; current_error;
               command_line = ref (Array.to_list Sys.argv);
               eval_envs = Hashtbl.create 4;
               eval_env_counter = ref 0;
               extension_lib_env = ref None;
               on_call = ref None; on_return = ref None;
               debug_state = ref None } in
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
  (* Environment specifier helpers *)
  let make_env_spec env se =
    let id = !(inst.eval_env_counter) in
    inst.eval_env_counter := id + 1;
    Hashtbl.replace inst.eval_envs id (env, se);
    Datum.Vector [| Datum.Symbol "%env-spec"; Datum.Fixnum id |]
  in
  let get_env_spec name v =
    match v with
    | Datum.Vector [| Datum.Symbol "%env-spec"; Datum.Fixnum id |] ->
      (match Hashtbl.find_opt inst.eval_envs id with
       | Some pair -> pair
       | None -> runtime_error (Printf.sprintf "%s: invalid environment specifier" name))
    | _ -> runtime_error (Printf.sprintf "%s: expected environment specifier" name)
  in
  register_late "interaction-environment" (fun args -> match args with
    | [] -> make_env_spec inst.global_env inst.syn_env
    | _ -> runtime_error (Printf.sprintf "interaction-environment: expected 0 arguments, got %d" (List.length args)));
  (* Internal primitives that core forms depend on at runtime.
     These must be injected into fresh eval environments so that
     expander-generated code (e.g. delay → %make-promise) works. *)
  let internal_prims = [ "%make-promise" ] in
  register_late "environment" (fun args ->
    (* (environment import-set ...) — create fresh env from imports *)
    let env = Env.empty () in
    let se = Expander.core_env () in
    (* Seed with internal primitives needed by core form expansions *)
    List.iter (fun name ->
      let sym = Symbol.intern inst.symbols name in
      match Env.lookup_slot inst.global_env sym with
      | Some slot -> Env.define_slot env sym slot
      | None -> ()
    ) internal_prims;
    List.iter (fun spec ->
      let spec_syn = Syntax.from_datum Loc.none spec in
      let iset = Library.parse_import_set spec_syn in
      let lookup_fn name = !lib_lookup_ref inst name in
      let (rt_bindings, syn_bindings) = Library.resolve_import lookup_fn iset in
      List.iter (fun (name, _id, slot) ->
        let sym = Symbol.intern inst.symbols name in
        Env.define_slot env sym slot
      ) rt_bindings;
      List.iter (fun (name, binding) ->
        Expander.define_binding se name binding
      ) syn_bindings
    ) args;
    make_env_spec env se);
  register_late "eval" (fun args -> match args with
    | [expr; env_spec] ->
      let (env, se) = get_env_spec "eval" env_spec in
      let expr_syn = Syntax.from_datum Loc.none expr in
      let gensym_counter = ref 0 in
      let gensym () =
        let n = !gensym_counter in
        gensym_counter := n + 1;
        Printf.sprintf "%%eval%d" n
      in
      let (features, has_library, read_include) = make_expander_callbacks inst in
      let expanded = Expander.expand ~syn_env:se ~gensym
        ~features ~has_library ~read_include expr_syn in
      let code = Compiler.compile inst.symbols expanded in
      Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) env code
    | _ -> runtime_error (Printf.sprintf "eval: expected 2 arguments, got %d" (List.length args)));
  register_late "load" (fun args -> match args with
    | [Datum.Str s] ->
      let path = Bytes.to_string s in
      let port = Port.of_file path in
      let rec loop () =
        let expr = Reader.read_syntax inst.readtable port in
        match expr.Syntax.datum with
        | Syntax.Eof -> Datum.Void
        | _ ->
          let expanded = expand_with_callbacks inst expr in
          let code = Compiler.compile inst.symbols expanded in
          ignore (Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) inst.global_env code);
          loop ()
      in
      loop ()
    | [Datum.Str s; env_spec] ->
      let path = Bytes.to_string s in
      let (env, se) = get_env_spec "load" env_spec in
      let port = Port.of_file path in
      let gensym_counter = ref 0 in
      let gensym () =
        let n = !gensym_counter in
        gensym_counter := n + 1;
        Printf.sprintf "%%load%d" n
      in
      let (features, has_library, read_include) = make_expander_callbacks inst in
      let rec loop () =
        let expr = Reader.read_syntax inst.readtable port in
        match expr.Syntax.datum with
        | Syntax.Eof -> Datum.Void
        | _ ->
          let expanded = Expander.expand ~syn_env:se ~gensym
            ~features ~has_library ~read_include expr in
          let code = Compiler.compile inst.symbols expanded in
          ignore (Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) env code);
          loop ()
      in
      loop ()
    | [_] -> runtime_error "load: expected string"
    | _ -> runtime_error (Printf.sprintf "load: expected 1 or 2 arguments, got %d" (List.length args)));
  register_late "command-line" (fun args -> match args with
    | [] ->
      Datum.list_of (List.map (fun s -> Datum.Str (Bytes.of_string s)) !(inst.command_line))
    | _ -> runtime_error (Printf.sprintf "command-line: expected 0 arguments, got %d" (List.length args)));
  register_late "force" (fun args -> match args with
    | [Datum.Promise p] ->
      (* Iterative forcing per R7RS §4.2.5 *)
      while not p.promise_done do
        let result = call inst p.promise_value [] in
        if not p.promise_done then
          (match result with
           | Datum.Promise p2 ->
             p.promise_done <- p2.promise_done;
             p.promise_value <- p2.promise_value;
             p2.promise_done <- true;
             p2.promise_value <- Datum.Promise p
           | v ->
             p.promise_done <- true;
             p.promise_value <- v)
      done;
      p.promise_value
    | [v] -> v  (* R7RS: (force non-promise) returns the value *)
    | _ -> runtime_error (Printf.sprintf "force: expected 1 argument, got %d" (List.length args)));
  (* --- SRFI 151 bitwise primitives --- *)
  register_late "bitwise-not" (fun args -> match args with
    | [Datum.Fixnum n] -> Datum.Fixnum (lnot n)
    | _ -> runtime_error "bitwise-not: expected 1 integer");
  register_late "bitwise-and" (fun args ->
    let to_int = function Datum.Fixnum n -> n
      | _ -> runtime_error "bitwise-and: expected integers" in
    Datum.Fixnum (List.fold_left (fun acc x -> acc land to_int x) (-1) args));
  register_late "bitwise-ior" (fun args ->
    let to_int = function Datum.Fixnum n -> n
      | _ -> runtime_error "bitwise-ior: expected integers" in
    Datum.Fixnum (List.fold_left (fun acc x -> acc lor to_int x) 0 args));
  register_late "bitwise-xor" (fun args ->
    let to_int = function Datum.Fixnum n -> n
      | _ -> runtime_error "bitwise-xor: expected integers" in
    Datum.Fixnum (List.fold_left (fun acc x -> acc lxor to_int x) 0 args));
  register_late "arithmetic-shift" (fun args -> match args with
    | [Datum.Fixnum n; Datum.Fixnum count] ->
      if count >= 0 then Datum.Fixnum (n lsl count)
      else Datum.Fixnum (n asr (- count))
    | _ -> runtime_error "arithmetic-shift: expected 2 integers");
  register_late "bit-count" (fun args -> match args with
    | [Datum.Fixnum n] ->
      let n = if n < 0 then lnot n else n in
      let rec popcount n acc =
        if n = 0 then acc
        else popcount (n lsr 1) (acc + (n land 1))
      in
      Datum.Fixnum (popcount n 0)
    | _ -> runtime_error "bit-count: expected 1 integer");
  register_late "integer-length" (fun args -> match args with
    | [Datum.Fixnum n] ->
      let n = if n < 0 then lnot n else n in
      let rec ilog n acc =
        if n = 0 then acc
        else ilog (n lsr 1) (acc + 1)
      in
      Datum.Fixnum (ilog n 0)
    | _ -> runtime_error "integer-length: expected 1 integer");
  register_late "bitwise-if" (fun args -> match args with
    | [Datum.Fixnum mask; Datum.Fixnum i; Datum.Fixnum j] ->
      Datum.Fixnum ((mask land i) lor ((lnot mask) land j))
    | _ -> runtime_error "bitwise-if: expected 3 integers");
  register_late "bit-set?" (fun args -> match args with
    | [Datum.Fixnum k; Datum.Fixnum n] ->
      Datum.Bool (n land (1 lsl k) <> 0)
    | _ -> runtime_error "bit-set?: expected 2 integers");
  register_late "copy-bit" (fun args -> match args with
    | [Datum.Fixnum k; Datum.Fixnum n; Datum.Bool bit] ->
      if bit then Datum.Fixnum (n lor (1 lsl k))
      else Datum.Fixnum (n land (lnot (1 lsl k)))
    | _ -> runtime_error "copy-bit: expected index, integer, boolean");
  register_late "bit-swap" (fun args -> match args with
    | [Datum.Fixnum i; Datum.Fixnum j; Datum.Fixnum n] ->
      let bi = (n lsr i) land 1 in
      let bj = (n lsr j) land 1 in
      if bi = bj then Datum.Fixnum n
      else Datum.Fixnum (n lxor ((1 lsl i) lor (1 lsl j)))
    | _ -> runtime_error "bit-swap: expected 3 integers");
  register_late "any-bit-set?" (fun args -> match args with
    | [Datum.Fixnum i; Datum.Fixnum j] ->
      Datum.Bool (i land j <> 0)
    | _ -> runtime_error "any-bit-set?: expected 2 integers");
  register_late "every-bit-set?" (fun args -> match args with
    | [Datum.Fixnum i; Datum.Fixnum j] ->
      Datum.Bool (i land j = j)
    | _ -> runtime_error "every-bit-set?: expected 2 integers");
  register_late "first-set-bit" (fun args -> match args with
    | [Datum.Fixnum n] ->
      if n = 0 then Datum.Fixnum (-1)
      else
        let rec fsb n k =
          if n land 1 = 1 then k
          else fsb (n lsr 1) (k + 1)
        in
        Datum.Fixnum (fsb (if n < 0 then lnot n lor 1 else n) 0)
    | _ -> runtime_error "first-set-bit: expected 1 integer");
  register_late "bit-field" (fun args -> match args with
    | [Datum.Fixnum n; Datum.Fixnum start; Datum.Fixnum end_] ->
      let width = end_ - start in
      let mask = (1 lsl width) - 1 in
      Datum.Fixnum ((n lsr start) land mask)
    | _ -> runtime_error "bit-field: expected 3 integers");
  register_late "bit-field-any?" (fun args -> match args with
    | [Datum.Fixnum n; Datum.Fixnum start; Datum.Fixnum end_] ->
      let width = end_ - start in
      let mask = (1 lsl width) - 1 in
      Datum.Bool ((n lsr start) land mask <> 0)
    | _ -> runtime_error "bit-field-any?: expected 3 integers");
  register_late "bit-field-every?" (fun args -> match args with
    | [Datum.Fixnum n; Datum.Fixnum start; Datum.Fixnum end_] ->
      let width = end_ - start in
      let mask = (1 lsl width) - 1 in
      Datum.Bool ((n lsr start) land mask = mask)
    | _ -> runtime_error "bit-field-every?: expected 3 integers");
  register_late "bit-field-replace" (fun args -> match args with
    | [Datum.Fixnum n; Datum.Fixnum replacement; Datum.Fixnum start; Datum.Fixnum end_] ->
      let width = end_ - start in
      let mask = (1 lsl width) - 1 in
      let cleared = n land (lnot (mask lsl start)) in
      Datum.Fixnum (cleared lor ((replacement land mask) lsl start))
    | _ -> runtime_error "bit-field-replace: expected 4 integers");
  register_late "bit-field-rotate" (fun args -> match args with
    | [Datum.Fixnum n; Datum.Fixnum count; Datum.Fixnum start; Datum.Fixnum end_] ->
      let width = end_ - start in
      if width = 0 then Datum.Fixnum n
      else
        let mask = (1 lsl width) - 1 in
        let field = (n lsr start) land mask in
        let count = ((count mod width) + width) mod width in
        let rotated = ((field lsl count) lor (field lsr (width - count))) land mask in
        let cleared = n land (lnot (mask lsl start)) in
        Datum.Fixnum (cleared lor (rotated lsl start))
    | _ -> runtime_error "bit-field-rotate: expected 4 integers");
  register_late "bit-field-reverse" (fun args -> match args with
    | [Datum.Fixnum n; Datum.Fixnum start; Datum.Fixnum end_] ->
      let width = end_ - start in
      let mask = (1 lsl width) - 1 in
      let field = (n lsr start) land mask in
      let rec rev_bits v w acc =
        if w = 0 then acc
        else rev_bits (v lsr 1) (w - 1) ((acc lsl 1) lor (v land 1))
      in
      let reversed = rev_bits field width 0 in
      let cleared = n land (lnot (mask lsl start)) in
      Datum.Fixnum (cleared lor (reversed lsl start))
    | _ -> runtime_error "bit-field-reverse: expected 3 integers");
  (* --- SRFI 69 — Basic Hash Tables --- *)
  let rec datum_hash (d : Datum.t) = match d with
    | Bool b -> Hashtbl.hash b
    | Fixnum n -> Hashtbl.hash n
    | Flonum f -> Hashtbl.hash f
    | Char c -> Uchar.to_int c
    | Str s -> Hashtbl.hash (Bytes.to_string s)
    | Symbol s -> Hashtbl.hash s
    | Nil -> 0
    | Pair { car; cdr } -> datum_hash car * 31 + datum_hash cdr
    | Vector v -> Array.fold_left (fun h e -> h * 31 + datum_hash e) 0 v
    | Bytevector bv -> Hashtbl.hash bv
    | _ -> 0
  in
  let ht_raw_hash ht key = match ht.Datum.ht_hash with
    | Datum.Bool true -> datum_hash key
    | hash_fn ->
      (match call inst hash_fn [key] with
       | Datum.Fixnum n -> n
       | v -> runtime_error (Printf.sprintf "hash function must return an integer, got %s" (Datum.to_string v)))
  in
  let ht_index h cap = (h land max_int) mod cap in
  let ht_keys_equal ht a b = match ht.Datum.ht_equal with
    | Datum.Bool true -> scheme_equal a b
    | eq_fn -> Datum.is_true (call inst eq_fn [a; b])
  in
  let ht_maybe_resize ht =
    let cap = Array.length ht.Datum.ht_data in
    if ht.Datum.ht_size > cap * 2 && cap < max_int / 2 then begin
      let new_cap = cap * 2 in
      let new_data = Array.make new_cap [] in
      Array.iter (List.iter (fun ((k, _v) as e) ->
        let idx = ht_index (ht_raw_hash ht k) new_cap in
        new_data.(idx) <- e :: new_data.(idx)
      )) ht.Datum.ht_data;
      ht.Datum.ht_data <- new_data
    end
  in
  let require_ht name = function
    | Datum.Hash_table ht -> ht
    | v -> runtime_error (Printf.sprintf "%s: expected hash-table, got %s" name (Datum.to_string v))
  in
  let require_mutable name ht =
    if not ht.Datum.ht_mutable then
      runtime_error (Printf.sprintf "%s: hash-table is immutable" name)
  in
  let ht_find_bucket ht key =
    let h = ht_raw_hash ht key in
    let idx = ht_index h (Array.length ht.Datum.ht_data) in
    (idx, ht.Datum.ht_data.(idx))
  in
  let ht_lookup ht key =
    let (_idx, bucket) = ht_find_bucket ht key in
    let rec search = function
      | [] -> None
      | (k, v) :: rest ->
        if ht_keys_equal ht k key then Some v else search rest
    in
    search bucket
  in
  let ht_set ht key value =
    let h = ht_raw_hash ht key in
    let idx = ht_index h (Array.length ht.Datum.ht_data) in
    let bucket = ht.Datum.ht_data.(idx) in
    let rec replace = function
      | [] ->
        ht.Datum.ht_data.(idx) <- (key, value) :: bucket;
        ht.Datum.ht_size <- ht.Datum.ht_size + 1;
        ht_maybe_resize ht
      | (k, _) :: rest ->
        if ht_keys_equal ht k key then
          ht.Datum.ht_data.(idx) <-
            List.map (fun ((k2, _) as e) ->
              if ht_keys_equal ht k2 key then (k2, value) else e
            ) bucket
        else
          replace rest
    in
    replace bucket
  in
  let ht_delete ht key =
    let h = ht_raw_hash ht key in
    let idx = ht_index h (Array.length ht.Datum.ht_data) in
    let bucket = ht.Datum.ht_data.(idx) in
    let new_bucket = List.filter (fun (k, _) -> not (ht_keys_equal ht k key)) bucket in
    let removed = List.length bucket - List.length new_bucket in
    if removed > 0 then begin
      ht.Datum.ht_data.(idx) <- new_bucket;
      ht.Datum.ht_size <- ht.Datum.ht_size - removed
    end
  in
  register_late "make-hash-table" (fun args ->
    let (eq_fn, hash_fn, cap) = match args with
      | [] -> (Datum.Bool true, Datum.Bool true, 16)
      | [eq] -> (eq, Datum.Bool true, 16)
      | [eq; hf] -> (eq, hf, 16)
      | [eq; hf; Datum.Fixnum c] -> (eq, hf, (max 1 c))
      | _ -> runtime_error "make-hash-table: expected 0-3 arguments"
    in
    Datum.Hash_table {
      ht_data = Array.make cap [];
      ht_size = 0;
      ht_equal = eq_fn;
      ht_hash = hash_fn;
      ht_mutable = true;
    });
  register_late "hash-table?" (fun args -> match args with
    | [Datum.Hash_table _] -> Datum.Bool true
    | [_] -> Datum.Bool false
    | _ -> runtime_error "hash-table?: expected 1 argument");
  register_late "alist->hash-table" (fun args ->
    let (alist, eq_fn, hash_fn) = match args with
      | [a] -> (a, Datum.Bool true, Datum.Bool true)
      | [a; eq] -> (a, eq, Datum.Bool true)
      | [a; eq; hf] -> (a, eq, hf)
      | _ -> runtime_error "alist->hash-table: expected 1-3 arguments"
    in
    let ht : Datum.hash_table = {
      ht_data = Array.make 16 [];
      ht_size = 0;
      ht_equal = eq_fn;
      ht_hash = hash_fn;
      ht_mutable = true;
    } in
    (* Walk alist: earlier keys shadow later, so only insert if not present *)
    let rec walk = function
      | Datum.Nil -> ()
      | Datum.Pair { car = Datum.Pair { car = k; cdr = v }; cdr = rest } ->
        (match ht_lookup ht k with
         | None -> ht_set ht k v
         | Some _ -> ());
        walk rest
      | _ -> runtime_error "alist->hash-table: expected alist"
    in
    walk alist;
    Datum.Hash_table ht);
  register_late "hash-table-equivalence-function" (fun args -> match args with
    | [ht_val] ->
      let ht = require_ht "hash-table-equivalence-function" ht_val in
      (match ht.ht_equal with
       | Datum.Bool true ->
         let sym = Symbol.intern inst.symbols "equal?" in
         (match Env.lookup inst.global_env sym with
          | Some v -> v
          | None -> Datum.Bool true)
       | proc -> proc)
    | _ -> runtime_error "hash-table-equivalence-function: expected 1 argument");
  register_late "hash-table-hash-function" (fun args -> match args with
    | [ht_val] ->
      let ht = require_ht "hash-table-hash-function" ht_val in
      (match ht.ht_hash with
       | Datum.Bool true ->
         let sym = Symbol.intern inst.symbols "hash" in
         (match Env.lookup inst.global_env sym with
          | Some v -> v
          | None -> Datum.Bool true)
       | proc -> proc)
    | _ -> runtime_error "hash-table-hash-function: expected 1 argument");
  register_late "hash-table-ref" (fun args ->
    match args with
    | [ht_val; key] ->
      let ht = require_ht "hash-table-ref" ht_val in
      (match ht_lookup ht key with
       | Some v -> v
       | None -> runtime_error "hash-table-ref: key not found")
    | [ht_val; key; thunk] ->
      let ht = require_ht "hash-table-ref" ht_val in
      (match ht_lookup ht key with
       | Some v -> v
       | None -> call inst thunk [])
    | _ -> runtime_error "hash-table-ref: expected 2 or 3 arguments");
  register_late "hash-table-ref/default" (fun args -> match args with
    | [ht_val; key; default] ->
      let ht = require_ht "hash-table-ref/default" ht_val in
      (match ht_lookup ht key with
       | Some v -> v
       | None -> default)
    | _ -> runtime_error "hash-table-ref/default: expected 3 arguments");
  register_late "hash-table-set!" (fun args -> match args with
    | [ht_val; key; value] ->
      let ht = require_ht "hash-table-set!" ht_val in
      require_mutable "hash-table-set!" ht;
      ht_set ht key value;
      Datum.Void
    | _ -> runtime_error "hash-table-set!: expected 3 arguments");
  register_late "hash-table-delete!" (fun args -> match args with
    | [ht_val; key] ->
      let ht = require_ht "hash-table-delete!" ht_val in
      require_mutable "hash-table-delete!" ht;
      ht_delete ht key;
      Datum.Void
    | _ -> runtime_error "hash-table-delete!: expected 2 arguments");
  register_late "hash-table-exists?" (fun args -> match args with
    | [ht_val; key] ->
      let ht = require_ht "hash-table-exists?" ht_val in
      Datum.Bool (ht_lookup ht key <> None)
    | _ -> runtime_error "hash-table-exists?: expected 2 arguments");
  register_late "hash-table-update!" (fun args ->
    match args with
    | [ht_val; key; fn] ->
      let ht = require_ht "hash-table-update!" ht_val in
      require_mutable "hash-table-update!" ht;
      (match ht_lookup ht key with
       | Some v -> ht_set ht key (call inst fn [v]); Datum.Void
       | None -> runtime_error "hash-table-update!: key not found")
    | [ht_val; key; fn; thunk] ->
      let ht = require_ht "hash-table-update!" ht_val in
      require_mutable "hash-table-update!" ht;
      let old = match ht_lookup ht key with
        | Some v -> v
        | None -> call inst thunk []
      in
      ht_set ht key (call inst fn [old]);
      Datum.Void
    | _ -> runtime_error "hash-table-update!: expected 3 or 4 arguments");
  register_late "hash-table-update!/default" (fun args -> match args with
    | [ht_val; key; fn; default] ->
      let ht = require_ht "hash-table-update!/default" ht_val in
      require_mutable "hash-table-update!/default" ht;
      let old = match ht_lookup ht key with
        | Some v -> v
        | None -> default
      in
      ht_set ht key (call inst fn [old]);
      Datum.Void
    | _ -> runtime_error "hash-table-update!/default: expected 4 arguments");
  register_late "hash-table-size" (fun args -> match args with
    | [ht_val] ->
      let ht = require_ht "hash-table-size" ht_val in
      Datum.Fixnum ht.ht_size
    | _ -> runtime_error "hash-table-size: expected 1 argument");
  register_late "hash-table-keys" (fun args -> match args with
    | [ht_val] ->
      let ht = require_ht "hash-table-keys" ht_val in
      let keys = Array.fold_left (fun acc bucket ->
        List.fold_left (fun a (k, _) -> k :: a) acc bucket
      ) [] ht.ht_data in
      Datum.list_of keys
    | _ -> runtime_error "hash-table-keys: expected 1 argument");
  register_late "hash-table-values" (fun args -> match args with
    | [ht_val] ->
      let ht = require_ht "hash-table-values" ht_val in
      let vals = Array.fold_left (fun acc bucket ->
        List.fold_left (fun a (_, v) -> v :: a) acc bucket
      ) [] ht.ht_data in
      Datum.list_of vals
    | _ -> runtime_error "hash-table-values: expected 1 argument");
  register_late "hash-table-walk" (fun args -> match args with
    | [ht_val; proc] ->
      let ht = require_ht "hash-table-walk" ht_val in
      Array.iter (List.iter (fun (k, v) ->
        ignore (call inst proc [k; v])
      )) ht.ht_data;
      Datum.Void
    | _ -> runtime_error "hash-table-walk: expected 2 arguments");
  register_late "hash-table-fold" (fun args -> match args with
    | [ht_val; f; init] ->
      let ht = require_ht "hash-table-fold" ht_val in
      let acc = ref init in
      Array.iter (List.iter (fun (k, v) ->
        acc := call inst f [k; v; !acc]
      )) ht.ht_data;
      !acc
    | _ -> runtime_error "hash-table-fold: expected 3 arguments");
  register_late "hash-table->alist" (fun args -> match args with
    | [ht_val] ->
      let ht = require_ht "hash-table->alist" ht_val in
      let pairs = Array.fold_left (fun acc bucket ->
        List.fold_left (fun a (k, v) ->
          Datum.Pair { car = k; cdr = v } :: a
        ) acc bucket
      ) [] ht.ht_data in
      Datum.list_of pairs
    | _ -> runtime_error "hash-table->alist: expected 1 argument");
  register_late "hash-table-copy" (fun args ->
    match args with
    | [ht_val] | [ht_val; _] ->
      let ht = require_ht "hash-table-copy" ht_val in
      let mutable_ = match args with
        | [_; Datum.Bool b] -> b
        | [_] -> true
        | _ -> true
      in
      let new_data = Array.copy ht.ht_data in
      Datum.Hash_table {
        ht_data = new_data;
        ht_size = ht.ht_size;
        ht_equal = ht.ht_equal;
        ht_hash = ht.ht_hash;
        ht_mutable = mutable_;
      }
    | _ -> runtime_error "hash-table-copy: expected 1 or 2 arguments");
  register_late "hash-table-merge!" (fun args -> match args with
    | [ht1_val; ht2_val] ->
      let ht1 = require_ht "hash-table-merge!" ht1_val in
      let ht2 = require_ht "hash-table-merge!" ht2_val in
      require_mutable "hash-table-merge!" ht1;
      Array.iter (List.iter (fun (k, v) ->
        ht_set ht1 k v
      )) ht2.ht_data;
      ht1_val
    | _ -> runtime_error "hash-table-merge!: expected 2 arguments");
  register_late "hash" (fun args ->
    match args with
    | [obj] -> Datum.Fixnum (datum_hash obj land max_int)
    | [obj; Datum.Fixnum bound] ->
      Datum.Fixnum ((datum_hash obj land max_int) mod bound)
    | _ -> runtime_error "hash: expected 1 or 2 arguments");
  register_late "string-hash" (fun args ->
    match args with
    | [Datum.Str s] ->
      Datum.Fixnum (Hashtbl.hash (Bytes.to_string s) land max_int)
    | [Datum.Str s; Datum.Fixnum bound] ->
      Datum.Fixnum ((Hashtbl.hash (Bytes.to_string s) land max_int) mod bound)
    | _ -> runtime_error "string-hash: expected string and optional bound");
  register_late "string-ci-hash" (fun args ->
    match args with
    | [Datum.Str s] ->
      Datum.Fixnum (Hashtbl.hash (String.lowercase_ascii (Bytes.to_string s)) land max_int)
    | [Datum.Str s; Datum.Fixnum bound] ->
      Datum.Fixnum ((Hashtbl.hash (String.lowercase_ascii (Bytes.to_string s)) land max_int) mod bound)
    | _ -> runtime_error "string-ci-hash: expected string and optional bound");
  register_late "hash-by-identity" (fun args ->
    match args with
    | [obj] -> Datum.Fixnum (Hashtbl.hash obj land max_int)
    | [obj; Datum.Fixnum bound] ->
      Datum.Fixnum ((Hashtbl.hash obj land max_int) mod bound)
    | _ -> runtime_error "hash-by-identity: expected 1 or 2 arguments");
  register_late "hash-table-clear!" (fun args -> match args with
    | [ht_val] ->
      let ht = require_ht "hash-table-clear!" ht_val in
      require_mutable "hash-table-clear!" ht;
      Array.fill ht.ht_data 0 (Array.length ht.ht_data) [];
      ht.ht_size <- 0;
      Datum.Void
    | _ -> runtime_error "hash-table-clear!: expected 1 argument");
  register_late "hash-table-mutable?" (fun args -> match args with
    | [ht_val] ->
      let ht = require_ht "hash-table-mutable?" ht_val in
      Datum.Bool ht.ht_mutable
    | _ -> runtime_error "hash-table-mutable?: expected 1 argument");
  (* --- SRFI 14 — Character Sets --- *)
  let cs_make () = { Datum.cs_bits = Bytes.make 32 '\x00' } in
  let cs_copy cs = { Datum.cs_bits = Bytes.copy cs.Datum.cs_bits } in
  let cs_test cs n =
    n >= 0 && n < 256 &&
    Char.code (Bytes.get cs.Datum.cs_bits (n / 8)) land (1 lsl (n mod 8)) <> 0 in
  let cs_set cs n =
    if n >= 0 && n < 256 then
      Bytes.set cs.Datum.cs_bits (n / 8)
        (Char.chr (Char.code (Bytes.get cs.Datum.cs_bits (n / 8)) lor (1 lsl (n mod 8)))) in
  let cs_clear cs n =
    if n >= 0 && n < 256 then
      Bytes.set cs.Datum.cs_bits (n / 8)
        (Char.chr (Char.code (Bytes.get cs.Datum.cs_bits (n / 8)) land (lnot (1 lsl (n mod 8))))) in
  let require_cs name = function
    | Datum.Char_set cs -> cs
    | v -> runtime_error (Printf.sprintf "%s: expected char-set, got %s" name (Datum.to_string v)) in
  let require_char name = function
    | Datum.Char c -> Uchar.to_int c
    | v -> runtime_error (Printf.sprintf "%s: expected char, got %s" name (Datum.to_string v)) in
  register_late "char-set?" (fun args -> match args with
    | [Datum.Char_set _] -> Datum.Bool true
    | [_] -> Datum.Bool false
    | _ -> runtime_error "char-set?: expected 1 argument");
  register_late "char-set" (fun args ->
    let cs = cs_make () in
    List.iter (fun a -> cs_set cs (require_char "char-set" a)) args;
    Datum.Char_set cs);
  register_late "char-set-copy" (fun args -> match args with
    | [cs_val] -> Datum.Char_set (cs_copy (require_cs "char-set-copy" cs_val))
    | _ -> runtime_error "char-set-copy: expected 1 argument");
  register_late "char-set-contains?" (fun args -> match args with
    | [cs_val; Datum.Char c] ->
      Datum.Bool (cs_test (require_cs "char-set-contains?" cs_val) (Uchar.to_int c))
    | _ -> runtime_error "char-set-contains?: expected char-set and char");
  register_late "char-set=?" (fun args ->
    match args with
    | [] | [_] -> Datum.Bool true
    | _ ->
      let first = require_cs "char-set=?" (List.hd args) in
      Datum.Bool (List.for_all (fun a ->
        Bytes.equal first.Datum.cs_bits (require_cs "char-set=?" a).Datum.cs_bits
      ) (List.tl args)));
  register_late "char-set<=?" (fun args ->
    match args with
    | [] | [_] -> Datum.Bool true
    | _ ->
      let rec check = function
        | [] | [_] -> true
        | a :: b :: rest ->
          let ca = require_cs "char-set<=?" a in
          let cb = require_cs "char-set<=?" b in
          let subset = ref true in
          for i = 0 to 31 do
            let ba = Char.code (Bytes.get ca.Datum.cs_bits i) in
            let bb = Char.code (Bytes.get cb.Datum.cs_bits i) in
            if ba land bb <> ba then subset := false
          done;
          !subset && check (b :: rest)
      in
      Datum.Bool (check args));
  register_late "char-set>=?" (fun args ->
    match args with
    | [] | [_] -> Datum.Bool true
    | _ ->
      let rec check = function
        | [] | [_] -> true
        | a :: b :: rest ->
          let ca = require_cs "char-set>=?" a in
          let cb = require_cs "char-set>=?" b in
          let superset = ref true in
          for i = 0 to 31 do
            let ba = Char.code (Bytes.get ca.Datum.cs_bits i) in
            let bb = Char.code (Bytes.get cb.Datum.cs_bits i) in
            if ba lor bb <> ba then superset := false
          done;
          !superset && check (b :: rest)
      in
      Datum.Bool (check args));
  register_late "char-set-adjoin" (fun args ->
    match args with
    | cs_val :: chars ->
      let cs = cs_copy (require_cs "char-set-adjoin" cs_val) in
      List.iter (fun a -> cs_set cs (require_char "char-set-adjoin" a)) chars;
      Datum.Char_set cs
    | _ -> runtime_error "char-set-adjoin: expected char-set and chars");
  register_late "char-set-adjoin!" (fun args ->
    match args with
    | cs_val :: chars ->
      let cs = require_cs "char-set-adjoin!" cs_val in
      List.iter (fun a -> cs_set cs (require_char "char-set-adjoin!" a)) chars;
      Datum.Char_set cs
    | _ -> runtime_error "char-set-adjoin!: expected char-set and chars");
  register_late "char-set-delete" (fun args ->
    match args with
    | cs_val :: chars ->
      let cs = cs_copy (require_cs "char-set-delete" cs_val) in
      List.iter (fun a -> cs_clear cs (require_char "char-set-delete" a)) chars;
      Datum.Char_set cs
    | _ -> runtime_error "char-set-delete: expected char-set and chars");
  register_late "char-set-delete!" (fun args ->
    match args with
    | cs_val :: chars ->
      let cs = require_cs "char-set-delete!" cs_val in
      List.iter (fun a -> cs_clear cs (require_char "char-set-delete!" a)) chars;
      Datum.Char_set cs
    | _ -> runtime_error "char-set-delete!: expected char-set and chars");
  register_late "char-set-complement" (fun args -> match args with
    | [cs_val] ->
      let cs = require_cs "char-set-complement" cs_val in
      let result = cs_make () in
      for i = 0 to 31 do
        Bytes.set result.Datum.cs_bits i
          (Char.chr (Char.code (Bytes.get cs.Datum.cs_bits i) lxor 0xFF))
      done;
      Datum.Char_set result
    | _ -> runtime_error "char-set-complement: expected 1 argument");
  register_late "char-set-union" (fun args ->
    let result = cs_make () in
    List.iter (fun a ->
      let cs = require_cs "char-set-union" a in
      for i = 0 to 31 do
        Bytes.set result.Datum.cs_bits i
          (Char.chr (Char.code (Bytes.get result.Datum.cs_bits i) lor
                     Char.code (Bytes.get cs.Datum.cs_bits i)))
      done
    ) args;
    Datum.Char_set result);
  register_late "char-set-intersection" (fun args ->
    match args with
    | [] ->
      let result = cs_make () in
      for i = 0 to 31 do
        Bytes.set result.Datum.cs_bits i (Char.chr 0xFF)
      done;
      Datum.Char_set result
    | first :: rest ->
      let result = cs_copy (require_cs "char-set-intersection" first) in
      List.iter (fun a ->
        let cs = require_cs "char-set-intersection" a in
        for i = 0 to 31 do
          Bytes.set result.Datum.cs_bits i
            (Char.chr (Char.code (Bytes.get result.Datum.cs_bits i) land
                       Char.code (Bytes.get cs.Datum.cs_bits i)))
        done
      ) rest;
      Datum.Char_set result);
  register_late "char-set-difference" (fun args ->
    match args with
    | first :: rest ->
      let result = cs_copy (require_cs "char-set-difference" first) in
      List.iter (fun a ->
        let cs = require_cs "char-set-difference" a in
        for i = 0 to 31 do
          Bytes.set result.Datum.cs_bits i
            (Char.chr (Char.code (Bytes.get result.Datum.cs_bits i) land
                       (Char.code (Bytes.get cs.Datum.cs_bits i) lxor 0xFF)))
        done
      ) rest;
      Datum.Char_set result
    | _ -> runtime_error "char-set-difference: expected at least 1 argument");
  register_late "char-set-xor" (fun args ->
    let result = cs_make () in
    List.iter (fun a ->
      let cs = require_cs "char-set-xor" a in
      for i = 0 to 31 do
        Bytes.set result.Datum.cs_bits i
          (Char.chr (Char.code (Bytes.get result.Datum.cs_bits i) lxor
                     Char.code (Bytes.get cs.Datum.cs_bits i)))
      done
    ) args;
    Datum.Char_set result);
  register_late "char-set-union!" (fun args ->
    match args with
    | first :: rest ->
      let cs = require_cs "char-set-union!" first in
      List.iter (fun a ->
        let other = require_cs "char-set-union!" a in
        for i = 0 to 31 do
          Bytes.set cs.Datum.cs_bits i
            (Char.chr (Char.code (Bytes.get cs.Datum.cs_bits i) lor
                       Char.code (Bytes.get other.Datum.cs_bits i)))
        done
      ) rest;
      Datum.Char_set cs
    | _ -> runtime_error "char-set-union!: expected at least 1 argument");
  register_late "char-set-intersection!" (fun args ->
    match args with
    | first :: rest ->
      let cs = require_cs "char-set-intersection!" first in
      List.iter (fun a ->
        let other = require_cs "char-set-intersection!" a in
        for i = 0 to 31 do
          Bytes.set cs.Datum.cs_bits i
            (Char.chr (Char.code (Bytes.get cs.Datum.cs_bits i) land
                       Char.code (Bytes.get other.Datum.cs_bits i)))
        done
      ) rest;
      Datum.Char_set cs
    | _ -> runtime_error "char-set-intersection!: expected at least 1 argument");
  register_late "char-set-difference!" (fun args ->
    match args with
    | first :: rest ->
      let cs = require_cs "char-set-difference!" first in
      List.iter (fun a ->
        let other = require_cs "char-set-difference!" a in
        for i = 0 to 31 do
          Bytes.set cs.Datum.cs_bits i
            (Char.chr (Char.code (Bytes.get cs.Datum.cs_bits i) land
                       (Char.code (Bytes.get other.Datum.cs_bits i) lxor 0xFF)))
        done
      ) rest;
      Datum.Char_set cs
    | _ -> runtime_error "char-set-difference!: expected at least 1 argument");
  register_late "char-set-xor!" (fun args ->
    match args with
    | first :: rest ->
      let cs = require_cs "char-set-xor!" first in
      List.iter (fun a ->
        let other = require_cs "char-set-xor!" a in
        for i = 0 to 31 do
          Bytes.set cs.Datum.cs_bits i
            (Char.chr (Char.code (Bytes.get cs.Datum.cs_bits i) lxor
                       Char.code (Bytes.get other.Datum.cs_bits i)))
        done
      ) rest;
      Datum.Char_set cs
    | _ -> runtime_error "char-set-xor!: expected at least 1 argument");
  register_late "list->char-set" (fun args -> match args with
    | [lst] ->
      let cs = cs_make () in
      let rec walk = function
        | Datum.Nil -> ()
        | Datum.Pair { car; cdr } ->
          cs_set cs (require_char "list->char-set" car);
          walk cdr
        | _ -> runtime_error "list->char-set: expected proper list"
      in
      walk lst;
      Datum.Char_set cs
    | _ -> runtime_error "list->char-set: expected 1 argument");
  register_late "string->char-set" (fun args -> match args with
    | [Datum.Str s] ->
      let cs = cs_make () in
      Bytes.iter (fun c -> cs_set cs (Char.code c)) s;
      Datum.Char_set cs
    | _ -> runtime_error "string->char-set: expected 1 string");
  register_late "char-set->list" (fun args -> match args with
    | [cs_val] ->
      let cs = require_cs "char-set->list" cs_val in
      let acc = ref [] in
      for i = 255 downto 0 do
        if cs_test cs i then
          acc := Datum.Char (Uchar.of_int i) :: !acc
      done;
      Datum.list_of !acc
    | _ -> runtime_error "char-set->list: expected 1 argument");
  register_late "char-set->string" (fun args -> match args with
    | [cs_val] ->
      let cs = require_cs "char-set->string" cs_val in
      let buf = Buffer.create 32 in
      for i = 0 to 255 do
        if cs_test cs i then Buffer.add_char buf (Char.chr i)
      done;
      Datum.Str (Bytes.of_string (Buffer.contents buf))
    | _ -> runtime_error "char-set->string: expected 1 argument");
  register_late "ucs-range->char-set" (fun args ->
    match args with
    | [Datum.Fixnum lo; Datum.Fixnum hi] ->
      let cs = cs_make () in
      for i = lo to hi - 1 do cs_set cs i done;
      Datum.Char_set cs
    | _ -> runtime_error "ucs-range->char-set: expected 2 integers");
  register_late "char-set-fold" (fun args -> match args with
    | [proc; init; cs_val] ->
      let cs = require_cs "char-set-fold" cs_val in
      let acc = ref init in
      for i = 0 to 255 do
        if cs_test cs i then
          acc := call inst proc [Datum.Char (Uchar.of_int i); !acc]
      done;
      !acc
    | _ -> runtime_error "char-set-fold: expected 3 arguments");
  register_late "char-set-for-each" (fun args -> match args with
    | [proc; cs_val] ->
      let cs = require_cs "char-set-for-each" cs_val in
      for i = 0 to 255 do
        if cs_test cs i then
          ignore (call inst proc [Datum.Char (Uchar.of_int i)])
      done;
      Datum.Void
    | _ -> runtime_error "char-set-for-each: expected 2 arguments");
  register_late "char-set-map" (fun args -> match args with
    | [proc; cs_val] ->
      let cs = require_cs "char-set-map" cs_val in
      let result = cs_make () in
      for i = 0 to 255 do
        if cs_test cs i then begin
          let v = call inst proc [Datum.Char (Uchar.of_int i)] in
          cs_set result (require_char "char-set-map" v)
        end
      done;
      Datum.Char_set result
    | _ -> runtime_error "char-set-map: expected 2 arguments");
  register_late "char-set-count" (fun args -> match args with
    | [pred; cs_val] ->
      let cs = require_cs "char-set-count" cs_val in
      let n = ref 0 in
      for i = 0 to 255 do
        if cs_test cs i then
          if Datum.is_true (call inst pred [Datum.Char (Uchar.of_int i)]) then
            incr n
      done;
      Datum.Fixnum !n
    | _ -> runtime_error "char-set-count: expected 2 arguments");
  register_late "char-set-every" (fun args -> match args with
    | [pred; cs_val] ->
      let cs = require_cs "char-set-every" cs_val in
      let result = ref true in
      for i = 0 to 255 do
        if cs_test cs i && !result then
          if not (Datum.is_true (call inst pred [Datum.Char (Uchar.of_int i)])) then
            result := false
      done;
      Datum.Bool !result
    | _ -> runtime_error "char-set-every: expected 2 arguments");
  register_late "char-set-any" (fun args -> match args with
    | [pred; cs_val] ->
      let cs = require_cs "char-set-any" cs_val in
      let result = ref false in
      for i = 0 to 255 do
        if cs_test cs i && not !result then
          if Datum.is_true (call inst pred [Datum.Char (Uchar.of_int i)]) then
            result := true
      done;
      Datum.Bool !result
    | _ -> runtime_error "char-set-any: expected 2 arguments");
  register_late "char-set-filter" (fun args -> match args with
    | [pred; cs_val] ->
      let cs = require_cs "char-set-filter" cs_val in
      let result = cs_make () in
      for i = 0 to 255 do
        if cs_test cs i then
          if Datum.is_true (call inst pred [Datum.Char (Uchar.of_int i)]) then
            cs_set result i
      done;
      Datum.Char_set result
    | _ -> runtime_error "char-set-filter: expected 2 arguments");
  register_late "char-set-cursor" (fun args -> match args with
    | [cs_val] ->
      let cs = require_cs "char-set-cursor" cs_val in
      let cursor = ref 256 in
      for i = 255 downto 0 do
        if cs_test cs i then cursor := i
      done;
      Datum.Fixnum !cursor
    | _ -> runtime_error "char-set-cursor: expected 1 argument");
  register_late "char-set-ref" (fun args -> match args with
    | [_cs_val; Datum.Fixnum cursor] ->
      if cursor < 0 || cursor >= 256 then
        runtime_error "char-set-ref: cursor out of range"
      else
        Datum.Char (Uchar.of_int cursor)
    | _ -> runtime_error "char-set-ref: expected char-set and cursor");
  register_late "char-set-cursor-next" (fun args -> match args with
    | [cs_val; Datum.Fixnum cursor] ->
      let cs = require_cs "char-set-cursor-next" cs_val in
      let next = ref 256 in
      for i = 255 downto cursor + 1 do
        if cs_test cs i then next := i
      done;
      Datum.Fixnum !next
    | _ -> runtime_error "char-set-cursor-next: expected char-set and cursor");
  register_late "end-of-char-set?" (fun args -> match args with
    | [Datum.Fixnum cursor] -> Datum.Bool (cursor >= 256)
    | _ -> runtime_error "end-of-char-set?: expected cursor");
  register_late "char-set-size" (fun args -> match args with
    | [cs_val] ->
      let cs = require_cs "char-set-size" cs_val in
      let n = ref 0 in
      for i = 0 to 255 do
        if cs_test cs i then incr n
      done;
      Datum.Fixnum !n
    | _ -> runtime_error "char-set-size: expected 1 argument");
  (* Predefined char-sets *)
  let cs_from_pred pred =
    let cs = cs_make () in
    for i = 0 to 255 do if pred i then cs_set cs i done;
    Datum.Char_set cs
  in
  let define_cs name v =
    let sym = Symbol.intern inst.symbols name in
    Env.define inst.global_env sym v;
    Expander.define_binding inst.syn_env name Expander.var_binding
  in
  define_cs "char-set:letter"
    (cs_from_pred (fun i -> let c = Char.chr i in (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')));
  define_cs "char-set:digit"
    (cs_from_pred (fun i -> let c = Char.chr i in c >= '0' && c <= '9'));
  define_cs "char-set:letter+digit"
    (cs_from_pred (fun i -> let c = Char.chr i in
       (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')));
  define_cs "char-set:whitespace"
    (cs_from_pred (fun i -> let c = Char.chr i in
       c = ' ' || c = '\t' || c = '\n' || c = '\r' || i = 0x0B || i = 0x0C));
  define_cs "char-set:upper-case"
    (cs_from_pred (fun i -> let c = Char.chr i in c >= 'A' && c <= 'Z'));
  define_cs "char-set:lower-case"
    (cs_from_pred (fun i -> let c = Char.chr i in c >= 'a' && c <= 'z'));
  define_cs "char-set:punctuation"
    (cs_from_pred (fun i -> let c = Char.chr i in
       (c >= '!' && c <= '/') || (c >= ':' && c <= '@') ||
       (c >= '[' && c <= '`') || (c >= '{' && c <= '~')));
  define_cs "char-set:symbol"
    (cs_from_pred (fun i -> let c = Char.chr i in
       c = '$' || c = '+' || c = '<' || c = '=' || c = '>' ||
       c = '^' || c = '`' || c = '|' || c = '~'));
  define_cs "char-set:blank"
    (cs_from_pred (fun i -> let c = Char.chr i in c = ' ' || c = '\t'));
  define_cs "char-set:ascii"
    (cs_from_pred (fun i -> i < 128));
  define_cs "char-set:empty" (Datum.Char_set (cs_make ()));
  let full_cs = cs_make () in
  for i = 0 to 31 do Bytes.set full_cs.Datum.cs_bits i (Char.chr 0xFF) done;
  define_cs "char-set:full" (Datum.Char_set full_cs);
  define_cs "char-set:hex-digit"
    (cs_from_pred (fun i -> let c = Char.chr i in
       (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')));
  define_cs "char-set:graphic"
    (cs_from_pred (fun i -> i >= 0x21 && i <= 0x7E));
  define_cs "char-set:printing"
    (cs_from_pred (fun i -> (i >= 0x20 && i <= 0x7E)));
  define_cs "char-set:iso-control"
    (cs_from_pred (fun i -> i < 0x20 || (i >= 0x7F && i <= 0x9F)));
  define_cs "char-set:title-case" (Datum.Char_set (cs_make ()));
  let srfi_14_names = [
    "char-set?"; "char-set"; "char-set-copy"; "char-set-contains?";
    "char-set=?"; "char-set<=?"; "char-set>=?";
    "char-set-adjoin"; "char-set-adjoin!"; "char-set-delete"; "char-set-delete!";
    "char-set-complement"; "char-set-union"; "char-set-intersection";
    "char-set-difference"; "char-set-xor";
    "char-set-union!"; "char-set-intersection!"; "char-set-difference!"; "char-set-xor!";
    "list->char-set"; "string->char-set"; "char-set->list"; "char-set->string";
    "ucs-range->char-set";
    "char-set-fold"; "char-set-for-each"; "char-set-map";
    "char-set-count"; "char-set-every"; "char-set-any"; "char-set-filter";
    "char-set-cursor"; "char-set-ref"; "char-set-cursor-next"; "end-of-char-set?";
    "char-set-size";
    "char-set:letter"; "char-set:digit"; "char-set:letter+digit";
    "char-set:whitespace"; "char-set:upper-case"; "char-set:lower-case";
    "char-set:punctuation"; "char-set:symbol"; "char-set:blank";
    "char-set:ascii"; "char-set:empty"; "char-set:full";
    "char-set:hex-digit"; "char-set:graphic"; "char-set:printing";
    "char-set:iso-control"; "char-set:title-case";
  ] in
  build_library inst ["srfi"; "14"] srfi_14_names [];
  (* --- SRFI 13 — String Library --- *)
  let char_pred_of _name arg =
    match arg with
    | Datum.Char c -> (fun ch -> Uchar.equal ch c)
    | Datum.Char_set cs -> (fun ch -> cs_test cs (Uchar.to_int ch))
    | _ -> (fun ch -> Datum.is_true (call inst arg [Datum.Char ch]))
  in
  let get_start_end name s args =
    let len = Bytes.length s in
    let start = match args with Datum.Fixnum n :: _ -> n | _ -> 0 in
    let end_ = match args with _ :: Datum.Fixnum n :: _ -> n | _ :: _ -> len | [] -> len in
    if start < 0 || end_ > len || start > end_ then
      runtime_error (Printf.sprintf "%s: invalid range %d..%d for string of length %d" name start end_ len);
    (start, end_)
  in
  register_late "string-null?" (fun args -> match args with
    | [Datum.Str s] -> Datum.Bool (Bytes.length s = 0)
    | _ -> runtime_error "string-null?: expected 1 string");
  register_late "string-every" (fun args ->
    match args with
    | pred :: Datum.Str s :: rest ->
      let p = char_pred_of "string-every" pred in
      let start, end_ = get_start_end "string-every" s rest in
      let result = ref true in
      for i = start to end_ - 1 do
        if !result && not (p (Uchar.of_int (Char.code (Bytes.get s i)))) then
          result := false
      done;
      Datum.Bool !result
    | _ -> runtime_error "string-every: expected pred, string, [start, [end]]");
  register_late "string-any" (fun args ->
    match args with
    | pred :: Datum.Str s :: rest ->
      let p = char_pred_of "string-any" pred in
      let start, end_ = get_start_end "string-any" s rest in
      let result = ref false in
      for i = start to end_ - 1 do
        if not !result && p (Uchar.of_int (Char.code (Bytes.get s i))) then
          result := true
      done;
      Datum.Bool !result
    | _ -> runtime_error "string-any: expected pred, string, [start, [end]]");
  register_late "string-tabulate" (fun args -> match args with
    | [proc; Datum.Fixnum len] ->
      let buf = Bytes.create len in
      for i = 0 to len - 1 do
        match call inst proc [Datum.Fixnum i] with
        | Datum.Char c -> Bytes.set buf i (Char.chr (Uchar.to_int c))
        | _ -> runtime_error "string-tabulate: procedure must return a character"
      done;
      Datum.Str buf
    | _ -> runtime_error "string-tabulate: expected proc and length");
  register_late "string-take" (fun args -> match args with
    | [Datum.Str s; Datum.Fixnum n] -> Datum.Str (Bytes.sub s 0 n)
    | _ -> runtime_error "string-take: expected string and integer");
  register_late "string-take-right" (fun args -> match args with
    | [Datum.Str s; Datum.Fixnum n] ->
      let len = Bytes.length s in
      Datum.Str (Bytes.sub s (len - n) n)
    | _ -> runtime_error "string-take-right: expected string and integer");
  register_late "string-drop" (fun args -> match args with
    | [Datum.Str s; Datum.Fixnum n] ->
      Datum.Str (Bytes.sub s n (Bytes.length s - n))
    | _ -> runtime_error "string-drop: expected string and integer");
  register_late "string-drop-right" (fun args -> match args with
    | [Datum.Str s; Datum.Fixnum n] ->
      Datum.Str (Bytes.sub s 0 (Bytes.length s - n))
    | _ -> runtime_error "string-drop-right: expected string and integer");
  register_late "string-pad" (fun args ->
    match args with
    | Datum.Str s :: Datum.Fixnum len :: rest ->
      let ch = match rest with Datum.Char c :: _ -> Char.chr (Uchar.to_int c) | _ -> ' ' in
      let slen = Bytes.length s in
      if slen >= len then Datum.Str (Bytes.sub s (slen - len) len)
      else begin
        let result = Bytes.make len ch in
        Bytes.blit s 0 result (len - slen) slen;
        Datum.Str result
      end
    | _ -> runtime_error "string-pad: expected string, length, [char]");
  register_late "string-pad-right" (fun args ->
    match args with
    | Datum.Str s :: Datum.Fixnum len :: rest ->
      let ch = match rest with Datum.Char c :: _ -> Char.chr (Uchar.to_int c) | _ -> ' ' in
      let slen = Bytes.length s in
      if slen >= len then Datum.Str (Bytes.sub s 0 len)
      else begin
        let result = Bytes.make len ch in
        Bytes.blit s 0 result 0 slen;
        Datum.Str result
      end
    | _ -> runtime_error "string-pad-right: expected string, length, [char]");
  register_late "string-trim" (fun args ->
    match args with
    | Datum.Str s :: rest ->
      let p = match rest with pred :: _ -> char_pred_of "string-trim" pred
        | [] -> (fun c -> Uchar.to_int c = Char.code ' ' || Uchar.to_int c = Char.code '\t' ||
                          Uchar.to_int c = Char.code '\n' || Uchar.to_int c = Char.code '\r') in
      let len = Bytes.length s in
      let i = ref 0 in
      while !i < len && p (Uchar.of_int (Char.code (Bytes.get s !i))) do incr i done;
      Datum.Str (Bytes.sub s !i (len - !i))
    | _ -> runtime_error "string-trim: expected string, [pred]");
  register_late "string-trim-right" (fun args ->
    match args with
    | Datum.Str s :: rest ->
      let p = match rest with pred :: _ -> char_pred_of "string-trim-right" pred
        | [] -> (fun c -> Uchar.to_int c = Char.code ' ' || Uchar.to_int c = Char.code '\t' ||
                          Uchar.to_int c = Char.code '\n' || Uchar.to_int c = Char.code '\r') in
      let len = Bytes.length s in
      let j = ref (len - 1) in
      while !j >= 0 && p (Uchar.of_int (Char.code (Bytes.get s !j))) do decr j done;
      Datum.Str (Bytes.sub s 0 (!j + 1))
    | _ -> runtime_error "string-trim-right: expected string, [pred]");
  register_late "string-trim-both" (fun args ->
    match args with
    | Datum.Str s :: rest ->
      let p = match rest with pred :: _ -> char_pred_of "string-trim-both" pred
        | [] -> (fun c -> Uchar.to_int c = Char.code ' ' || Uchar.to_int c = Char.code '\t' ||
                          Uchar.to_int c = Char.code '\n' || Uchar.to_int c = Char.code '\r') in
      let len = Bytes.length s in
      let i = ref 0 in
      while !i < len && p (Uchar.of_int (Char.code (Bytes.get s !i))) do incr i done;
      let j = ref (len - 1) in
      while !j >= !i && p (Uchar.of_int (Char.code (Bytes.get s !j))) do decr j done;
      Datum.Str (Bytes.sub s !i (!j - !i + 1))
    | _ -> runtime_error "string-trim-both: expected string, [pred]");
  register_late "string-prefix-length" (fun args -> match args with
    | [Datum.Str s1; Datum.Str s2] ->
      let len = min (Bytes.length s1) (Bytes.length s2) in
      let i = ref 0 in
      while !i < len && Bytes.get s1 !i = Bytes.get s2 !i do incr i done;
      Datum.Fixnum !i
    | _ -> runtime_error "string-prefix-length: expected 2 strings");
  register_late "string-suffix-length" (fun args -> match args with
    | [Datum.Str s1; Datum.Str s2] ->
      let l1 = Bytes.length s1 and l2 = Bytes.length s2 in
      let len = min l1 l2 in
      let i = ref 0 in
      while !i < len && Bytes.get s1 (l1 - 1 - !i) = Bytes.get s2 (l2 - 1 - !i) do incr i done;
      Datum.Fixnum !i
    | _ -> runtime_error "string-suffix-length: expected 2 strings");
  register_late "string-prefix?" (fun args -> match args with
    | [Datum.Str prefix; Datum.Str s] ->
      let plen = Bytes.length prefix and slen = Bytes.length s in
      Datum.Bool (plen <= slen &&
                  Bytes.sub s 0 plen = prefix)
    | _ -> runtime_error "string-prefix?: expected 2 strings");
  register_late "string-suffix?" (fun args -> match args with
    | [Datum.Str suffix; Datum.Str s] ->
      let sfxlen = Bytes.length suffix and slen = Bytes.length s in
      Datum.Bool (sfxlen <= slen &&
                  Bytes.sub s (slen - sfxlen) sfxlen = suffix)
    | _ -> runtime_error "string-suffix?: expected 2 strings");
  register_late "string-index" (fun args ->
    match args with
    | pred :: Datum.Str s :: rest ->
      let p = char_pred_of "string-index" pred in
      let start, end_ = get_start_end "string-index" s rest in
      let result = ref (Datum.Bool false) in
      let found = ref false in
      for i = start to end_ - 1 do
        if not !found && p (Uchar.of_int (Char.code (Bytes.get s i))) then begin
          result := Datum.Fixnum i;
          found := true
        end
      done;
      !result
    | _ -> runtime_error "string-index: expected pred, string, [start, [end]]");
  register_late "string-index-right" (fun args ->
    match args with
    | pred :: Datum.Str s :: rest ->
      let p = char_pred_of "string-index-right" pred in
      let start, end_ = get_start_end "string-index-right" s rest in
      let result = ref (Datum.Bool false) in
      for i = end_ - 1 downto start do
        if !result = Datum.Bool false && p (Uchar.of_int (Char.code (Bytes.get s i))) then
          result := Datum.Fixnum i
      done;
      !result
    | _ -> runtime_error "string-index-right: expected pred, string, [start, [end]]");
  register_late "string-skip" (fun args ->
    match args with
    | pred :: Datum.Str s :: rest ->
      let p = char_pred_of "string-skip" pred in
      let start, end_ = get_start_end "string-skip" s rest in
      let result = ref (Datum.Bool false) in
      let found = ref false in
      for i = start to end_ - 1 do
        if not !found && not (p (Uchar.of_int (Char.code (Bytes.get s i)))) then begin
          result := Datum.Fixnum i;
          found := true
        end
      done;
      !result
    | _ -> runtime_error "string-skip: expected pred, string, [start, [end]]");
  register_late "string-skip-right" (fun args ->
    match args with
    | pred :: Datum.Str s :: rest ->
      let p = char_pred_of "string-skip-right" pred in
      let start, end_ = get_start_end "string-skip-right" s rest in
      let result = ref (Datum.Bool false) in
      for i = end_ - 1 downto start do
        if !result = Datum.Bool false && not (p (Uchar.of_int (Char.code (Bytes.get s i)))) then
          result := Datum.Fixnum i
      done;
      !result
    | _ -> runtime_error "string-skip-right: expected pred, string, [start, [end]]");
  register_late "string-contains" (fun args -> match args with
    | [Datum.Str haystack; Datum.Str needle] ->
      let hlen = Bytes.length haystack and nlen = Bytes.length needle in
      if nlen = 0 then Datum.Fixnum 0
      else begin
        let result = ref (Datum.Bool false) in
        let found = ref false in
        for i = 0 to hlen - nlen do
          if not !found && Bytes.sub haystack i nlen = needle then begin
            result := Datum.Fixnum i;
            found := true
          end
        done;
        !result
      end
    | _ -> runtime_error "string-contains: expected 2 strings");
  register_late "string-contains-ci" (fun args -> match args with
    | [Datum.Str haystack; Datum.Str needle] ->
      let hlen = Bytes.length haystack and nlen = Bytes.length needle in
      let lower b = Bytes.of_string (String.lowercase_ascii (Bytes.to_string b)) in
      let needle_low = lower needle in
      if nlen = 0 then Datum.Fixnum 0
      else begin
        let result = ref (Datum.Bool false) in
        let found = ref false in
        for i = 0 to hlen - nlen do
          if not !found && lower (Bytes.sub haystack i nlen) = needle_low then begin
            result := Datum.Fixnum i;
            found := true
          end
        done;
        !result
      end
    | _ -> runtime_error "string-contains-ci: expected 2 strings");
  register_late "string-count" (fun args ->
    match args with
    | pred :: Datum.Str s :: rest ->
      let p = char_pred_of "string-count" pred in
      let start, end_ = get_start_end "string-count" s rest in
      let n = ref 0 in
      for i = start to end_ - 1 do
        if p (Uchar.of_int (Char.code (Bytes.get s i))) then incr n
      done;
      Datum.Fixnum !n
    | _ -> runtime_error "string-count: expected pred, string, [start, [end]]");
  register_late "string-reverse" (fun args -> match args with
    | [Datum.Str s] ->
      let len = Bytes.length s in
      let result = Bytes.create len in
      for i = 0 to len - 1 do
        Bytes.set result i (Bytes.get s (len - 1 - i))
      done;
      Datum.Str result
    | _ -> runtime_error "string-reverse: expected 1 string");
  register_late "string-concatenate" (fun args -> match args with
    | [lst] ->
      let buf = Buffer.create 64 in
      let rec walk = function
        | Datum.Nil -> ()
        | Datum.Pair { car = Datum.Str s; cdr } ->
          Buffer.add_bytes buf s;
          walk cdr
        | _ -> runtime_error "string-concatenate: expected list of strings"
      in
      walk lst;
      Datum.Str (Bytes.of_string (Buffer.contents buf))
    | _ -> runtime_error "string-concatenate: expected 1 argument");
  register_late "string-concatenate-reverse" (fun args ->
    match args with
    | [lst] ->
      let strs = ref [] in
      let rec walk = function
        | Datum.Nil -> ()
        | Datum.Pair { car = Datum.Str s; cdr } ->
          strs := s :: !strs;
          walk cdr
        | _ -> runtime_error "string-concatenate-reverse: expected list of strings"
      in
      walk lst;
      let buf = Buffer.create 64 in
      List.iter (Buffer.add_bytes buf) !strs;
      Datum.Str (Bytes.of_string (Buffer.contents buf))
    | _ -> runtime_error "string-concatenate-reverse: expected 1 argument");
  register_late "string-fold" (fun args ->
    match args with
    | proc :: init :: Datum.Str s :: rest ->
      let start, end_ = get_start_end "string-fold" s rest in
      let acc = ref init in
      for i = start to end_ - 1 do
        acc := call inst proc [Datum.Char (Uchar.of_int (Char.code (Bytes.get s i))); !acc]
      done;
      !acc
    | _ -> runtime_error "string-fold: expected proc, init, string, [start, [end]]");
  register_late "string-fold-right" (fun args ->
    match args with
    | proc :: init :: Datum.Str s :: rest ->
      let start, end_ = get_start_end "string-fold-right" s rest in
      let acc = ref init in
      for i = end_ - 1 downto start do
        acc := call inst proc [Datum.Char (Uchar.of_int (Char.code (Bytes.get s i))); !acc]
      done;
      !acc
    | _ -> runtime_error "string-fold-right: expected proc, init, string, [start, [end]]");
  register_late "string-filter" (fun args ->
    match args with
    | pred :: Datum.Str s :: rest ->
      let p = char_pred_of "string-filter" pred in
      let start, end_ = get_start_end "string-filter" s rest in
      let buf = Buffer.create (end_ - start) in
      for i = start to end_ - 1 do
        let c = Bytes.get s i in
        if p (Uchar.of_int (Char.code c)) then Buffer.add_char buf c
      done;
      Datum.Str (Bytes.of_string (Buffer.contents buf))
    | _ -> runtime_error "string-filter: expected pred, string, [start, [end]]");
  register_late "string-delete" (fun args ->
    match args with
    | pred :: Datum.Str s :: rest ->
      let p = char_pred_of "string-delete" pred in
      let start, end_ = get_start_end "string-delete" s rest in
      let buf = Buffer.create (end_ - start) in
      for i = start to end_ - 1 do
        let c = Bytes.get s i in
        if not (p (Uchar.of_int (Char.code c))) then Buffer.add_char buf c
      done;
      Datum.Str (Bytes.of_string (Buffer.contents buf))
    | _ -> runtime_error "string-delete: expected pred, string, [start, [end]]");
  register_late "string-replace" (fun args -> match args with
    | [Datum.Str s1; Datum.Str s2; Datum.Fixnum start; Datum.Fixnum end_] ->
      let len1 = Bytes.length s1 and len2 = Bytes.length s2 in
      let result_len = start + len2 + (len1 - end_) in
      let result = Bytes.create result_len in
      Bytes.blit s1 0 result 0 start;
      Bytes.blit s2 0 result start len2;
      Bytes.blit s1 end_ result (start + len2) (len1 - end_);
      Datum.Str result
    | _ -> runtime_error "string-replace: expected string, string, start, end");
  register_late "string-titlecase" (fun args -> match args with
    | [Datum.Str s] ->
      let len = Bytes.length s in
      let result = Bytes.copy s in
      let word_start = ref true in
      for i = 0 to len - 1 do
        let c = Bytes.get result i in
        if c = ' ' || c = '\t' || c = '\n' || c = '\r' then
          word_start := true
        else begin
          if !word_start then begin
            Bytes.set result i (Char.uppercase_ascii c);
            word_start := false
          end else
            Bytes.set result i (Char.lowercase_ascii c)
        end
      done;
      Datum.Str result
    | _ -> runtime_error "string-titlecase: expected 1 string");
  register_late "xsubstring" (fun args -> match args with
    | [Datum.Str s; Datum.Fixnum from; Datum.Fixnum to_] ->
      let len = Bytes.length s in
      if len = 0 then runtime_error "xsubstring: empty string";
      let result_len = to_ - from in
      let result = Bytes.create result_len in
      for i = 0 to result_len - 1 do
        Bytes.set result i (Bytes.get s (((from + i) mod len + len) mod len))
      done;
      Datum.Str result
    | [Datum.Str s; Datum.Fixnum from] ->
      let len = Bytes.length s in
      if len = 0 then runtime_error "xsubstring: empty string";
      let to_ = from + len in
      let result_len = to_ - from in
      let result = Bytes.create result_len in
      for i = 0 to result_len - 1 do
        Bytes.set result i (Bytes.get s (((from + i) mod len + len) mod len))
      done;
      Datum.Str result
    | _ -> runtime_error "xsubstring: expected string, from, [to]");
  let srfi_13_names = [
    "string-null?"; "string-every"; "string-any"; "string-tabulate";
    "string-take"; "string-take-right"; "string-drop"; "string-drop-right";
    "string-pad"; "string-pad-right";
    "string-trim"; "string-trim-right"; "string-trim-both";
    "string-prefix-length"; "string-suffix-length";
    "string-prefix?"; "string-suffix?";
    "string-index"; "string-index-right"; "string-skip"; "string-skip-right";
    "string-contains"; "string-contains-ci";
    "string-count"; "string-reverse";
    "string-concatenate"; "string-concatenate-reverse";
    "string-fold"; "string-fold-right";
    "string-filter"; "string-delete";
    "string-replace"; "string-titlecase"; "xsubstring";
  ] in
  build_library inst ["srfi"; "13"] srfi_13_names [];
  (* --- SRFI 115 — Regexp --- *)
  let rec sre_to_node datum num_groups =
    match datum with
    | Datum.Str s ->
      let nodes = ref [] in
      Bytes.iter (fun c -> nodes := Regexp_engine.Lit (Char.code c) :: !nodes) s;
      (Regexp_engine.Seq (List.rev !nodes), num_groups)
    | Datum.Char c ->
      (Regexp_engine.Lit (Uchar.to_int c), num_groups)
    | Datum.Char_set cs ->
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | Datum.Symbol s -> sre_symbol_to_node s num_groups
    | Datum.Pair _ -> begin
      match Datum.to_list datum with
      | Some (Datum.Symbol op :: args) -> sre_form_to_node op args num_groups
      | _ -> runtime_error (Printf.sprintf "regexp: invalid SRE: %s" (Datum.to_string datum))
    end
    | _ -> runtime_error (Printf.sprintf "regexp: invalid SRE: %s" (Datum.to_string datum))
  and sre_symbol_to_node s num_groups =
    match s with
    | "any" -> (Regexp_engine.Any, num_groups)
    | "bos" -> (Regexp_engine.Bos, num_groups)
    | "eos" -> (Regexp_engine.Eos, num_groups)
    | "bol" -> (Regexp_engine.Bol, num_groups)
    | "eol" -> (Regexp_engine.Eol, num_groups)
    | "alphabetic" | "alpha" ->
      let cs = cs_make () in
      for i = 0 to 255 do
        let c = Char.chr i in
        if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then cs_set cs i
      done;
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | "numeric" | "num" ->
      let cs = cs_make () in
      for i = Char.code '0' to Char.code '9' do cs_set cs i done;
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | "alphanumeric" | "alnum" ->
      let cs = cs_make () in
      for i = 0 to 255 do
        let c = Char.chr i in
        if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
          cs_set cs i
      done;
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | "whitespace" | "space" | "white" ->
      let cs = cs_make () in
      List.iter (fun c -> cs_set cs (Char.code c)) [' '; '\t'; '\n'; '\r'];
      cs_set cs 0x0B; cs_set cs 0x0C;
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | "upper-case" | "upper" ->
      let cs = cs_make () in
      for i = Char.code 'A' to Char.code 'Z' do cs_set cs i done;
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | "lower-case" | "lower" ->
      let cs = cs_make () in
      for i = Char.code 'a' to Char.code 'z' do cs_set cs i done;
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | "punctuation" | "punct" ->
      let cs = cs_make () in
      for i = 0 to 255 do
        let c = Char.chr i in
        if (c >= '!' && c <= '/') || (c >= ':' && c <= '@') ||
           (c >= '[' && c <= '`') || (c >= '{' && c <= '~') then
          cs_set cs i
      done;
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | "digit" ->
      let cs = cs_make () in
      for i = Char.code '0' to Char.code '9' do cs_set cs i done;
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | "hex-digit" | "xdigit" ->
      let cs = cs_make () in
      for i = Char.code '0' to Char.code '9' do cs_set cs i done;
      for i = Char.code 'A' to Char.code 'F' do cs_set cs i done;
      for i = Char.code 'a' to Char.code 'f' do cs_set cs i done;
      (Regexp_engine.Class (Bytes.copy cs.Datum.cs_bits), num_groups)
    | _ -> runtime_error (Printf.sprintf "regexp: unknown SRE symbol: %s" s)
  and sre_form_to_node op args num_groups =
    match op with
    | ":" | "seq" ->
      let nodes, ng = List.fold_left (fun (acc, ng) a ->
        let n, ng' = sre_to_node a ng in
        (n :: acc, ng')
      ) ([], num_groups) args in
      (Regexp_engine.Seq (List.rev nodes), ng)
    | "or" ->
      (match args with
       | [] -> (Regexp_engine.Seq [], num_groups)  (* matches empty *)
       | [a] -> sre_to_node a num_groups
       | a :: rest ->
         let na, ng = sre_to_node a num_groups in
         let rest_datum = Datum.list_of (Datum.Symbol "or" :: rest) in
         let nb, ng' = sre_to_node rest_datum ng in
         (Regexp_engine.Alt (na, nb), ng'))
    | "*" ->
      (match args with
       | [a] ->
         let n, ng = sre_to_node a num_groups in
         (Regexp_engine.Rep (n, 0, None, true), ng)
       | _ -> runtime_error "regexp: * expects exactly 1 argument")
    | "+" ->
      (match args with
       | [a] ->
         let n, ng = sre_to_node a num_groups in
         (Regexp_engine.Rep (n, 1, None, true), ng)
       | _ -> runtime_error "regexp: + expects exactly 1 argument")
    | "?" ->
      (match args with
       | [a] ->
         let n, ng = sre_to_node a num_groups in
         (Regexp_engine.Rep (n, 0, Some 1, true), ng)
       | _ -> runtime_error "regexp: ? expects exactly 1 argument")
    | "=" ->
      (match args with
       | [Datum.Fixnum count; a] ->
         let n, ng = sre_to_node a num_groups in
         (Regexp_engine.Rep (n, count, Some count, true), ng)
       | _ -> runtime_error "regexp: = expects count and pattern")
    | ">=" ->
      (match args with
       | [Datum.Fixnum min; a] ->
         let n, ng = sre_to_node a num_groups in
         (Regexp_engine.Rep (n, min, None, true), ng)
       | _ -> runtime_error "regexp: >= expects min and pattern")
    | "**" ->
      (match args with
       | [Datum.Fixnum min; Datum.Fixnum max; a] ->
         let n, ng = sre_to_node a num_groups in
         (Regexp_engine.Rep (n, min, Some max, true), ng)
       | _ -> runtime_error "regexp: ** expects min, max, and pattern")
    | "submatch" | "->"|  "$" ->
      let idx = num_groups in
      let ng = num_groups + 1 in
      (match args with
       | [a] ->
         let n, ng' = sre_to_node a ng in
         (Regexp_engine.Group (idx, n), ng')
       | _ ->
         let nodes, ng' = List.fold_left (fun (acc, ng) a ->
           let n, ng' = sre_to_node a ng in
           (n :: acc, ng')
         ) ([], ng) args in
         (Regexp_engine.Group (idx, Regexp_engine.Seq (List.rev nodes)), ng'))
    | "~" | "complement" ->
      (match args with
       | [a] ->
         let n, ng = sre_to_node a num_groups in
         (match n with
          | Regexp_engine.Class bits ->
            let nbits = Bytes.copy bits in
            for i = 0 to 31 do
              Bytes.set nbits i (Char.chr (Char.code (Bytes.get nbits i) lxor 0xFF))
            done;
            (Regexp_engine.Class nbits, ng)
          | _ -> runtime_error "regexp: complement only works on character classes")
       | _ -> runtime_error "regexp: complement expects 1 argument")
    | "w/nocase" ->
      (match args with
       | [a] ->
         let n, ng = sre_to_node a num_groups in
         (sre_nocase n, ng)
       | _ ->
         let nodes, ng' = List.fold_left (fun (acc, ng) a ->
           let n, ng' = sre_to_node a ng in
           (n :: acc, ng')
         ) ([], num_groups) args in
         (sre_nocase (Regexp_engine.Seq (List.rev nodes)), ng'))
    | _ -> runtime_error (Printf.sprintf "regexp: unknown SRE form: %s" op)
  and sre_nocase node =
    match node with
    | Regexp_engine.Lit b ->
      let c = Char.chr b in
      if c >= 'A' && c <= 'Z' then
        Regexp_engine.Alt (Regexp_engine.Lit b, Regexp_engine.Lit (Char.code (Char.lowercase_ascii c)))
      else if c >= 'a' && c <= 'z' then
        Regexp_engine.Alt (Regexp_engine.Lit b, Regexp_engine.Lit (Char.code (Char.uppercase_ascii c)))
      else Regexp_engine.Lit b
    | Regexp_engine.Seq nodes -> Regexp_engine.Seq (List.map sre_nocase nodes)
    | Regexp_engine.Alt (a, b) -> Regexp_engine.Alt (sre_nocase a, sre_nocase b)
    | Regexp_engine.Rep (n, min, max, g) -> Regexp_engine.Rep (sre_nocase n, min, max, g)
    | Regexp_engine.Group (idx, n) -> Regexp_engine.Group (idx, sre_nocase n)
    | Regexp_engine.Class bits ->
      let nbits = Bytes.copy bits in
      let set_bit n =
        if n >= 0 && n < 256 then
          Bytes.set nbits (n / 8) (Char.chr (Char.code (Bytes.get nbits (n / 8)) lor (1 lsl (n mod 8))))
      in
      for i = 0 to 255 do
        let c = Char.chr i in
        if class_test nbits i then begin
          if c >= 'A' && c <= 'Z' then set_bit (Char.code (Char.lowercase_ascii c))
          else if c >= 'a' && c <= 'z' then set_bit (Char.code (Char.uppercase_ascii c))
        end
      done;
      Regexp_engine.Class nbits
    | other -> other
  and class_test bits n =
    n >= 0 && n < 256 &&
    Char.code (Bytes.get bits (n / 8)) land (1 lsl (n mod 8)) <> 0
  in
  let compile_sre datum =
    let node, num_groups = sre_to_node datum 1 in
    let rx = Regexp_engine.compile (Regexp_engine.Group (0, node)) (num_groups) in
    { Datum.rx_compiled = Obj.repr rx; rx_source = datum }
  in
  let _require_rx name = function
    | Datum.Regexp rx -> rx
    | v -> runtime_error (Printf.sprintf "%s: expected regexp, got %s" name (Datum.to_string v))
  in
  let get_compiled rx = (Obj.obj rx.Datum.rx_compiled : Regexp_engine.compiled) in
  let ensure_rx _name = function
    | Datum.Regexp rx -> rx
    | Datum.Str s ->
      compile_sre (Datum.Str s)
    | other ->
      compile_sre other
  in
  let make_match_obj input (result : Regexp_engine.match_result) =
    if not result.matched then Datum.Bool false
    else
      let n = Array.length result.groups in
      let groups = Array.init n (fun i ->
        match result.groups.(i) with
        | Some (s, e) ->
          Datum.Pair { car = Datum.Fixnum s; cdr = Datum.Fixnum e }
        | None -> Datum.Bool false
      ) in
      Datum.Vector [|
        Datum.Symbol "match-obj";
        Datum.Str (Bytes.copy input);
        Datum.Vector groups
      |]
  in
  let is_match_obj = function
    | Datum.Vector [| Datum.Symbol "match-obj"; _; _ |] -> true
    | _ -> false
  in
  let match_obj_input = function
    | Datum.Vector [| _; Datum.Str s; _ |] -> s
    | _ -> runtime_error "not a match object"
  in
  let match_obj_groups = function
    | Datum.Vector [| _; _; Datum.Vector g |] -> g
    | _ -> runtime_error "not a match object"
  in
  register_late "regexp" (fun args -> match args with
    | [sre] -> Datum.Regexp (compile_sre sre)
    | _ -> runtime_error "regexp: expected 1 argument");
  register_late "regexp?" (fun args -> match args with
    | [Datum.Regexp _] -> Datum.Bool true
    | [_] -> Datum.Bool false
    | _ -> runtime_error "regexp?: expected 1 argument");
  register_late "regexp-matches" (fun args -> match args with
    | [rx_val; Datum.Str s] ->
      let rx = ensure_rx "regexp-matches" rx_val in
      let compiled = get_compiled rx in
      let result = Regexp_engine.exec compiled s 0 (Bytes.length s) in
      make_match_obj s result
    | _ -> runtime_error "regexp-matches: expected regexp/sre and string");
  register_late "regexp-matches?" (fun args -> match args with
    | [rx_val; Datum.Str s] ->
      let rx = ensure_rx "regexp-matches?" rx_val in
      let compiled = get_compiled rx in
      let result = Regexp_engine.exec compiled s 0 (Bytes.length s) in
      Datum.Bool result.matched
    | _ -> runtime_error "regexp-matches?: expected regexp/sre and string");
  register_late "regexp-search" (fun args -> match args with
    | [rx_val; Datum.Str s] ->
      let rx = ensure_rx "regexp-search" rx_val in
      let compiled = get_compiled rx in
      let result = Regexp_engine.search compiled s 0 (Bytes.length s) in
      make_match_obj s result
    | _ -> runtime_error "regexp-search: expected regexp/sre and string");
  register_late "regexp-replace" (fun args ->
    match args with
    | [rx_val; Datum.Str s; replacement] ->
      let rx = ensure_rx "regexp-replace" rx_val in
      let compiled = get_compiled rx in
      let result = Regexp_engine.search compiled s 0 (Bytes.length s) in
      if not result.matched then Datum.Str (Bytes.copy s)
      else begin
        match result.groups.(0) with
        | Some (start, end_) ->
          let rep_str = match replacement with
            | Datum.Str r -> Bytes.to_string r
            | _ ->
              let m = make_match_obj s result in
              let v = call inst replacement [m] in
              (match v with Datum.Str r -> Bytes.to_string r
               | _ -> Datum.to_display_string v)
          in
          let buf = Buffer.create (Bytes.length s) in
          Buffer.add_bytes buf (Bytes.sub s 0 start);
          Buffer.add_string buf rep_str;
          Buffer.add_bytes buf (Bytes.sub s end_ (Bytes.length s - end_));
          Datum.Str (Bytes.of_string (Buffer.contents buf))
        | None -> Datum.Str (Bytes.copy s)
      end
    | _ -> runtime_error "regexp-replace: expected regexp/sre, string, and replacement");
  register_late "regexp-replace-all" (fun args ->
    match args with
    | [rx_val; Datum.Str s; replacement] ->
      let rx = ensure_rx "regexp-replace-all" rx_val in
      let compiled = get_compiled rx in
      let len = Bytes.length s in
      let buf = Buffer.create len in
      let pos = ref 0 in
      let continue = ref true in
      while !continue do
        if !pos > len then continue := false
        else begin
          let result = Regexp_engine.search compiled s !pos len in
          if not result.matched then begin
            Buffer.add_bytes buf (Bytes.sub s !pos (len - !pos));
            continue := false
          end else begin
            match result.groups.(0) with
            | Some (start, end_) ->
              Buffer.add_bytes buf (Bytes.sub s !pos (start - !pos));
              let rep_str = match replacement with
                | Datum.Str r -> Bytes.to_string r
                | _ ->
                  let m = make_match_obj s result in
                  let v = call inst replacement [m] in
                  (match v with Datum.Str r -> Bytes.to_string r
                   | _ -> Datum.to_display_string v)
              in
              Buffer.add_string buf rep_str;
              pos := if end_ = start then begin
                if end_ < len then begin
                  Buffer.add_char buf (Bytes.get s end_);
                  end_ + 1
                end else begin
                  continue := false;
                  end_
                end
              end else end_
            | None -> Buffer.add_bytes buf (Bytes.sub s !pos (len - !pos)); continue := false
          end
        end
      done;
      Datum.Str (Bytes.of_string (Buffer.contents buf))
    | _ -> runtime_error "regexp-replace-all: expected regexp/sre, string, and replacement");
  register_late "regexp-fold" (fun args ->
    match args with
    | [rx_val; proc; init; Datum.Str s] ->
      let rx = ensure_rx "regexp-fold" rx_val in
      let compiled = get_compiled rx in
      let len = Bytes.length s in
      let acc = ref init in
      let pos = ref 0 in
      let idx = ref 0 in
      let continue = ref true in
      while !continue do
        if !pos > len then continue := false
        else begin
          let result = Regexp_engine.search compiled s !pos len in
          if not result.matched then
            continue := false
          else begin
            match result.groups.(0) with
            | Some (start, end_) ->
              let m = make_match_obj s result in
              acc := call inst proc [Datum.Fixnum !idx; m; !acc];
              incr idx;
              pos := if end_ = start then begin
                if end_ < len then end_ + 1
                else begin continue := false; end_ end
              end else end_
            | None -> continue := false
          end
        end
      done;
      !acc
    | [rx_val; proc; init; Datum.Str s; finish] ->
      let rx = ensure_rx "regexp-fold" rx_val in
      let compiled = get_compiled rx in
      let len = Bytes.length s in
      let acc = ref init in
      let pos = ref 0 in
      let idx = ref 0 in
      let continue = ref true in
      while !continue do
        if !pos > len then continue := false
        else begin
          let result = Regexp_engine.search compiled s !pos len in
          if not result.matched then
            continue := false
          else begin
            match result.groups.(0) with
            | Some (start, end_) ->
              let m = make_match_obj s result in
              acc := call inst proc [Datum.Fixnum !idx; m; !acc];
              incr idx;
              pos := if end_ = start then begin
                if end_ < len then end_ + 1
                else begin continue := false; end_ end
              end else end_
            | None -> continue := false
          end
        end
      done;
      call inst finish [Datum.Fixnum !idx; Datum.Bool false; !acc]
    | _ -> runtime_error "regexp-fold: expected regexp/sre, proc, init, string, [finish]");
  register_late "regexp-extract" (fun args -> match args with
    | [rx_val; Datum.Str s] ->
      let rx = ensure_rx "regexp-extract" rx_val in
      let compiled = get_compiled rx in
      let len = Bytes.length s in
      let acc = ref [] in
      let pos = ref 0 in
      let continue = ref true in
      while !continue do
        if !pos > len then continue := false
        else begin
          let result = Regexp_engine.search compiled s !pos len in
          if not result.matched then
            continue := false
          else begin
            match result.groups.(0) with
            | Some (start, end_) ->
              acc := Datum.Str (Bytes.sub s start (end_ - start)) :: !acc;
              pos := if end_ = start then begin
                if end_ < len then end_ + 1
                else begin continue := false; end_ end
              end else end_
            | None -> continue := false
          end
        end
      done;
      Datum.list_of (List.rev !acc)
    | _ -> runtime_error "regexp-extract: expected regexp/sre and string");
  register_late "regexp-split" (fun args -> match args with
    | [rx_val; Datum.Str s] ->
      let rx = ensure_rx "regexp-split" rx_val in
      let compiled = get_compiled rx in
      let len = Bytes.length s in
      let acc = ref [] in
      let pos = ref 0 in
      let continue = ref true in
      while !continue do
        if !pos > len then continue := false
        else begin
          let result = Regexp_engine.search compiled s !pos len in
          if not result.matched then begin
            acc := Datum.Str (Bytes.sub s !pos (len - !pos)) :: !acc;
            continue := false
          end else begin
            match result.groups.(0) with
            | Some (start, end_) ->
              acc := Datum.Str (Bytes.sub s !pos (start - !pos)) :: !acc;
              pos := if end_ = start then begin
                if end_ < len then begin
                  acc := Datum.Str (Bytes.sub s end_ 1) :: !acc;
                  end_ + 1
                end else begin continue := false; end_ end
              end else end_
            | None ->
              acc := Datum.Str (Bytes.sub s !pos (len - !pos)) :: !acc;
              continue := false
          end
        end
      done;
      Datum.list_of (List.rev !acc)
    | _ -> runtime_error "regexp-split: expected regexp/sre and string");
  register_late "regexp-match?" (fun args -> match args with
    | [v] -> Datum.Bool (is_match_obj v)
    | _ -> runtime_error "regexp-match?: expected 1 argument");
  register_late "regexp-match-count" (fun args -> match args with
    | [m] ->
      let groups = match_obj_groups m in
      Datum.Fixnum (Array.length groups)
    | _ -> runtime_error "regexp-match-count: expected 1 argument");
  register_late "regexp-match-submatch" (fun args -> match args with
    | [m; Datum.Fixnum idx] ->
      let input = match_obj_input m in
      let groups = match_obj_groups m in
      if idx < 0 || idx >= Array.length groups then Datum.Bool false
      else begin
        match groups.(idx) with
        | Datum.Pair { car = Datum.Fixnum s; cdr = Datum.Fixnum e } ->
          Datum.Str (Bytes.sub input s (e - s))
        | _ -> Datum.Bool false
      end
    | _ -> runtime_error "regexp-match-submatch: expected match and index");
  register_late "regexp-match-submatch-start" (fun args -> match args with
    | [m; Datum.Fixnum idx] ->
      let groups = match_obj_groups m in
      if idx < 0 || idx >= Array.length groups then Datum.Bool false
      else begin
        match groups.(idx) with
        | Datum.Pair { car = Datum.Fixnum s; _ } -> Datum.Fixnum s
        | _ -> Datum.Bool false
      end
    | _ -> runtime_error "regexp-match-submatch-start: expected match and index");
  register_late "regexp-match-submatch-end" (fun args -> match args with
    | [m; Datum.Fixnum idx] ->
      let groups = match_obj_groups m in
      if idx < 0 || idx >= Array.length groups then Datum.Bool false
      else begin
        match groups.(idx) with
        | Datum.Pair { car = _; cdr = Datum.Fixnum e } -> Datum.Fixnum e
        | _ -> Datum.Bool false
      end
    | _ -> runtime_error "regexp-match-submatch-end: expected match and index");
  let srfi_115_names = [
    "regexp"; "regexp?";
    "regexp-matches"; "regexp-matches?"; "regexp-search";
    "regexp-replace"; "regexp-replace-all";
    "regexp-fold"; "regexp-extract"; "regexp-split";
    "regexp-match?"; "regexp-match-count";
    "regexp-match-submatch"; "regexp-match-submatch-start"; "regexp-match-submatch-end";
  ] in
  build_library inst ["srfi"; "115"] srfi_115_names [];
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
    ignore (Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) lib_env code)
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
    | { datum = Syntax.Symbol "include-shared"; _ } :: args ->
      (match args with
       | [{ datum = Syntax.Str name; _ }] ->
         if tracking then fasl_decls := Fasl.Lib_native name :: !fasl_decls;
         let sld_dir = Option.map Filename.dirname sld_path in
         inst.extension_lib_env := Some (lib_env, lib_syn_env);
         Fun.protect ~finally:(fun () -> inst.extension_lib_env := None)
           (fun () ->
             !load_native_ref inst
               ~search_dirs:!(inst.search_paths) ~sld_dir name)
       | _ -> raise (Compiler.Compile_error (loc,
           "include-shared: expected a single string argument")))
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
     let syn_bindings =
       Hashtbl.fold (fun name b acc -> (name, b) :: acc) syntax_exports []
     in
     let fasl : Fasl.lib_fasl = {
       lib_name;
       has_syntax_exports = has_syn;
       exports = List.rev !export_specs;
       declarations = List.rev !fasl_decls;
       syntax_bindings = syn_bindings;
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
      ignore (Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) lib_env code)
    | Fasl.Lib_native name ->
      let lib_syn_env = Expander.core_env () in
      inst.extension_lib_env := Some (lib_env, lib_syn_env);
      Fun.protect ~finally:(fun () -> inst.extension_lib_env := None)
        (fun () ->
          !load_native_ref inst
            ~search_dirs:!(inst.search_paths) ~sld_dir:None name)
  ) fasl.declarations;
  let syntax_exports = Hashtbl.create (List.length fasl.syntax_bindings) in
  List.iter (fun (name, binding) ->
    Hashtbl.replace syntax_exports name binding
  ) fasl.syntax_bindings;
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
      if not (Hashtbl.mem syntax_exports external_name) then
        failwith (Printf.sprintf
          "FASL replay: exported name not defined: %s" internal_name)
  ) fasl.exports;
  let lib : Library.t = {
    name = fasl.lib_name;
    env = lib_env;
    exports;
    syntax_exports;
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
  | [] -> ()  (* not found; will fail later at resolve_import *)
  | sld_path :: _ ->
    let fasl_path = Fasl.fasl_path_for sld_path in
    if !(inst.fasl_cache) && Fasl.is_cache_valid ~sld_path ~fasl_path then begin
      (* Try loading from FASL cache *)
      try
        let fasl = Fasl.read_lib_fasl inst.symbols fasl_path in
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
  Expander.define_binding inst.syn_env name (Expander.var_binding);
  match !(inst.extension_lib_env) with
  | None -> ()
  | Some (lib_env, lib_syn_env) ->
    Env.define lib_env sym prim;
    Expander.define_binding lib_syn_env name (Expander.var_binding)

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
    Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) inst.global_env code

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
  ignore (Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) inst.global_env code)

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
      last := Vm.execute ~winds:inst.winds ?on_call:(!(inst.on_call)) ?on_return:(!(inst.on_return)) ?debug_state:(!(inst.debug_state)) inst.global_env code
    | Fasl.Lib_native name ->
      !load_native_ref inst
        ~search_dirs:!(inst.search_paths) ~sld_dir:None name
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
