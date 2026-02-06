type t = {
  symbols : Symbol.table;
  global_env : Env.t;
  readtable : Readtable.t;
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
  Datum.Primitive { prim_name = name; prim_fn = fn }

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

let prim_lt args =
  match args with
  | [a; b] ->
    if is_numeric a && is_numeric b then
      Datum.Bool (as_flonum "<" a < as_flonum "<" b)
    else runtime_error "<: expected numeric arguments"
  | _ -> runtime_error (Printf.sprintf "<: expected 2 arguments, got %d" (List.length args))

let prim_num_eq args =
  match args with
  | [a; b] ->
    if is_numeric a && is_numeric b then
      Datum.Bool (Float.equal (as_flonum "=" a) (as_flonum "=" b))
    else runtime_error "=: expected numeric arguments"
  | _ -> runtime_error (Printf.sprintf "=: expected 2 arguments, got %d" (List.length args))

let prim_gt args =
  match args with
  | [a; b] ->
    if is_numeric a && is_numeric b then
      Datum.Bool (as_flonum ">" a > as_flonum ">" b)
    else runtime_error ">: expected numeric arguments"
  | _ -> runtime_error (Printf.sprintf ">: expected 2 arguments, got %d" (List.length args))

(* --- List primitives --- *)

let prim_cons args =
  match args with
  | [a; b] -> Datum.Pair (a, b)
  | _ -> runtime_error (Printf.sprintf "cons: expected 2 arguments, got %d" (List.length args))

let prim_car args =
  match args with
  | [Datum.Pair (a, _)] -> a
  | [v] -> runtime_error (Printf.sprintf "car: expected pair, got %s" (Datum.to_string v))
  | _ -> runtime_error (Printf.sprintf "car: expected 1 argument, got %d" (List.length args))

let prim_cdr args =
  match args with
  | [Datum.Pair (_, d)] -> d
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

(* --- I/O primitives --- *)

let prim_display args =
  match args with
  | [v] -> print_string (Datum.to_display_string v); Datum.Void
  | _ -> runtime_error (Printf.sprintf "display: expected 1 argument, got %d" (List.length args))

let prim_newline args =
  match args with
  | [] -> print_newline (); Datum.Void
  | _ -> runtime_error (Printf.sprintf "newline: expected 0 arguments, got %d" (List.length args))

(* --- Primitive registration --- *)

let register_primitives symbols env =
  let register name fn =
    let sym = Symbol.intern symbols name in
    Env.define env sym (make_prim name fn)
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
  register "newline" prim_newline

(* --- Instance creation --- *)

let create ?(readtable = Readtable.default) () =
  let symbols = Symbol.create_table () in
  let global_env = Env.empty () in
  register_primitives symbols global_env;
  { symbols; global_env; readtable }

let intern inst name = Symbol.intern inst.symbols name

(* --- Evaluation --- *)

let eval_syntax inst expr =
  let code = Compiler.compile inst.symbols expr in
  Vm.execute inst.global_env code

let eval_string inst src =
  let port = Port.of_string src in
  let expr = Reader.read_syntax inst.readtable port in
  eval_syntax inst expr
