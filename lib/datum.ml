type intrinsic_id =
  | Intrinsic_call_cc
  | Intrinsic_apply
  | Intrinsic_call_with_values
  | Intrinsic_dynamic_wind

type error_tag = General_error | Read_error | File_error

type error_obj = {
  err_message : string;
  err_irritants : t list;
  err_tag : error_tag;
}

and t =
  | Bool of bool
  | Fixnum of int
  | Flonum of float
  | Char of Uchar.t
  | Str of bytes
  | Symbol of string
  | Pair of { mutable car : t; mutable cdr : t }
  | Vector of t array
  | Bytevector of bytes
  | Nil
  | Eof
  | Void
  | Primitive of primitive
  | Closure of closure
  | Continuation of continuation
  | Values of t list
  | Error_object of error_obj
  | Port of Port.t
  | Promise of promise
  | Hash_table of hash_table
  | Char_set of char_set
  | Regexp of regexp

and promise = {
  mutable promise_done : bool;
  mutable promise_value : t;
}

and hash_table = {
  mutable ht_data : (t * t) list array;
  mutable ht_size : int;
  ht_equal : t;
  ht_hash : t;
  ht_mutable : bool;
}

and char_set = {
  cs_bits : bytes;  (* 32 bytes = 256 bits, one per code point 0-255 *)
}

and regexp = {
  rx_compiled : Obj.t;  (* actually Regexp_engine.compiled *)
  rx_source : t;        (* original SRE datum for display *)
}

and primitive = {
  prim_name : string;
  prim_fn : t list -> t;
  prim_intrinsic : intrinsic_id option;
}

and closure = {
  clos_name : string;
  clos_code : code;
  clos_env : env;
}

and code = {
  instructions : Opcode.t array;
  source_map : Loc.t array;
  constants : t array;
  symbols : Symbol.t array;
  children : code array;
  params : Symbol.t array;
  variadic : bool;
  name : string;
}

and env = frame list
and frame = (int, t ref) Hashtbl.t

and call_frame = {
  saved_code : code;
  saved_pc : int;
  saved_env : env;
  saved_sp : int;
}

and wind = {
  wind_before : t;
  wind_after : t;
}

and cont_frame =
  | CF_standard of call_frame
  | CF_cwv_pending of call_frame * t
  | CF_dw_before of call_frame * t * t * t
  | CF_dw_thunk of call_frame * t * t
  | CF_dw_after of call_frame * t

and continuation = {
  cont_stack : t array;
  cont_sp : int;
  cont_frames : cont_frame list;
  cont_code : code;
  cont_pc : int;
  cont_env : env;
  cont_winds : wind list;
}

let rec equal a b =
  match (a, b) with
  | Bool x, Bool y -> x = y
  | Fixnum x, Fixnum y -> x = y
  | Flonum x, Flonum y -> Float.equal x y
  | Char x, Char y -> Uchar.equal x y
  | Str x, Str y -> Bytes.equal x y
  | Symbol x, Symbol y -> String.equal x y
  | Pair { car = a1; cdr = a2 }, Pair { car = b1; cdr = b2 } -> equal a1 b1 && equal a2 b2
  | Vector xs, Vector ys ->
    Array.length xs = Array.length ys
    && Array.for_all2 equal xs ys
  | Bytevector x, Bytevector y -> Bytes.equal x y
  | Nil, Nil -> true
  | Eof, Eof -> true
  | Void, Void -> true
  | Primitive a, Primitive b -> String.equal a.prim_name b.prim_name
  | Closure _, Closure _ -> false
  | Continuation _, Continuation _ -> false
  | Values xs, Values ys ->
    List.length xs = List.length ys
    && List.for_all2 equal xs ys
  | Error_object a, Error_object b ->
    String.equal a.err_message b.err_message
    && a.err_tag = b.err_tag
    && List.length a.err_irritants = List.length b.err_irritants
    && List.for_all2 equal a.err_irritants b.err_irritants
  | Port _, Port _ -> false
  | Promise _, Promise _ -> false
  | Hash_table _, Hash_table _ -> false
  | Char_set a, Char_set b -> Bytes.equal a.cs_bits b.cs_bits
  | Regexp _, Regexp _ -> false
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
  | Str s -> Format.fprintf fmt "%S" (Bytes.to_string s)
  | Symbol s -> Format.fprintf fmt "%s" s
  | Pair { car; cdr } ->
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
  | Void -> Format.fprintf fmt "#<void>"
  | Primitive p -> Format.fprintf fmt "#<primitive %s>" p.prim_name
  | Closure c -> Format.fprintf fmt "#<closure %s>" c.clos_name
  | Continuation _ -> Format.fprintf fmt "#<continuation>"
  | Values vs -> Format.fprintf fmt "#<values %d>" (List.length vs)
  | Error_object e -> Format.fprintf fmt "#<error \"%s\">" e.err_message
  | Port p ->
    if Port.is_input p then
      Format.fprintf fmt "#<input-port %s>" (Port.file_name p)
    else
      Format.fprintf fmt "#<output-port %s>" (Port.file_name p)
  | Promise p ->
    if p.promise_done then
      Format.fprintf fmt "#<promise (forced %a)>" pp p.promise_value
    else
      Format.fprintf fmt "#<promise>"
  | Hash_table ht ->
    Format.fprintf fmt "#<hash-table (%d)>" ht.ht_size
  | Char_set _ ->
    Format.fprintf fmt "#<char-set>"
  | Regexp _ ->
    Format.fprintf fmt "#<regexp>"

and pp_tail fmt = function
  | Nil -> ()
  | Pair { car; cdr } ->
    Format.fprintf fmt " ";
    pp fmt car;
    pp_tail fmt cdr
  | other ->
    Format.fprintf fmt " . ";
    pp fmt other

let to_string d =
  Format.asprintf "%a" pp d

let rec pp_display fmt = function
  | Str s -> Format.pp_print_string fmt (Bytes.to_string s)
  | Char c ->
    let buf = Buffer.create 4 in
    Buffer.add_utf_8_uchar buf c;
    Format.pp_print_string fmt (Buffer.contents buf)
  | Pair { car; cdr } ->
    Format.fprintf fmt "(";
    pp_display fmt car;
    pp_display_tail fmt cdr;
    Format.fprintf fmt ")"
  | Vector elts ->
    Format.fprintf fmt "#(";
    Array.iteri
      (fun i e ->
        if i > 0 then Format.fprintf fmt " ";
        pp_display fmt e)
      elts;
    Format.fprintf fmt ")"
  | other -> pp fmt other

and pp_display_tail fmt = function
  | Nil -> ()
  | Pair { car; cdr } ->
    Format.fprintf fmt " ";
    pp_display fmt car;
    pp_display_tail fmt cdr
  | other ->
    Format.fprintf fmt " . ";
    pp_display fmt other

let to_display_string d =
  Format.asprintf "%a" pp_display d

let is_true = function
  | Bool false -> false
  | _ -> true

let list_of xs =
  List.fold_right (fun x acc -> Pair { car = x; cdr = acc }) xs Nil

let rec to_list = function
  | Nil -> Some []
  | Pair { car; cdr } ->
    (match to_list cdr with
     | Some rest -> Some (car :: rest)
     | None -> None)
  | _ -> None
