(* C embedding API bridge.

   Handle-based value management for the C FFI.  Instance and value handles
   are integer indices into hash tables.  Handle 0 is reserved as the
   error sentinel (WILE_NULL). *)

(* ---- Internal types ---- *)

type instance_entry = {
  inst : Instance.t;
  values : (int, Datum.t) Hashtbl.t;
  mutable next_val : int;
  mutable last_error : string option;
}

let instances : (int, instance_entry) Hashtbl.t = Hashtbl.create 16
let next_inst = ref 1
let global_error : string option ref = ref None

(* ---- Handle allocation ---- *)

let alloc_value entry d =
  let h = entry.next_val in
  entry.next_val <- h + 1;
  Hashtbl.replace entry.values h d;
  h

let resolve_value entry vh =
  Hashtbl.find_opt entry.values vh

(* ---- Error wrapping ---- *)

let fmt_loc loc msg =
  if loc = Loc.none then msg
  else Printf.sprintf "%s: %s" (Loc.to_string loc) msg

let with_instance ih f =
  match Hashtbl.find_opt instances ih with
  | None ->
    global_error := Some "invalid instance handle";
    0
  | Some entry ->
    entry.last_error <- None;
    (try f entry with
     | Vm.Runtime_error msg ->
       entry.last_error <- Some msg; 0
     | Reader.Read_error (loc, msg) ->
       entry.last_error <- Some (fmt_loc loc msg); 0
     | Compiler.Compile_error (loc, msg) ->
       entry.last_error <- Some (fmt_loc loc msg); 0
     | Fasl.Fasl_error msg ->
       entry.last_error <- Some msg; 0
     | Env.Unbound_variable sym ->
       entry.last_error <- Some ("unbound variable: " ^ Symbol.name sym); 0
     | Sys_error msg ->
       entry.last_error <- Some msg; 0
     | Failure msg ->
       entry.last_error <- Some msg; 0)

let with_instance_unit ih f =
  match Hashtbl.find_opt instances ih with
  | None ->
    global_error := Some "invalid instance handle"
  | Some entry ->
    entry.last_error <- None;
    (try f entry with
     | Vm.Runtime_error msg ->
       entry.last_error <- Some msg
     | Reader.Read_error (loc, msg) ->
       entry.last_error <- Some (fmt_loc loc msg)
     | Compiler.Compile_error (loc, msg) ->
       entry.last_error <- Some (fmt_loc loc msg)
     | Fasl.Fasl_error msg ->
       entry.last_error <- Some msg
     | Env.Unbound_variable sym ->
       entry.last_error <- Some ("unbound variable: " ^ Symbol.name sym)
     | Sys_error msg ->
       entry.last_error <- Some msg
     | Failure msg ->
       entry.last_error <- Some msg)

(* ---- Instance lifecycle ---- *)

let create_instance () =
  let ih = !next_inst in
  next_inst := ih + 1;
  let inst = Instance.create () in
  let entry = {
    inst;
    values = Hashtbl.create 256;
    next_val = 1;
    last_error = None;
  } in
  Hashtbl.replace instances ih entry;
  ih

let destroy_instance ih =
  Hashtbl.remove instances ih

(* ---- Error handling ---- *)

let error_message ih =
  match Hashtbl.find_opt instances ih with
  | Some entry ->
    (match entry.last_error with Some msg -> msg | None -> "")
  | None ->
    (match !global_error with Some msg -> msg | None -> "")

(* ---- Evaluation ---- *)

let eval_string ih src =
  with_instance ih (fun entry ->
    let result = Instance.eval_string entry.inst src in
    alloc_value entry result)

let do_load_file ih path =
  with_instance ih (fun entry ->
    Instance.load_file entry.inst path;
    1)

let do_load_fasl ih path =
  with_instance ih (fun entry ->
    Instance.load_fasl entry.inst path;
    1)

(* ---- Lookup and call ---- *)

let do_lookup ih name =
  with_instance ih (fun entry ->
    match Instance.lookup entry.inst name with
    | Some d -> alloc_value entry d
    | None -> 0)

let do_call ih proc_h arg_hs =
  with_instance ih (fun entry ->
    match resolve_value entry proc_h with
    | None ->
      entry.last_error <- Some "invalid procedure handle";
      0
    | Some proc ->
      let args = Array.to_list (Array.map (fun h ->
        match resolve_value entry h with
        | Some d -> d
        | None -> failwith "invalid argument handle"
      ) arg_hs) in
      let result = Instance.call entry.inst proc args in
      alloc_value entry result)

(* ---- Primitive registration ---- *)

external c_dispatch_primitive : int -> int -> int array -> int
  = "wile_c_dispatch_primitive"

let do_define_primitive ih name prim_id =
  with_instance_unit ih (fun entry ->
    let fn args =
      let arg_handles = Array.of_list (List.map (alloc_value entry) args) in
      let result_h = c_dispatch_primitive prim_id ih arg_handles in
      if result_h = 0 then Datum.Void
      else
        match resolve_value entry result_h with
        | Some d ->
          Hashtbl.remove entry.values result_h;
          d
        | None -> Datum.Void
    in
    Instance.define_primitive entry.inst name fn)

(* ---- Value constructors ---- *)

let make_nil ih =
  with_instance ih (fun entry -> alloc_value entry Datum.Nil)

let make_void ih =
  with_instance ih (fun entry -> alloc_value entry Datum.Void)

let make_bool ih b =
  with_instance ih (fun entry -> alloc_value entry (Datum.Bool (b <> 0)))

let make_fixnum ih n =
  with_instance ih (fun entry -> alloc_value entry (Datum.Fixnum n))

let make_flonum ih d =
  with_instance ih (fun entry -> alloc_value entry (Datum.Flonum d))

let make_string ih s =
  with_instance ih (fun entry ->
    alloc_value entry (Datum.Str (Bytes.of_string s)))

let make_symbol ih name =
  with_instance ih (fun entry ->
    alloc_value entry (Datum.Symbol name))

let make_cons ih car_h cdr_h =
  with_instance ih (fun entry ->
    match resolve_value entry car_h, resolve_value entry cdr_h with
    | Some a, Some d ->
      alloc_value entry (Datum.Pair { car = a; cdr = d })
    | _ ->
      entry.last_error <- Some "invalid handle for cons";
      0)

let make_vector ih elts =
  with_instance ih (fun entry ->
    let vs = Array.map (fun h ->
      match resolve_value entry h with
      | Some d -> d
      | None -> failwith "invalid handle in vector"
    ) elts in
    alloc_value entry (Datum.Vector vs))

let make_list ih elts =
  with_instance ih (fun entry ->
    let ds = Array.to_list (Array.map (fun h ->
      match resolve_value entry h with
      | Some d -> d
      | None -> failwith "invalid handle in list"
    ) elts) in
    alloc_value entry (Datum.list_of ds))

(* ---- Type predicates ---- *)

let is_nil ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some Datum.Nil -> 1
    | _ -> 0)

let is_bool ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Bool _) -> 1
    | _ -> 0)

let is_fixnum ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Fixnum _) -> 1
    | _ -> 0)

let is_flonum ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Flonum _) -> 1
    | _ -> 0)

let is_string ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Str _) -> 1
    | _ -> 0)

let is_symbol ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Symbol _) -> 1
    | _ -> 0)

let is_pair ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Pair _) -> 1
    | _ -> 0)

let is_vector ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Vector _) -> 1
    | _ -> 0)

let is_true ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some d -> if Datum.is_true d then 1 else 0
    | None -> 0)

(* ---- Value extractors ---- *)

let get_bool ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Bool b) -> if b then 1 else 0
    | _ ->
      entry.last_error <- Some "not a boolean";
      0)

let get_fixnum ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Fixnum n) -> n
    | _ ->
      entry.last_error <- Some "not a fixnum";
      0)

let get_flonum ih vh =
  match Hashtbl.find_opt instances ih with
  | None ->
    global_error := Some "invalid instance handle";
    0.0
  | Some entry ->
    entry.last_error <- None;
    (match resolve_value entry vh with
     | Some (Datum.Flonum d) -> d
     | _ ->
       entry.last_error <- Some "not a flonum";
       0.0)

let get_string ih vh =
  match Hashtbl.find_opt instances ih with
  | None ->
    global_error := Some "invalid instance handle";
    ""
  | Some entry ->
    entry.last_error <- None;
    (match resolve_value entry vh with
     | Some (Datum.Str b) -> Bytes.to_string b
     | _ ->
       entry.last_error <- Some "not a string";
       "")

let get_symbol_name ih vh =
  match Hashtbl.find_opt instances ih with
  | None ->
    global_error := Some "invalid instance handle";
    ""
  | Some entry ->
    entry.last_error <- None;
    (match resolve_value entry vh with
     | Some (Datum.Symbol s) -> s
     | _ ->
       entry.last_error <- Some "not a symbol";
       "")

let do_car ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Pair { car; _ }) -> alloc_value entry car
    | _ ->
      entry.last_error <- Some "not a pair";
      0)

let do_cdr ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Pair { cdr; _ }) -> alloc_value entry cdr
    | _ ->
      entry.last_error <- Some "not a pair";
      0)

let do_vector_length ih vh =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Vector v) -> Array.length v
    | _ ->
      entry.last_error <- Some "not a vector";
      0)

let do_vector_ref ih vh i =
  with_instance ih (fun entry ->
    match resolve_value entry vh with
    | Some (Datum.Vector v) ->
      if i >= 0 && i < Array.length v then
        alloc_value entry v.(i)
      else begin
        entry.last_error <- Some "vector index out of range";
        0
      end
    | _ ->
      entry.last_error <- Some "not a vector";
      0)

(* ---- Display and write ---- *)

let display_to_string ih vh =
  match Hashtbl.find_opt instances ih with
  | None ->
    global_error := Some "invalid instance handle";
    ""
  | Some entry ->
    entry.last_error <- None;
    (match resolve_value entry vh with
     | Some d -> Datum.to_display_string d
     | None -> "")

let write_to_string ih vh =
  match Hashtbl.find_opt instances ih with
  | None ->
    global_error := Some "invalid instance handle";
    ""
  | Some entry ->
    entry.last_error <- None;
    (match resolve_value entry vh with
     | Some d -> Datum.to_string d
     | None -> "")

(* ---- Memory management ---- *)

let release ih vh =
  match Hashtbl.find_opt instances ih with
  | Some entry -> Hashtbl.remove entry.values vh
  | None -> ()

(* ---- Temporary handle for C extensions ---- *)

let temporary_handle inst =
  let ih = !next_inst in
  next_inst := ih + 1;
  let entry = {
    inst;
    values = Hashtbl.create 256;
    next_val = 1;
    last_error = None;
  } in
  Hashtbl.replace instances ih entry;
  ih

let release_handle ih =
  Hashtbl.remove instances ih

(* ---- Handle resolution (for testing) ---- *)

let resolve ih vh =
  match Hashtbl.find_opt instances ih with
  | None -> None
  | Some entry -> resolve_value entry vh

(* ---- Extension bridge ---- *)

let () =
  Extension.c_temporary_handle_ref := temporary_handle;
  Extension.c_release_handle_ref := release_handle

(* ---- Callback registration ---- *)

let () =
  Callback.register "wile_create_instance" create_instance;
  Callback.register "wile_destroy_instance" destroy_instance;
  Callback.register "wile_error_message" error_message;
  Callback.register "wile_eval_string" eval_string;
  Callback.register "wile_load_file" do_load_file;
  Callback.register "wile_load_fasl" do_load_fasl;
  Callback.register "wile_lookup" do_lookup;
  Callback.register "wile_call" do_call;
  Callback.register "wile_define_primitive" do_define_primitive;
  Callback.register "wile_make_nil" make_nil;
  Callback.register "wile_make_void" make_void;
  Callback.register "wile_make_bool" make_bool;
  Callback.register "wile_make_fixnum" make_fixnum;
  Callback.register "wile_make_flonum" make_flonum;
  Callback.register "wile_make_string" make_string;
  Callback.register "wile_make_symbol" make_symbol;
  Callback.register "wile_make_cons" make_cons;
  Callback.register "wile_make_vector" make_vector;
  Callback.register "wile_make_list" make_list;
  Callback.register "wile_is_nil" is_nil;
  Callback.register "wile_is_bool" is_bool;
  Callback.register "wile_is_fixnum" is_fixnum;
  Callback.register "wile_is_flonum" is_flonum;
  Callback.register "wile_is_string" is_string;
  Callback.register "wile_is_symbol" is_symbol;
  Callback.register "wile_is_pair" is_pair;
  Callback.register "wile_is_vector" is_vector;
  Callback.register "wile_is_true" is_true;
  Callback.register "wile_get_bool" get_bool;
  Callback.register "wile_get_fixnum" get_fixnum;
  Callback.register "wile_get_flonum" get_flonum;
  Callback.register "wile_get_string" get_string;
  Callback.register "wile_get_symbol_name" get_symbol_name;
  Callback.register "wile_car" do_car;
  Callback.register "wile_cdr" do_cdr;
  Callback.register "wile_vector_length" do_vector_length;
  Callback.register "wile_vector_ref" do_vector_ref;
  Callback.register "wile_display_string" display_to_string;
  Callback.register "wile_write_string" write_to_string;
  Callback.register "wile_release" release
