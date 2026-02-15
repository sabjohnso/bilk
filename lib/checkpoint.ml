exception Checkpoint_error of string

let error msg = raise (Checkpoint_error msg)
let errorf fmt = Printf.ksprintf error fmt

(* ── Binary helpers (same conventions as Fasl) ── *)

let write_u8 buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF))

let write_u16 buf v =
  let b = Bytes.create 2 in
  Bytes.set_uint16_le b 0 v;
  Buffer.add_bytes buf b

let write_u32 buf v =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int v);
  Buffer.add_bytes buf b

let write_i64 buf v =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 (Int64.of_int v);
  Buffer.add_bytes buf b

let write_f64 buf v =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 (Int64.bits_of_float v);
  Buffer.add_bytes buf b

let write_str buf s =
  let len = String.length s in
  write_u16 buf len;
  Buffer.add_string buf s

let write_bytes_data buf data =
  let len = Bytes.length data in
  write_u32 buf len;
  Buffer.add_bytes buf data

let check_bounds data pos need =
  if !pos + need > Bytes.length data then
    error "unexpected end of checkpoint data"

let read_u8 data pos =
  check_bounds data pos 1;
  let v = Bytes.get_uint8 data !pos in
  pos := !pos + 1;
  v

let read_u16 data pos =
  check_bounds data pos 2;
  let v = Bytes.get_uint16_le data !pos in
  pos := !pos + 2;
  v

let read_u32 data pos =
  check_bounds data pos 4;
  let v = Int32.to_int (Bytes.get_int32_le data !pos) in
  pos := !pos + 4;
  v

let read_i64 data pos =
  check_bounds data pos 8;
  let v = Bytes.get_int64_le data !pos in
  pos := !pos + 8;
  let n = Int64.to_int v in
  if Int64.of_int n <> v then
    error "fixnum overflow during checkpoint load";
  n

let read_f64 data pos =
  check_bounds data pos 8;
  let bits = Bytes.get_int64_le data !pos in
  pos := !pos + 8;
  Int64.float_of_bits bits

let read_str data pos =
  let len = read_u16 data pos in
  check_bounds data pos len;
  let s = Bytes.sub_string data !pos len in
  pos := !pos + len;
  s

let read_bytes_data data pos =
  let len = read_u32 data pos in
  check_bounds data pos len;
  let b = Bytes.sub data !pos len in
  pos := !pos + len;
  b

(* ── Header ── *)

let magic = "WFAS"
let header_size = 16
let format_type_checkpoint = 3

let write_header buf =
  Buffer.add_string buf magic;
  write_u16 buf Fasl.version_major;
  write_u16 buf Fasl.version_minor;
  write_u8 buf format_type_checkpoint;
  for _ = 1 to 7 do write_u8 buf 0 done

let read_header data pos =
  check_bounds data pos header_size;
  let m = Bytes.sub_string data !pos 4 in
  pos := !pos + 4;
  if m <> magic then
    errorf "bad checkpoint magic: expected WFAS, got %s" m;
  let maj = read_u16 data pos in
  let _min = read_u16 data pos in
  if maj <> Fasl.version_major then
    errorf "checkpoint version mismatch: expected %d.x, got %d.x"
      Fasl.version_major maj;
  let fmt = read_u8 data pos in
  if fmt <> format_type_checkpoint then
    errorf "expected checkpoint FASL (type %d), got type %d"
      format_type_checkpoint fmt;
  pos := !pos + 7

(* ── Frame identity tracking ── *)

(* During serialization we assign integer IDs to frames by physical identity.
   Frame 0 is always the global frame.  We discover additional frames by
   walking closures reachable from the global environment. *)

type frame_table = {
  mutable frames : Datum.frame list;  (* ordered by ID *)
}

let frame_table_create global_frame =
  { frames = [global_frame] }

let frame_id tbl (frame : Datum.frame) =
  let rec search i = function
    | [] ->
      tbl.frames <- tbl.frames @ [frame];
      List.length tbl.frames - 1
    | f :: rest ->
      if f == frame then i
      else search (i + 1) rest
  in
  search 0 tbl.frames

(* ── Extended datum encoding (tags 0-16 match Fasl, 17+ are new) ── *)

(* Walk all frames reachable from a datum to register them *)
let rec discover_frames tbl (d : Datum.t) =
  match d with
  | Closure c ->
    List.iter (fun frame -> ignore (frame_id tbl frame)) c.clos_env;
    (* Also discover frames from code constants *)
    discover_frames_in_code tbl c.clos_code
  | Pair { car; cdr } ->
    discover_frames tbl car;
    discover_frames tbl cdr
  | Vector elts ->
    Array.iter (discover_frames tbl) elts
  | Values vs ->
    List.iter (discover_frames tbl) vs
  | Hash_table ht ->
    Array.iter (fun bucket ->
      List.iter (fun (k, v) ->
        discover_frames tbl k;
        discover_frames tbl v
      ) bucket
    ) ht.ht_data
  | Promise p ->
    discover_frames tbl p.promise_value
  | _ -> ()

and discover_frames_in_code tbl (code : Datum.code) =
  Array.iter (discover_frames tbl) code.constants;
  Array.iter (discover_frames_in_code tbl) code.children

(* Write an extended datum *)
let rec write_ext_datum buf tbl (d : Datum.t) =
  match d with
  (* Standard FASL tags 0-16 *)
  | Bool false -> write_u8 buf 0
  | Bool true -> write_u8 buf 1
  | Fixnum n -> write_u8 buf 2; write_i64 buf n
  | Flonum f -> write_u8 buf 3; write_f64 buf f
  | Char c -> write_u8 buf 4; write_u32 buf (Uchar.to_int c)
  | Str s -> write_u8 buf 5; write_bytes_data buf s
  | Symbol name -> write_u8 buf 6; write_str buf name
  | Pair { car; cdr } ->
    write_u8 buf 7;
    write_ext_datum buf tbl car;
    write_ext_datum buf tbl cdr
  | Vector elts ->
    write_u8 buf 8;
    write_u32 buf (Array.length elts);
    Array.iter (write_ext_datum buf tbl) elts
  | Bytevector bv -> write_u8 buf 9; write_bytes_data buf bv
  | Nil -> write_u8 buf 10
  | Eof -> write_u8 buf 11
  | Void -> write_u8 buf 12
  | Complex (re, im) ->
    write_u8 buf 14;
    write_ext_datum buf tbl re;
    write_ext_datum buf tbl im
  | Bignum z -> write_u8 buf 15; write_str buf (Z.to_string z)
  | Rational (n, d) ->
    write_u8 buf 16;
    write_str buf (Z.to_string n);
    write_str buf (Z.to_string d)
  (* Extended tags 17+ *)
  | Primitive p ->
    write_u8 buf 17;
    write_str buf p.prim_name
  | Closure c ->
    write_u8 buf 18;
    write_str buf c.clos_name;
    (* Embed code as FASL blob *)
    let code_bytes = Fasl.write_code c.clos_code in
    write_bytes_data buf code_bytes;
    (* Environment: depth + frame IDs *)
    let env = c.clos_env in
    write_u16 buf (List.length env);
    List.iter (fun frame ->
      write_u32 buf (frame_id tbl frame)
    ) env
  | Hash_table ht ->
    write_u8 buf 19;
    write_u8 buf (if ht.ht_mutable then 1 else 0);
    write_ext_datum buf tbl ht.ht_equal;
    write_ext_datum buf tbl ht.ht_hash;
    (* Flatten all buckets into (k,v) pairs *)
    let pairs = Array.fold_left (fun acc bucket ->
      List.rev_append bucket acc
    ) [] ht.ht_data in
    let pairs = List.rev pairs in
    write_u32 buf (List.length pairs);
    List.iter (fun (k, v) ->
      write_ext_datum buf tbl k;
      write_ext_datum buf tbl v
    ) pairs
  | Promise p ->
    write_u8 buf 20;
    write_u8 buf (if p.promise_done then 1 else 0);
    write_ext_datum buf tbl p.promise_value
  | Values vs ->
    write_u8 buf 21;
    write_u16 buf (List.length vs);
    List.iter (write_ext_datum buf tbl) vs
  | Char_set cs ->
    write_u8 buf 22;
    Buffer.add_bytes buf cs.cs_bits
  (* Skipped: Continuation, Port, Regexp, Error_object → Void on restore *)
  | Continuation _ | Port _ | Regexp _ | Error_object _ ->
    write_u8 buf 23

(* Replicate the Scheme runtime's datum hashing for hash-table reconstruction.
   Must match the datum_hash in Instance.register_primitives exactly. *)
let rec datum_hash (d : Datum.t) = match d with
  | Bool b -> Hashtbl.hash b
  | Fixnum n -> Hashtbl.hash n
  | Bignum z -> Z.hash z
  | Rational (n, d) -> Z.hash n * 31 + Z.hash d
  | Flonum f -> Hashtbl.hash f
  | Char c -> Uchar.to_int c
  | Str s -> Hashtbl.hash (Bytes.to_string s)
  | Symbol s -> Hashtbl.hash s
  | Nil -> 0
  | Pair { car; cdr } -> datum_hash car * 31 + datum_hash cdr
  | Vector v -> Array.fold_left (fun h e -> h * 31 + datum_hash e) 0 v
  | Bytevector bv -> Hashtbl.hash bv
  | _ -> 0

(* Read an extended datum.  [frames] is the pre-created frame array;
   [prims] maps primitive names to their Datum.t values from the fresh instance. *)
let rec read_ext_datum symbols data pos frames prims =
  let tag = read_u8 data pos in
  match tag with
  | 0 -> Datum.Bool false
  | 1 -> Datum.Bool true
  | 2 -> Datum.Fixnum (read_i64 data pos)
  | 3 -> Datum.Flonum (read_f64 data pos)
  | 4 -> Datum.Char (Uchar.of_int (read_u32 data pos))
  | 5 -> Datum.Str (read_bytes_data data pos)
  | 6 -> Datum.Symbol (read_str data pos)
  | 7 ->
    let car = read_ext_datum symbols data pos frames prims in
    let cdr = read_ext_datum symbols data pos frames prims in
    Datum.Pair { car; cdr }
  | 8 ->
    let count = read_u32 data pos in
    let elts = Array.init count (fun _ ->
      read_ext_datum symbols data pos frames prims) in
    Datum.Vector elts
  | 9 -> Datum.Bytevector (read_bytes_data data pos)
  | 10 -> Datum.Nil
  | 11 -> Datum.Eof
  | 12 -> Datum.Void
  | 14 ->
    let re = read_ext_datum symbols data pos frames prims in
    let im = read_ext_datum symbols data pos frames prims in
    Datum.Complex (re, im)
  | 15 ->
    let s = read_str data pos in
    let z = Z.of_string s in
    if Z.fits_int z then Datum.Fixnum (Z.to_int z) else Datum.Bignum z
  | 16 ->
    let ns = read_str data pos in
    let ds = read_str data pos in
    Datum.Rational (Z.of_string ns, Z.of_string ds)
  (* Tag 17: Primitive — look up by name *)
  | 17 ->
    let name = read_str data pos in
    begin match Hashtbl.find_opt prims name with
    | Some d -> d
    | None -> errorf "checkpoint: unknown primitive: %s" name
    end
  (* Tag 18: Closure *)
  | 18 ->
    let name = read_str data pos in
    let code_bytes = read_bytes_data data pos in
    let code = Fasl.read_code symbols code_bytes in
    let env_depth = read_u16 data pos in
    let env = List.init env_depth (fun _ ->
      let fid = read_u32 data pos in
      if fid < 0 || fid >= Array.length frames then
        errorf "checkpoint: invalid frame id: %d" fid;
      frames.(fid)
    ) in
    Datum.Closure { clos_name = name; clos_code = code; clos_env = env }
  (* Tag 19: Hash_table *)
  | 19 ->
    let mutable_ = read_u8 data pos <> 0 in
    let equal = read_ext_datum symbols data pos frames prims in
    let hash = read_ext_datum symbols data pos frames prims in
    let count = read_u32 data pos in
    (* Read all pairs first *)
    let pairs = List.init count (fun _ ->
      let k = read_ext_datum symbols data pos frames prims in
      let v = read_ext_datum symbols data pos frames prims in
      (k, v)
    ) in
    (* Rebuild hash table using the same datum_hash as the Scheme runtime *)
    let num_buckets = max 8 (count * 2) in
    let ht_data = Array.make num_buckets [] in
    List.iter (fun ((k, _v) as entry) ->
      let h = (datum_hash k land max_int) mod num_buckets in
      ht_data.(h) <- entry :: ht_data.(h)
    ) pairs;
    Datum.Hash_table {
      ht_data; ht_size = count;
      ht_equal = equal; ht_hash = hash;
      ht_mutable = mutable_
    }
  (* Tag 20: Promise *)
  | 20 ->
    let done_ = read_u8 data pos <> 0 in
    let value = read_ext_datum symbols data pos frames prims in
    Datum.Promise { promise_done = done_; promise_value = value }
  (* Tag 21: Values *)
  | 21 ->
    let count = read_u16 data pos in
    let vs = List.init count (fun _ ->
      read_ext_datum symbols data pos frames prims) in
    Datum.Values vs
  (* Tag 22: Char_set *)
  | 22 ->
    check_bounds data pos 32;
    let bits = Bytes.sub data !pos 32 in
    pos := !pos + 32;
    Datum.Char_set { cs_bits = bits }
  (* Tag 23: Skipped → Void *)
  | 23 -> Datum.Void
  | _ -> errorf "checkpoint: unknown datum tag: %d" tag

(* ── Binding (Expander.binding) encoding ── *)

let write_syn_binding buf (b : Expander.binding) =
  match b with
  | Var -> write_u8 buf 0
  | Core name -> write_u8 buf 1; write_str buf name
  | Macro tf ->
    write_u8 buf 2;
    write_u16 buf (List.length tf.literals);
    List.iter (write_str buf) tf.literals;
    write_u16 buf (List.length tf.rules);
    List.iter (fun (r : Expander.rule) ->
      let empty_tbl = { frames = [] } in
      let pat_d = Syntax.to_datum r.pattern in
      let tmpl_d = Syntax.to_datum r.template in
      write_ext_datum buf empty_tbl pat_d;
      write_ext_datum buf empty_tbl tmpl_d
    ) tf.rules

let read_syn_binding symbols data pos frames prims =
  let tag = read_u8 data pos in
  match tag with
  | 0 -> Expander.Var
  | 1 -> Expander.Core (read_str data pos)
  | 2 ->
    let lit_count = read_u16 data pos in
    let literals = List.init lit_count (fun _ -> read_str data pos) in
    let rule_count = read_u16 data pos in
    let rules = List.init rule_count (fun _ ->
      let pat = read_ext_datum symbols data pos frames prims in
      let tmpl = read_ext_datum symbols data pos frames prims in
      Expander.{ pattern = Syntax.from_datum Loc.none pat;
                 template = Syntax.from_datum Loc.none tmpl }
    ) in
    Expander.Macro { literals; rules; def_env = [] }
  | _ -> errorf "checkpoint: unknown binding tag: %d" tag

(* ── Snapshot ── *)

let snapshot (inst : Instance.t) =
  let buf = Buffer.create 4096 in
  write_header buf;
  (* Gensym counter *)
  write_u32 buf !(inst.gensym_counter);
  (* Discover all reachable frames from the global environment *)
  let global_frame = match inst.global_env with
    | [] -> error "checkpoint: empty global environment"
    | f :: _ -> f
  in
  let tbl = frame_table_create global_frame in
  (* Walk all bindings in global frame to discover closure frames *)
  Hashtbl.iter (fun _id slot ->
    discover_frames tbl !slot
  ) global_frame;
  (* Write frame count *)
  let num_frames = List.length tbl.frames in
  write_u32 buf num_frames;
  (* Write each frame's bindings *)
  List.iter (fun (frame : Datum.frame) ->
    let bindings = Hashtbl.fold (fun id slot acc -> (id, !slot) :: acc) frame [] in
    write_u32 buf (List.length bindings);
    List.iter (fun (sym_id, value) ->
      (* Look up the symbol name from the instance's symbol table *)
      let name = match List.find_opt (fun s -> Symbol.id s = sym_id)
                         (Symbol.all inst.symbols) with
        | Some s -> Symbol.name s
        | None -> errorf "checkpoint: unknown symbol id: %d" sym_id
      in
      write_str buf name;
      write_ext_datum buf tbl value
    ) bindings
  ) tbl.frames;
  (* Global env chain: depth + frame IDs *)
  write_u16 buf (List.length inst.global_env);
  List.iter (fun frame ->
    write_u32 buf (frame_id tbl frame)
  ) inst.global_env;
  (* Library names *)
  let libs = Library.list_all inst.libraries in
  let lib_names = List.map (fun (lib : Library.t) -> lib.name) libs in
  write_u16 buf (List.length lib_names);
  List.iter (fun name ->
    write_u16 buf (List.length name);
    List.iter (write_str buf) name
  ) lib_names;
  (* User syntax bindings from top frame of syn_env *)
  let syn_bindings = match inst.syn_env with
    | [] -> []
    | top :: _ ->
      (* Only serialize Macro bindings (user-defined), not Var/Core *)
      Hashtbl.fold (fun name binding acc ->
        match binding with
        | Expander.Macro _ -> (name, binding) :: acc
        | _ -> acc
      ) top []
  in
  write_u16 buf (List.length syn_bindings);
  List.iter (fun (name, binding) ->
    write_str buf name;
    write_syn_binding buf binding
  ) syn_bindings;
  Buffer.to_bytes buf

(* ── Restore ── *)

let restore data =
  let pos = ref 0 in
  read_header data pos;
  (* Gensym counter *)
  let gensym = read_u32 data pos in
  (* Create fresh instance to get primitives *)
  let fresh = Instance.create () in
  let fresh_global = match fresh.global_env with
    | [] -> error "checkpoint: fresh instance has empty environment"
    | f :: _ -> f
  in
  (* Snapshot primitive table: name → Datum.t *)
  let prims = Hashtbl.create 256 in
  Hashtbl.iter (fun id slot ->
    match !slot with
    | Datum.Primitive p ->
      Hashtbl.replace prims p.prim_name !slot;
      (* Also store by symbol name for lookup *)
      let name = match List.find_opt (fun s -> Symbol.id s = id)
                         (Symbol.all fresh.symbols) with
        | Some s -> Symbol.name s
        | None -> ""
      in
      if name <> "" then
        Hashtbl.replace prims name !slot
    | _ -> ()
  ) fresh_global;
  (* Read frame count and pre-create frames *)
  let num_frames = read_u32 data pos in
  if num_frames < 1 then
    error "checkpoint: must have at least one frame (global)";
  let frames = Array.init num_frames (fun i ->
    if i = 0 then fresh_global
    else Hashtbl.create 16
  ) in
  (* Clear the global frame — we'll repopulate it *)
  Hashtbl.reset fresh_global;
  (* Fill each frame *)
  for i = 0 to num_frames - 1 do
    let num_bindings = read_u32 data pos in
    for _ = 1 to num_bindings do
      let name = read_str data pos in
      let value = read_ext_datum fresh.symbols data pos frames prims in
      let sym = Symbol.intern fresh.symbols name in
      Hashtbl.replace frames.(i) (Symbol.id sym) (ref value)
    done
  done;
  (* Read global env chain *)
  let env_depth = read_u16 data pos in
  let global_env : Datum.env = List.init env_depth (fun _ ->
    let fid = read_u32 data pos in
    if fid < 0 || fid >= Array.length frames then
      errorf "checkpoint: invalid global env frame id: %d" fid;
    frames.(fid)
  ) in
  (* Patch the fresh instance's global_env.  Since Instance.t has an immutable
     global_env field (Datum.env = frame list), and we need to replace it,
     we must construct a new Instance.t.  However, Instance.t fields are not
     mutable directly.  The global_env is a list of frames, and frame 0 is
     the same physical object as fresh_global, so the fresh instance's
     global_env already points to the repopulated frame.  But the env chain
     may differ (e.g. if checkpoint had multiple frames in global_env).
     Since Instance.t.global_env is not mutable, we need to rebuild. *)
  let inst = {
    Instance.symbols = fresh.symbols;
    global_env;
    readtable = fresh.readtable;
    winds = ref [];
    handlers = ref [];
    syn_env = fresh.syn_env;
    gensym_counter = ref gensym;
    libraries = fresh.libraries;
    search_paths = fresh.search_paths;
    features = fresh.features;
    loading_libs = ref [];
    fasl_cache = fresh.fasl_cache;
    current_input = fresh.current_input;
    current_output = fresh.current_output;
    current_error = fresh.current_error;
    command_line = fresh.command_line;
    eval_envs = Hashtbl.create 4;
    eval_env_counter = ref 0;
    extension_lib_env = ref None;
    on_call = ref None;
    on_return = ref None;
    debug_state = ref None;
  } in
  (* Re-import libraries *)
  let num_libs = read_u16 data pos in
  for _ = 1 to num_libs do
    let part_count = read_u16 data pos in
    let lib_name = List.init part_count (fun _ -> read_str data pos) in
    ignore (Instance.ensure_library inst lib_name)
  done;
  (* Read user syntax bindings *)
  let num_syn = read_u16 data pos in
  for _ = 1 to num_syn do
    let name = read_str data pos in
    let binding = read_syn_binding fresh.symbols data pos frames prims in
    Expander.define_binding inst.syn_env name binding
  done;
  inst
