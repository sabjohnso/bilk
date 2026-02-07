exception Fasl_error of string

let version_major = 1
let version_minor = 0

let fasl_error msg = raise (Fasl_error msg)

(* --- Binary write helpers --- *)

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

(* --- Binary read helpers --- *)

let check_bounds data pos need =
  if !pos + need > Bytes.length data then
    fasl_error "unexpected end of FASL data"

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
    fasl_error "fixnum overflow during FASL load";
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

(* --- Header --- *)

let magic = "WFAS"
let header_size = 16

let write_header buf format_type =
  Buffer.add_string buf magic;
  write_u16 buf version_major;
  write_u16 buf version_minor;
  write_u8 buf format_type;
  (* 7 bytes reserved *)
  for _ = 1 to 7 do write_u8 buf 0 done

let read_header data pos =
  check_bounds data pos header_size;
  let m = Bytes.sub_string data !pos 4 in
  pos := !pos + 4;
  if m <> magic then
    fasl_error (Printf.sprintf "bad FASL magic: expected WFAS, got %s" m);
  let maj = read_u16 data pos in
  let min = read_u16 data pos in
  if maj <> version_major then
    fasl_error (Printf.sprintf "FASL version mismatch: expected %d.x, got %d.%d"
      version_major maj min);
  let fmt = read_u8 data pos in
  (* skip 7 reserved bytes *)
  pos := !pos + 7;
  fmt

(* --- Opcode encoding --- *)

let write_opcode buf (op : Opcode.t) =
  match op with
  | Halt -> write_u8 buf 0
  | Const i -> write_u8 buf 1; write_u32 buf i
  | Lookup i -> write_u8 buf 2; write_u32 buf i
  | Define i -> write_u8 buf 3; write_u32 buf i
  | SetBang i -> write_u8 buf 4; write_u32 buf i
  | Push -> write_u8 buf 5
  | Jump i -> write_u8 buf 6; write_u32 buf i
  | JumpFalse i -> write_u8 buf 7; write_u32 buf i
  | Call i -> write_u8 buf 8; write_u32 buf i
  | TailCall i -> write_u8 buf 9; write_u32 buf i
  | Return -> write_u8 buf 10
  | MakeClosure i -> write_u8 buf 11; write_u32 buf i

let read_opcode data pos =
  let tag = read_u8 data pos in
  match tag with
  | 0 -> Opcode.Halt
  | 1 -> Opcode.Const (read_u32 data pos)
  | 2 -> Opcode.Lookup (read_u32 data pos)
  | 3 -> Opcode.Define (read_u32 data pos)
  | 4 -> Opcode.SetBang (read_u32 data pos)
  | 5 -> Opcode.Push
  | 6 -> Opcode.Jump (read_u32 data pos)
  | 7 -> Opcode.JumpFalse (read_u32 data pos)
  | 8 -> Opcode.Call (read_u32 data pos)
  | 9 -> Opcode.TailCall (read_u32 data pos)
  | 10 -> Opcode.Return
  | 11 -> Opcode.MakeClosure (read_u32 data pos)
  | _ -> fasl_error (Printf.sprintf "unknown opcode tag: %d" tag)

(* --- Datum encoding --- *)

let rec write_datum buf (d : Datum.t) =
  match d with
  | Bool false -> write_u8 buf 0
  | Bool true -> write_u8 buf 1
  | Fixnum n -> write_u8 buf 2; write_i64 buf n
  | Flonum f -> write_u8 buf 3; write_f64 buf f
  | Char c -> write_u8 buf 4; write_u32 buf (Uchar.to_int c)
  | Str s -> write_u8 buf 5; write_bytes_data buf s
  | Symbol name -> write_u8 buf 6; write_str buf name
  | Pair { car; cdr } -> write_u8 buf 7; write_datum buf car; write_datum buf cdr
  | Vector elts ->
    write_u8 buf 8;
    write_u32 buf (Array.length elts);
    Array.iter (write_datum buf) elts
  | Bytevector bv -> write_u8 buf 9; write_bytes_data buf bv
  | Nil -> write_u8 buf 10
  | Eof -> write_u8 buf 11
  | Void -> write_u8 buf 12
  | Primitive p ->
    fasl_error (Printf.sprintf "cannot serialize primitive: %s" p.prim_name)
  | Closure c ->
    fasl_error (Printf.sprintf "cannot serialize closure: %s" c.clos_name)
  | Continuation _ ->
    fasl_error "cannot serialize continuation"
  | Values _ ->
    fasl_error "cannot serialize multiple values"
  | Error_object _ ->
    fasl_error "cannot serialize error object"

let rec read_datum symbols data pos =
  let tag = read_u8 data pos in
  match tag with
  | 0 -> Datum.Bool false
  | 1 -> Datum.Bool true
  | 2 -> Datum.Fixnum (read_i64 data pos)
  | 3 -> Datum.Flonum (read_f64 data pos)
  | 4 -> Datum.Char (Uchar.of_int (read_u32 data pos))
  | 5 -> Datum.Str (read_bytes_data data pos)
  | 6 ->
    let name = read_str data pos in
    Datum.Symbol name
  | 7 ->
    let car = read_datum symbols data pos in
    let cdr = read_datum symbols data pos in
    Datum.Pair { car; cdr }
  | 8 ->
    let count = read_u32 data pos in
    let elts = Array.init count (fun _ -> read_datum symbols data pos) in
    Datum.Vector elts
  | 9 -> Datum.Bytevector (read_bytes_data data pos)
  | 10 -> Datum.Nil
  | 11 -> Datum.Eof
  | 12 -> Datum.Void
  | _ -> fasl_error (Printf.sprintf "unknown datum tag: %d" tag)

(* --- Code object encoding --- *)

let rec write_code_obj buf (code : Datum.code) =
  write_str buf code.name;
  write_u8 buf (if code.variadic then 1 else 0);
  write_u16 buf (Array.length code.params);
  Array.iter (fun sym -> write_str buf (Symbol.name sym)) code.params;
  write_u16 buf (Array.length code.symbols);
  Array.iter (fun sym -> write_str buf (Symbol.name sym)) code.symbols;
  write_u16 buf (Array.length code.constants);
  Array.iter (write_datum buf) code.constants;
  write_u32 buf (Array.length code.instructions);
  Array.iter (write_opcode buf) code.instructions;
  write_u16 buf (Array.length code.children);
  Array.iter (write_code_obj buf) code.children

let rec read_code_obj symbols data pos =
  let name = read_str data pos in
  let variadic = read_u8 data pos <> 0 in
  let params_count = read_u16 data pos in
  let params = Array.init params_count (fun _ ->
    Symbol.intern symbols (read_str data pos)) in
  let symbols_count = read_u16 data pos in
  let syms = Array.init symbols_count (fun _ ->
    Symbol.intern symbols (read_str data pos)) in
  let constants_count = read_u16 data pos in
  let constants = Array.init constants_count (fun _ ->
    read_datum symbols data pos) in
  let instructions_count = read_u32 data pos in
  let instructions = Array.init instructions_count (fun _ ->
    read_opcode data pos) in
  let children_count = read_u16 data pos in
  let children = Array.init children_count (fun _ ->
    read_code_obj symbols data pos) in
  { Datum.name; variadic; params; symbols = syms; constants;
    instructions; children }

(* --- Public code API --- *)

let write_code code =
  let buf = Buffer.create 256 in
  write_header buf 0;
  write_code_obj buf code;
  Buffer.to_bytes buf

let read_code symbols data =
  let pos = ref 0 in
  let fmt = read_header data pos in
  if fmt <> 0 then
    fasl_error (Printf.sprintf "expected code FASL (type 0), got type %d" fmt);
  read_code_obj symbols data pos

(* --- File I/O --- *)

let write_to_file path data =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_bytes oc data)

let read_from_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let len = in_channel_length ic in
    let data = Bytes.create len in
    really_input ic data 0 len;
    data)

let write_code_to_file path code =
  let data = write_code code in
  write_to_file path data

let read_code_from_file symbols path =
  let data = read_from_file path in
  read_code symbols data

(* --- Import set encoding --- *)

let rec write_import_set buf (iset : Library.import_set) =
  match iset with
  | Import_lib parts ->
    write_u8 buf 0;
    write_u16 buf (List.length parts);
    List.iter (write_str buf) parts
  | Import_only (inner, names) ->
    write_u8 buf 1;
    write_import_set buf inner;
    write_u16 buf (List.length names);
    List.iter (write_str buf) names
  | Import_except (inner, names) ->
    write_u8 buf 2;
    write_import_set buf inner;
    write_u16 buf (List.length names);
    List.iter (write_str buf) names
  | Import_prefix (inner, prefix) ->
    write_u8 buf 3;
    write_import_set buf inner;
    write_str buf prefix
  | Import_rename (inner, pairs) ->
    write_u8 buf 4;
    write_import_set buf inner;
    write_u16 buf (List.length pairs);
    List.iter (fun (from, to_) ->
      write_str buf from;
      write_str buf to_
    ) pairs

let rec read_import_set data pos =
  let tag = read_u8 data pos in
  match tag with
  | 0 ->
    let count = read_u16 data pos in
    let parts = List.init count (fun _ -> read_str data pos) in
    Library.Import_lib parts
  | 1 ->
    let inner = read_import_set data pos in
    let count = read_u16 data pos in
    let names = List.init count (fun _ -> read_str data pos) in
    Library.Import_only (inner, names)
  | 2 ->
    let inner = read_import_set data pos in
    let count = read_u16 data pos in
    let names = List.init count (fun _ -> read_str data pos) in
    Library.Import_except (inner, names)
  | 3 ->
    let inner = read_import_set data pos in
    let prefix = read_str data pos in
    Library.Import_prefix (inner, prefix)
  | 4 ->
    let inner = read_import_set data pos in
    let count = read_u16 data pos in
    let pairs = List.init count (fun _ ->
      let from = read_str data pos in
      let to_ = read_str data pos in
      (from, to_)
    ) in
    Library.Import_rename (inner, pairs)
  | _ -> fasl_error (Printf.sprintf "unknown import set tag: %d" tag)

(* --- Export spec encoding --- *)

let write_export_spec buf (spec : Library.export_spec) =
  match spec with
  | Export_id name ->
    write_u8 buf 0;
    write_str buf name
  | Export_rename (internal, external_) ->
    write_u8 buf 1;
    write_str buf internal;
    write_str buf external_

let read_export_spec data pos =
  let tag = read_u8 data pos in
  match tag with
  | 0 -> Library.Export_id (read_str data pos)
  | 1 ->
    let internal = read_str data pos in
    let external_ = read_str data pos in
    Library.Export_rename (internal, external_)
  | _ -> fasl_error (Printf.sprintf "unknown export spec tag: %d" tag)

(* --- Library declaration encoding --- *)

type lib_declaration =
  | Lib_import of Library.import_set
  | Lib_code of Datum.code

type lib_fasl = {
  lib_name : Library.library_name;
  has_syntax_exports : bool;
  exports : Library.export_spec list;
  declarations : lib_declaration list;
}

let write_lib_declaration buf decl =
  match decl with
  | Lib_import iset ->
    write_u8 buf 0;
    write_import_set buf iset
  | Lib_code code ->
    write_u8 buf 1;
    write_code_obj buf code

let read_lib_declaration symbols data pos =
  let tag = read_u8 data pos in
  match tag with
  | 0 -> Lib_import (read_import_set data pos)
  | 1 -> Lib_code (read_code_obj symbols data pos)
  | _ -> fasl_error (Printf.sprintf "unknown declaration tag: %d" tag)

(* --- Library FASL public API --- *)

let write_lib_fasl path fasl =
  let buf = Buffer.create 512 in
  write_header buf 1;
  write_u16 buf (List.length fasl.lib_name);
  List.iter (write_str buf) fasl.lib_name;
  write_u8 buf (if fasl.has_syntax_exports then 1 else 0);
  write_u16 buf (List.length fasl.exports);
  List.iter (write_export_spec buf) fasl.exports;
  write_u16 buf (List.length fasl.declarations);
  List.iter (write_lib_declaration buf) fasl.declarations;
  write_to_file path (Buffer.to_bytes buf)

let read_lib_fasl symbols path =
  let data = read_from_file path in
  let pos = ref 0 in
  let fmt = read_header data pos in
  if fmt <> 1 then
    fasl_error (Printf.sprintf "expected library FASL (type 1), got type %d" fmt);
  let name_count = read_u16 data pos in
  let lib_name = List.init name_count (fun _ -> read_str data pos) in
  let has_syntax_exports = read_u8 data pos <> 0 in
  let export_count = read_u16 data pos in
  let exports = List.init export_count (fun _ -> read_export_spec data pos) in
  let decl_count = read_u16 data pos in
  let declarations = List.init decl_count (fun _ ->
    read_lib_declaration symbols data pos) in
  { lib_name; has_syntax_exports; exports; declarations }

(* --- Cache helpers --- *)

let fasl_path_for sld_path =
  let base = Filename.chop_extension sld_path in
  base ^ ".fasl"

let is_cache_valid ~sld_path ~fasl_path =
  try
    let sld_stat = Unix.stat sld_path in
    let fasl_stat = Unix.stat fasl_path in
    fasl_stat.Unix.st_mtime >= sld_stat.Unix.st_mtime
  with Unix.Unix_error _ -> false
