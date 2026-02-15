exception Session_error of string

let error msg = raise (Session_error msg)
let errorf fmt = Printf.ksprintf error fmt

type named_checkpoint = { name : string; data : bytes }

type t = {
  mutable checkpoints : named_checkpoint list;  (* in creation order *)
}

(* --- Construction --- *)

let create () = { checkpoints = [] }

(* --- Checkpoint / Revert --- *)

let checkpoint t name inst =
  let data = Checkpoint.snapshot inst in
  t.checkpoints <- List.filter (fun cp -> cp.name <> name) t.checkpoints;
  t.checkpoints <- t.checkpoints @ [{ name; data }]

let revert t name =
  match List.find_opt (fun cp -> cp.name = name) t.checkpoints with
  | None -> errorf "unknown checkpoint: %s" name
  | Some cp -> Checkpoint.restore cp.data

let list_checkpoints t =
  List.map (fun cp -> cp.name) t.checkpoints

(* --- Binary file format ---
   Magic: "BSES" (4 bytes)
   num_checkpoints: u32
   For each:
     name_len: u16, name: bytes
     data_len: u32, data: bytes
*)

let session_magic = "BSES"

let save t path =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc session_magic;
    let buf = Bytes.create 4 in
    Bytes.set_int32_le buf 0 (Int32.of_int (List.length t.checkpoints));
    output_bytes oc buf;
    List.iter (fun cp ->
      let name_bytes = Bytes.of_string cp.name in
      let name_len_buf = Bytes.create 2 in
      Bytes.set_uint16_le name_len_buf 0 (Bytes.length name_bytes);
      output_bytes oc name_len_buf;
      output_bytes oc name_bytes;
      let data_len_buf = Bytes.create 4 in
      Bytes.set_int32_le data_len_buf 0 (Int32.of_int (Bytes.length cp.data));
      output_bytes oc data_len_buf;
      output_bytes oc cp.data
    ) t.checkpoints)

let load path =
  if not (Sys.file_exists path) then
    errorf "session file not found: %s" path;
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let len = in_channel_length ic in
    if len < 8 then error "session file too short";
    let data = Bytes.create len in
    really_input ic data 0 len;
    let magic = Bytes.sub_string data 0 4 in
    if magic <> session_magic then
      errorf "bad session file magic: expected BSES, got %s" magic;
    let pos = ref 4 in
    let read_u16 () =
      if !pos + 2 > len then error "unexpected end of session file";
      let v = Bytes.get_uint16_le data !pos in
      pos := !pos + 2; v
    in
    let read_u32 () =
      if !pos + 4 > len then error "unexpected end of session file";
      let v = Int32.to_int (Bytes.get_int32_le data !pos) in
      pos := !pos + 4; v
    in
    let num_cps = read_u32 () in
    let t = create () in
    for _ = 1 to num_cps do
      let name_len = read_u16 () in
      if !pos + name_len > len then error "unexpected end of session file";
      let name = Bytes.sub_string data !pos name_len in
      pos := !pos + name_len;
      let data_len = read_u32 () in
      if !pos + data_len > len then error "unexpected end of session file";
      let cp_data = Bytes.sub data !pos data_len in
      pos := !pos + data_len;
      t.checkpoints <- t.checkpoints @ [{ name; data = cp_data }]
    done;
    t)
