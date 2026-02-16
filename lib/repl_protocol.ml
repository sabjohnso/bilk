type client_msg =
  | Key_input of string
  | Resize of int * int
  | Ping
  | Disconnect

type server_msg =
  | Output of string
  | Pong
  | Scrollback of string
  | Server_exit

exception Protocol_error of string

(* --- Wire helpers --- *)

let write_u32 buf n =
  Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (n land 0xff))

let read_u32 data off =
  (Char.code data.[off] lsl 24) lor
  (Char.code data.[off + 1] lsl 16) lor
  (Char.code data.[off + 2] lsl 8) lor
  Char.code data.[off + 3]

let write_u16 buf n =
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (n land 0xff))

let read_u16 data off =
  (Char.code data.[off] lsl 8) lor
  Char.code data.[off + 1]

let write_bytes buf s =
  write_u32 buf (String.length s);
  Buffer.add_string buf s

let read_bytes data off =
  let len = read_u32 data off in
  let s = String.sub data (off + 4) len in
  (s, off + 4 + len)

(* --- Frame writing --- *)

let write_frame buf tag payload_fn =
  let payload = Buffer.create 64 in
  payload_fn payload;
  let payload_len = Buffer.length payload + 1 in (* +1 for tag byte *)
  write_u32 buf payload_len;
  Buffer.add_char buf (Char.chr tag);
  Buffer.add_buffer buf payload

let write_client_msg buf = function
  | Key_input s ->
    write_frame buf 0x01 (fun p -> write_bytes p s)
  | Resize (rows, cols) ->
    write_frame buf 0x02 (fun p -> write_u16 p rows; write_u16 p cols)
  | Ping ->
    write_frame buf 0x03 (fun _ -> ())
  | Disconnect ->
    write_frame buf 0x04 (fun _ -> ())

let write_server_msg buf = function
  | Output s ->
    write_frame buf 0x81 (fun p -> write_bytes p s)
  | Pong ->
    write_frame buf 0x82 (fun _ -> ())
  | Scrollback s ->
    write_frame buf 0x83 (fun p -> write_bytes p s)
  | Server_exit ->
    write_frame buf 0x84 (fun _ -> ())

(* --- Frame reading --- *)

let frame_available data offset =
  let len = String.length data in
  if offset + 4 > len then false
  else begin
    let payload_len = read_u32 data offset in
    offset + 4 + payload_len <= len
  end

let read_client_msg data offset =
  if not (frame_available data offset) then
    raise (Protocol_error "incomplete frame");
  let payload_len = read_u32 data offset in
  let tag_off = offset + 4 in
  let tag = Char.code data.[tag_off] in
  let body_off = tag_off + 1 in
  let next = offset + 4 + payload_len in
  let msg = match tag with
    | 0x01 ->
      let (s, _) = read_bytes data body_off in
      Key_input s
    | 0x02 ->
      let rows = read_u16 data body_off in
      let cols = read_u16 data (body_off + 2) in
      Resize (rows, cols)
    | 0x03 -> Ping
    | 0x04 -> Disconnect
    | _ -> raise (Protocol_error (Printf.sprintf "unknown client tag 0x%02x" tag))
  in
  (msg, next)

let read_server_msg data offset =
  if not (frame_available data offset) then
    raise (Protocol_error "incomplete frame");
  let payload_len = read_u32 data offset in
  let tag_off = offset + 4 in
  let tag = Char.code data.[tag_off] in
  let body_off = tag_off + 1 in
  let next = offset + 4 + payload_len in
  let msg = match tag with
    | 0x81 ->
      let (s, _) = read_bytes data body_off in
      Output s
    | 0x82 -> Pong
    | 0x83 ->
      let (s, _) = read_bytes data body_off in
      Scrollback s
    | 0x84 -> Server_exit
    | _ -> raise (Protocol_error (Printf.sprintf "unknown server tag 0x%02x" tag))
  in
  (msg, next)
