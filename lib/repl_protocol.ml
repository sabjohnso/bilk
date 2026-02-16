type client_msg =
  | Eval of string
  | Complete of string
  | Interrupt
  | Input of string
  | Resume of string
  | Disconnect

type status = Ready | Busy

type server_msg =
  | Output of string
  | Result of string
  | Error of string
  | Completions of string list
  | Read_request of string
  | Status of status
  | Session_ok
  | Session_deny

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
  let payload_len = Buffer.length payload + 1 in
  write_u32 buf payload_len;
  Buffer.add_char buf (Char.chr tag);
  Buffer.add_buffer buf payload

let write_client_msg buf = function
  | Eval s ->
    write_frame buf 0x01 (fun p -> write_bytes p s)
  | Complete s ->
    write_frame buf 0x02 (fun p -> write_bytes p s)
  | Interrupt ->
    write_frame buf 0x03 (fun _ -> ())
  | Input s ->
    write_frame buf 0x04 (fun p -> write_bytes p s)
  | Resume s ->
    write_frame buf 0x05 (fun p -> write_bytes p s)
  | Disconnect ->
    write_frame buf 0x06 (fun _ -> ())

let write_server_msg buf = function
  | Output s ->
    write_frame buf 0x81 (fun p -> write_bytes p s)
  | Result s ->
    write_frame buf 0x82 (fun p -> write_bytes p s)
  | Error s ->
    write_frame buf 0x83 (fun p -> write_bytes p s)
  | Completions lst ->
    write_frame buf 0x84 (fun p ->
      write_u16 p (List.length lst);
      List.iter (fun s -> write_bytes p s) lst)
  | Read_request s ->
    write_frame buf 0x85 (fun p -> write_bytes p s)
  | Status st ->
    write_frame buf 0x86 (fun p ->
      Buffer.add_char p (match st with Ready -> '\x00' | Busy -> '\x01'))
  | Session_ok ->
    write_frame buf 0x87 (fun _ -> ())
  | Session_deny ->
    write_frame buf 0x88 (fun _ -> ())

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
      Eval s
    | 0x02 ->
      let (s, _) = read_bytes data body_off in
      Complete s
    | 0x03 -> Interrupt
    | 0x04 ->
      let (s, _) = read_bytes data body_off in
      Input s
    | 0x05 ->
      let (s, _) = read_bytes data body_off in
      Resume s
    | 0x06 -> Disconnect
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
    | 0x82 ->
      let (s, _) = read_bytes data body_off in
      Result s
    | 0x83 ->
      let (s, _) = read_bytes data body_off in
      Error s
    | 0x84 ->
      let count = read_u16 data body_off in
      let off = ref (body_off + 2) in
      let lst = List.init count (fun _ ->
        let (s, next) = read_bytes data !off in
        off := next;
        s)
      in
      Completions lst
    | 0x85 ->
      let (s, _) = read_bytes data body_off in
      Read_request s
    | 0x86 ->
      let b = Char.code data.[body_off] in
      Status (if b = 0 then Ready else Busy)
    | 0x87 -> Session_ok
    | 0x88 -> Session_deny
    | _ -> raise (Protocol_error (Printf.sprintf "unknown server tag 0x%02x" tag))
  in
  (msg, next)
