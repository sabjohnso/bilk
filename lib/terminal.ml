type key =
  | Char of char
  | Enter
  | Backspace
  | Delete
  | Tab
  | Up
  | Down
  | Left
  | Right
  | Home
  | End_key
  | Ctrl_a
  | Ctrl_b
  | Ctrl_c
  | Ctrl_d
  | Ctrl_e
  | Ctrl_f
  | Ctrl_k
  | Ctrl_l
  | Ctrl_n
  | Ctrl_p
  | Ctrl_u
  | Ctrl_w
  | Alt_left
  | Alt_right
  | Alt_backspace
  | Alt_enter
  | Ctrl_right
  | Ctrl_left
  | Alt_s
  | Alt_r
  | Alt_open_paren
  | Alt_9
  | Unknown

type t = {
  fd : Unix.file_descr;
  saved : Unix.terminal_io;
}

let enter_raw fd =
  let saved = Unix.tcgetattr fd in
  let raw = { saved with
    Unix.c_icanon = false;
    c_echo = false;
    c_isig = false;
    c_ixon = false;
    c_icrnl = false;
    c_opost = true;
    c_vmin = 1;
    c_vtime = 0;
  } in
  Unix.tcsetattr fd Unix.TCSAFLUSH raw;
  { fd; saved }

let leave_raw t =
  Unix.tcsetattr t.fd Unix.TCSAFLUSH t.saved

let fd t = t.fd

(* Read a single byte from the terminal. Returns -1 on EOF. *)
let read_byte fd =
  let buf = Bytes.create 1 in
  let n = Unix.read fd buf 0 1 in
  if n = 0 then -1
  else Char.code (Bytes.get buf 0)

(* Try to read a byte with a short timeout.
   Returns -1 if nothing available. *)
let read_byte_timeout fd timeout_ms =
  let timeout = float_of_int timeout_ms /. 1000.0 in
  let ready, _, _ = Unix.select [fd] [] [] timeout in
  match ready with
  | [] -> -1
  | _ -> read_byte fd

(* Parse a CSI (Control Sequence Introducer) sequence.
   Called after ESC [ has been consumed. Reads parameter bytes and final byte. *)
let read_csi fd =
  let buf = Buffer.create 8 in
  let rec collect () =
    let b = read_byte_timeout fd 50 in
    if b < 0 then
      Buffer.contents buf
    else begin
      Buffer.add_char buf (Char.chr b);
      (* CSI parameters are 0x30-0x3F, intermediates 0x20-0x2F,
         final bytes 0x40-0x7E *)
      if b >= 0x40 && b <= 0x7E then
        Buffer.contents buf
      else
        collect ()
    end
  in
  collect ()

let decode_csi seq =
  match seq with
  | "A" -> Up
  | "B" -> Down
  | "C" -> Right
  | "D" -> Left
  | "H" -> Home
  | "F" -> End_key
  | "3~" -> Delete
  | "1~" | "7~" -> Home
  | "4~" | "8~" -> End_key
  | "1;3C" -> Alt_right
  | "1;3D" -> Alt_left
  | "1;5C" -> Ctrl_right
  | "1;5D" -> Ctrl_left
  | _ -> Unknown

(* Parse key bytes — pure function for testing *)
let parse_key_bytes buf len =
  if len = 0 then Unknown
  else
    let b0 = Char.code (Bytes.get buf 0) in
    match b0 with
    | 0x0D | 0x0A -> Enter
    | 0x7F -> Backspace
    | 0x08 -> Backspace
    | 0x09 -> Tab
    | 0x01 -> Ctrl_a
    | 0x02 -> Ctrl_b
    | 0x03 -> Ctrl_c
    | 0x04 -> Ctrl_d
    | 0x05 -> Ctrl_e
    | 0x06 -> Ctrl_f
    | 0x0B -> Ctrl_k
    | 0x0C -> Ctrl_l
    | 0x0E -> Ctrl_n
    | 0x10 -> Ctrl_p
    | 0x15 -> Ctrl_u
    | 0x17 -> Ctrl_w
    | 0x1B ->
      if len = 1 then Unknown
      else begin
        let b1 = Char.code (Bytes.get buf 1) in
        if b1 = 0x5B (* '[' *) then begin
          (* CSI sequence *)
          if len <= 2 then Unknown
          else
            let csi = Bytes.sub_string buf 2 (len - 2) in
            decode_csi csi
        end else if b1 = 0x0D || b1 = 0x0A then
          Alt_enter
        else if b1 = 0x7F then
          Alt_backspace
        else if b1 = 0x08 then
          Alt_backspace
        else if b1 = Char.code 'b' then
          Alt_left
        else if b1 = Char.code 'f' then
          Alt_right
        else if b1 = Char.code 's' then
          Alt_s
        else if b1 = Char.code 'r' then
          Alt_r
        else if b1 = Char.code '(' then
          Alt_open_paren
        else if b1 = Char.code '9' then
          Alt_9
        else
          Unknown
      end
    | c when c >= 32 && c < 127 -> Char (Char.chr c)
    | _ -> Unknown

let read_key t =
  let b0 = read_byte t.fd in
  if b0 < 0 then Ctrl_d (* EOF *)
  else if b0 = 0x1B then begin
    (* Escape — check for multi-byte sequence *)
    let b1 = read_byte_timeout t.fd 50 in
    if b1 < 0 then
      Unknown (* bare Escape *)
    else if b1 = 0x5B then begin
      (* CSI: ESC [ ... *)
      let csi = read_csi t.fd in
      decode_csi csi
    end else if b1 = 0x4F then begin
      (* SS3: ESC O ... *)
      let b2 = read_byte_timeout t.fd 50 in
      if b2 < 0 then Unknown
      else match Char.chr b2 with
        | 'A' -> Up
        | 'B' -> Down
        | 'C' -> Right
        | 'D' -> Left
        | 'H' -> Home
        | 'F' -> End_key
        | _ -> Unknown
    end else if b1 = 0x0D || b1 = 0x0A then
      Alt_enter
    else if b1 = 0x7F || b1 = 0x08 then
      Alt_backspace
    else if b1 = Char.code 'b' then
      Alt_left
    else if b1 = Char.code 'f' then
      Alt_right
    else if b1 = Char.code 's' then
      Alt_s
    else if b1 = Char.code 'r' then
      Alt_r
    else if b1 = Char.code '(' then
      Alt_open_paren
    else if b1 = Char.code '9' then
      Alt_9
    else
      Unknown
  end else begin
    let buf = Bytes.create 1 in
    Bytes.set buf 0 (Char.chr b0);
    parse_key_bytes buf 1
  end

(* Output helpers *)

let write_string t s =
  let len = String.length s in
  let _ = Unix.write_substring t.fd s 0 len in
  ()

let move_cursor_to t row col =
  write_string t (Printf.sprintf "\x1b[%d;%dH" row col)

let clear_to_end t =
  write_string t "\x1b[0K"

let clear_below t =
  write_string t "\x1b[0J"

let move_to_column t col =
  write_string t (Printf.sprintf "\x1b[%dG" col)

let get_cursor_pos t =
  write_string t "\x1b[6n";
  (* Read response: ESC [ row ; col R *)
  let buf = Buffer.create 16 in
  let rec read_response () =
    let b = read_byte_timeout t.fd 100 in
    if b < 0 then (1, 1)
    else begin
      Buffer.add_char buf (Char.chr b);
      if Char.chr b = 'R' then begin
        let s = Buffer.contents buf in
        (* Parse "ESC[row;colR" — buffer starts after we consumed ESC[ already,
           but we're reading the full response here *)
        try
          let esc_pos = String.index s '\x1b' in
          let bracket_pos = String.index_from s esc_pos '[' in
          let semi_pos = String.index_from s bracket_pos ';' in
          let r_pos = String.index_from s semi_pos 'R' in
          let row = int_of_string (String.sub s (bracket_pos + 1) (semi_pos - bracket_pos - 1)) in
          let col = int_of_string (String.sub s (semi_pos + 1) (r_pos - semi_pos - 1)) in
          (row, col)
        with _ -> (1, 1)
      end else
        read_response ()
    end
  in
  read_response ()

let get_terminal_size t =
  (* Save cursor, move to far bottom-right, query position, restore *)
  write_string t "\x1b[s\x1b[9999;9999H\x1b[6n";
  let buf = Buffer.create 16 in
  let rec read_response () =
    let b = read_byte_timeout t.fd 100 in
    if b < 0 then (24, 80)
    else begin
      Buffer.add_char buf (Char.chr b);
      if Char.chr b = 'R' then begin
        let s = Buffer.contents buf in
        write_string t "\x1b[u";  (* restore cursor *)
        try
          let esc_pos = String.index s '\x1b' in
          let bracket_pos = String.index_from s esc_pos '[' in
          let semi_pos = String.index_from s bracket_pos ';' in
          let r_pos = String.index_from s semi_pos 'R' in
          let rows = int_of_string (String.sub s (bracket_pos + 1) (semi_pos - bracket_pos - 1)) in
          let cols = int_of_string (String.sub s (semi_pos + 1) (r_pos - semi_pos - 1)) in
          (rows, cols)
        with _ ->
          write_string t "\x1b[u";
          (24, 80)
      end else
        read_response ()
    end
  in
  read_response ()
