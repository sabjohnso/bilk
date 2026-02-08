type input_source =
  | In_string of { content : string; mutable pos : int }
  | In_channel of { chan : in_channel; mutable lookahead : char option }

type output_sink =
  | Out_buffer of Buffer.t
  | Out_channel of out_channel

type direction =
  | Input of input_source
  | Output of output_sink

type t = {
  direction : direction;
  mutable line : int;
  mutable col : int;
  file : string;
  mutable readtable_obj : Obj.t option;
  mutable is_open : bool;
}

let check_open name t =
  if not t.is_open then
    failwith (Printf.sprintf "%s: port is closed" name)

let check_input name t =
  check_open name t;
  match t.direction with
  | Input src -> src
  | Output _ -> failwith (Printf.sprintf "%s: not an input port" name)

let check_output name t =
  check_open name t;
  match t.direction with
  | Output sink -> sink
  | Input _ -> failwith (Printf.sprintf "%s: not an output port" name)

(* --- Input constructors --- *)

let of_string ?(file = "<string>") content =
  { direction = Input (In_string { content; pos = 0 });
    line = 1; col = 1; file; readtable_obj = None; is_open = true }

let of_file path =
  let content = In_channel.with_open_bin path In_channel.input_all in
  of_string ~file:path content

let of_in_channel ?(file = "<channel>") chan =
  { direction = Input (In_channel { chan; lookahead = None });
    line = 1; col = 1; file; readtable_obj = None; is_open = true }

let open_input_file path =
  of_file path

(* --- Output constructors --- *)

let of_out_channel ?(file = "<channel>") chan =
  { direction = Output (Out_channel chan);
    line = 1; col = 1; file; readtable_obj = None; is_open = true }

let open_output_string () =
  { direction = Output (Out_buffer (Buffer.create 256));
    line = 1; col = 1; file = "<string>"; readtable_obj = None; is_open = true }

let open_output_file path =
  let chan = open_out path in
  { direction = Output (Out_channel chan);
    line = 1; col = 1; file = path; readtable_obj = None; is_open = true }

(* --- Readtable --- *)

let readtable_obj t = t.readtable_obj

let set_readtable_obj t obj = t.readtable_obj <- Some obj

(* --- Input operations --- *)

let peek_char t =
  let src = check_input "peek-char" t in
  match src with
  | In_string s ->
    if s.pos >= String.length s.content then None
    else Some s.content.[s.pos]
  | In_channel c ->
    match c.lookahead with
    | Some _ as ch -> ch
    | None ->
      (try
         let ch = input_char c.chan in
         c.lookahead <- Some ch;
         Some ch
       with End_of_file -> None)

let read_char t =
  let src = check_input "read-char" t in
  let result = match src with
    | In_string s ->
      if s.pos >= String.length s.content then None
      else begin
        let c = s.content.[s.pos] in
        s.pos <- s.pos + 1;
        (* Handle \r\n *)
        if c = '\r' && s.pos < String.length s.content && s.content.[s.pos] = '\n' then
          s.pos <- s.pos + 1;
        Some c
      end
    | In_channel c ->
      let ch = match c.lookahead with
        | Some ch -> c.lookahead <- None; Some ch
        | None ->
          (try Some (input_char c.chan)
           with End_of_file -> None)
      in
      (* Handle \r\n for channel *)
      (match ch with
       | Some '\r' ->
         (try
            let next = input_char c.chan in
            if next <> '\n' then c.lookahead <- Some next
          with End_of_file -> ());
         ch
       | _ -> ch)
  in
  (* Update line/col tracking *)
  (match result with
   | Some '\n' | Some '\r' ->
     t.line <- t.line + 1;
     t.col <- 1
   | Some _ ->
     t.col <- t.col + 1
   | None -> ());
  result

let current_loc t =
  Loc.make t.file t.line t.col

let position t =
  match t.direction with
  | Input (In_string s) -> s.pos
  | Input (In_channel _) -> 0
  | Output _ -> 0

(* --- Output operations --- *)

let write_char t c =
  let sink = check_output "write-char" t in
  match sink with
  | Out_buffer buf -> Buffer.add_char buf c
  | Out_channel chan -> output_char chan c

let write_uchar t uc =
  let sink = check_output "write-char" t in
  let buf_tmp = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf_tmp uc;
  let s = Buffer.contents buf_tmp in
  match sink with
  | Out_buffer buf -> Buffer.add_string buf s
  | Out_channel chan -> output_string chan s

let write_string t s =
  let sink = check_output "write-string" t in
  match sink with
  | Out_buffer buf -> Buffer.add_string buf s
  | Out_channel chan -> output_string chan s

let write_u8 t byte =
  let sink = check_output "write-u8" t in
  let c = Char.chr (byte land 0xFF) in
  match sink with
  | Out_buffer buf -> Buffer.add_char buf c
  | Out_channel chan -> output_char chan c

let write_bytes t data off len =
  let sink = check_output "write-bytevector" t in
  match sink with
  | Out_buffer buf -> Buffer.add_subbytes buf data off len
  | Out_channel chan -> output chan data off len

let flush t =
  let sink = check_output "flush-output-port" t in
  match sink with
  | Out_buffer _ -> ()
  | Out_channel chan -> Stdlib.flush chan

let get_output_string t =
  check_open "get-output-string" t;
  match t.direction with
  | Output (Out_buffer buf) -> Buffer.contents buf
  | _ -> failwith "get-output-string: not a string output port"

(* --- Read operations --- *)

let read_line t =
  let src = check_input "read-line" t in
  match src with
  | In_string s ->
    if s.pos >= String.length s.content then None
    else begin
      let buf = Buffer.create 80 in
      let rec loop () =
        if s.pos >= String.length s.content then ()
        else
          let c = s.content.[s.pos] in
          s.pos <- s.pos + 1;
          if c = '\n' then begin
            t.line <- t.line + 1;
            t.col <- 1
          end else if c = '\r' then begin
            if s.pos < String.length s.content && s.content.[s.pos] = '\n' then
              s.pos <- s.pos + 1;
            t.line <- t.line + 1;
            t.col <- 1
          end else begin
            Buffer.add_char buf c;
            t.col <- t.col + 1;
            loop ()
          end
      in
      loop ();
      Some (Buffer.contents buf)
    end
  | In_channel c ->
    let first = match c.lookahead with
      | Some ch -> c.lookahead <- None; Some ch
      | None ->
        (try Some (input_char c.chan)
         with End_of_file -> None)
    in
    match first with
    | None -> None
    | Some ch ->
      let buf = Buffer.create 80 in
      let rec process ch =
        if ch = '\n' then begin
          t.line <- t.line + 1; t.col <- 1
        end else if ch = '\r' then begin
          (try
             let next = input_char c.chan in
             if next <> '\n' then c.lookahead <- Some next
           with End_of_file -> ());
          t.line <- t.line + 1; t.col <- 1
        end else begin
          Buffer.add_char buf ch;
          t.col <- t.col + 1;
          (try
             let next = match c.lookahead with
               | Some ch -> c.lookahead <- None; ch
               | None -> input_char c.chan
             in
             process next
           with End_of_file -> ())
        end
      in
      process ch;
      Some (Buffer.contents buf)

let read_u8 t =
  let src = check_input "read-u8" t in
  match src with
  | In_string s ->
    if s.pos >= String.length s.content then None
    else begin
      let b = Char.code s.content.[s.pos] in
      s.pos <- s.pos + 1;
      Some b
    end
  | In_channel c ->
    (match c.lookahead with
     | Some ch -> c.lookahead <- None; Some (Char.code ch)
     | None ->
       (try Some (Char.code (input_char c.chan))
        with End_of_file -> None))

let peek_u8 t =
  let src = check_input "peek-u8" t in
  match src with
  | In_string s ->
    if s.pos >= String.length s.content then None
    else Some (Char.code s.content.[s.pos])
  | In_channel c ->
    match c.lookahead with
    | Some ch -> Some (Char.code ch)
    | None ->
      (try
         let ch = input_char c.chan in
         c.lookahead <- Some ch;
         Some (Char.code ch)
       with End_of_file -> None)

(* --- Close --- *)

let close t =
  if t.is_open then begin
    t.is_open <- false;
    match t.direction with
    | Input (In_channel c) ->
      (try close_in c.chan with _ -> ())
    | Output (Out_channel chan) ->
      (try Stdlib.flush chan; close_out chan with _ -> ())
    | _ -> ()
  end

(* --- Predicates --- *)

let is_input t =
  match t.direction with
  | Input _ -> true
  | Output _ -> false

let is_output t =
  match t.direction with
  | Output _ -> true
  | Input _ -> false

let is_open t = t.is_open

let file_name t = t.file
