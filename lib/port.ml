type t = {
  content : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
  file : string;
}

let of_string ?(file = "<string>") content =
  { content; pos = 0; line = 1; col = 1; file }

let peek_char t =
  if t.pos >= String.length t.content then None
  else Some t.content.[t.pos]

let read_char t =
  if t.pos >= String.length t.content then None
  else begin
    let c = t.content.[t.pos] in
    t.pos <- t.pos + 1;
    (* Handle newlines: \n, \r\n, and \r alone *)
    if c = '\n' then begin
      t.line <- t.line + 1;
      t.col <- 1
    end else if c = '\r' then begin
      (* Check for \r\n *)
      if t.pos < String.length t.content && t.content.[t.pos] = '\n' then begin
        t.pos <- t.pos + 1  (* consume the \n too *)
      end;
      t.line <- t.line + 1;
      t.col <- 1
    end else
      t.col <- t.col + 1;
    Some c
  end

let current_loc t =
  Loc.make t.file t.line t.col
