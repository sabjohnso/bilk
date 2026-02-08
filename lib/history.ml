type t = {
  entries : string array;
  mutable count : int;       (* number of valid entries *)
  mutable nav_pos : int;     (* -1 = past newest *)
  max_length : int;
}

let create ?(max_length = 1000) () =
  { entries = Array.make (max max_length 1) "";
    count = 0;
    nav_pos = -1;
    max_length = max max_length 1;
  }

let length t = t.count

let to_list t =
  List.init t.count (fun i -> t.entries.(i))

let add t entry =
  if entry = "" then ()
  else if t.count > 0 && t.entries.(t.count - 1) = entry then ()
  else begin
    if t.count < t.max_length then begin
      t.entries.(t.count) <- entry;
      t.count <- t.count + 1
    end else begin
      (* Shift everything left by 1, drop oldest *)
      Array.blit t.entries 1 t.entries 0 (t.max_length - 1);
      t.entries.(t.max_length - 1) <- entry
    end;
    t.nav_pos <- -1
  end

let reset_nav t =
  t.nav_pos <- -1

let prev t =
  if t.count = 0 then None
  else begin
    let pos =
      if t.nav_pos < 0 then t.count - 1
      else t.nav_pos - 1
    in
    if pos < 0 then None
    else begin
      t.nav_pos <- pos;
      Some t.entries.(pos)
    end
  end

let next t =
  if t.nav_pos < 0 then None
  else begin
    let pos = t.nav_pos + 1 in
    if pos >= t.count then begin
      t.nav_pos <- -1;
      None
    end else begin
      t.nav_pos <- pos;
      Some t.entries.(pos)
    end
  end

let has_prefix prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let prev_matching t prefix =
  if t.count = 0 then None
  else begin
    let start =
      if t.nav_pos < 0 then t.count - 1
      else t.nav_pos - 1
    in
    let pos = ref start in
    let found = ref false in
    while !pos >= 0 && not !found do
      if has_prefix prefix t.entries.(!pos) then
        found := true
      else
        decr pos
    done;
    if !found then begin
      t.nav_pos <- !pos;
      Some t.entries.(!pos)
    end else
      None
  end

let next_matching t prefix =
  if t.nav_pos < 0 then None
  else begin
    let pos = ref (t.nav_pos + 1) in
    let found = ref false in
    while !pos < t.count && not !found do
      if has_prefix prefix t.entries.(!pos) then
        found := true
      else
        incr pos
    done;
    if !found then begin
      t.nav_pos <- !pos;
      Some t.entries.(!pos)
    end else begin
      t.nav_pos <- -1;
      None
    end
  end

let save_to_file t path =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    for i = 0 to t.count - 1 do
      output_string oc t.entries.(i);
      output_char oc '\n'
    done)

let load_from_file t path =
  if Sys.file_exists path then begin
    let ic = open_in path in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      try
        while true do
          let line = input_line ic in
          add t line
        done
      with End_of_file -> ())
  end
