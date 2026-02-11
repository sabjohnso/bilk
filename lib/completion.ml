let is_ident_char c =
  match c with
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-'
  | '!' | '?' | '+' | '*' | '/' | '<' | '>' | '='
  | '.' | ':' | '~' | '^' | '&' | '%' | '@' -> true
  | ',' -> true  (* for REPL commands *)
  | _ -> false

let extract_prefix text cursor =
  let p = ref (cursor - 1) in
  while !p >= 0 && is_ident_char text.[!p] do decr p done;
  let start = !p + 1 in
  let prefix = String.sub text start (cursor - start) in
  (prefix, start)

let find_matches prefix candidates =
  let plen = String.length prefix in
  let matches = List.filter (fun s ->
    String.length s >= plen &&
    String.sub s 0 plen = prefix
  ) candidates in
  List.sort String.compare matches

let common_prefix strs =
  match strs with
  | [] -> ""
  | [s] -> s
  | first :: rest ->
    let len = ref (String.length first) in
    List.iter (fun s ->
      len := min !len (String.length s);
      let i = ref 0 in
      while !i < !len && first.[!i] = s.[!i] do incr i done;
      len := !i
    ) rest;
    String.sub first 0 !len

let format_columns ~width candidates =
  match candidates with
  | [] -> ""
  | _ ->
    let max_len = List.fold_left (fun acc s -> max acc (String.length s)) 0 candidates in
    let col_width = max_len + 2 in
    let ncols = max 1 (width / col_width) in
    let buf = Buffer.create 256 in
    List.iteri (fun i s ->
      if i > 0 && i mod ncols = 0 then
        Buffer.add_char buf '\n';
      Buffer.add_string buf s;
      let pad = col_width - String.length s in
      if pad > 0 && (i + 1) mod ncols <> 0 then
        Buffer.add_string buf (String.make pad ' ')
    ) candidates;
    Buffer.contents buf
