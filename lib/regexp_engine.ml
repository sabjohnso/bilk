type node =
  | Lit of int
  | Any
  | Class of bytes
  | NClass of bytes
  | Seq of node list
  | Alt of node * node
  | Rep of node * int * int option * bool
  | Group of int * node
  | Bos
  | Eos
  | Bol
  | Eol

type compiled = {
  root : node;
  num_groups : int;
}

type match_result = {
  matched : bool;
  groups : (int * int) option array;
}

let compile root num_groups = { root; num_groups }

let class_test bits n =
  n >= 0 && n < 256 &&
  Char.code (Bytes.get bits (n / 8)) land (1 lsl (n mod 8)) <> 0

(* Backtracking matcher. Returns Some pos if matched, None if not. *)
let rec try_match node input pos end_ groups =
  match node with
  | Lit b ->
    if pos < end_ && Char.code (Bytes.get input pos) = b then
      Some (pos + 1)
    else None
  | Any ->
    if pos < end_ && Bytes.get input pos <> '\n' then
      Some (pos + 1)
    else None
  | Class bits ->
    if pos < end_ && class_test bits (Char.code (Bytes.get input pos)) then
      Some (pos + 1)
    else None
  | NClass bits ->
    if pos < end_ && not (class_test bits (Char.code (Bytes.get input pos)))
       && Bytes.get input pos <> '\n' then
      Some (pos + 1)
    else None
  | Seq nodes ->
    try_match_seq nodes input pos end_ groups
  | Alt (a, b) ->
    (* Save group state for backtracking *)
    let saved = Array.copy groups in
    (match try_match a input pos end_ groups with
     | Some _ as result -> result
     | None ->
       Array.blit saved 0 groups 0 (Array.length groups);
       try_match b input pos end_ groups)
  | Rep (child, min, max, greedy) ->
    try_match_rep child min max greedy input pos end_ groups 0
  | Group (idx, child) ->
    let start = pos in
    (match try_match child input pos end_ groups with
     | Some end_pos ->
       groups.(idx) <- Some (start, end_pos);
       Some end_pos
     | None -> None)
  | Bos ->
    if pos = 0 then Some pos else None
  | Eos ->
    if pos = end_ then Some pos else None
  | Bol ->
    if pos = 0 || (pos > 0 && Bytes.get input (pos - 1) = '\n') then
      Some pos
    else None
  | Eol ->
    if pos = end_ || Bytes.get input pos = '\n' then
      Some pos
    else None

and try_match_seq nodes input pos end_ groups =
  match nodes with
  | [] -> Some pos
  | n :: rest ->
    match try_match n input pos end_ groups with
    | Some new_pos -> try_match_seq rest input new_pos end_ groups
    | None -> None

and try_match_rep child min max greedy input pos end_ groups count =
  let at_max = match max with Some m -> count >= m | None -> false in
  if greedy then begin
    (* Greedy: try to match more first *)
    if not at_max then begin
      let saved = Array.copy groups in
      match try_match child input pos end_ groups with
      | Some new_pos when new_pos > pos ->
        (* Made progress, try to continue *)
        (match try_match_rep child min max greedy input new_pos end_ groups (count + 1) with
         | Some _ as result -> result
         | None ->
           (* Backtrack: accept count matches *)
           Array.blit saved 0 groups 0 (Array.length groups);
           if count >= min then Some pos else None)
      | _ ->
        Array.blit saved 0 groups 0 (Array.length groups);
        if count >= min then Some pos else None
    end else begin
      if count >= min then Some pos else None
    end
  end else begin
    (* Non-greedy: try to stop first *)
    if count >= min then begin
      Some pos
    end else begin
      if not at_max then
        match try_match child input pos end_ groups with
        | Some new_pos ->
          try_match_rep child min max greedy input new_pos end_ groups (count + 1)
        | None -> None
      else None
    end
  end

let exec compiled input start end_ =
  let groups = Array.make compiled.num_groups None in
  match try_match compiled.root input start end_ groups with
  | Some final_pos when final_pos = end_ ->
    groups.(0) <- Some (start, end_);
    { matched = true; groups }
  | _ ->
    { matched = false; groups = Array.make compiled.num_groups None }

let search compiled input start end_ =
  let rec try_from pos =
    if pos > end_ then
      { matched = false; groups = Array.make compiled.num_groups None }
    else begin
      let groups = Array.make compiled.num_groups None in
      match try_match compiled.root input pos end_ groups with
      | Some final_pos ->
        groups.(0) <- Some (pos, final_pos);
        { matched = true; groups }
      | None -> try_from (pos + 1)
    end
  in
  try_from start
