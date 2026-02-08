type edit_result = {
  text : string;
  cursor : int;
}

(* --- Helpers --- *)

let no_change text cursor = { text; cursor }

let is_open c = c = '(' || c = '['
let is_close c = c = ')' || c = ']'

(* Check if position is inside a string or comment *)
let in_string_or_comment rt text pos =
  let tokens = Tokenizer.tokenize rt text in
  List.exists (fun (tok : Tokenizer.token) ->
    pos >= tok.span.start && pos < tok.span.stop &&
    (tok.kind = String_lit || tok.kind = Comment)
  ) tokens

(* Find token at a given position *)
let token_at rt text pos =
  let tokens = Tokenizer.tokenize rt text in
  List.find_opt (fun (tok : Tokenizer.token) ->
    pos >= tok.span.start && pos < tok.span.stop
  ) tokens

(* --- Navigation --- *)

let find_matching_paren rt text pos =
  let len = String.length text in
  if pos < 0 || pos >= len then None
  else
    let c = text.[pos] in
    if is_open c then begin
      (* Forward search for matching close *)
      if in_string_or_comment rt text pos then None
      else
        let depth = ref 1 in
        let tokens = Tokenizer.tokenize rt text in
        let found = ref None in
        List.iter (fun (tok : Tokenizer.token) ->
          if !found = None && tok.span.start > pos then
            match tok.kind with
            | Paren_open -> incr depth
            | Paren_close ->
              decr depth;
              if !depth = 0 then found := Some tok.span.start
            | _ -> ()
        ) tokens;
        !found
    end else if is_close c then begin
      (* Backward search for matching open *)
      if in_string_or_comment rt text pos then None
      else
        let depth = ref 1 in
        let tokens = Tokenizer.tokenize rt text in
        let rev_tokens = List.rev tokens in
        let found = ref None in
        List.iter (fun (tok : Tokenizer.token) ->
          if !found = None && tok.span.start < pos then
            match tok.kind with
            | Paren_close -> incr depth
            | Paren_open ->
              decr depth;
              if !depth = 0 then found := Some tok.span.start
            | _ -> ()
        ) rev_tokens;
        !found
    end else
      None

let rec sexp_forward rt text pos =
  let len = String.length text in
  let tokens = Tokenizer.tokenize rt text in
  (* Find first non-whitespace token at or after pos *)
  let rec find = function
    | [] -> None
    | (tok : Tokenizer.token) :: rest ->
      if tok.span.stop <= pos then find rest
      else if tok.span.start < pos && tok.kind = Whitespace then find rest
      else
        match tok.kind with
        | Whitespace -> find rest
        | Paren_open ->
          (* Find matching close *)
          let open_pos = tok.span.start in
          (match find_matching_paren rt text open_pos with
           | Some close_pos ->
             (* Move past the close paren *)
             if close_pos + 1 <= len then Some (close_pos + 1)
             else Some len
           | None -> Some tok.span.stop)
        | Paren_close -> None  (* Hit end of enclosing list *)
        | Quote_shorthand | Datum_comment ->
          (* Skip the prefix, then skip the next sexp *)
          let after_prefix = tok.span.stop in
          sexp_forward_from rt text after_prefix (tok :: rest)
        | _ -> Some tok.span.stop
  in
  find (List.filter (fun (tok : Tokenizer.token) ->
    tok.span.stop > pos || tok.span.start >= pos) tokens)

and sexp_forward_from rt text pos _ctx =
  (* Skip whitespace and find next sexp after pos *)
  let tokens = Tokenizer.tokenize rt text in
  let rec find = function
    | [] -> Some pos  (* Nothing after prefix *)
    | (tok : Tokenizer.token) :: rest ->
      if tok.span.stop <= pos then find rest
      else
        match tok.kind with
        | Whitespace -> find rest
        | Paren_open ->
          (match find_matching_paren rt text tok.span.start with
           | Some close_pos -> Some (close_pos + 1)
           | None -> Some tok.span.stop)
        | Quote_shorthand | Datum_comment ->
          sexp_forward_from rt text tok.span.stop rest
        | _ -> Some tok.span.stop
  in
  find tokens

let rec sexp_backward rt text pos =
  let tokens = Tokenizer.tokenize rt text in
  let rev_tokens = List.rev tokens in
  (* Find first non-whitespace token ending at or before pos *)
  let rec find = function
    | [] -> None
    | (tok : Tokenizer.token) :: rest ->
      if tok.span.start >= pos then find rest
      else if tok.span.stop > pos && tok.kind = Whitespace then find rest
      else
        match tok.kind with
        | Whitespace -> find rest
        | Paren_close ->
          (* The close paren is at tok.span.start *)
          let close_pos = tok.span.start in
          (match find_matching_paren rt text close_pos with
           | Some open_pos ->
             (* Check for quote/datum-comment prefixes before the open *)
             skip_prefixes_backward rt text open_pos
           | None -> Some tok.span.start)
        | Paren_open -> None  (* Hit start of enclosing list *)
        | _ ->
          (* Simple atom — go to start, then check for quote prefixes *)
          skip_prefixes_backward rt text tok.span.start
  in
  find rev_tokens

and skip_prefixes_backward rt text pos =
  (* Walk backward past any quote/datum-comment prefixes *)
  let tokens = Tokenizer.tokenize rt text in
  let rec find_prefix p =
    if p <= 0 then Some p
    else
      let prev_tok = List.find_opt (fun (tok : Tokenizer.token) ->
        tok.span.stop = p
      ) tokens in
      match prev_tok with
      | Some tok when tok.kind = Quote_shorthand || tok.kind = Datum_comment ->
        find_prefix tok.span.start
      | _ -> Some p
  in
  find_prefix pos

let enclosing_paren rt text pos =
  let tokens = Tokenizer.tokenize rt text in
  (* Walk tokens, tracking a stack of open-paren positions *)
  let stack = Stack.create () in
  let result = ref None in
  List.iter (fun (tok : Tokenizer.token) ->
    if !result = None then
      match tok.kind with
      | Paren_open -> Stack.push tok.span.start stack
      | Paren_close ->
        if not (Stack.is_empty stack) then begin
          let open_pos = Stack.pop stack in
          let close_pos = tok.span.start in
          if pos > open_pos && pos <= close_pos then
            (* This is a candidate; keep the innermost *)
            (match !result with
             | None -> result := Some (open_pos, close_pos)
             | Some (prev_open, _) ->
               if open_pos > prev_open then
                 result := Some (open_pos, close_pos))
        end
      | _ -> ()
  ) tokens;
  !result

(* --- Balanced insertion --- *)

let insert_open_paren text cursor =
  let len = String.length text in
  let before = String.sub text 0 cursor in
  let after = String.sub text cursor (len - cursor) in
  { text = before ^ "()" ^ after; cursor = cursor + 1 }

let insert_close_paren rt text cursor =
  let len = String.length text in
  if in_string_or_comment rt text cursor then begin
    let before = String.sub text 0 cursor in
    let after = String.sub text cursor (len - cursor) in
    { text = before ^ ")" ^ after; cursor = cursor + 1 }
  end else begin
    (* Skip whitespace to find the next close paren *)
    let p = ref cursor in
    while !p < len && (text.[!p] = ' ' || text.[!p] = '\t' || text.[!p] = '\n') do
      incr p
    done;
    if !p < len && is_close text.[!p] then
      (* Move past the existing close paren *)
      { text; cursor = !p + 1 }
    else begin
      let before = String.sub text 0 cursor in
      let after = String.sub text cursor (len - cursor) in
      { text = before ^ ")" ^ after; cursor = cursor + 1 }
    end
  end

let insert_double_quote rt text cursor =
  let len = String.length text in
  (* Check if cursor is inside a string at the closing quote *)
  let tok = token_at rt text cursor in
  match tok with
  | Some t when t.kind = Tokenizer.String_lit ->
    (* Inside a string — if cursor is at the last char position (the closing quote) *)
    if cursor = t.span.stop - 1 && cursor < len && text.[cursor] = '"' then
      (* Move past closing quote *)
      { text; cursor = cursor + 1 }
    else begin
      (* Inside string body — insert literal quote *)
      let before = String.sub text 0 cursor in
      let after = String.sub text cursor (len - cursor) in
      { text = before ^ "\"" ^ after; cursor = cursor + 1 }
    end
  | _ ->
    (* Not in a string — insert pair *)
    let before = String.sub text 0 cursor in
    let after = String.sub text cursor (len - cursor) in
    { text = before ^ "\"\"" ^ after; cursor = cursor + 1 }

(* --- Balanced deletion --- *)

let delete_char_at text pos =
  let len = String.length text in
  let before = String.sub text 0 pos in
  let after = String.sub text (pos + 1) (len - pos - 1) in
  before ^ after

let delete_range text start stop =
  let len = String.length text in
  let before = String.sub text 0 start in
  let after = String.sub text stop (len - stop) in
  before ^ after

let backspace_paredit rt text cursor =
  if cursor <= 0 then no_change text cursor
  else
    let prev = text.[cursor - 1] in
    (* Check what token the character before cursor belongs to *)
    let tok = token_at rt text (cursor - 1) in
    match tok with
    | Some t when t.kind = Tokenizer.String_lit ->
      if cursor - 1 = t.span.start then
        (* prev is the opening quote *)
        if t.span.stop - t.span.start = 2 then
          (* Empty string "" — delete both quotes *)
          { text = delete_range text (cursor - 1) (cursor + 1);
            cursor = cursor - 1 }
        else
          (* Non-empty string — skip opening quote *)
          no_change text cursor
      else if cursor - 1 = t.span.stop - 1 then
        (* prev is the closing quote — move inside *)
        { text; cursor = cursor - 1 }
      else
        (* Inside string body — normal delete *)
        { text = delete_char_at text (cursor - 1); cursor = cursor - 1 }
    | Some t when t.kind = Tokenizer.Comment ->
      (* Inside comment — normal delete *)
      { text = delete_char_at text (cursor - 1); cursor = cursor - 1 }
    | _ ->
      if is_open prev then begin
        let len = String.length text in
        if cursor < len && is_close text.[cursor] then
          (* Empty pair — delete both *)
          { text = delete_range text (cursor - 1) (cursor + 1);
            cursor = cursor - 1 }
        else
          (* Non-empty list — skip *)
          no_change text cursor
      end else if is_close prev then
        (* Move inside the closing delimiter *)
        { text; cursor = cursor - 1 }
      else
        (* Normal character — delete it *)
        { text = delete_char_at text (cursor - 1); cursor = cursor - 1 }

let delete_paredit rt text cursor =
  let len = String.length text in
  if cursor >= len then no_change text cursor
  else
    let ch = text.[cursor] in
    let tok = token_at rt text cursor in
    match tok with
    | Some t when t.kind = Tokenizer.String_lit ->
      if cursor = t.span.start then
        (* ch is the opening quote *)
        if t.span.stop - t.span.start = 2 then
          (* Empty string "" — delete both *)
          { text = delete_range text cursor (cursor + 2); cursor }
        else
          (* Non-empty string — skip *)
          no_change text cursor
      else if cursor = t.span.stop - 1 then
        (* ch is the closing quote — skip *)
        no_change text cursor
      else
        (* Inside string body — normal delete *)
        { text = delete_char_at text cursor; cursor }
    | Some t when t.kind = Tokenizer.Comment ->
      { text = delete_char_at text cursor; cursor }
    | _ ->
      if is_open ch then begin
        if cursor + 1 < len && is_close text.[cursor + 1] then
          (* Empty pair — delete both *)
          { text = delete_range text cursor (cursor + 2); cursor }
        else
          (* Non-empty — skip *)
          no_change text cursor
      end else if is_close ch then
        no_change text cursor
      else
        { text = delete_char_at text cursor; cursor }

(* --- Structural operations --- *)

let slurp_forward rt text cursor =
  match enclosing_paren rt text cursor with
  | None -> no_change text cursor
  | Some (_, close_pos) ->
    let len = String.length text in
    (* Find the next sexp after close_pos + 1 *)
    match sexp_forward rt text (close_pos + 1) with
    | None -> no_change text cursor
    | Some sexp_end ->
      (* Remove the close paren at close_pos, then insert it at sexp_end - 1 *)
      let buf = Buffer.create len in
      for i = 0 to len - 1 do
        if i = close_pos then ()  (* skip old close paren *)
        else Buffer.add_char buf text.[i]
      done;
      let intermediate = Buffer.contents buf in
      (* sexp_end was in original coordinates; adjust for removed paren *)
      let new_end = sexp_end - 1 in
      let before = String.sub intermediate 0 new_end in
      let after = String.sub intermediate new_end (String.length intermediate - new_end) in
      let new_text = before ^ ")" ^ after in
      { text = new_text; cursor }

let barf_forward rt text cursor =
  match enclosing_paren rt text cursor with
  | None -> no_change text cursor
  | Some (open_pos, close_pos) ->
    (* Find the last sexp inside the list *)
    match sexp_backward rt text close_pos with
    | None -> no_change text cursor  (* Empty list or only whitespace *)
    | Some last_sexp_start ->
      if last_sexp_start <= open_pos + 1 then
        (* Only one element — don't barf *)
        no_change text cursor
      else begin
        (* Find where whitespace before last sexp starts *)
        let ws_start = ref last_sexp_start in
        while !ws_start > open_pos + 1 && (text.[!ws_start - 1] = ' ' ||
              text.[!ws_start - 1] = '\t' || text.[!ws_start - 1] = '\n') do
          decr ws_start
        done;
        let new_close_pos = !ws_start in
        (* Build: text[0..open_pos] ++ text[open_pos+1..new_close_pos-1] ++ ")" ++
                  " " ++ text[last_sexp_start..close_pos-1] ++ text[close_pos+1..] *)
        let before_list = String.sub text 0 (open_pos + 1) in
        let inside_kept = String.sub text (open_pos + 1) (new_close_pos - open_pos - 1) in
        let barfed = String.sub text last_sexp_start (close_pos - last_sexp_start) in
        let after_list = String.sub text (close_pos + 1) (String.length text - close_pos - 1) in
        let new_text = before_list ^ inside_kept ^ ") " ^ barfed ^ after_list in
        (* Adjust cursor *)
        let new_cursor =
          if cursor >= last_sexp_start then
            (* Cursor was on barfed element — adjust for repositioning *)
            let shift = (new_close_pos + 2) - last_sexp_start in
            cursor + shift
          else if cursor > new_close_pos then
            (* Cursor was in whitespace before barfed element *)
            new_close_pos + String.length before_list - (open_pos + 1)
          else cursor
        in
        { text = new_text; cursor = new_cursor }
      end

let slurp_backward rt text cursor =
  match enclosing_paren rt text cursor with
  | None -> no_change text cursor
  | Some (open_pos, _) ->
    let len = String.length text in
    (* Find the previous sexp before open_pos *)
    match sexp_backward rt text open_pos with
    | None -> no_change text cursor
    | Some sexp_start ->
      (* Remove the open paren at open_pos, insert it at sexp_start *)
      let buf = Buffer.create len in
      for i = 0 to len - 1 do
        if i = open_pos then ()
        else Buffer.add_char buf text.[i]
      done;
      let intermediate = Buffer.contents buf in
      let before = String.sub intermediate 0 sexp_start in
      let after = String.sub intermediate sexp_start
          (String.length intermediate - sexp_start) in
      let new_text = before ^ "(" ^ after in
      (* Cursor shifts right by 0 if after open_pos, or stays if before *)
      let new_cursor =
        if cursor > open_pos then cursor
        else cursor + 1
      in
      { text = new_text; cursor = new_cursor }

let barf_backward rt text cursor =
  match enclosing_paren rt text cursor with
  | None -> no_change text cursor
  | Some (open_pos, close_pos) ->
    (* Find the first sexp inside the list (after open_pos + 1) *)
    match sexp_forward rt text (open_pos + 1) with
    | None -> no_change text cursor
    | Some first_sexp_end ->
      if first_sexp_end >= close_pos then
        (* Only one element — don't barf *)
        no_change text cursor
      else begin
        (* Find where whitespace after first sexp ends *)
        let len = String.length text in
        let ws_end = ref first_sexp_end in
        while !ws_end < close_pos && (text.[!ws_end] = ' ' ||
              text.[!ws_end] = '\t' || text.[!ws_end] = '\n') do
          incr ws_end
        done;
        let new_open_pos = !ws_end in
        (* Build: text[0..open_pos-1] ++ text[open_pos+1..first_sexp_end-1] ++
                  " (" ++ text[new_open_pos..close_pos] ++ text[close_pos+1..] *)
        let before = String.sub text 0 open_pos in
        let barfed = String.sub text (open_pos + 1) (first_sexp_end - open_pos - 1) in
        let inside_kept = String.sub text new_open_pos (close_pos - new_open_pos + 1) in
        let after = String.sub text (close_pos + 1) (len - close_pos - 1) in
        let new_text = before ^ barfed ^ " (" ^ inside_kept ^ after in
        (* Adjust cursor *)
        let new_cursor =
          if cursor <= open_pos then cursor
          else if cursor <= first_sexp_end then
            (* Cursor was on barfed element — it moves left by 1 (open paren removed) *)
            cursor - 1
          else
            (* Cursor inside remaining list — adjust for repositioning *)
            let shift = (String.length before + String.length barfed + 2) - new_open_pos in
            cursor + shift
        in
        { text = new_text; cursor = new_cursor }
      end

let wrap_round rt text cursor =
  match sexp_forward rt text cursor with
  | None -> no_change text cursor
  | Some sexp_end ->
    let len = String.length text in
    (* Insert '(' before cursor and ')' after sexp_end *)
    let before = String.sub text 0 cursor in
    let middle = String.sub text cursor (sexp_end - cursor) in
    let after = String.sub text sexp_end (len - sexp_end) in
    { text = before ^ "(" ^ middle ^ ")" ^ after;
      cursor = cursor + 1 }

let splice rt text cursor =
  match enclosing_paren rt text cursor with
  | None -> no_change text cursor
  | Some (open_pos, close_pos) ->
    let len = String.length text in
    (* Remove the open and close parens *)
    let buf = Buffer.create len in
    for i = 0 to len - 1 do
      if i = open_pos || i = close_pos then ()
      else Buffer.add_char buf text.[i]
    done;
    let new_cursor =
      if cursor > close_pos then cursor - 2
      else if cursor > open_pos then cursor - 1
      else cursor
    in
    { text = Buffer.contents buf; cursor = new_cursor }

let raise_sexp rt text cursor =
  match enclosing_paren rt text cursor with
  | None -> no_change text cursor
  | Some (open_pos, close_pos) ->
    (* Skip whitespace at cursor to find the actual sexp start *)
    let len = String.length text in
    let sexp_start = ref cursor in
    while !sexp_start < close_pos &&
          (text.[!sexp_start] = ' ' || text.[!sexp_start] = '\t' ||
           text.[!sexp_start] = '\n') do
      incr sexp_start
    done;
    (* Find the sexp at the adjusted position *)
    match sexp_forward rt text !sexp_start with
    | None -> no_change text cursor
    | Some sexp_end ->
      let sexp_text = String.sub text !sexp_start (sexp_end - !sexp_start) in
      let before = String.sub text 0 open_pos in
      let after = String.sub text (close_pos + 1) (len - close_pos - 1) in
      { text = before ^ sexp_text ^ after;
        cursor = open_pos }

(* --- Indentation --- *)

let body_indent_forms = [
  "define"; "define-syntax"; "define-record-type"; "define-library";
  "define-values";
  "lambda";
  "let"; "let*"; "letrec"; "letrec*"; "let-values"; "let*-values";
  "let-syntax"; "letrec-syntax";
  "begin"; "when"; "unless"; "cond"; "case"; "do";
  "guard"; "syntax-rules"; "parameterize";
  "with-exception-handler"; "dynamic-wind";
  "call-with-current-continuation"; "call/cc";
  "call-with-values"; "call-with-port";
  "with-input-from-file"; "with-output-to-file";
]

let col_of_pos text pos =
  let rec scan i =
    if i < 0 then pos
    else if text.[i] = '\n' then pos - i - 1
    else scan (i - 1)
  in
  scan (pos - 1)

let row_of_pos text pos =
  let row = ref 0 in
  for i = 0 to min (pos - 1) (String.length text - 1) do
    if text.[i] = '\n' then incr row
  done;
  !row

let compute_indent rt text cursor =
  let tokens = Tokenizer.tokenize rt text in
  let tokens_before = List.filter (fun (tok : Tokenizer.token) ->
    tok.span.start < cursor
  ) tokens in
  (* Build paren stack to find innermost unclosed open paren *)
  let stack = Stack.create () in
  List.iter (fun (tok : Tokenizer.token) ->
    match tok.kind with
    | Paren_open -> Stack.push tok.span.start stack
    | Paren_close ->
      if not (Stack.is_empty stack) then ignore (Stack.pop stack)
    | _ -> ()
  ) tokens_before;
  if Stack.is_empty stack then 0
  else
    let open_pos = Stack.top stack in
    let open_col = col_of_pos text open_pos in
    (* Find top-level elements inside this form (depth 0 relative to open_pos) *)
    let depth = ref 0 in
    let elems = ref [] in
    List.iter (fun (tok : Tokenizer.token) ->
      if tok.span.start > open_pos && tok.span.start < cursor then
        match tok.kind with
        | Paren_open ->
          if !depth = 0 then elems := tok.span.start :: !elems;
          incr depth
        | Paren_close -> if !depth > 0 then decr depth
        | Whitespace -> ()
        | _ ->
          if !depth = 0 then elems := tok.span.start :: !elems
    ) tokens_before;
    let elems = List.rev !elems in
    match elems with
    | [] -> open_col + 2
    | head_pos :: rest ->
      let head_tok = List.find (fun (tok : Tokenizer.token) ->
        tok.span.start = head_pos
      ) tokens_before in
      let head_text = String.sub text head_tok.span.start
          (head_tok.span.stop - head_tok.span.start) in
      let is_body_form =
        (head_tok.kind = Tokenizer.Symbol || head_tok.kind = Tokenizer.Keyword)
        && List.mem head_text body_indent_forms
      in
      if is_body_form then
        open_col + 2
      else
        (* Align with first argument if on same line as head *)
        match rest with
        | second_pos :: _ ->
          if row_of_pos text head_pos = row_of_pos text second_pos then
            col_of_pos text second_pos
          else
            open_col + 2
        | [] -> open_col + 2
