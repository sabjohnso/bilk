type context =
  | Identifier of string * int
  | String_literal of string * int
  | Repl_command_arg of string * string * int
  | Import_library of string list * string * int
  | No_context

(* Check if cursor is inside a string literal by scanning for unmatched
   opening quote. Returns Some (content_start) if inside a string. *)
let find_enclosing_string text cursor =
  let in_string = ref false in
  let string_start = ref 0 in
  let i = ref 0 in
  while !i < cursor do
    let c = text.[!i] in
    if !in_string then begin
      if c = '\\' && !i + 1 < cursor then
        i := !i + 1  (* skip escaped char *)
      else if c = '"' then
        in_string := false
    end else begin
      if c = '"' then begin
        in_string := true;
        string_start := !i + 1
      end
    end;
    i := !i + 1
  done;
  if !in_string then Some !string_start
  else None

(* Try to detect a REPL command argument context.
   Pattern: ",command arg_prefix" where command starts at position 0. *)
let detect_repl_command text cursor =
  if String.length text = 0 || text.[0] <> ',' then None
  else
    (* Find the end of the command name *)
    let cmd_end = ref 1 in
    while !cmd_end < String.length text
          && text.[!cmd_end] <> ' ' && text.[!cmd_end] <> '\t' do
      incr cmd_end
    done;
    let cmd = String.sub text 0 !cmd_end in
    (* Skip whitespace after command *)
    let arg_start = ref !cmd_end in
    while !arg_start < String.length text
          && (text.[!arg_start] = ' ' || text.[!arg_start] = '\t') do
      incr arg_start
    done;
    if !arg_start > !cmd_end && cursor >= !arg_start then begin
      let arg_prefix = String.sub text !arg_start (cursor - !arg_start) in
      Some (cmd, arg_prefix, !arg_start)
    end else
      None

(* Try to detect an import library context.
   Pattern: "(import (part1 part2 ... prefix" *)
let detect_import_library rt text cursor =
  let tokens = Tokenizer.tokenize rt text in
  (* Find the import keyword *)
  let has_import = ref false in
  let import_paren_pos = ref 0 in
  let prev_was_paren = ref false in
  let prev_paren_pos = ref 0 in
  List.iter (fun (tok : Tokenizer.token) ->
    if tok.kind = Tokenizer.Paren_open then begin
      prev_was_paren := true;
      prev_paren_pos := tok.span.start
    end else begin
      if !prev_was_paren && tok.kind = Tokenizer.Keyword
         && tok.span.stop <= cursor then begin
        let name = String.sub text tok.span.start
            (tok.span.stop - tok.span.start) in
        if name = "import" then begin
          has_import := true;
          import_paren_pos := !prev_paren_pos
        end
      end;
      if tok.kind <> Tokenizer.Whitespace then
        prev_was_paren := false
    end
  ) tokens;
  if not !has_import then None
  else begin
    (* Find the innermost open paren after import that contains the cursor *)
    let lib_paren = ref (-1) in
    List.iter (fun (tok : Tokenizer.token) ->
      if tok.kind = Tokenizer.Paren_open
         && tok.span.start > !import_paren_pos
         && tok.span.stop <= cursor then
        lib_paren := tok.span.start
    ) tokens;
    if !lib_paren < 0 then None
    else begin
      (* Collect symbols after the lib paren up to cursor *)
      let parts = ref [] in
      let last_sym_start = ref (!lib_paren + 1) in
      List.iter (fun (tok : Tokenizer.token) ->
        if tok.span.start > !lib_paren && tok.span.stop <= cursor then begin
          if tok.kind = Tokenizer.Symbol || tok.kind = Tokenizer.Keyword then
            parts := String.sub text tok.span.start
                (tok.span.stop - tok.span.start) :: !parts
        end;
        if tok.span.start > !lib_paren && tok.span.start < cursor then
          last_sym_start := tok.span.start
      ) tokens;
      let parts = List.rev !parts in
      (* The last part might be a partial prefix at cursor *)
      let (prefix, prefix_start) =
        Completion.extract_prefix text cursor
      in
      if prefix_start >= !lib_paren + 1 then begin
        (* Parts before the current prefix *)
        let resolved = List.filter (fun p -> p <> prefix) parts in
        Some (resolved, prefix, prefix_start)
      end else
        None
    end
  end

let detect rt text cursor =
  (* 1. Check if inside a string literal *)
  match find_enclosing_string text cursor with
  | Some content_start ->
    let content = String.sub text content_start (cursor - content_start) in
    String_literal (content, content_start)
  | None ->
    (* 2. Check for REPL command argument *)
    match detect_repl_command text cursor with
    | Some (cmd, arg, off) -> Repl_command_arg (cmd, arg, off)
    | None ->
      (* 3. Check for import library *)
      match detect_import_library rt text cursor with
      | Some (parts, prefix, off) -> Import_library (parts, prefix, off)
      | None ->
        (* 4. Fall back to identifier *)
        let (prefix, start) = Completion.extract_prefix text cursor in
        if prefix <> "" then Identifier (prefix, start)
        else No_context
