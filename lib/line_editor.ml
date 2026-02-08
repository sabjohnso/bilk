type completeness_check = string -> bool

type config = {
  prompt : string;
  continuation_prompt : string;
  history_file : string option;
  max_history : int;
  is_complete : completeness_check option;
  highlight : (string -> int -> string) option;
  paredit : bool ref option;
  readtable : Readtable.t option;
}

type input_result =
  | Input of string
  | Eof
  | Interrupted

type t = {
  config : config;
  term : Terminal.t;
  history : History.t;
}

(* Internal editing state for one read_input call.
   The buffer holds the full multi-line text with \n separators.
   cursor is a byte offset into the text. *)
type edit_state = {
  content : Buffer.t;
  mutable cursor : int;
  mutable saved_input : string;  (* saved before history navigation *)
  mutable rendered_row : int;    (* cursor row at last render, for move-up *)
}

let create config =
  let term = Terminal.enter_raw Unix.stdin in
  let history = History.create ~max_length:config.max_history () in
  Option.iter (fun f -> History.load_from_file history f) config.history_file;
  { config; term; history }

let destroy t =
  Terminal.leave_raw t.term;
  Option.iter (fun f -> History.save_to_file t.history f) t.config.history_file

let history_add t entry = History.add t.history entry

let save_history t =
  Option.iter (fun f -> History.save_to_file t.history f) t.config.history_file

(* --- Multi-line helpers --- *)

let content_string st = Buffer.contents st.content

(* Split text into lines *)
let lines_of_text text =
  String.split_on_char '\n' text

(* Derive row (0-based) from cursor position *)
let cursor_row text cursor =
  let row = ref 0 in
  for i = 0 to min cursor (String.length text) - 1 do
    if text.[i] = '\n' then incr row
  done;
  !row

(* Derive column (0-based) from cursor position *)
let cursor_col text cursor =
  let col = ref 0 in
  for i = 0 to min cursor (String.length text) - 1 do
    if text.[i] = '\n' then col := 0
    else incr col
  done;
  !col

(* Get the number of lines *)
let num_lines text =
  let n = ref 1 in
  String.iter (fun c -> if c = '\n' then incr n) text;
  !n

(* Get the start byte offset of a given row (0-based) *)
let row_start text row =
  if row = 0 then 0
  else begin
    let pos = ref 0 in
    let count = ref 0 in
    let len = String.length text in
    while !pos < len && !count < row do
      if text.[!pos] = '\n' then incr count;
      incr pos
    done;
    !pos
  end

(* Get the length of a given row (0-based) *)
let row_length text row =
  let start = row_start text row in
  let len = String.length text in
  let end_pos = ref start in
  while !end_pos < len && text.[!end_pos] <> '\n' do
    incr end_pos
  done;
  !end_pos - start

(* Convert (row, col) to byte offset, clamping col to line length *)
let pos_of_row_col text row col =
  let start = row_start text row in
  let rlen = row_length text row in
  start + min col rlen

(* --- Paredit helpers --- *)

let paredit_active t =
  match t.config.paredit with
  | Some r -> !r
  | None -> false

let get_readtable t =
  match t.config.readtable with
  | Some rt -> rt
  | None -> Readtable.default

let apply_paredit_result st (result : Paredit.edit_result) =
  Buffer.clear st.content;
  Buffer.add_string st.content result.text;
  st.cursor <- result.cursor

(* --- Rendering --- *)

let effective_prompt t =
  if paredit_active t then
    (* Insert [paredit] before the trailing "> " *)
    let p = t.config.prompt in
    let len = String.length p in
    if len >= 2 && p.[len - 2] = '>' && p.[len - 1] = ' ' then
      String.sub p 0 (len - 2) ^ "[paredit]> "
    else
      p ^ "[paredit] "
  else
    t.config.prompt

let effective_continuation_prompt t =
  let prompt = effective_prompt t in
  let target_len = String.length prompt in
  let cont = t.config.continuation_prompt in
  let cont_len = String.length cont in
  if cont_len >= target_len then cont
  else cont ^ String.make (target_len - cont_len) ' '

let render t st =
  let text = content_string st in
  let lines = lines_of_text text in
  let nlines = List.length lines in
  let cur_row = cursor_row text st.cursor in
  let cur_col = cursor_col text st.cursor in
  let prompt = effective_prompt t in
  let cont_prompt = effective_continuation_prompt t in
  (* Move to start of editing area — use the cursor row from the
     previous render so we move up the correct number of lines even
     when the content changed (e.g. history navigation from a multi-line
     entry to a shorter one). *)
  if st.rendered_row > 0 then
    Terminal.write_string t.term (Printf.sprintf "\x1b[%dA" st.rendered_row);
  Terminal.write_string t.term "\r";
  (* Highlight the full text if a highlighter is configured *)
  let highlighted_lines = match t.config.highlight with
    | Some hl ->
      let highlighted = hl text st.cursor in
      lines_of_text highlighted
    | None -> lines
  in
  (* Write each line with appropriate prompt *)
  List.iteri (fun i _line ->
    let line_prompt = if i = 0 then prompt else cont_prompt in
    Terminal.write_string t.term line_prompt;
    let display_line =
      if i < List.length highlighted_lines then List.nth highlighted_lines i
      else ""
    in
    Terminal.write_string t.term display_line;
    Terminal.clear_to_end t.term;
    if i < nlines - 1 then
      Terminal.write_string t.term "\r\n"
  ) lines;
  (* Clear any residual lines below *)
  Terminal.clear_below t.term;
  (* Reposition cursor using raw text coordinates (ignoring ANSI escapes) *)
  let last_line = nlines - 1 in
  let lines_up = last_line - cur_row in
  if lines_up > 0 then
    Terminal.write_string t.term (Printf.sprintf "\x1b[%dA" lines_up);
  let prompt_len = if cur_row = 0 then String.length prompt
                   else String.length cont_prompt in
  Terminal.move_to_column t.term (prompt_len + cur_col + 1);
  st.rendered_row <- cur_row

(* --- Word movement helpers --- *)

let is_word_char c =
  match c with
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-'
  | '!' | '?' | '+' | '*' | '/' | '<' | '>' | '=' -> true
  | _ -> false

let word_backward text pos =
  let p = ref (pos - 1) in
  while !p >= 0 && not (is_word_char text.[!p]) do decr p done;
  while !p >= 0 && is_word_char text.[!p] do decr p done;
  !p + 1

let word_forward text pos =
  let len = String.length text in
  let p = ref pos in
  while !p < len && is_word_char text.[!p] do incr p done;
  while !p < len && not (is_word_char text.[!p]) do incr p done;
  !p

(* --- Editing operations --- *)

let insert_string st s =
  let text = content_string st in
  let len = String.length text in
  let before = String.sub text 0 st.cursor in
  let after = String.sub text st.cursor (len - st.cursor) in
  let slen = String.length s in
  Buffer.clear st.content;
  Buffer.add_string st.content before;
  Buffer.add_string st.content s;
  Buffer.add_string st.content after;
  st.cursor <- st.cursor + slen

let insert_char st c =
  insert_string st (String.make 1 c)

let insert_newline st =
  insert_string st "\n"

let insert_newline_indented t st =
  let text = content_string st in
  match t.config.readtable with
  | Some rt ->
    let indent = Paredit.compute_indent rt text st.cursor in
    insert_string st ("\n" ^ String.make indent ' ')
  | None -> insert_newline st

let delete_backward st =
  if st.cursor > 0 then begin
    let text = content_string st in
    let len = String.length text in
    let before = String.sub text 0 (st.cursor - 1) in
    let after = String.sub text st.cursor (len - st.cursor) in
    Buffer.clear st.content;
    Buffer.add_string st.content (before ^ after);
    st.cursor <- st.cursor - 1
  end

let delete_forward st =
  let text = content_string st in
  let len = String.length text in
  if st.cursor < len then begin
    let before = String.sub text 0 st.cursor in
    let after = String.sub text (st.cursor + 1) (len - st.cursor - 1) in
    Buffer.clear st.content;
    Buffer.add_string st.content (before ^ after)
  end

let kill_to_end st =
  let text = content_string st in
  let len = String.length text in
  (* Kill to end of current line (not end of all text) *)
  let end_pos = ref st.cursor in
  while !end_pos < len && text.[!end_pos] <> '\n' do incr end_pos done;
  if !end_pos = st.cursor && !end_pos < len && text.[!end_pos] = '\n' then
    (* At end of line with newline following — delete the newline (join lines) *)
    incr end_pos;
  let before = String.sub text 0 st.cursor in
  let after = String.sub text !end_pos (len - !end_pos) in
  Buffer.clear st.content;
  Buffer.add_string st.content (before ^ after)

let kill_to_start st =
  let text = content_string st in
  let len = String.length text in
  (* Kill to start of current line *)
  let start_pos = ref (st.cursor - 1) in
  while !start_pos >= 0 && text.[!start_pos] <> '\n' do decr start_pos done;
  let line_start = !start_pos + 1 in
  let before = String.sub text 0 line_start in
  let after = String.sub text st.cursor (len - st.cursor) in
  Buffer.clear st.content;
  Buffer.add_string st.content (before ^ after);
  st.cursor <- line_start

let kill_word_backward st =
  let text = content_string st in
  let new_pos = word_backward text st.cursor in
  let len = String.length text in
  let before = String.sub text 0 new_pos in
  let after = String.sub text st.cursor (len - st.cursor) in
  Buffer.clear st.content;
  Buffer.add_string st.content (before ^ after);
  st.cursor <- new_pos

let set_content st text =
  Buffer.clear st.content;
  Buffer.add_string st.content text;
  st.cursor <- String.length text

(* --- Main read loop --- *)

let read_input t =
  let st = {
    content = Buffer.create 80;
    cursor = 0;
    saved_input = "";
    rendered_row = 0;
  } in
  History.reset_nav t.history;
  render t st;
  let rec loop () =
    let key = Terminal.read_key t.term in
    match key with
    | Terminal.Enter ->
      let text = content_string st in
      let cursor_at_end =
        let len = String.length text in
        let p = ref st.cursor in
        while !p < len && (text.[!p] = ' ' || text.[!p] = '\t' || text.[!p] = '\n') do
          incr p
        done;
        !p >= len
      in
      let should_submit = match t.config.is_complete with
        | None -> cursor_at_end
        | Some f ->
          (* Empty input submits (allows blank lines).
             Non-empty input only submits if cursor is at end
             (modulo trailing whitespace) AND expression is complete. *)
          text = "" || (cursor_at_end && f text)
      in
      if should_submit then begin
        (* Move cursor to end and print newline *)
        let nlines = num_lines text in
        let cur_row = cursor_row text st.cursor in
        let remaining = nlines - 1 - cur_row in
        if remaining > 0 then
          Terminal.write_string t.term (Printf.sprintf "\x1b[%dB" remaining);
        Terminal.write_string t.term "\r\n";
        Input text
      end else begin
        insert_newline_indented t st;
        render t st;
        loop ()
      end

    | Terminal.Alt_enter ->
      insert_newline_indented t st;
      render t st;
      loop ()

    | Terminal.Ctrl_c ->
      (* Move to end and print newline *)
      let text = content_string st in
      let nlines = num_lines text in
      let cur_row = cursor_row text st.cursor in
      let remaining = nlines - 1 - cur_row in
      if remaining > 0 then
        Terminal.write_string t.term (Printf.sprintf "\x1b[%dB" remaining);
      Terminal.write_string t.term "\r\n";
      Interrupted

    | Terminal.Ctrl_d ->
      let text = content_string st in
      if String.length text = 0 then begin
        Terminal.write_string t.term "\r\n";
        Eof
      end else begin
        delete_forward st;
        render t st;
        loop ()
      end

    | Terminal.Char c ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        match c with
        | '(' | '[' ->
          apply_paredit_result st (Paredit.insert_open_paren text st.cursor)
        | ')' | ']' ->
          apply_paredit_result st (Paredit.insert_close_paren rt text st.cursor)
        | '"' ->
          apply_paredit_result st (Paredit.insert_double_quote rt text st.cursor)
        | _ -> insert_char st c
      end else
        insert_char st c;
      render t st;
      loop ()

    | Terminal.Backspace ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.backspace_paredit rt text st.cursor)
      end else
        delete_backward st;
      render t st;
      loop ()

    | Terminal.Alt_backspace ->
      kill_word_backward st;
      render t st;
      loop ()

    | Terminal.Delete ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.delete_paredit rt text st.cursor)
      end else
        delete_forward st;
      render t st;
      loop ()

    | Terminal.Left | Terminal.Ctrl_b ->
      if st.cursor > 0 then st.cursor <- st.cursor - 1;
      render t st;
      loop ()

    | Terminal.Right | Terminal.Ctrl_f ->
      let len = Buffer.length st.content in
      if st.cursor < len then st.cursor <- st.cursor + 1;
      render t st;
      loop ()

    | Terminal.Home | Terminal.Ctrl_a ->
      (* Move to start of current line *)
      let text = content_string st in
      let p = ref (st.cursor - 1) in
      while !p >= 0 && text.[!p] <> '\n' do decr p done;
      st.cursor <- !p + 1;
      render t st;
      loop ()

    | Terminal.End_key | Terminal.Ctrl_e ->
      (* Move to end of current line *)
      let text = content_string st in
      let len = String.length text in
      let p = ref st.cursor in
      while !p < len && text.[!p] <> '\n' do incr p done;
      st.cursor <- !p;
      render t st;
      loop ()

    | Terminal.Alt_left ->
      let text = content_string st in
      st.cursor <- word_backward text st.cursor;
      render t st;
      loop ()

    | Terminal.Alt_right ->
      let text = content_string st in
      st.cursor <- word_forward text st.cursor;
      render t st;
      loop ()

    | Terminal.Ctrl_k ->
      kill_to_end st;
      render t st;
      loop ()

    | Terminal.Ctrl_u ->
      kill_to_start st;
      render t st;
      loop ()

    | Terminal.Ctrl_w ->
      kill_word_backward st;
      render t st;
      loop ()

    | Terminal.Up | Terminal.Ctrl_p ->
      let text = content_string st in
      let row = cursor_row text st.cursor in
      if row > 0 then begin
        (* Move up within multi-line input *)
        let col = cursor_col text st.cursor in
        st.cursor <- pos_of_row_col text (row - 1) col;
        render t st
      end else begin
        (* On first line — navigate history, with prefix filtering *)
        let prefix = String.sub text 0 st.cursor in
        let hist_fn = if prefix = "" then fun () -> History.prev t.history
                      else fun () -> History.prev_matching t.history prefix in
        (match hist_fn () with
         | Some entry ->
           if st.saved_input = "" && text <> "" then
             st.saved_input <- text;
           set_content st entry;
           (* Keep cursor at end of prefix for prefix search *)
           if prefix <> "" then
             st.cursor <- String.length prefix
         | None -> ());
        render t st
      end;
      loop ()

    | Terminal.Down | Terminal.Ctrl_n ->
      let text = content_string st in
      let row = cursor_row text st.cursor in
      let nlines = num_lines text in
      if row < nlines - 1 then begin
        (* Move down within multi-line input *)
        let col = cursor_col text st.cursor in
        st.cursor <- pos_of_row_col text (row + 1) col;
        render t st
      end else begin
        (* On last line — navigate history, with prefix filtering *)
        let prefix = String.sub text 0 st.cursor in
        let hist_fn = if prefix = "" then fun () -> History.next t.history
                      else fun () -> History.next_matching t.history prefix in
        (match hist_fn () with
         | Some entry ->
           set_content st entry;
           if prefix <> "" then
             st.cursor <- String.length prefix
         | None ->
           set_content st st.saved_input;
           st.saved_input <- "";
           if prefix <> "" then
             st.cursor <- min (String.length prefix) (String.length (content_string st)));
        render t st
      end;
      loop ()

    | Terminal.Ctrl_l ->
      Terminal.write_string t.term "\x1b[2J\x1b[H";
      render t st;
      loop ()

    | Terminal.Ctrl_right ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.slurp_forward rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Ctrl_left ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.barf_forward rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Alt_open_paren | Terminal.Alt_9 ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.wrap_round rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Alt_s ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.splice rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Alt_r ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.raise_sexp rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Tab | Terminal.Unknown ->
      loop ()
  in
  loop ()
