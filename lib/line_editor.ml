type completeness_check = string -> bool

type completion_result =
  | No_completions
  | Single of string * int
  | Multiple of string * int * string
  | Menu of {
      text : string;
      cursor : int;
      candidates : string list;
      start : int;
    }

type config = {
  prompt : string;
  continuation_prompt : string;
  history_file : string option;
  max_history : int;
  is_complete : completeness_check option;
  highlight : (string -> int -> string) option;
  paredit : bool ref option;
  readtable : Readtable.t option;
  complete : (string -> int -> width:int -> completion_result) option;
  on_idle : (unit -> unit) option;
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

(* Completion menu state — active while user is navigating candidates. *)
type menu_state = {
  candidates : string list;
  mutable selected : int;
  start : int;              (* byte offset in text where prefix begins *)
  original_text : string;   (* text before common prefix was applied *)
  original_cursor : int;    (* cursor before common prefix was applied *)
  width : int;              (* terminal width, cached at menu open *)
}

(* Internal editing state for one read_input call.
   The buffer holds the full multi-line text with \n separators.
   cursor is a byte offset into the text. *)
type edit_state = {
  content : Buffer.t;
  mutable cursor : int;
  mutable saved_input : string;  (* saved before history navigation *)
  mutable rendered_row : int;    (* cursor row at last render, for move-up *)
  mutable menu : menu_state option;
  term_width : int;              (* terminal width, queried once at start *)
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

(* Visible width of a string, ignoring ANSI escape sequences *)
let visible_length s =
  let len = String.length s in
  let vis = ref 0 in
  let i = ref 0 in
  while !i < len do
    if s.[!i] = '\x1b' then begin
      (* Skip ESC [ ... final_byte *)
      incr i;
      if !i < len && s.[!i] = '[' then begin
        incr i;
        while !i < len && not (s.[!i] >= '\x40' && s.[!i] <= '\x7e') do incr i done;
        if !i < len then incr i  (* skip the final byte *)
      end
    end else begin
      incr vis;
      incr i
    end
  done;
  !vis

(* Byte offset in [s] of the [n]th visible character (0-indexed),
   skipping ANSI CSI sequences.  Returns [String.length s] if there
   are fewer than [n] visible characters. *)
let visible_byte_offset s n =
  let len = String.length s in
  let vis = ref 0 in
  let i = ref 0 in
  while !i < len && !vis < n do
    if s.[!i] = '\x1b' then begin
      incr i;
      if !i < len && s.[!i] = '[' then begin
        incr i;
        while !i < len && not (s.[!i] >= '\x40' && s.[!i] <= '\x7e') do incr i done;
        if !i < len then incr i
      end
    end else begin
      incr vis;
      incr i
    end
  done;
  !i

(* Strip all ANSI CSI sequences from [s], returning only visible characters. *)
let strip_ansi s =
  let len = String.length s in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if s.[!i] = '\x1b' then begin
      incr i;
      if !i < len && s.[!i] = '[' then begin
        incr i;
        while !i < len && not (s.[!i] >= '\x40' && s.[!i] <= '\x7e') do incr i done;
        if !i < len then incr i
      end
    end else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let effective_prompt t =
  if paredit_active t then
    let p = t.config.prompt in
    let visible = strip_ansi p in
    let vlen = String.length visible in
    if vlen >= 2 && visible.[vlen - 2] = '>' && visible.[vlen - 1] = ' ' then
      (* Splice "[paredit]" just before the '>' in the original string *)
      let gt_byte = visible_byte_offset p (vlen - 2) in
      String.sub p 0 gt_byte ^ "[paredit]" ^
        String.sub p gt_byte (String.length p - gt_byte)
    else
      p ^ "[paredit] "
  else
    t.config.prompt

let effective_continuation_prompt t =
  let prompt = effective_prompt t in
  let target_len = visible_length prompt in
  let cont = t.config.continuation_prompt in
  let cont_len = visible_length cont in
  if cont_len >= target_len then cont
  else cont ^ String.make (target_len - cont_len) ' '

(* Render the input lines to the terminal. After this call, the terminal
   cursor is at the end of the last input line.
   Returns (nlines, cur_row, cur_col, prompt_vlen, cont_prompt_vlen). *)
let render_input t st =
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
  (nlines, cur_row, cur_col,
   visible_length prompt, visible_length cont_prompt)

(* Reposition the terminal cursor to the input cursor position and
   clear any residual content below the last input line. *)
let finish_render t st nlines cur_row cur_col prompt_vlen cont_prompt_vlen =
  Terminal.clear_below t.term;
  let last_line = nlines - 1 in
  let lines_up = last_line - cur_row in
  if lines_up > 0 then
    Terminal.write_string t.term (Printf.sprintf "\x1b[%dA" lines_up);
  let prompt_len = if cur_row = 0 then prompt_vlen else cont_prompt_vlen in
  Terminal.move_to_column t.term (prompt_len + cur_col + 1);
  st.rendered_row <- cur_row

let render t st =
  let (nlines, cur_row, cur_col, pv, cpv) = render_input t st in
  finish_render t st nlines cur_row cur_col pv cpv

(* Render input lines plus the completion menu below them.
   Used when the menu first opens (input text may have changed). *)
let render_menu t st menu =
  let (nlines, cur_row, cur_col, pv, cpv) = render_input t st in
  (* Move below the last input line *)
  Terminal.write_string t.term "\r\n";
  (* Write highlighted candidates *)
  let display = Completion.format_columns_highlighted
    ~width:menu.width ~highlight:menu.selected menu.candidates in
  Terminal.write_string t.term display;
  Terminal.clear_below t.term;
  (* Count menu display lines (1 + number of newlines in display) *)
  let menu_newlines =
    String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc)
      0 display
  in
  (* Move back up to cursor position in input:
     from end of menu display, go up past menu lines + remaining input lines *)
  let total_up = menu_newlines + nlines - cur_row in
  if total_up > 0 then
    Terminal.write_string t.term (Printf.sprintf "\x1b[%dA" total_up);
  let prompt_len = if cur_row = 0 then pv else cpv in
  Terminal.move_to_column t.term (prompt_len + cur_col + 1);
  st.rendered_row <- cur_row

(* Redraw only the menu display (for Tab/Shift-Tab cycling).
   Avoids re-rendering input and avoids querying terminal size.
   Moves cursor from its current position down to the menu area,
   rewrites the highlighted candidates, and moves back. *)
let cycle_menu t st menu =
  let text = content_string st in
  let nlines = num_lines text in
  let cur_row = cursor_row text st.cursor in
  let cur_col = cursor_col text st.cursor in
  let prompt = effective_prompt t in
  let cont_prompt = effective_continuation_prompt t in
  (* Move from cursor position down to the menu area *)
  let lines_down = nlines - cur_row in
  if lines_down > 0 then
    Terminal.write_string t.term (Printf.sprintf "\x1b[%dB" lines_down);
  Terminal.write_string t.term "\r";
  (* Redraw the menu with updated highlight *)
  let display = Completion.format_columns_highlighted
    ~width:menu.width ~highlight:menu.selected menu.candidates in
  Terminal.write_string t.term display;
  Terminal.clear_below t.term;
  (* Move back up to cursor position *)
  let menu_newlines =
    String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc)
      0 display
  in
  let total_up = menu_newlines + nlines - cur_row in
  if total_up > 0 then
    Terminal.write_string t.term (Printf.sprintf "\x1b[%dA" total_up);
  let prompt_len = if cur_row = 0 then visible_length prompt
                   else visible_length cont_prompt in
  Terminal.move_to_column t.term (prompt_len + cur_col + 1)

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

(* --- Completion helper --- *)

let handle_completion t st =
  match t.config.complete with
  | None -> ()
  | Some complete_fn ->
    let text = content_string st in
    let width = st.term_width in
    begin match complete_fn text st.cursor ~width with
    | No_completions ->
      Terminal.write_string t.term "\x07"  (* bell *)
    | Single (new_text, new_cursor) ->
      Buffer.clear st.content;
      Buffer.add_string st.content new_text;
      st.cursor <- new_cursor;
      render t st
    | Multiple (new_text, new_cursor, display) ->
      Buffer.clear st.content;
      Buffer.add_string st.content new_text;
      st.cursor <- new_cursor;
      (* Show candidates below the input *)
      let text' = content_string st in
      let nlines = num_lines text' in
      let cur_row = cursor_row text' st.cursor in
      let remaining = nlines - 1 - cur_row in
      if remaining > 0 then
        Terminal.write_string t.term (Printf.sprintf "\x1b[%dB" remaining);
      Terminal.write_string t.term "\r\n";
      Terminal.write_string t.term display;
      Terminal.write_string t.term "\r\n";
      (* Re-render the input *)
      st.rendered_row <- 0;
      render t st
    | Menu { text = new_text; cursor = new_cursor; candidates = [_]; _ } ->
      (* Single match — auto-complete without showing menu *)
      Buffer.clear st.content;
      Buffer.add_string st.content new_text;
      st.cursor <- new_cursor;
      render t st
    | Menu { text = new_text; cursor = new_cursor; candidates; start } ->
      let original_text = content_string st in
      let original_cursor = st.cursor in
      Buffer.clear st.content;
      Buffer.add_string st.content new_text;
      st.cursor <- new_cursor;
      let m = {
        candidates;
        selected = 0;
        start;
        original_text;
        original_cursor;
        width;
      } in
      st.menu <- Some m;
      render_menu t st m
    end

(* Silently try to reopen the completion menu after typing/deleting.
   Unlike handle_completion: no bell on no matches, and single matches
   are kept in the menu (not auto-completed) so the user can confirm
   with Enter/Tab or cancel with Escape. *)
let try_reopen_menu t st =
  match t.config.complete with
  | None -> ()
  | Some complete_fn ->
    let text = content_string st in
    let cur = st.cursor in
    let width = st.term_width in
    begin match complete_fn text cur ~width with
    | No_completions | Single _ | Multiple _ -> ()
    | Menu { text = new_text; cursor = new_cursor; candidates; start } ->
      let original_text = text in
      let original_cursor = cur in
      Buffer.clear st.content;
      Buffer.add_string st.content new_text;
      st.cursor <- new_cursor;
      let m = {
        candidates;
        selected = 0;
        start;
        original_text;
        original_cursor;
        width;
      } in
      st.menu <- Some m;
      render_menu t st m
    end

(* --- Main read loop --- *)

let read_input t =
  (* If previous output left the cursor mid-line, move to a fresh line
     so the prompt doesn't overwrite output from display/write/etc. *)
  let (_row, col) = Terminal.get_cursor_pos t.term in
  if col > 1 then
    Terminal.write_string t.term "\r\n";
  (* Query terminal size once before rendering. get_terminal_size uses
     cursor-movement escape sequences that can displace the cursor, so we
     must not call it during the editing loop. *)
  let (_rows, cols) = Terminal.get_terminal_size t.term in
  let st = {
    content = Buffer.create 80;
    cursor = 0;
    saved_input = "";
    rendered_row = 0;
    menu = None;
    term_width = max 40 cols;
  } in
  History.reset_nav t.history;
  render t st;
  let read_key () =
    match t.config.on_idle with
    | Some idle -> Terminal.read_key_with_idle t.term ~idle
    | None -> Terminal.read_key t.term
  in
  let insert_char_maybe_paredit c =
    if paredit_active t then begin
      let text = content_string st in
      let rt = get_readtable t in
      match c with
      | '(' | '[' ->
        apply_paredit_result st (Paredit.insert_open_paren text st.cursor)
      | ')' | ']' ->
        let result = Paredit.insert_close_paren rt text st.cursor in
        if result.text = text && result.cursor = st.cursor then
          Terminal.write_string t.term "\x07"  (* bell *)
        else
          apply_paredit_result st result
      | '"' ->
        apply_paredit_result st (Paredit.insert_double_quote rt text st.cursor)
      | _ -> insert_char st c
    end else
      insert_char st c
  in
  let delete_backward_maybe_paredit () =
    if paredit_active t then begin
      let text = content_string st in
      let rt = get_readtable t in
      apply_paredit_result st (Paredit.backspace_paredit rt text st.cursor)
    end else
      delete_backward st
  in
  let rec loop () =
    let key = read_key () in
    match st.menu with
    | Some menu -> handle_menu_key key menu
    | None -> handle_normal_key key

  and handle_menu_key key menu =
    match key with
    | Terminal.Tab ->
      menu.selected <- (menu.selected + 1) mod (List.length menu.candidates);
      cycle_menu t st menu;
      loop ()
    | Terminal.Shift_tab ->
      let n = List.length menu.candidates in
      menu.selected <- (menu.selected + n - 1) mod n;
      cycle_menu t st menu;
      loop ()
    | Terminal.Enter ->
      (* Accept: replace prefix→cursor with selected candidate *)
      let candidate = List.nth menu.candidates menu.selected in
      let text = content_string st in
      let before = String.sub text 0 menu.start in
      let after = String.sub text st.cursor (String.length text - st.cursor) in
      Buffer.clear st.content;
      Buffer.add_string st.content (before ^ candidate ^ after);
      st.cursor <- menu.start + String.length candidate;
      st.menu <- None;
      render t st;
      loop ()
    | Terminal.Escape ->
      (* Cancel: restore original text/cursor *)
      Buffer.clear st.content;
      Buffer.add_string st.content menu.original_text;
      st.cursor <- menu.original_cursor;
      st.menu <- None;
      render t st;
      loop ()
    | Terminal.Char c ->
      (* Dismiss menu, insert char, try to reopen *)
      st.menu <- None;
      insert_char_maybe_paredit c;
      render t st;
      try_reopen_menu t st;
      loop ()
    | Terminal.Backspace ->
      (* Dismiss menu, delete backward, try to reopen *)
      st.menu <- None;
      delete_backward_maybe_paredit ();
      render t st;
      try_reopen_menu t st;
      loop ()
    | Terminal.Ctrl_c | Terminal.Ctrl_d ->
      st.menu <- None;
      render t st;
      handle_normal_key key
    | _ ->
      (* Dismiss menu, delegate to normal handler *)
      st.menu <- None;
      render t st;
      handle_normal_key key

  and handle_normal_key key =
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
          text = "" || (cursor_at_end && f text)
      in
      if should_submit then begin
        st.cursor <- String.length text + 1;
        render t st;
        Terminal.write_string t.term "\r\n";
        Input text
      end else begin
        insert_newline st;
        if paredit_active t then begin
          let text = content_string st in
          let rt = get_readtable t in
          let row = cursor_row text st.cursor in
          apply_paredit_result st (Paredit.indent_from rt text st.cursor row)
        end;
        render t st;
        loop ()
      end

    | Terminal.Alt_enter ->
      insert_newline st;
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        let row = cursor_row text st.cursor in
        apply_paredit_result st (Paredit.indent_from rt text st.cursor row)
      end;
      render t st;
      loop ()

    | Terminal.Ctrl_c ->
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
      insert_char_maybe_paredit c;
      render t st;
      loop ()

    | Terminal.Backspace ->
      delete_backward_maybe_paredit ();
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
      let text = content_string st in
      let p = ref (st.cursor - 1) in
      while !p >= 0 && text.[!p] <> '\n' do decr p done;
      st.cursor <- !p + 1;
      render t st;
      loop ()

    | Terminal.End_key | Terminal.Ctrl_e ->
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
        let col = cursor_col text st.cursor in
        st.cursor <- pos_of_row_col text (row - 1) col;
        render t st
      end else begin
        let prefix = String.sub text 0 st.cursor in
        let hist_fn = if prefix = "" then fun () -> History.prev t.history
                      else fun () -> History.prev_matching t.history prefix in
        (match hist_fn () with
         | Some entry ->
           if st.saved_input = "" && text <> "" then
             st.saved_input <- text;
           set_content st entry;
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
        let col = cursor_col text st.cursor in
        st.cursor <- pos_of_row_col text (row + 1) col;
        render t st
      end else begin
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

    | Terminal.Tab ->
      if paredit_active t
         && not (Completion.should_complete_at (content_string st) st.cursor)
      then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.indent_line rt text st.cursor);
        render t st
      end else
        handle_completion t st;
      loop ()

    | Terminal.Shift_tab ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.indent_all rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Alt_ctrl_f ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.navigate_forward rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Alt_ctrl_b ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.navigate_backward rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Alt_ctrl_d ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.forward_down rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Alt_ctrl_u ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.backward_up rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Alt_ctrl_n ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.forward_up rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Alt_ctrl_p ->
      if paredit_active t then begin
        let text = content_string st in
        let rt = get_readtable t in
        apply_paredit_result st (Paredit.backward_down rt text st.cursor);
        render t st
      end;
      loop ()

    | Terminal.Escape ->
      loop ()

    | Terminal.Unknown ->
      loop ()
  in
  loop ()
