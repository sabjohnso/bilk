type style = {
  fg : int option;
  bg : int option;
  bold : bool;
  italic : bool;
  underline : bool;
}

type theme = {
  name : string;
  paren : style array;
  string_style : style;
  number_style : style;
  keyword_style : style;
  comment_style : style;
  symbol_style : style;
  boolean_style : style;
  char_style : style;
  quote_style : style;
  error_style : style;
  default_style : style;
  defn_name_style : style;
  param_style : style;
}

let default_style = {
  fg = None; bg = None;
  bold = false; italic = false; underline = false;
}

let fg color = { default_style with fg = Some color }
let fg_bold color = { default_style with fg = Some color; bold = true }

(* --- ANSI escape helpers --- *)

let style_to_ansi s =
  let parts = ref [] in
  Option.iter (fun c -> parts := Printf.sprintf "38;5;%d" c :: !parts) s.fg;
  Option.iter (fun c -> parts := Printf.sprintf "48;5;%d" c :: !parts) s.bg;
  if s.bold then parts := "1" :: !parts;
  if s.italic then parts := "3" :: !parts;
  if s.underline then parts := "4" :: !parts;
  match !parts with
  | [] -> ""
  | ps -> Printf.sprintf "\x1b[%sm" (String.concat ";" (List.rev ps))

let ansi_reset = "\x1b[0m"

let strip_ansi text =
  let buf = Buffer.create (String.length text) in
  let len = String.length text in
  let i = ref 0 in
  while !i < len do
    if text.[!i] = '\x1b' then begin
      incr i;
      (* Skip to final byte of escape sequence *)
      if !i < len && text.[!i] = '[' then begin
        incr i;
        while !i < len && not (text.[!i] >= '@' && text.[!i] <= '~') do
          incr i
        done;
        if !i < len then incr i  (* skip final byte *)
      end
    end else begin
      Buffer.add_char buf text.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* --- Built-in themes --- *)

let dark_theme = {
  name = "dark";
  paren = [| default_style |];
  string_style = fg 114;        (* green *)
  number_style = fg 208;        (* orange *)
  keyword_style = fg_bold 69;   (* blue, bold *)
  comment_style = { default_style with fg = Some 245; italic = true };
  symbol_style = fg 252;        (* light gray *)
  boolean_style = fg 141;       (* purple *)
  char_style = fg 141;          (* purple *)
  quote_style = fg 208;         (* orange *)
  error_style = fg_bold 196;    (* red, bold *)
  default_style;
  defn_name_style = fg_bold 81; (* bright cyan, bold *)
  param_style = fg 180;         (* tan/wheat *)
}

let light_theme = {
  name = "light";
  paren = [| default_style |];
  string_style = fg 28;         (* dark green *)
  number_style = fg 166;        (* dark orange *)
  keyword_style = fg_bold 25;   (* dark blue, bold *)
  comment_style = { default_style with fg = Some 243; italic = true };
  symbol_style = fg 235;        (* dark gray *)
  boolean_style = fg 91;        (* dark purple *)
  char_style = fg 91;           (* dark purple *)
  quote_style = fg 166;         (* dark orange *)
  error_style = fg_bold 160;    (* dark red, bold *)
  default_style;
  defn_name_style = fg_bold 24; (* dark teal, bold *)
  param_style = fg 130;         (* brown/amber *)
}

(* --- Matching paren finder --- *)

let find_matching_paren tokens cursor_pos =
  (* Check for open paren starting at cursor (cursor right before it) *)
  let open_at_cursor = List.find_opt (fun (t : Tokenizer.token) ->
    t.kind = Tokenizer.Paren_open && t.span.start = cursor_pos
  ) tokens in
  (* Check for close paren ending at cursor (cursor right after it) *)
  let close_before_cursor = List.find_opt (fun (t : Tokenizer.token) ->
    t.kind = Tokenizer.Paren_close && t.span.stop = cursor_pos
  ) tokens in
  (* Prefer open paren at cursor, then close paren before cursor *)
  match open_at_cursor with
  | Some t ->
    (* Forward search for matching close *)
    let depth = ref 0 in
    let result = ref None in
    List.iter (fun (tok : Tokenizer.token) ->
      if !result = None && tok.span.start >= t.span.start then begin
        if tok.kind = Tokenizer.Paren_open then incr depth
        else if tok.kind = Tokenizer.Paren_close then begin
          decr depth;
          if !depth = 0 then
            result := Some (t.span.start, tok.span.start)
        end
      end
    ) tokens;
    !result
  | None ->
    match close_before_cursor with
    | Some t ->
      (* Backward search for matching open *)
      let depth = ref 0 in
      let result = ref None in
      List.iter (fun (tok : Tokenizer.token) ->
        if !result = None && tok.span.start <= t.span.start then begin
          if tok.kind = Tokenizer.Paren_close then incr depth
          else if tok.kind = Tokenizer.Paren_open then begin
            decr depth;
            if !depth = 0 then
              result := Some (tok.span.start, t.span.start)
          end
        end
      ) (List.rev tokens);
      !result
    | None -> None

(* --- Semantic analysis --- *)

type sem_role = Defn_name | Param | Bound_var

type sem_mark = {
  role : sem_role;
  name : string;
  scope_start : int;
  scope_end : int;
}

let analyze_semantics (tokens : Tokenizer.token list) text =
  let arr = Array.of_list tokens in
  let n = Array.length arr in
  let marks = Hashtbl.create 16 in
  let tok_text (t : Tokenizer.token) =
    String.sub text t.span.start (t.span.stop - t.span.start)
  in
  let skip_ws i =
    let j = ref i in
    while !j < n && arr.(!j).kind = Tokenizer.Whitespace do incr j done;
    !j
  in
  let find_close i =
    let depth = ref 1 in
    let j = ref (i + 1) in
    while !j < n && !depth > 0 do
      (match arr.(!j).kind with
       | Tokenizer.Paren_open -> incr depth
       | Tokenizer.Paren_close -> decr depth
       | _ -> ());
      if !depth > 0 then incr j
    done;
    if !j < n && !depth = 0 then Some !j else None
  in
  let is_id (t : Tokenizer.token) =
    t.kind = Tokenizer.Symbol || t.kind = Tokenizer.Keyword
  in
  let add_mark (tok : Tokenizer.token) role scope_s scope_e =
    Hashtbl.replace marks tok.span.start
      { role; name = tok_text tok; scope_start = scope_s; scope_end = scope_e }
  in
  let collect_params from_i to_i scope_s scope_e =
    let pi = ref (skip_ws from_i) in
    while !pi < to_i do
      if is_id arr.(!pi) then
        add_mark arr.(!pi) Param scope_s scope_e;
      pi := skip_ws (!pi + 1)
    done
  in
  let process_binding_pairs open_i close_i scope_s scope_e =
    let i = ref (skip_ws (open_i + 1)) in
    while !i < close_i do
      if arr.(!i).kind = Tokenizer.Paren_open then begin
        (match find_close !i with
         | None -> i := close_i
         | Some pair_close ->
           let name_i = skip_ws (!i + 1) in
           if name_i < pair_close && is_id arr.(name_i) then
             add_mark arr.(name_i) Bound_var scope_s scope_e;
           i := skip_ws (pair_close + 1))
      end else
        i := skip_ws (!i + 1)
    done
  in
  let rec process_form open_i =
    match find_close open_i with
    | None -> ()
    | Some close_i ->
      let scope_s = arr.(open_i).span.start in
      let scope_e = arr.(close_i).span.stop in
      let head_i = skip_ws (open_i + 1) in
      if head_i < close_i && is_id arr.(head_i) then begin
        let head = tok_text arr.(head_i) in
        (match head with
         | "define" | "define-syntax" | "define-record-type"
         | "define-library" | "define-values" ->
           let next_i = skip_ws (head_i + 1) in
           if next_i < close_i then begin
             if arr.(next_i).kind = Tokenizer.Paren_open then begin
               (* (define (name params...) body) *)
               (match find_close next_i with
                | None -> ()
                | Some params_close ->
                  let name_i = skip_ws (next_i + 1) in
                  if name_i < params_close && is_id arr.(name_i) then begin
                    add_mark arr.(name_i) Defn_name scope_s scope_e;
                    collect_params (name_i + 1) params_close scope_s scope_e
                  end)
             end else if is_id arr.(next_i) then
               (* (define name expr) *)
               add_mark arr.(next_i) Defn_name scope_s scope_e
           end
         | "lambda" ->
           let next_i = skip_ws (head_i + 1) in
           if next_i < close_i && arr.(next_i).kind = Tokenizer.Paren_open then
             (match find_close next_i with
              | None -> ()
              | Some params_close ->
                collect_params (next_i + 1) params_close scope_s scope_e)
         | "let" | "let*" | "letrec" | "letrec*"
         | "let-values" | "let*-values" | "let-syntax" | "letrec-syntax" ->
           let next_i = skip_ws (head_i + 1) in
           if next_i < close_i then begin
             if is_id arr.(next_i) then begin
               (* Named let: (let name ((var val) ...) body) *)
               add_mark arr.(next_i) Defn_name scope_s scope_e;
               let bindings_i = skip_ws (next_i + 1) in
               if bindings_i < close_i && arr.(bindings_i).kind = Tokenizer.Paren_open then
                 (match find_close bindings_i with
                  | Some bc -> process_binding_pairs bindings_i bc scope_s scope_e
                  | None -> ())
             end else if arr.(next_i).kind = Tokenizer.Paren_open then
               (match find_close next_i with
                | Some bc -> process_binding_pairs next_i bc scope_s scope_e
                | None -> ())
           end
         | "do" ->
           let next_i = skip_ws (head_i + 1) in
           if next_i < close_i && arr.(next_i).kind = Tokenizer.Paren_open then
             (match find_close next_i with
              | Some bc -> process_binding_pairs next_i bc scope_s scope_e
              | None -> ())
         | _ -> ())
      end;
      scan_subforms (open_i + 1) close_i
  and scan_subforms from_i to_i =
    let i = ref from_i in
    while !i < to_i do
      if arr.(!i).kind = Tokenizer.Paren_open then begin
        process_form !i;
        (match find_close !i with
         | Some ci -> i := ci + 1
         | None -> i := to_i)
      end else
        incr i
    done
  in
  scan_subforms 0 n;
  marks

(* Find binding info for identifier at cursor position *)
let find_cursor_binding (tokens : Tokenizer.token list) text cursor_pos marks =
  if cursor_pos < 0 then None
  else
    let cursor_tok = List.find_opt (fun (t : Tokenizer.token) ->
      cursor_pos >= t.span.start && cursor_pos <= t.span.stop
      && (t.kind = Tokenizer.Symbol || t.kind = Tokenizer.Keyword)
    ) tokens in
    match cursor_tok with
    | None -> None
    | Some ct ->
      let name = String.sub text ct.span.start (ct.span.stop - ct.span.start) in
      if Hashtbl.mem marks ct.span.start then
        (* Cursor is on a binding site — just bold it *)
        Some (ct.span.start, ct.span.stop, None)
      else
        (* Search for the innermost binding with this name whose scope
           contains the cursor position *)
        let best = ref None in
        Hashtbl.iter (fun _pos mark ->
          if mark.name = name
             && cursor_pos >= mark.scope_start
             && cursor_pos < mark.scope_end then
            match !best with
            | None -> best := Some mark
            | Some prev ->
              if mark.scope_start > prev.scope_start then
                best := Some mark
        ) marks;
        match !best with
        | Some m ->
          (* Find the token position for the binding *)
          let binding_tok = List.find_opt (fun (t : Tokenizer.token) ->
            t.span.start >= m.scope_start && t.span.stop <= m.scope_end
            && (t.kind = Tokenizer.Symbol || t.kind = Tokenizer.Keyword)
            && String.sub text t.span.start (t.span.stop - t.span.start) = name
            && Hashtbl.mem marks t.span.start
          ) tokens in
          (match binding_tok with
           | Some bt -> Some (ct.span.start, ct.span.stop,
                              Some (bt.span.start, bt.span.stop))
           | None -> Some (ct.span.start, ct.span.stop, None))
        | None -> Some (ct.span.start, ct.span.stop, None)

(* --- Highlighting --- *)

let highlight_line theme rt text cursor_pos =
  let tokens = Tokenizer.tokenize rt text in
  let matching = find_matching_paren tokens cursor_pos in
  let is_matching_pos pos =
    match matching with
    | Some (p1, p2) -> pos = p1 || pos = p2
    | None -> false
  in
  let marks = analyze_semantics tokens text in
  let cursor_info = find_cursor_binding tokens text cursor_pos marks in
  let is_cursor_tok_start pos =
    match cursor_info with
    | Some (s, _, _) -> pos = s
    | None -> false
  in
  let is_binding_highlight start stop =
    match cursor_info with
    | Some (_, _, Some (bs, be)) -> start = bs && stop = be
    | _ -> false
  in
  let buf = Buffer.create (String.length text * 2) in
  let depth = ref 0 in
  List.iter (fun (tok : Tokenizer.token) ->
    let base_style = match tok.kind with
      | Tokenizer.Paren_open ->
        let d = !depth in
        incr depth;
        theme.paren.(d mod Array.length theme.paren)
      | Tokenizer.Paren_close ->
        if !depth > 0 then decr depth;
        let d = !depth in
        theme.paren.(d mod Array.length theme.paren)
      | Tokenizer.String_lit -> theme.string_style
      | Tokenizer.Number_lit -> theme.number_style
      | Tokenizer.Comment -> theme.comment_style
      | Tokenizer.Boolean_lit -> theme.boolean_style
      | Tokenizer.Char_lit -> theme.char_style
      | Tokenizer.Quote_shorthand -> theme.quote_style
      | Tokenizer.Datum_comment -> theme.comment_style
      | Tokenizer.Whitespace -> theme.default_style
      | Tokenizer.Hash_prefix -> theme.default_style
      | Tokenizer.Error -> theme.error_style
      | Tokenizer.Symbol | Tokenizer.Keyword ->
        (match Hashtbl.find_opt marks tok.span.start with
         | Some m ->
           (match m.role with
            | Defn_name -> theme.defn_name_style
            | Param | Bound_var -> theme.param_style)
         | None ->
           if tok.kind = Tokenizer.Keyword then theme.keyword_style
           else theme.symbol_style)
    in
    let style =
      if is_matching_pos tok.span.start then
        { base_style with underline = true }
      else if is_cursor_tok_start tok.span.start then
        { base_style with bold = true }
      else if is_binding_highlight tok.span.start tok.span.stop then
        { base_style with bold = true }
      else
        base_style
    in
    let s = style_to_ansi style in
    let tok_text = String.sub text tok.span.start (tok.span.stop - tok.span.start) in
    if s <> "" then begin
      Buffer.add_string buf s;
      Buffer.add_string buf tok_text;
      Buffer.add_string buf ansi_reset
    end else
      Buffer.add_string buf tok_text
  ) tokens;
  Buffer.contents buf

(* --- Theme loading from s-expression file --- *)

(* Minimal s-expression parser for theme files *)
type sexp = Atom of string | List of sexp list

let rec parse_sexp chars =
  match chars with
  | [] -> None
  | c :: rest when c = ' ' || c = '\t' || c = '\n' || c = '\r' ->
    parse_sexp rest
  | '(' :: rest ->
    let items, rest' = parse_sexp_list rest [] in
    Some (List items, rest')
  | ')' :: _ -> None
  | ';' :: rest ->
    (* Skip line comment *)
    let rec skip = function
      | [] -> parse_sexp []
      | '\n' :: rest -> parse_sexp rest
      | _ :: rest -> skip rest
    in
    skip rest
  | '"' :: rest ->
    let buf = Buffer.create 16 in
    let rec scan = function
      | [] -> (Atom (Buffer.contents buf), [])
      | '"' :: rest -> (Atom (Buffer.contents buf), rest)
      | '\\' :: c :: rest -> Buffer.add_char buf c; scan rest
      | c :: rest -> Buffer.add_char buf c; scan rest
    in
    let atom, rest' = scan rest in
    Some (atom, rest')
  | c :: rest ->
    let buf = Buffer.create 16 in
    let rec scan = function
      | [] -> (Atom (Buffer.contents buf), [])
      | ch :: _ as rest when ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r'
                             || ch = '(' || ch = ')' ->
        (Atom (Buffer.contents buf), rest)
      | ch :: rest -> Buffer.add_char buf ch; scan rest
    in
    let atom, rest' = scan (c :: rest) in
    Some (atom, rest')
and parse_sexp_list chars acc =
  match chars with
  | [] -> (List.rev acc, [])
  | ')' :: rest -> (List.rev acc, rest)
  | _ ->
    match parse_sexp chars with
    | Some (sexp, rest) -> parse_sexp_list rest (sexp :: acc)
    | None -> (List.rev acc, chars)

let sexp_to_value = function
  | Atom "#t" | Atom "#true" -> `Bool true
  | Atom "#f" | Atom "#false" -> `Bool false
  | Atom s ->
    (try `Int (int_of_string s)
     with Failure _ -> `String s)
  | List _ -> `None

let load_theme path =
  let ic = open_in path in
  let content = Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    Bytes.to_string buf
  ) in
  let chars = List.init (String.length content) (fun i -> content.[i]) in
  match parse_sexp chars with
  | Some (List (Atom "theme" :: Atom name :: sections), _) ->
    let t = ref { dark_theme with name } in
    List.iter (function
      | List (Atom section :: props) ->
        let s = ref default_style in
        let paren_colors = ref [] in
        List.iter (function
          | List [Atom "fg"; v] ->
            (match sexp_to_value v with
             | `Int n ->
               if section = "paren" then
                 paren_colors := n :: !paren_colors
               else
                 s := { !s with fg = Some n }
             | _ -> ())
          | List [Atom "bg"; v] ->
            (match sexp_to_value v with `Int n -> s := { !s with bg = Some n } | _ -> ())
          | List [Atom "bold"; v] ->
            (match sexp_to_value v with `Bool b -> s := { !s with bold = b } | _ -> ())
          | List [Atom "italic"; v] ->
            (match sexp_to_value v with `Bool b -> s := { !s with italic = b } | _ -> ())
          | List [Atom "underline"; v] ->
            (match sexp_to_value v with `Bool b -> s := { !s with underline = b } | _ -> ())
          | _ -> ()
        ) props;
        (match section with
         | "keyword" -> t := { !t with keyword_style = !s }
         | "string" -> t := { !t with string_style = !s }
         | "number" -> t := { !t with number_style = !s }
         | "comment" -> t := { !t with comment_style = !s }
         | "symbol" -> t := { !t with symbol_style = !s }
         | "boolean" -> t := { !t with boolean_style = !s }
         | "char" -> t := { !t with char_style = !s }
         | "quote" -> t := { !t with quote_style = !s }
         | "error" -> t := { !t with error_style = !s }
         | "defn-name" -> t := { !t with defn_name_style = !s }
         | "param" -> t := { !t with param_style = !s }
         | "paren" ->
           let colors = List.rev !paren_colors in
           if colors <> [] then
             t := { !t with paren = Array.of_list (List.map fg colors) }
         | _ -> ())
      | _ -> ()
    ) sections;
    !t
  | _ -> failwith (Printf.sprintf "Invalid theme file: %s" path)
