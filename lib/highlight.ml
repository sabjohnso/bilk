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
}

(* --- Matching paren finder --- *)

let find_matching_paren tokens cursor_pos =
  (* Check if cursor is adjacent to a paren *)
  let find_token_at pos =
    List.find_opt (fun (t : Tokenizer.token) ->
      t.span.start = pos || t.span.stop = pos
    ) tokens
  in
  let paren_at = find_token_at cursor_pos in
  match paren_at with
  | Some t when t.kind = Tokenizer.Paren_open && t.span.start = cursor_pos ->
    (* Forward search for matching close *)
    let depth = ref 0 in
    let result = ref None in
    List.iter (fun (tok : Tokenizer.token) ->
      if !result = None then begin
        if tok.kind = Tokenizer.Paren_open then incr depth
        else if tok.kind = Tokenizer.Paren_close then begin
          decr depth;
          if !depth = 0 && tok.span.start >= t.span.start then
            result := Some (t.span.start, tok.span.start)
        end
      end
    ) (List.filter (fun (tok : Tokenizer.token) ->
      tok.span.start >= t.span.start) tokens);
    !result
  | Some t when t.kind = Tokenizer.Paren_close && t.span.stop = cursor_pos ->
    (* Backward search for matching open *)
    let depth = ref 0 in
    let result = ref None in
    List.iter (fun (tok : Tokenizer.token) ->
      if tok.span.start <= t.span.start then begin
        if tok.kind = Tokenizer.Paren_close then incr depth
        else if tok.kind = Tokenizer.Paren_open then begin
          decr depth;
          if !depth = 0 then
            result := Some (tok.span.start, t.span.start)
        end
      end
    ) (List.rev tokens);
    !result
  | _ -> None

(* --- Highlighting --- *)

let highlight_line theme rt text cursor_pos =
  let tokens = Tokenizer.tokenize rt text in
  let matching = find_matching_paren tokens cursor_pos in
  let is_matching_pos pos =
    match matching with
    | Some (p1, p2) -> pos = p1 || pos = p2
    | None -> false
  in
  let buf = Buffer.create (String.length text * 2) in
  let depth = ref 0 in
  List.iter (fun (tok : Tokenizer.token) ->
    let s = style_to_ansi (match tok.kind with
      | Tokenizer.Paren_open ->
        let d = !depth in
        incr depth;
        let base = theme.paren.(d mod Array.length theme.paren) in
        if is_matching_pos tok.span.start then
          { base with underline = true }
        else base
      | Tokenizer.Paren_close ->
        if !depth > 0 then decr depth;
        let d = !depth in
        let base = theme.paren.(d mod Array.length theme.paren) in
        if is_matching_pos tok.span.start then
          { base with underline = true }
        else base
      | Tokenizer.String_lit -> theme.string_style
      | Tokenizer.Number_lit -> theme.number_style
      | Tokenizer.Keyword -> theme.keyword_style
      | Tokenizer.Comment -> theme.comment_style
      | Tokenizer.Symbol -> theme.symbol_style
      | Tokenizer.Boolean_lit -> theme.boolean_style
      | Tokenizer.Char_lit -> theme.char_style
      | Tokenizer.Quote_shorthand -> theme.quote_style
      | Tokenizer.Datum_comment -> theme.comment_style
      | Tokenizer.Whitespace -> theme.default_style
      | Tokenizer.Hash_prefix -> theme.default_style
      | Tokenizer.Error -> theme.error_style
    ) in
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
         | "paren" ->
           let colors = List.rev !paren_colors in
           if colors <> [] then
             t := { !t with paren = Array.of_list (List.map fg colors) }
         | _ -> ())
      | _ -> ()
    ) sections;
    !t
  | _ -> failwith (Printf.sprintf "Invalid theme file: %s" path)
