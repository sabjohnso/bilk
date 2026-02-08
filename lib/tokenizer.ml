type span = { start : int; stop : int }

type token_kind =
  | Paren_open
  | Paren_close
  | String_lit
  | Number_lit
  | Boolean_lit
  | Char_lit
  | Symbol
  | Keyword
  | Quote_shorthand
  | Comment
  | Datum_comment
  | Whitespace
  | Hash_prefix
  | Error

type token = { kind : token_kind; span : span }

let keywords = [
  "define"; "define-syntax"; "define-record-type"; "define-library";
  "define-values";
  "lambda"; "if"; "cond"; "case"; "else"; "=>"; "and"; "or";
  "when"; "unless"; "begin"; "do";
  "let"; "let*"; "letrec"; "letrec*"; "let-values"; "let*-values";
  "let-syntax"; "letrec-syntax";
  "set!"; "quote"; "quasiquote"; "unquote"; "unquote-splicing";
  "syntax-rules"; "syntax-error";
  "import"; "export"; "library"; "include"; "include-ci";
  "cond-expand"; "guard";
]

let keyword_set = Hashtbl.create 64
let () = List.iter (fun k -> Hashtbl.replace keyword_set k true) keywords

let is_keyword name = Hashtbl.mem keyword_set name

(* --- Tokenizer state --- *)

type state = {
  text : string;
  len : int;
  rt : Readtable.t;
  mutable pos : int;
}

let advance st = st.pos <- st.pos + 1
let at_end st = st.pos >= st.len

(* Skip characters while predicate holds *)
let skip_while st pred =
  while st.pos < st.len && pred st.text.[st.pos] do
    advance st
  done

(* --- Individual token scanners --- *)

let scan_whitespace st =
  let start = st.pos in
  skip_while st (Readtable.is_whitespace st.rt);
  { kind = Whitespace; span = { start; stop = st.pos } }

let scan_line_comment st =
  let start = st.pos in
  advance st; (* skip ';' *)
  skip_while st (fun c -> c <> '\n');
  { kind = Comment; span = { start; stop = st.pos } }

let scan_string st =
  let start = st.pos in
  advance st; (* skip opening '"' *)
  let rec loop () =
    if at_end st then ()  (* unterminated — span to end *)
    else match st.text.[st.pos] with
      | '"' -> advance st  (* closing quote *)
      | '\\' ->
        advance st;  (* skip backslash *)
        if not (at_end st) then advance st;  (* skip escaped char *)
        loop ()
      | _ -> advance st; loop ()
  in
  loop ();
  { kind = String_lit; span = { start; stop = st.pos } }

let scan_block_comment st =
  let start = st.pos in
  advance st; advance st; (* skip '#|' *)
  let depth = ref 1 in
  while !depth > 0 && not (at_end st) do
    if st.pos + 1 < st.len then begin
      let c0 = st.text.[st.pos] in
      let c1 = st.text.[st.pos + 1] in
      if c0 = '#' && c1 = '|' then begin
        incr depth; advance st; advance st
      end else if c0 = '|' && c1 = '#' then begin
        decr depth; advance st; advance st
      end else
        advance st
    end else
      advance st
  done;
  { kind = Comment; span = { start; stop = st.pos } }

(* Check if a constituent token looks like a number *)
let is_number_like s =
  let len = String.length s in
  if len = 0 then false
  else
    let start = ref 0 in
    (* Skip sign *)
    if s.[0] = '+' || s.[0] = '-' then begin
      if len = 1 then false
      else begin start := 1; true end
    end else true
    &&
    (* Check for #e, #i, #b, #o, #d, #x prefix *)
    (if len >= 2 && s.[0] = '#' then
       match s.[1] with
       | 'e' | 'i' | 'b' | 'o' | 'd' | 'x'
       | 'E' | 'I' | 'B' | 'O' | 'D' | 'X' -> true
       | _ -> false
     else
       let c = s.[!start] in
       (c >= '0' && c <= '9') ||
       (c = '.' && len > !start + 1 && s.[!start + 1] >= '0' && s.[!start + 1] <= '9') ||
       (* inf/nan *)
       (len >= !start + 5 &&
        let sub = String.sub s !start (min 5 (len - !start)) in
        sub = "+inf." || sub = "-inf." || sub = "+nan." || sub = "-nan."))

let scan_constituent st =
  let start = st.pos in
  skip_while st (fun c -> not (Readtable.is_delimiter st.rt c));
  let text = String.sub st.text start (st.pos - start) in
  let kind =
    if is_keyword text then Keyword
    else if is_number_like text then Number_lit
    else Symbol
  in
  { kind; span = { start; stop = st.pos } }

let scan_pipe_identifier st =
  let start = st.pos in
  advance st; (* skip opening '|' *)
  let rec loop () =
    if at_end st then ()
    else match st.text.[st.pos] with
      | '|' -> advance st
      | '\\' -> advance st; if not (at_end st) then advance st; loop ()
      | _ -> advance st; loop ()
  in
  loop ();
  { kind = Symbol; span = { start; stop = st.pos } }

let scan_hash st =
  let start = st.pos in
  advance st; (* skip '#' *)
  if at_end st then
    { kind = Hash_prefix; span = { start; stop = st.pos } }
  else
    let c = st.text.[st.pos] in
    match c with
    | 't' | 'f' ->
      advance st;
      (* Check for #true / #false *)
      if c = 't' && st.pos + 3 <= st.len &&
         String.sub st.text st.pos 3 = "rue" then begin
        st.pos <- st.pos + 3
      end;
      if c = 'f' && st.pos + 4 <= st.len &&
         String.sub st.text st.pos 4 = "alse" then begin
        st.pos <- st.pos + 4
      end;
      { kind = Boolean_lit; span = { start; stop = st.pos } }
    | '\\' ->
      advance st; (* skip backslash *)
      (* Read character name or single char *)
      if not (at_end st) then begin
        let c2 = st.text.[st.pos] in
        advance st;
        (* Check for named characters like #\space, #\newline *)
        if (c2 >= 'a' && c2 <= 'z') || (c2 >= 'A' && c2 <= 'Z') then
          skip_while st (fun ch ->
            (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))
      end;
      { kind = Char_lit; span = { start; stop = st.pos } }
    | '(' ->
      advance st;
      { kind = Paren_open; span = { start; stop = st.pos } }
    | '|' ->
      st.pos <- start;  (* back up to '#' *)
      scan_block_comment st
    | ';' ->
      advance st;
      { kind = Datum_comment; span = { start; stop = st.pos } }
    | 'u' ->
      (* #u8( *)
      if st.pos + 2 <= st.len &&
         st.text.[st.pos] = 'u' &&
         st.pos + 1 < st.len && st.text.[st.pos + 1] = '8' &&
         st.pos + 2 < st.len && st.text.[st.pos + 2] = '(' then begin
        st.pos <- st.pos + 3;
        { kind = Paren_open; span = { start; stop = st.pos } }
      end else begin
        (* Fall through to constituent *)
        skip_while st (fun ch -> not (Readtable.is_delimiter st.rt ch));
        { kind = Hash_prefix; span = { start; stop = st.pos } }
      end
    | 'e' | 'i' | 'b' | 'o' | 'd' | 'x'
    | 'E' | 'I' | 'B' | 'O' | 'D' | 'X' ->
      (* Numeric prefix: #e, #i, #b, #o, #d, #x *)
      skip_while st (fun ch -> not (Readtable.is_delimiter st.rt ch));
      { kind = Number_lit; span = { start; stop = st.pos } }
    | '!' ->
      (* Directive: #!fold-case etc — treat as comment *)
      skip_while st (fun ch -> ch <> '\n');
      { kind = Comment; span = { start; stop = st.pos } }
    | _ ->
      skip_while st (fun ch -> not (Readtable.is_delimiter st.rt ch));
      { kind = Hash_prefix; span = { start; stop = st.pos } }

let scan_quote st =
  let start = st.pos in
  let c = st.text.[st.pos] in
  advance st;
  if c = ',' && not (at_end st) && st.text.[st.pos] = '@' then
    advance st;
  { kind = Quote_shorthand; span = { start; stop = st.pos } }

(* --- Main tokenizer --- *)

let tokenize rt text =
  let st = { text; len = String.length text; rt; pos = 0 } in
  let tokens = ref [] in
  while not (at_end st) do
    let c = st.text.[st.pos] in
    let tok =
      if Readtable.is_whitespace rt c then
        scan_whitespace st
      else match c with
        | ';' -> scan_line_comment st
        | '"' -> scan_string st
        | '(' | '[' -> let start = st.pos in advance st;
          { kind = Paren_open; span = { start; stop = st.pos } }
        | ')' | ']' -> let start = st.pos in advance st;
          { kind = Paren_close; span = { start; stop = st.pos } }
        | '\'' | '`' -> scan_quote st
        | ',' -> scan_quote st
        | '#' -> scan_hash st
        | '|' -> scan_pipe_identifier st
        | _ ->
          if Readtable.is_delimiter rt c then begin
            let start = st.pos in advance st;
            { kind = Error; span = { start; stop = st.pos } }
          end else
            scan_constituent st
    in
    tokens := tok :: !tokens
  done;
  List.rev !tokens
