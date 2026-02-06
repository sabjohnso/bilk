exception Read_error of Loc.t * string

type state = {
  port : Port.t;
  mutable readtable : Readtable.t;
  labels : (int, Syntax.t) Hashtbl.t;
}

let make_state readtable port =
  { port; readtable; labels = Hashtbl.create 16 }

let error state msg =
  raise (Read_error (Port.current_loc state.port, msg))

(* Skip whitespace and line comments *)
let rec skip_atmosphere state =
  match Port.peek_char state.port with
  | None -> ()
  | Some c when Readtable.is_whitespace state.readtable c ->
    ignore (Port.read_char state.port);
    skip_atmosphere state
  | Some ';' ->
    (* Line comment: skip until newline *)
    ignore (Port.read_char state.port);
    skip_line_comment state;
    skip_atmosphere state
  | _ -> ()

and skip_line_comment state =
  match Port.read_char state.port with
  | None -> ()
  | Some '\n' -> ()
  | Some '\r' -> ()
  | Some _ -> skip_line_comment state

(* Check if char is a delimiter (terminates a token) *)
let is_delimiter state c =
  Readtable.is_delimiter state.readtable c

(* Read constituent characters into a buffer, respecting fold_case *)
let read_token state =
  let buf = Buffer.create 32 in
  let fold = Readtable.fold_case state.readtable in
  let rec loop () =
    match Port.peek_char state.port with
    | None -> ()
    | Some c when is_delimiter state c -> ()
    | Some c ->
      ignore (Port.read_char state.port);
      let c' = if fold then Char.lowercase_ascii c else c in
      Buffer.add_char buf c';
      loop ()
  in
  loop ();
  Buffer.contents buf

(* Parse a hex digit *)
let hex_digit c =
  match c with
  | '0'..'9' -> Some (Char.code c - Char.code '0')
  | 'a'..'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A'..'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

(* Parse \x<hex>; escape sequence, returns codepoint *)
let read_hex_escape state =
  let rec loop acc =
    match Port.peek_char state.port with
    | Some ';' ->
      ignore (Port.read_char state.port);
      acc
    | Some c ->
      (match hex_digit c with
       | Some d ->
         ignore (Port.read_char state.port);
         loop (acc * 16 + d)
       | None -> error state "invalid hex escape")
    | None -> error state "unterminated hex escape"
  in
  loop 0

let skip_intraline_ws state =
  let rec loop () =
    match Port.peek_char state.port with
    | Some ' ' | Some '\t' ->
      ignore (Port.read_char state.port);
      loop ()
    | _ -> ()
  in
  loop ()

(* Read a string literal *)
let read_string state =
  let buf = Buffer.create 64 in
  let rec loop () =
    match Port.read_char state.port with
    | None -> error state "unterminated string"
    | Some '"' -> Buffer.contents buf
    | Some '\\' ->
      (match Port.read_char state.port with
       | None -> error state "unterminated string"
       | Some 'n' -> Buffer.add_char buf '\n'; loop ()
       | Some 't' -> Buffer.add_char buf '\t'; loop ()
       | Some 'r' -> Buffer.add_char buf '\r'; loop ()
       | Some '"' -> Buffer.add_char buf '"'; loop ()
       | Some '\\' -> Buffer.add_char buf '\\'; loop ()
       | Some '|' -> Buffer.add_char buf '|'; loop ()
       | Some 'a' -> Buffer.add_char buf '\007'; loop ()
       | Some 'b' -> Buffer.add_char buf '\008'; loop ()
       | Some 'x' ->
         let cp = read_hex_escape state in
         if cp <= 0x7F then Buffer.add_char buf (Char.chr cp)
         else Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
         loop ()
       | Some '\n' -> skip_intraline_ws state; loop ()
       | Some '\r' ->
         (* \<CR> or \<CR><LF> = line continuation *)
         (match Port.peek_char state.port with
          | Some '\n' -> ignore (Port.read_char state.port)
          | _ -> ());
         skip_intraline_ws state;
         loop ()
       | Some c -> error state (Printf.sprintf "unknown escape \\%c" c))
    | Some c -> Buffer.add_char buf c; loop ()
  in
  loop ()

(* Read an escaped identifier |...| *)
let read_escaped_identifier state =
  let buf = Buffer.create 32 in
  let rec loop () =
    match Port.read_char state.port with
    | None -> error state "unterminated escaped identifier"
    | Some '|' -> Buffer.contents buf
    | Some '\\' ->
      (match Port.read_char state.port with
       | None -> error state "unterminated escaped identifier"
       | Some 'n' -> Buffer.add_char buf '\n'; loop ()
       | Some 't' -> Buffer.add_char buf '\t'; loop ()
       | Some 'r' -> Buffer.add_char buf '\r'; loop ()
       | Some '|' -> Buffer.add_char buf '|'; loop ()
       | Some '\\' -> Buffer.add_char buf '\\'; loop ()
       | Some 'a' -> Buffer.add_char buf '\007'; loop ()
       | Some 'b' -> Buffer.add_char buf '\008'; loop ()
       | Some 'x' ->
         let cp = read_hex_escape state in
         if cp <= 0x7F then Buffer.add_char buf (Char.chr cp)
         else Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
         loop ()
       | Some c -> error state (Printf.sprintf "unknown escape \\%c in identifier" c))
    | Some c -> Buffer.add_char buf c; loop ()
  in
  loop ()

(* Named characters *)
let named_char name =
  match String.lowercase_ascii name with
  | "alarm" -> Some (Uchar.of_int 0x07)
  | "backspace" -> Some (Uchar.of_int 0x08)
  | "delete" -> Some (Uchar.of_int 0x7F)
  | "escape" -> Some (Uchar.of_int 0x1B)
  | "newline" -> Some (Uchar.of_int 0x0A)
  | "null" -> Some (Uchar.of_int 0x00)
  | "return" -> Some (Uchar.of_int 0x0D)
  | "space" -> Some (Uchar.of_int 0x20)
  | "tab" -> Some (Uchar.of_int 0x09)
  | _ -> None

(* Read #\... character literal *)
let read_character state =
  (* Collect the character name/value *)
  let buf = Buffer.create 16 in
  let rec loop () =
    match Port.peek_char state.port with
    | None -> ()
    | Some c when is_delimiter state c -> ()
    | Some c ->
      ignore (Port.read_char state.port);
      Buffer.add_char buf c;
      loop ()
  in
  loop ();
  let s = Buffer.contents buf in
  if String.length s = 0 then
    error state "empty character literal"
  else if String.length s = 1 then
    Uchar.of_char s.[0]
  else if s.[0] = 'x' || s.[0] = 'X' then begin
    (* Hex character: #\xNNNN *)
    let hex = String.sub s 1 (String.length s - 1) in
    try
      let cp = int_of_string ("0x" ^ hex) in
      if cp < 0 || cp > 0x10FFFF then
        error state "character out of range"
      else
        Uchar.of_int cp
    with _ -> error state "invalid hex character"
  end else
    match named_char s with
    | Some u -> u
    | None -> error state (Printf.sprintf "unknown character name: %s" s)

(* Number parsing *)

type num_prefix = {
  radix : int option;  (* None = default 10 *)
  exact : bool option; (* None = default, Some true = #e, Some false = #i *)
}

let parse_integer radix s =
  let len = String.length s in
  if len = 0 then None
  else
    let start, sign =
      match s.[0] with
      | '+' -> (1, 1)
      | '-' -> (1, -1)
      | _ -> (0, 1)
    in
    if start >= len then None
    else
      let rec loop acc i =
        if i >= len then Some (sign * acc)
        else
          let c = s.[i] in
          let digit =
            match c with
            | '0'..'9' -> Char.code c - Char.code '0'
            | 'a'..'f' -> Char.code c - Char.code 'a' + 10
            | 'A'..'F' -> Char.code c - Char.code 'A' + 10
            | _ -> radix (* invalid *)
          in
          if digit >= radix then None
          else loop (acc * radix + digit) (i + 1)
      in
      loop 0 start

let parse_float s =
  (* Check for special values first *)
  let s_lower = String.lowercase_ascii s in
  match s_lower with
  | "+inf.0" | "inf.0" -> Some Float.infinity
  | "-inf.0" -> Some Float.neg_infinity
  | "+nan.0" | "nan.0" | "-nan.0" -> Some Float.nan
  | _ ->
    try Some (float_of_string s)
    with _ -> None

let is_float_syntax s =
  (* Has decimal point or exponent marker *)
  let has_dot = String.contains s '.' in
  let has_exp =
    String.contains s 'e' || String.contains s 'E' ||
    String.contains s 's' || String.contains s 'S' ||
    String.contains s 'f' || String.contains s 'F' ||
    String.contains s 'd' || String.contains s 'D' ||
    String.contains s 'l' || String.contains s 'L'
  in
  has_dot || has_exp

let normalize_exponent s =
  (* Replace scheme exponent markers with 'e' for float_of_string *)
  String.map (fun c ->
    match c with
    | 's' | 'S' | 'f' | 'F' | 'd' | 'D' | 'l' | 'L' -> 'e'
    | _ -> c
  ) s

let parse_number prefix token =
  let radix = Option.value prefix.radix ~default:10 in
  let token_lower = String.lowercase_ascii token in
  (* Check for special infinity/nan *)
  match token_lower with
  | "+inf.0" | "-inf.0" | "+nan.0" | "-nan.0" | "inf.0" | "nan.0" ->
    (match parse_float token_lower with
     | Some f -> Some (Syntax.Flonum f)
     | None -> None)
  | _ ->
    if radix = 10 && is_float_syntax token then begin
      let normalized = normalize_exponent token in
      match parse_float normalized with
      | Some f ->
        (match prefix.exact with
         | Some true -> Some (Syntax.Fixnum (int_of_float f))
         | _ -> Some (Syntax.Flonum f))
      | None -> None
    end else begin
      match parse_integer radix token with
      | Some n ->
        (match prefix.exact with
         | Some false -> Some (Syntax.Flonum (float_of_int n))
         | _ -> Some (Syntax.Fixnum n))
      | None -> None
    end

(* Parse a token as either number or symbol *)
let parse_atom state prefix token =
  if String.length token = 0 then
    error state "unexpected empty token";
  match parse_number prefix token with
  | Some datum -> datum
  | None ->
    (* Must be a symbol *)
    if prefix.radix <> None || prefix.exact <> None then
      error state (Printf.sprintf "invalid number: %s" token)
    else
      Syntax.Symbol token

(* Internal read result type for compound parsing *)
type read_result =
  | Datum of Syntax.t
  | Dot
  | Close_paren
  | Eof_signal

let rec read_raw state =
  skip_atmosphere state;
  let loc = Port.current_loc state.port in
  match Port.peek_char state.port with
  | None -> Eof_signal
  | Some '"' ->
    ignore (Port.read_char state.port);
    let s = read_string state in
    Datum (Syntax.make loc (Syntax.Str s))
  | Some '(' ->
    ignore (Port.read_char state.port);
    read_list state loc
  | Some ')' ->
    ignore (Port.read_char state.port);
    Close_paren
  | Some '\'' ->
    ignore (Port.read_char state.port);
    read_quote state loc "quote"
  | Some '`' ->
    ignore (Port.read_char state.port);
    read_quote state loc "quasiquote"
  | Some ',' ->
    ignore (Port.read_char state.port);
    (match Port.peek_char state.port with
     | Some '@' ->
       ignore (Port.read_char state.port);
       read_quote state loc "unquote-splicing"
     | _ ->
       read_quote state loc "unquote")
  | Some '|' ->
    ignore (Port.read_char state.port);
    let name = read_escaped_identifier state in
    Datum (Syntax.make loc (Syntax.Symbol name))
  | Some '#' ->
    read_hash state loc
  | Some c when is_delimiter state c ->
    error state (Printf.sprintf "unexpected delimiter: %c" c)
  | _ ->
    let token = read_token state in
    if token = "." then Dot
    else Datum (Syntax.make loc (parse_atom state { radix = None; exact = None } token))

and read_hash state loc =
  ignore (Port.read_char state.port); (* consume # *)
  match Port.peek_char state.port with
  | None -> error state "unexpected end of input after #"
  | Some 't' ->
    ignore (Port.read_char state.port);
    (* Check for #true *)
    if peek_matches_delimiter_or_eof state then
      Datum (Syntax.make loc (Syntax.Bool true))
    else begin
      (* Read rest of token *)
      let rest = read_token state in
      if String.lowercase_ascii rest = "rue" then
        Datum (Syntax.make loc (Syntax.Bool true))
      else
        error state (Printf.sprintf "invalid # syntax: #t%s" rest)
    end
  | Some 'T' ->
    ignore (Port.read_char state.port);
    if peek_matches_delimiter_or_eof state then
      Datum (Syntax.make loc (Syntax.Bool true))
    else begin
      let rest = read_token state in
      if String.lowercase_ascii rest = "rue" then
        Datum (Syntax.make loc (Syntax.Bool true))
      else
        error state (Printf.sprintf "invalid # syntax: #T%s" rest)
    end
  | Some 'f' ->
    ignore (Port.read_char state.port);
    if peek_matches_delimiter_or_eof state then
      Datum (Syntax.make loc (Syntax.Bool false))
    else begin
      let rest = read_token state in
      if String.lowercase_ascii rest = "alse" then
        Datum (Syntax.make loc (Syntax.Bool false))
      else
        error state (Printf.sprintf "invalid # syntax: #f%s" rest)
    end
  | Some 'F' ->
    ignore (Port.read_char state.port);
    if peek_matches_delimiter_or_eof state then
      Datum (Syntax.make loc (Syntax.Bool false))
    else begin
      let rest = read_token state in
      if String.lowercase_ascii rest = "alse" then
        Datum (Syntax.make loc (Syntax.Bool false))
      else
        error state (Printf.sprintf "invalid # syntax: #F%s" rest)
    end
  | Some '\\' ->
    ignore (Port.read_char state.port);
    let c = read_character state in
    Datum (Syntax.make loc (Syntax.Char c))
  | Some '(' ->
    ignore (Port.read_char state.port);
    read_vector state loc
  | Some ('u' | 'U') ->
    ignore (Port.read_char state.port);
    (match Port.peek_char state.port with
     | Some '8' ->
       ignore (Port.read_char state.port);
       (match Port.peek_char state.port with
        | Some '(' ->
          ignore (Port.read_char state.port);
          read_bytevector state loc
        | _ -> error state "expected #u8(")
     | _ -> error state "expected #u8(")
  | Some ';' ->
    ignore (Port.read_char state.port);
    (* Datum comment: read and discard one datum *)
    (match read_raw state with
     | Datum _ -> read_raw state  (* return next datum *)
     | Dot -> error state "unexpected dot after #;"
     | Close_paren -> error state "unexpected ) after #;"
     | Eof_signal -> error state "unexpected end of input after #;")
  | Some '|' ->
    ignore (Port.read_char state.port);
    read_block_comment state;
    read_raw state
  | Some '!' ->
    ignore (Port.read_char state.port);
    read_directive state;
    read_raw state
  | Some ('b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X' | 'e' | 'E' | 'i' | 'I') ->
    (* Number prefix - need to "unread" the # *)
    read_number_with_prefix state loc
  | Some ('0'..'9') ->
    (* Datum label *)
    read_datum_label state loc
  | Some c ->
    error state (Printf.sprintf "unknown # syntax: #%c" c)

and peek_matches_delimiter_or_eof state =
  match Port.peek_char state.port with
  | None -> true
  | Some c -> is_delimiter state c

and read_number_with_prefix state loc =
  (* We're positioned right after consuming '#' but the prefix char is still there *)
  let first_char = Option.get (Port.read_char state.port) in
  let first_prefix =
    match first_char with
    | 'b' | 'B' -> { radix = Some 2; exact = None }
    | 'o' | 'O' -> { radix = Some 8; exact = None }
    | 'd' | 'D' -> { radix = Some 10; exact = None }
    | 'x' | 'X' -> { radix = Some 16; exact = None }
    | 'e' | 'E' -> { radix = None; exact = Some true }
    | 'i' | 'I' -> { radix = None; exact = Some false }
    | _ -> assert false
  in
  (* Check for second prefix *)
  let prefix =
    match Port.peek_char state.port with
    | Some '#' ->
      ignore (Port.read_char state.port);
      (match Port.peek_char state.port with
       | Some ('b' | 'B') when first_prefix.radix = None ->
         ignore (Port.read_char state.port);
         { first_prefix with radix = Some 2 }
       | Some ('o' | 'O') when first_prefix.radix = None ->
         ignore (Port.read_char state.port);
         { first_prefix with radix = Some 8 }
       | Some ('d' | 'D') when first_prefix.radix = None ->
         ignore (Port.read_char state.port);
         { first_prefix with radix = Some 10 }
       | Some ('x' | 'X') when first_prefix.radix = None ->
         ignore (Port.read_char state.port);
         { first_prefix with radix = Some 16 }
       | Some ('e' | 'E') when first_prefix.exact = None ->
         ignore (Port.read_char state.port);
         { first_prefix with exact = Some true }
       | Some ('i' | 'I') when first_prefix.exact = None ->
         ignore (Port.read_char state.port);
         { first_prefix with exact = Some false }
       | Some ('b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X') ->
         error state "duplicate radix prefix"
       | Some ('e' | 'E' | 'i' | 'I') ->
         error state "duplicate exactness prefix"
       | _ ->
         error state "invalid number prefix")
    | _ -> first_prefix
  in
  let token = read_token state in
  Datum (Syntax.make loc (parse_atom state prefix token))

and read_list state start_loc =
  let rec loop acc =
    skip_atmosphere state;
    match read_raw state with
    | Close_paren ->
      (* Build proper list from accumulated items *)
      List.fold_right (fun item tail ->
        Syntax.make start_loc (Syntax.Pair (item, tail))
      ) acc (Syntax.make start_loc Syntax.Nil)
      |> fun result -> Datum result
    | Dot ->
      if acc = [] then error state "unexpected dot"
      else begin
        (* Read final cdr *)
        skip_atmosphere state;
        match read_raw state with
        | Datum final_cdr ->
          skip_atmosphere state;
          (match read_raw state with
           | Close_paren ->
             List.fold_right (fun item tail ->
               Syntax.make start_loc (Syntax.Pair (item, tail))
             ) acc final_cdr
             |> fun result -> Datum result
           | _ -> error state "expected ) after dotted tail")
        | _ -> error state "expected datum after dot"
      end
    | Eof_signal -> error state "unterminated list"
    | Datum item -> loop (acc @ [item])
  in
  loop []

and read_vector state start_loc =
  let rec loop acc =
    skip_atmosphere state;
    match read_raw state with
    | Close_paren ->
      Datum (Syntax.make start_loc (Syntax.Vector (Array.of_list acc)))
    | Dot -> error state "unexpected dot in vector"
    | Eof_signal -> error state "unterminated vector"
    | Datum item -> loop (acc @ [item])
  in
  loop []

and read_bytevector state start_loc =
  let rec loop acc =
    skip_atmosphere state;
    match read_raw state with
    | Close_paren ->
      let bytes = Bytes.create (List.length acc) in
      List.iteri (fun i b -> Bytes.set bytes i (Char.chr b)) acc;
      Datum (Syntax.make start_loc (Syntax.Bytevector bytes))
    | Dot -> error state "unexpected dot in bytevector"
    | Eof_signal -> error state "unterminated bytevector"
    | Datum item ->
      (match item.datum with
       | Syntax.Fixnum n when n >= 0 && n <= 255 ->
         loop (acc @ [n])
       | Syntax.Fixnum _ -> error state "bytevector element out of range (0-255)"
       | _ -> error state "bytevector elements must be integers")
  in
  loop []

and read_quote state loc sym =
  skip_atmosphere state;
  match read_raw state with
  | Datum inner ->
    let sym_syntax = Syntax.make loc (Syntax.Symbol sym) in
    let nil = Syntax.make loc Syntax.Nil in
    let inner_list = Syntax.make loc (Syntax.Pair (inner, nil)) in
    Datum (Syntax.make loc (Syntax.Pair (sym_syntax, inner_list)))
  | Dot -> error state (Printf.sprintf "unexpected dot after %s" sym)
  | Close_paren -> error state (Printf.sprintf "unexpected ) after %s" sym)
  | Eof_signal -> error state (Printf.sprintf "unexpected end of input after %s" sym)

and read_block_comment state =
  let rec loop depth =
    match Port.read_char state.port with
    | None -> error state "unterminated block comment"
    | Some '|' ->
      (match Port.peek_char state.port with
       | Some '#' ->
         ignore (Port.read_char state.port);
         if depth = 0 then ()
         else loop (depth - 1)
       | _ -> loop depth)
    | Some '#' ->
      (match Port.peek_char state.port with
       | Some '|' ->
         ignore (Port.read_char state.port);
         loop (depth + 1)
       | _ -> loop depth)
    | Some _ -> loop depth
  in
  loop 0

and read_directive state =
  let buf = Buffer.create 16 in
  let rec loop () =
    match Port.peek_char state.port with
    | None -> ()
    | Some c when is_delimiter state c -> ()
    | Some c ->
      ignore (Port.read_char state.port);
      Buffer.add_char buf c;
      loop ()
  in
  loop ();
  let directive = String.lowercase_ascii (Buffer.contents buf) in
  match directive with
  | "fold-case" ->
    state.readtable <- Readtable.with_fold_case true state.readtable
  | "no-fold-case" ->
    state.readtable <- Readtable.with_fold_case false state.readtable
  | _ ->
    error state (Printf.sprintf "unknown directive: #!%s" directive)

and read_datum_label state loc =
  (* Read the number *)
  let buf = Buffer.create 8 in
  let rec read_digits () =
    match Port.peek_char state.port with
    | Some ('0'..'9' as c) ->
      ignore (Port.read_char state.port);
      Buffer.add_char buf c;
      read_digits ()
    | _ -> ()
  in
  read_digits ();
  let n = int_of_string (Buffer.contents buf) in
  (* Check for = or # *)
  match Port.peek_char state.port with
  | Some '=' ->
    ignore (Port.read_char state.port);
    (* Define label: read datum, store it, return it *)
    (match read_raw state with
     | Datum d ->
       Hashtbl.replace state.labels n d;
       Datum d
     | Dot -> error state "unexpected dot in datum label"
     | Close_paren -> error state "unexpected ) in datum label"
     | Eof_signal -> error state "unexpected end of input in datum label")
  | Some '#' ->
    ignore (Port.read_char state.port);
    (* Reference label *)
    (match Hashtbl.find_opt state.labels n with
     | Some d -> Datum (Syntax.make loc d.datum)
     | None -> error state (Printf.sprintf "undefined datum label: #%d#" n))
  | _ ->
    error state "expected = or # after datum label number"

let read_syntax readtable port =
  let state = make_state readtable port in
  match read_raw state with
  | Datum d -> d
  | Dot -> error state "unexpected dot"
  | Close_paren -> error state "unexpected )"
  | Eof_signal -> Syntax.make (Port.current_loc port) Syntax.Eof

let read readtable port =
  Syntax.to_datum (read_syntax readtable port)
