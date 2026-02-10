exception Lsp_server_error of string

type document = {
  uri : string;
  mutable version : int;
  mutable text : string;
} [@@warning "-69"]

type t = {
  inst : Instance.t;
  documents : (string, document) Hashtbl.t;
  readtable : Readtable.t;
}

let create inst =
  { inst;
    documents = Hashtbl.create 16;
    readtable = inst.Instance.readtable }

(* --- Position utilities --- *)

let lsp_pos_to_offset text line col =
  let len = String.length text in
  let cur_line = ref 0 in
  let i = ref 0 in
  while !i < len && !cur_line < line do
    if text.[!i] = '\n' then incr cur_line;
    incr i
  done;
  min len (!i + col)

let offset_to_lsp_pos text offset =
  let line = ref 0 in
  let col = ref 0 in
  let len = min offset (String.length text) in
  for i = 0 to len - 1 do
    if text.[i] = '\n' then begin
      incr line;
      col := 0
    end else
      incr col
  done;
  (!line, !col)

(* --- Diagnostics --- *)

let make_diagnostic line col end_col message =
  `Assoc [
    ("range", `Assoc [
      ("start", `Assoc [("line", `Int line); ("character", `Int col)]);
      ("end", `Assoc [("line", `Int line); ("character", `Int end_col)]);
    ]);
    ("severity", `Int 1);
    ("source", `String "wile");
    ("message", `String message);
  ]

let make_gensym inst =
  let n = !(inst.Instance.gensym_counter) in
  inst.Instance.gensym_counter := n + 1;
  Printf.sprintf "%%g%d" n

let compute_diagnostics t text uri =
  let port = Port.of_string ~file:uri text in
  let diags = ref [] in
  let add_diag loc msg =
    let line = loc.Loc.line - 1 in
    let col = loc.Loc.col - 1 in
    diags := make_diagnostic line col (col + 1) msg :: !diags
  in
  let forms = ref [] in
  (try
    let rec read_all () =
      let s = Reader.read_syntax t.readtable port in
      match s with
      | { Syntax.datum = Syntax.Eof; _ } -> ()
      | _ -> forms := s :: !forms; read_all ()
    in
    read_all ()
  with Reader.Read_error (loc, msg) ->
    add_diag loc msg);
  if !diags = [] then begin
    let features = t.inst.Instance.features in
    let gensym () = make_gensym t.inst in
    List.iter (fun form ->
      (try
        let expanded = Expander.expand
          ~syn_env:t.inst.Instance.syn_env ~gensym ~features form in
        (try
          ignore (Compiler.compile t.inst.Instance.symbols expanded)
        with Compiler.Compile_error (loc, msg) ->
          add_diag loc msg)
      with
      | Compiler.Compile_error (loc, msg) ->
        add_diag loc msg
      | exn ->
        add_diag form.Syntax.loc (Printexc.to_string exn))
    ) (List.rev !forms)
  end;
  `List (List.rev !diags)

let publish_diagnostics t oc uri =
  let doc = Hashtbl.find t.documents uri in
  let diags = compute_diagnostics t doc.text uri in
  Lsp.write_notification oc "textDocument/publishDiagnostics"
    (`Assoc [("uri", `String uri); ("diagnostics", diags)])

(* --- Hover --- *)

let handle_hover t params =
  let assoc = match params with `Assoc a -> a | _ -> [] in
  let td = match Lsp.get_opt "textDocument" assoc with
    | `Assoc a -> a | _ -> [] in
  let uri = Lsp.get_string "uri" td in
  let pos = match Lsp.get_opt "position" assoc with
    | `Assoc a -> a | _ -> [] in
  let line = Lsp.get_int "line" pos in
  let col = Lsp.get_int "character" pos in
  match Hashtbl.find_opt t.documents uri with
  | None -> `Null
  | Some doc ->
    let tokens = Tokenizer.tokenize t.readtable doc.text in
    let offset = lsp_pos_to_offset doc.text line col in
    let tok = List.find_opt (fun (tok : Tokenizer.token) ->
      offset >= tok.span.start && offset < tok.span.stop
      && (tok.kind = Tokenizer.Symbol || tok.kind = Tokenizer.Keyword)
    ) tokens in
    match tok with
    | None -> `Null
    | Some tk ->
      let name = String.sub doc.text tk.span.start
          (tk.span.stop - tk.span.start) in
      let info = match Expander.lookup_binding t.inst.Instance.syn_env name with
        | Some Expander.Var -> Printf.sprintf "`%s` — variable" name
        | Some (Expander.Core s) ->
          Printf.sprintf "`%s` — core form (%s)" name s
        | Some (Expander.Macro _) ->
          Printf.sprintf "`%s` — syntax (macro)" name
        | None -> Printf.sprintf "`%s` — unbound" name
      in
      let start_line, start_col = offset_to_lsp_pos doc.text tk.span.start in
      let end_line, end_col = offset_to_lsp_pos doc.text tk.span.stop in
      `Assoc [
        ("contents", `Assoc [
          ("kind", `String "markdown");
          ("value", `String info)]);
        ("range", `Assoc [
          ("start", `Assoc [
            ("line", `Int start_line);
            ("character", `Int start_col)]);
          ("end", `Assoc [
            ("line", `Int end_line);
            ("character", `Int end_col)])])]

(* --- Completion --- *)

let handle_completion t params =
  let assoc = match params with `Assoc a -> a | _ -> [] in
  let td = match Lsp.get_opt "textDocument" assoc with
    | `Assoc a -> a | _ -> [] in
  let uri = Lsp.get_string "uri" td in
  let pos = match Lsp.get_opt "position" assoc with
    | `Assoc a -> a | _ -> [] in
  let line = Lsp.get_int "line" pos in
  let col = Lsp.get_int "character" pos in
  match Hashtbl.find_opt t.documents uri with
  | None -> `List []
  | Some doc ->
    let offset = lsp_pos_to_offset doc.text line col in
    (* Walk backwards to find prefix *)
    let start = ref offset in
    while !start > 0 &&
          let c = doc.text.[!start - 1] in
          c <> ' ' && c <> '\t' && c <> '\n' && c <> '\r' &&
          c <> '(' && c <> ')' && c <> '[' && c <> ']' &&
          c <> '"' && c <> '\'' && c <> '`' && c <> ','
    do decr start done;
    let prefix = String.sub doc.text !start (offset - !start) in
    let names = Expander.binding_names t.inst.Instance.syn_env in
    let matches = List.filter (fun name ->
      String.length name >= String.length prefix &&
      String.sub name 0 (String.length prefix) = prefix
    ) names in
    let items = List.map (fun name ->
      let kind = match Expander.lookup_binding t.inst.Instance.syn_env name with
        | Some (Expander.Core _) -> 14  (* Keyword *)
        | Some (Expander.Macro _) -> 15 (* Snippet *)
        | Some Expander.Var -> 3        (* Function *)
        | None -> 6                     (* Variable *)
      in
      `Assoc [
        ("label", `String name);
        ("kind", `Int kind);
      ]
    ) matches in
    `List items

(* --- Go-to-definition --- *)

let handle_definition t params =
  let assoc = match params with `Assoc a -> a | _ -> [] in
  let td = match Lsp.get_opt "textDocument" assoc with
    | `Assoc a -> a | _ -> [] in
  let uri = Lsp.get_string "uri" td in
  let pos = match Lsp.get_opt "position" assoc with
    | `Assoc a -> a | _ -> [] in
  let line = Lsp.get_int "line" pos in
  let col = Lsp.get_int "character" pos in
  match Hashtbl.find_opt t.documents uri with
  | None -> `Null
  | Some doc ->
    let tokens = Tokenizer.tokenize t.readtable doc.text in
    let offset = lsp_pos_to_offset doc.text line col in
    let marks = Highlight.analyze_semantics tokens doc.text in
    let info = Highlight.find_cursor_binding tokens doc.text offset marks in
    match info with
    | Some (_, _, Some (bind_start, bind_stop)) ->
      let bl, bc = offset_to_lsp_pos doc.text bind_start in
      let el, ec = offset_to_lsp_pos doc.text bind_stop in
      `Assoc [
        ("uri", `String uri);
        ("range", `Assoc [
          ("start", `Assoc [
            ("line", `Int bl); ("character", `Int bc)]);
          ("end", `Assoc [
            ("line", `Int el); ("character", `Int ec)])])]
    | _ -> `Null

(* --- Document symbols --- *)

let handle_document_symbols t params =
  let assoc = match params with `Assoc a -> a | _ -> [] in
  let td = match Lsp.get_opt "textDocument" assoc with
    | `Assoc a -> a | _ -> [] in
  let uri = Lsp.get_string "uri" td in
  match Hashtbl.find_opt t.documents uri with
  | None -> `List []
  | Some doc ->
    let tokens = Tokenizer.tokenize t.readtable doc.text in
    let marks = Highlight.analyze_semantics tokens doc.text in
    let symbols = ref [] in
    Hashtbl.iter (fun offset mark ->
      if mark.Highlight.role = Highlight.Defn_name then begin
        let sl, sc = offset_to_lsp_pos doc.text offset in
        let el, ec = offset_to_lsp_pos doc.text (offset + String.length mark.name) in
        symbols := `Assoc [
          ("name", `String mark.name);
          ("kind", `Int 12);  (* Function *)
          ("range", `Assoc [
            ("start", `Assoc [("line", `Int sl); ("character", `Int sc)]);
            ("end", `Assoc [("line", `Int el); ("character", `Int ec)])]);
          ("selectionRange", `Assoc [
            ("start", `Assoc [("line", `Int sl); ("character", `Int sc)]);
            ("end", `Assoc [("line", `Int el); ("character", `Int ec)])]);
        ] :: !symbols
      end
    ) marks;
    `List (List.rev !symbols)

(* --- Semantic tokens --- *)

let token_type_index kind (mark : Highlight.sem_mark option) =
  match mark with
  | Some { role = Highlight.Defn_name; _ } -> 5  (* function *)
  | Some { role = Highlight.Param; _ } -> 6      (* parameter *)
  | Some { role = Highlight.Bound_var; _ } -> 4  (* variable *)
  | None ->
    match kind with
    | Tokenizer.Keyword -> 0         (* keyword *)
    | Tokenizer.String_lit -> 1      (* string *)
    | Tokenizer.Number_lit -> 2      (* number *)
    | Tokenizer.Comment -> 3         (* comment *)
    | Tokenizer.Symbol -> 4          (* variable *)
    | Tokenizer.Boolean_lit -> 2     (* number — constants *)
    | Tokenizer.Char_lit -> 1        (* string *)
    | Tokenizer.Quote_shorthand -> 8 (* operator *)
    | _ -> -1  (* skip *)

let handle_semantic_tokens t params =
  let assoc = match params with `Assoc a -> a | _ -> [] in
  let td = match Lsp.get_opt "textDocument" assoc with
    | `Assoc a -> a | _ -> [] in
  let uri = Lsp.get_string "uri" td in
  match Hashtbl.find_opt t.documents uri with
  | None -> `Assoc [("data", `List [])]
  | Some doc ->
    let tokens = Tokenizer.tokenize t.readtable doc.text in
    let marks = Highlight.analyze_semantics tokens doc.text in
    let data = ref [] in
    let prev_line = ref 0 in
    let prev_start = ref 0 in
    List.iter (fun (tok : Tokenizer.token) ->
      let mark = Hashtbl.find_opt marks tok.span.start in
      let type_idx = token_type_index tok.kind mark in
      if type_idx >= 0 then begin
        let tok_line, tok_col = offset_to_lsp_pos doc.text tok.span.start in
        let tok_len = tok.span.stop - tok.span.start in
        let delta_line = tok_line - !prev_line in
        let delta_start =
          if delta_line = 0 then tok_col - !prev_start
          else tok_col
        in
        let modifiers =
          match mark with
          | Some { role = Highlight.Defn_name; _ } -> 1  (* definition *)
          | _ -> 0
        in
        data := `Int modifiers :: `Int type_idx ::
                `Int tok_len :: `Int delta_start :: `Int delta_line :: !data;
        prev_line := tok_line;
        prev_start := tok_col
      end
    ) tokens;
    `Assoc [("data", `List (List.rev !data))]

(* --- Capabilities --- *)

let capabilities_json =
  `Assoc [
    ("capabilities", `Assoc [
      ("textDocumentSync", `Int 1);
      ("hoverProvider", `Bool true);
      ("completionProvider", `Assoc [
        ("triggerCharacters", `List [`String "("])]);
      ("definitionProvider", `Bool true);
      ("documentSymbolProvider", `Bool true);
      ("semanticTokensProvider", `Assoc [
        ("legend", `Assoc [
          ("tokenTypes", `List [
            `String "keyword"; `String "string"; `String "number";
            `String "comment"; `String "variable"; `String "function";
            `String "parameter"; `String "type"; `String "operator"]);
          ("tokenModifiers", `List [
            `String "definition"; `String "readonly"])]);
        ("full", `Bool true)])]);
    ("serverInfo", `Assoc [
      ("name", `String "wile");
      ("version", `String "0.1.0")])]

(* --- Session loop --- *)

let run_session t ic oc =
  (* Phase 1: Wait for initialize request *)
  let rec wait_initialize () =
    match Lsp.read_message ic with
    | Lsp.Request req when req.method_ = "initialize" ->
      Lsp.write_response oc (Lsp.make_response req.id capabilities_json)
    | Lsp.Request req ->
      Lsp.write_response oc
        (Lsp.make_error_response req.id Lsp.method_not_found
           "server not initialized");
      wait_initialize ()
    | Lsp.Notification _ -> wait_initialize ()
    | Lsp.Response _ -> wait_initialize ()
  in
  wait_initialize ();
  (* Phase 2: Wait for initialized notification *)
  let rec wait_initialized () =
    match Lsp.read_message ic with
    | Lsp.Notification n when n.method_ = "initialized" -> ()
    | _ -> wait_initialized ()
  in
  wait_initialized ();
  (* Phase 3: Main dispatch loop *)
  let shutdown = ref false in
  let running = ref true in
  while !running do
    let msg =
      try Some (Lsp.read_message ic)
      with Lsp.Lsp_error _ | End_of_file -> None
    in
    match msg with
    | None -> running := false
    | Some (Lsp.Request req) ->
      if !shutdown then begin
        Lsp.write_response oc
          (Lsp.make_error_response req.id Lsp.internal_error
             "server is shutting down");
      end else begin
        match req.method_ with
        | "shutdown" ->
          shutdown := true;
          Lsp.write_response oc (Lsp.make_response req.id `Null)
        | "textDocument/hover" ->
          let result = handle_hover t req.params in
          Lsp.write_response oc (Lsp.make_response req.id result)
        | "textDocument/completion" ->
          let result = handle_completion t req.params in
          Lsp.write_response oc (Lsp.make_response req.id result)
        | "textDocument/definition" ->
          let result = handle_definition t req.params in
          Lsp.write_response oc (Lsp.make_response req.id result)
        | "textDocument/documentSymbol" ->
          let result = handle_document_symbols t req.params in
          Lsp.write_response oc (Lsp.make_response req.id result)
        | "textDocument/semanticTokens/full" ->
          let result = handle_semantic_tokens t req.params in
          Lsp.write_response oc (Lsp.make_response req.id result)
        | _ ->
          Lsp.write_response oc
            (Lsp.make_error_response req.id Lsp.method_not_found
               (Printf.sprintf "unknown method: %s" req.method_))
      end
    | Some (Lsp.Notification n) ->
      if n.method_ = "exit" then
        running := false
      else begin
        match n.method_ with
        | "textDocument/didOpen" ->
          let p = match n.params with `Assoc a -> a | _ -> [] in
          let td = match Lsp.get_opt "textDocument" p with
            | `Assoc a -> a | _ -> [] in
          let uri = Lsp.get_string "uri" td in
          let text = Lsp.get_string "text" td in
          let version = match List.assoc_opt "version" td with
            | Some (`Int v) -> v | _ -> 0 in
          Hashtbl.replace t.documents uri { uri; version; text };
          publish_diagnostics t oc uri
        | "textDocument/didChange" ->
          let p = match n.params with `Assoc a -> a | _ -> [] in
          let td = match Lsp.get_opt "textDocument" p with
            | `Assoc a -> a | _ -> [] in
          let uri = Lsp.get_string "uri" td in
          let version = match List.assoc_opt "version" td with
            | Some (`Int v) -> v | _ -> 0 in
          let changes = match List.assoc_opt "contentChanges" p with
            | Some (`List l) -> l | _ -> [] in
          (* Full sync: take the last change's text *)
          let text = List.fold_left (fun _acc change ->
            match change with
            | `Assoc a -> Lsp.get_string "text" a
            | _ -> ""
          ) "" changes in
          (match Hashtbl.find_opt t.documents uri with
           | Some doc -> doc.version <- version; doc.text <- text
           | None -> Hashtbl.replace t.documents uri { uri; version; text });
          publish_diagnostics t oc uri
        | "textDocument/didSave" ->
          let p = match n.params with `Assoc a -> a | _ -> [] in
          let td = match Lsp.get_opt "textDocument" p with
            | `Assoc a -> a | _ -> [] in
          let uri = Lsp.get_string "uri" td in
          if Hashtbl.mem t.documents uri then
            publish_diagnostics t oc uri
        | "textDocument/didClose" ->
          let p = match n.params with `Assoc a -> a | _ -> [] in
          let td = match Lsp.get_opt "textDocument" p with
            | `Assoc a -> a | _ -> [] in
          let uri = Lsp.get_string "uri" td in
          Hashtbl.remove t.documents uri;
          (* Clear diagnostics *)
          Lsp.write_notification oc "textDocument/publishDiagnostics"
            (`Assoc [("uri", `String uri); ("diagnostics", `List [])])
        | _ -> ()  (* Ignore unknown notifications *)
      end
    | Some (Lsp.Response _) ->
      ()  (* Ignore unsolicited responses *)
  done
