exception Lsp_error of string

type request = {
  id : Yojson.Safe.t;
  method_ : string;
  params : Yojson.Safe.t;
}

type notification = {
  method_ : string;
  params : Yojson.Safe.t;
}

type response = {
  id : Yojson.Safe.t;
  result : Yojson.Safe.t;
  error : response_error option;
}

and response_error = {
  code : int;
  message : string;
  data : Yojson.Safe.t;
}

type message =
  | Request of request
  | Notification of notification
  | Response of response

(* --- JSON helpers --- *)

let get_string key assoc =
  match List.assoc_opt key assoc with
  | Some (`String s) -> s
  | _ -> raise (Lsp_error (Printf.sprintf "missing or invalid field: %s" key))

let get_int key assoc =
  match List.assoc_opt key assoc with
  | Some (`Int n) -> n
  | _ -> raise (Lsp_error (Printf.sprintf "missing or invalid field: %s" key))

let get_opt key assoc =
  match List.assoc_opt key assoc with
  | Some v -> v
  | None -> `Null

(* --- Standard error codes --- *)

let method_not_found = -32601
let internal_error = -32603

(* --- Message parsing --- *)

let message_of_json json =
  match json with
  | `Assoc assoc ->
    let method_opt = match List.assoc_opt "method" assoc with
      | Some (`String s) -> Some s
      | _ -> None
    in
    let id_opt = List.assoc_opt "id" assoc in
    (match method_opt, id_opt with
     | Some m, Some id ->
       (* Request: has both method and id *)
       Request { id; method_ = m;
                 params = get_opt "params" assoc }
     | Some m, None ->
       (* Notification: has method but no id *)
       Notification { method_ = m;
                      params = get_opt "params" assoc }
     | None, Some id ->
       (* Response: has id but no method *)
       let error = match List.assoc_opt "error" assoc with
         | Some (`Assoc err) ->
           Some { code = get_int "code" err;
                  message = get_string "message" err;
                  data = get_opt "data" err }
         | _ -> None
       in
       Response { id;
                  result = get_opt "result" assoc;
                  error }
     | None, None ->
       raise (Lsp_error "message must have 'method' or 'id' field"))
  | _ -> raise (Lsp_error "message must be a JSON object")

(* --- Response construction --- *)

let make_response id result =
  { id; result; error = None }

let make_error_response id code message =
  { id; result = `Null;
    error = Some { code; message; data = `Null } }

(* --- Wire format --- *)

let read_content_length ic =
  let line =
    try input_line ic
    with End_of_file -> raise (Lsp_error "unexpected end of input")
  in
  let line = if String.length line > 0 && line.[String.length line - 1] = '\r'
    then String.sub line 0 (String.length line - 1)
    else line
  in
  let prefix = "Content-Length: " in
  let plen = String.length prefix in
  if String.length line < plen ||
     String.sub line 0 plen <> prefix then
    raise (Lsp_error (Printf.sprintf "expected Content-Length header, got: %s" line));
  let num_str = String.sub line plen (String.length line - plen) in
  match int_of_string_opt num_str with
  | Some n when n >= 0 -> n
  | _ -> raise (Lsp_error (Printf.sprintf "invalid Content-Length: %s" num_str))

let read_message ic =
  let len = read_content_length ic in
  let sep =
    try input_line ic
    with End_of_file -> raise (Lsp_error "unexpected end of input after header")
  in
  let sep = String.trim sep in
  if sep <> "" then
    raise (Lsp_error (Printf.sprintf "expected blank line after header, got: %s" sep));
  let buf = Bytes.create len in
  let rec read_all off remaining =
    if remaining > 0 then begin
      let n = input ic buf off remaining in
      if n = 0 then raise (Lsp_error "unexpected end of input reading body");
      read_all (off + n) (remaining - n)
    end
  in
  read_all 0 len;
  let body_str = Bytes.to_string buf in
  let json =
    try Yojson.Safe.from_string body_str
    with Yojson.Json_error msg ->
      raise (Lsp_error (Printf.sprintf "JSON parse error: %s" msg))
  in
  message_of_json json

let write_json oc json =
  let body = Yojson.Safe.to_string json in
  let header = Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body) in
  output_string oc header;
  output_string oc body;
  flush oc

let response_to_json resp =
  let fields = [("jsonrpc", `String "2.0"); ("id", resp.id)] in
  match resp.error with
  | Some err ->
    let err_fields = [
      ("code", `Int err.code);
      ("message", `String err.message);
    ] in
    let err_fields = match err.data with
      | `Null -> err_fields
      | d -> err_fields @ [("data", d)]
    in
    `Assoc (fields @ [("error", `Assoc err_fields)])
  | None ->
    `Assoc (fields @ [("result", resp.result)])

let write_response oc resp =
  write_json oc (response_to_json resp)

let write_notification oc method_ params =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
    ("params", params);
  ] in
  write_json oc json
