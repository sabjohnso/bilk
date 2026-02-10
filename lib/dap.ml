exception Dap_error of string

type request = {
  seq : int;
  command : string;
  arguments : Yojson.Safe.t;
}

type response = {
  seq : int;
  request_seq : int;
  success : bool;
  command : string;
  message : string option;
  body : Yojson.Safe.t;
}

type event = {
  seq : int;
  event_name : string;
  body : Yojson.Safe.t;
}

type message =
  | Request of request
  | Response of response
  | Event of event

(* --- JSON helpers --- *)

let get_string key assoc =
  match List.assoc_opt key assoc with
  | Some (`String s) -> s
  | _ -> raise (Dap_error (Printf.sprintf "missing or invalid field: %s" key))

let get_int key assoc =
  match List.assoc_opt key assoc with
  | Some (`Int n) -> n
  | _ -> raise (Dap_error (Printf.sprintf "missing or invalid field: %s" key))

let get_opt_string key assoc =
  match List.assoc_opt key assoc with
  | Some (`String s) -> Some s
  | _ -> None

let get_opt key assoc =
  match List.assoc_opt key assoc with
  | Some v -> v
  | None -> `Null

let request_of_json json =
  match json with
  | `Assoc assoc ->
    { seq = get_int "seq" assoc;
      command = get_string "command" assoc;
      arguments = get_opt "arguments" assoc }
  | _ -> raise (Dap_error "request must be a JSON object")

let response_of_json json =
  match json with
  | `Assoc assoc ->
    { seq = get_int "seq" assoc;
      request_seq = get_int "request_seq" assoc;
      success = (match List.assoc_opt "success" assoc with
                 | Some (`Bool b) -> b
                 | _ -> raise (Dap_error "missing or invalid field: success"));
      command = get_string "command" assoc;
      message = get_opt_string "message" assoc;
      body = get_opt "body" assoc }
  | _ -> raise (Dap_error "response must be a JSON object")

let event_of_json json =
  match json with
  | `Assoc assoc ->
    { seq = get_int "seq" assoc;
      event_name = get_string "event" assoc;
      body = get_opt "body" assoc }
  | _ -> raise (Dap_error "event must be a JSON object")

let message_of_json json =
  match json with
  | `Assoc assoc ->
    (match get_string "type" assoc with
     | "request" -> Request (request_of_json json)
     | "response" -> Response (response_of_json json)
     | "event" -> Event (event_of_json json)
     | t -> raise (Dap_error (Printf.sprintf "unknown message type: %s" t)))
  | _ -> raise (Dap_error "message must be a JSON object")

let response_to_json (resp : response) =
  let fields = [
    ("seq", `Int resp.seq);
    ("type", `String "response");
    ("request_seq", `Int resp.request_seq);
    ("success", `Bool resp.success);
    ("command", `String resp.command);
  ] in
  let fields = match resp.message with
    | Some m -> fields @ [("message", `String m)]
    | None -> fields
  in
  let fields = match resp.body with
    | `Null -> fields
    | body -> fields @ [("body", body)]
  in
  `Assoc fields

let event_to_json evt =
  let fields = [
    ("seq", `Int evt.seq);
    ("type", `String "event");
    ("event", `String evt.event_name);
  ] in
  let fields = match evt.body with
    | `Null -> fields
    | body -> fields @ [("body", body)]
  in
  `Assoc fields

let make_response seq_ref (req : request) success body =
  let s = !seq_ref in
  incr seq_ref;
  { seq = s;
    request_seq = req.seq;
    success;
    command = req.command;
    message = None;
    body }

let make_event seq_ref name body =
  let s = !seq_ref in
  incr seq_ref;
  { seq = s;
    event_name = name;
    body }

(* --- Wire format --- *)

let read_content_length ic =
  let line =
    try input_line ic
    with End_of_file -> raise (Dap_error "unexpected end of input")
  in
  (* Strip trailing \r if present (Content-Length: N\r) *)
  let line = if String.length line > 0 && line.[String.length line - 1] = '\r'
    then String.sub line 0 (String.length line - 1)
    else line
  in
  let prefix = "Content-Length: " in
  let plen = String.length prefix in
  if String.length line < plen ||
     String.sub line 0 plen <> prefix then
    raise (Dap_error (Printf.sprintf "expected Content-Length header, got: %s" line));
  let num_str = String.sub line plen (String.length line - plen) in
  match int_of_string_opt num_str with
  | Some n when n >= 0 -> n
  | _ -> raise (Dap_error (Printf.sprintf "invalid Content-Length: %s" num_str))

let read_message ic =
  let len = read_content_length ic in
  (* Read blank separator line *)
  let sep =
    try input_line ic
    with End_of_file -> raise (Dap_error "unexpected end of input after header")
  in
  (* Accept empty line or just \r *)
  let sep = String.trim sep in
  if sep <> "" then
    raise (Dap_error (Printf.sprintf "expected blank line after header, got: %s" sep));
  let buf = Bytes.create len in
  let rec read_all off remaining =
    if remaining > 0 then begin
      let n = input ic buf off remaining in
      if n = 0 then raise (Dap_error "unexpected end of input reading body");
      read_all (off + n) (remaining - n)
    end
  in
  read_all 0 len;
  let body_str = Bytes.to_string buf in
  let json =
    try Yojson.Safe.from_string body_str
    with Yojson.Json_error msg ->
      raise (Dap_error (Printf.sprintf "JSON parse error: %s" msg))
  in
  message_of_json json

let write_message_json oc seq_ref json =
  let body = Yojson.Safe.to_string json in
  let header = Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body) in
  output_string oc header;
  output_string oc body;
  flush oc;
  ignore seq_ref

let write_response oc seq_ref resp =
  let json = response_to_json resp in
  write_message_json oc seq_ref json

let write_event oc seq_ref evt =
  let json = event_to_json evt in
  write_message_json oc seq_ref json
