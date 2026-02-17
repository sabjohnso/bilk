(* SSH connect-line parser — pure parsing, no I/O. *)

type connect_info = {
  port : int;
  key : string;
}

type connect_error =
  | No_bilk_on_remote of string
  | Ssh_failed of string
  | Invalid_connect_line of string

(* Split on whitespace, discarding empty tokens (handles multiple spaces). *)
let split_words s =
  String.split_on_char ' ' s
  |> List.filter (fun w -> w <> "")

let parse_connect_line s =
  match split_words s with
  | ["BILK"; "CONNECT"; port_str; key] ->
    (match int_of_string_opt port_str with
     | Some port when port >= 1 && port <= 65535 -> Some { port; key }
     | _ -> None)
  | _ -> None

let parse_target s =
  match String.split_on_char '@' s with
  | [user; host] when user <> "" && host <> "" -> Some (user, host)
  | _ -> None

let scan_for_connect lines =
  let rec search = function
    | [] -> Error (No_bilk_on_remote "no BILK CONNECT line found")
    | line :: rest ->
      if String.length line >= 12
         && String.sub line 0 12 = "BILK CONNECT" then
        match parse_connect_line line with
        | Some info -> Ok info
        | None -> Error (Invalid_connect_line line)
      else
        search rest
  in
  search lines

(* --- Typed serve configuration --- *)

type serve_config = {
  port : int;
  auto_checkpoint : bool;
  name : string;
  session_timeout : int;
}

let build_serve_args config =
  ["--port"; string_of_int config.port]
  @ (if config.auto_checkpoint then ["--auto-checkpoint"] else [])
  @ ["--name"; config.name]
  @ ["--session-timeout"; string_of_int config.session_timeout]
