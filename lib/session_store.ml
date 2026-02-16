exception Store_error of string

let error msg = raise (Store_error msg)
let errorf fmt = Printf.ksprintf error fmt

(* --- Name validation --- *)

let has_separator s =
  let len = String.length s in
  let rec loop i =
    if i >= len then false
    else match s.[i] with
      | '/' | '\\' | '\x00' -> true
      | _ -> loop (i + 1)
  in
  loop 0

let validate_name name =
  if name = "" then Error "session name must not be empty"
  else if name = "." || name = ".." then Error "invalid session name"
  else if has_separator name then Error "session name contains invalid characters"
  else Ok name

(* --- Path helpers --- *)

let sessions_dir ~home = Filename.concat home "sessions"

let session_path ~home name =
  Filename.concat (sessions_dir ~home) (name ^ ".bses")

let ensure_dir path =
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o755
  else if not (Sys.is_directory path) then
    errorf "%s exists but is not a directory" path

(* --- Store operations --- *)

let save ~home session name =
  match validate_name name with
  | Error msg -> error msg
  | Ok name ->
    ensure_dir (sessions_dir ~home);
    let path = session_path ~home name in
    (try Session.save session path
     with Sys_error msg -> error msg)

let load ~home name =
  match validate_name name with
  | Error msg -> error msg
  | Ok name ->
    let path = session_path ~home name in
    if not (Sys.file_exists path) then
      errorf "session not found: %s" name;
    (try Session.load path
     with
     | Session.Session_error msg -> error msg
     | Sys_error msg -> error msg)

let list ~home () =
  let dir = sessions_dir ~home in
  if not (Sys.file_exists dir) then []
  else begin
    let entries = Sys.readdir dir in
    let names = Array.to_list entries
      |> List.filter_map (fun entry ->
           if Filename.check_suffix entry ".bses" then
             Some (Filename.chop_suffix entry ".bses")
           else
             None)
    in
    List.sort String.compare names
  end

let delete ~home name =
  match validate_name name with
  | Error msg -> error msg
  | Ok name ->
    let path = session_path ~home name in
    (try Sys.remove path
     with Sys_error msg -> error msg)
