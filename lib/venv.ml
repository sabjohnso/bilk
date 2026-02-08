type config = {
  wile_version : string;
  created : string;
}

exception Venv_error of string

let cfg_name = "wile-venv.cfg"

let lib_path dir = Filename.concat dir "lib"

let is_venv dir =
  Sys.file_exists (Filename.concat dir cfg_name)

let mkdir_p dir =
  (* Create directory and parents if needed *)
  let rec ensure d =
    if not (Sys.file_exists d) then begin
      ensure (Filename.dirname d);
      Sys.mkdir d 0o755
    end
  in
  ensure dir

let create ~wile_version dir =
  if is_venv dir then
    raise (Venv_error (Printf.sprintf
      "directory already contains a virtual environment: %s" dir));
  mkdir_p dir;
  let lib_dir = lib_path dir in
  if not (Sys.file_exists lib_dir) then
    Sys.mkdir lib_dir 0o755;
  let cfg_path = Filename.concat dir cfg_name in
  let now =
    let t = Unix.gettimeofday () in
    let tm = Unix.gmtime t in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec
  in
  let oc = open_out cfg_path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    Printf.fprintf oc "wile-version = %s\n" wile_version;
    Printf.fprintf oc "created = %s\n" now)

let read_config dir =
  let cfg_path = Filename.concat dir cfg_name in
  if not (Sys.file_exists cfg_path) then
    raise (Venv_error (Printf.sprintf
      "not a virtual environment (missing %s): %s" cfg_name dir));
  let ic = open_in cfg_path in
  let wile_version = ref "" in
  let created = ref "" in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    try while true do
      let line = input_line ic in
      match String.split_on_char '=' line with
      | [key; value] ->
        let k = String.trim key in
        let v = String.trim value in
        if k = "wile-version" then wile_version := v
        else if k = "created" then created := v
      | _ -> ()
    done with End_of_file -> ());
  if !wile_version = "" then
    raise (Venv_error (Printf.sprintf
      "malformed %s: missing wile-version in %s" cfg_name dir));
  if !created = "" then
    raise (Venv_error (Printf.sprintf
      "malformed %s: missing created in %s" cfg_name dir));
  { wile_version = !wile_version; created = !created }
