exception Lockfile_error of string

let error msg = raise (Lockfile_error msg)
let errorf fmt = Printf.ksprintf error fmt

type locked_package = {
  name : string;
  version : Semver.t;
  sha256 : string;
}

type t = {
  created : string;
  packages : locked_package list;
}

type mismatch =
  | Hash_mismatch of { name : string; expected : string; actual : string }
  | Not_installed of { name : string; version : string }

(* --- Path --- *)

let lockfile_path project_dir =
  Filename.concat project_dir "bilk.lock"

(* --- S-expression helpers --- *)


(* --- Parse --- *)

let parse readtable path =
  let port = Port.of_file path in
  let sexp = Reader.read_syntax readtable port in
  match Syntax.to_proper_list sexp with
  | Some ({ Syntax.datum = Syntax.Symbol "lock"; _ } :: clauses) ->
    let created = ref None in
    let packages = ref [] in
    List.iter (fun clause ->
      match Syntax.to_proper_list clause with
      | Some ({ datum = Syntax.Symbol key; _ } :: vals) ->
        (match key with
         | "created" ->
           (match vals with
            | [{ datum = Syntax.Str s; _ }] -> created := Some s
            | _ -> error "created: expected a single string")
         | "packages" ->
           packages := List.map (fun pkg_syn ->
             match Syntax.to_proper_list pkg_syn with
             | Some ({ datum = Syntax.Symbol pkg_name; _ }
                     :: { datum = Syntax.Str ver_str; _ }
                     :: rest) ->
               let sha256 = match rest with
                 | [hash_clause] ->
                   (match Syntax.to_proper_list hash_clause with
                    | Some [{ datum = Syntax.Symbol "sha256"; _ };
                            { datum = Syntax.Str hash; _ }] -> hash
                    | _ -> error "expected (sha256 \"...\")")
                 | _ -> error "expected (name \"version\" (sha256 \"...\"))"
               in
               let version =
                 try Semver.parse ver_str
                 with Semver.Parse_error _ ->
                   errorf "invalid version %S" ver_str
               in
               { name = pkg_name; version; sha256 }
             | _ -> error "expected (name \"version\" (sha256 \"...\"))"
           ) vals
         | _ -> errorf "unknown clause: %s" key)
      | _ -> error "clause: expected (key value ...)"
    ) clauses;
    let created = match !created with
      | Some c -> c
      | None -> error "missing required field: created"
    in
    { created; packages = !packages }
  | _ -> error "expected (lock ...)"

(* --- Write --- *)

let write path lock =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    Printf.fprintf oc "(lock\n";
    Printf.fprintf oc "  (created \"%s\")\n" lock.created;
    Printf.fprintf oc "  (packages";
    if lock.packages = [] then
      Printf.fprintf oc ")"
    else begin
      Printf.fprintf oc "\n";
      List.iter (fun lp ->
        Printf.fprintf oc "    (%s \"%s\"\n"
          lp.name (Semver.to_string lp.version);
        Printf.fprintf oc "      (sha256 \"%s\"))\n" lp.sha256
      ) lock.packages;
      Printf.fprintf oc "  )"
    end;
    Printf.fprintf oc ")\n")

(* --- Content hashing --- *)

let hash_directory path =
  let cmd = Printf.sprintf
    "find %s -type f -print0 | sort -z | xargs -0 sha256sum | sha256sum"
    (Filename.quote path) in
  let ic = Unix.open_process_in cmd in
  Fun.protect ~finally:(fun () -> ignore (Unix.close_process_in ic))
    (fun () ->
       let line =
         try input_line ic
         with End_of_file -> error "sha256sum produced no output"
       in
       match String.split_on_char ' ' line with
       | hash :: _ when String.length hash = 64 -> hash
       | _ -> errorf "unexpected sha256sum output: %s" line)

(* --- Lock creation --- *)

let iso8601_now () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let version_dir ~registry_root ~name ~version =
  Filename.concat
    (Filename.concat registry_root name)
    version

let create ~registry_root deps =
  let resolved = Pkg_manager.resolve ~registry_root deps in
  let packages = List.map (fun (name, ver) ->
    let ver_str = Semver.to_string ver in
    let dir = version_dir ~registry_root ~name ~version:ver_str in
    let sha256 = hash_directory dir in
    { name; version = ver; sha256 }
  ) resolved in
  { created = iso8601_now (); packages }

(* --- Verification --- *)

let verify ~registry_root lock =
  List.filter_map (fun lp ->
    let ver_str = Semver.to_string lp.version in
    let dir = version_dir ~registry_root ~name:lp.name ~version:ver_str in
    if not (Sys.file_exists dir) then
      Some (Not_installed { name = lp.name; version = ver_str })
    else begin
      let actual = hash_directory dir in
      if actual <> lp.sha256 then
        Some (Hash_mismatch { name = lp.name;
                              expected = lp.sha256;
                              actual })
      else
        None
    end
  ) lock.packages
