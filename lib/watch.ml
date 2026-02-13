(* --- inotify C bindings --- *)

(* Unix.file_descr and int have the same representation (Val_int),
   so we declare the externals with the types we actually want. *)
external inotify_init : unit -> Unix.file_descr = "bilk_inotify_init"
external inotify_add_watch : Unix.file_descr -> string -> int64 -> int
  = "bilk_inotify_add_watch"
external inotify_rm_watch : Unix.file_descr -> int -> unit
  = "bilk_inotify_rm_watch"
external inotify_read : Unix.file_descr -> (int * int64 * string) list
  = "bilk_inotify_read"

external in_modify : unit -> int64 = "bilk_in_modify"
external in_create : unit -> int64 = "bilk_in_create"
external in_delete : unit -> int64 = "bilk_in_delete"
external in_moved_to : unit -> int64 = "bilk_in_moved_to"
external in_moved_from : unit -> int64 = "bilk_in_moved_from"
external in_isdir : unit -> int64 = "bilk_in_isdir"

(* Cache mask constants at module init to avoid repeated allocation *)
let mask_modify = in_modify ()
let mask_create = in_create ()
let mask_delete = in_delete ()
let mask_moved_to = in_moved_to ()
let mask_moved_from = in_moved_from ()
let mask_isdir = in_isdir ()

let inotify_watch_mask =
  Int64.logor mask_modify
    (Int64.logor mask_create
       (Int64.logor mask_delete
          (Int64.logor mask_moved_to mask_moved_from)))

type event = {
  path : string;
  kind : [ `Modified | `Created | `Deleted ];
}

type backend = Inotify | Polling

(* Mtime snapshot for polling: path -> mtime *)
type mtime_map = (string, float) Hashtbl.t

type t = {
  backend : backend;
  extensions : string list;
  poll_interval_ms : int;
  mutable inotify_fd : Unix.file_descr option;
  (* inotify: wd -> directory path *)
  wd_to_dir : (int, string) Hashtbl.t;
  (* polling: directories to scan *)
  poll_dirs : string list ref;
  (* polling: last known mtimes *)
  last_mtimes : mtime_map;
}

let has_matching_ext extensions path =
  List.exists (fun ext -> Filename.check_suffix path ext) extensions

(* Recursively list subdirectories *)
let rec list_dirs_recursive dir =
  let entries = try Sys.readdir dir with Sys_error _ -> [||] in
  let subdirs = Array.fold_left (fun acc name ->
    let full = Filename.concat dir name in
    if try Sys.is_directory full with Sys_error _ -> false then
      full :: list_dirs_recursive full @ acc
    else acc
  ) [] entries in
  dir :: subdirs

(* Scan a directory tree for files matching extensions, returning path -> mtime *)
let scan_mtimes extensions dirs =
  let tbl = Hashtbl.create 64 in
  List.iter (fun dir ->
    let all_dirs = list_dirs_recursive dir in
    List.iter (fun d ->
      let entries = try Sys.readdir d with Sys_error _ -> [||] in
      Array.iter (fun name ->
        if has_matching_ext extensions name then begin
          let full = Filename.concat d name in
          let mtime = try (Unix.stat full).Unix.st_mtime
                      with Unix.Unix_error _ -> 0.0 in
          Hashtbl.replace tbl full mtime
        end
      ) entries
    ) all_dirs
  ) dirs;
  tbl

(* Deduplicate events, keeping the first event for each path.
   The first event is semantically meaningful (e.g. Created before
   Modified means the file was just created). *)
let dedup_events events =
  let tbl = Hashtbl.create 16 in
  let order = ref [] in
  List.iter (fun (e : event) ->
    if not (Hashtbl.mem tbl e.path) then begin
      order := e.path :: !order;
      Hashtbl.replace tbl e.path e
    end
  ) events;
  List.filter_map (fun path -> Hashtbl.find_opt tbl path) (List.rev !order)

let create ?(poll_interval_ms = 200) ?(extensions = [".sld"; ".scm"]) () =
  let inotify_fd =
    try Some (inotify_init ()) with Unix.Unix_error _ -> None
  in
  let backend = match inotify_fd with Some _ -> Inotify | None -> Polling in
  { backend;
    extensions;
    poll_interval_ms;
    inotify_fd;
    wd_to_dir = Hashtbl.create 16;
    poll_dirs = ref [];
    last_mtimes = Hashtbl.create 64;
  }

let destroy t =
  match t.inotify_fd with
  | None -> ()
  | Some fd ->
    Hashtbl.iter (fun wd _dir ->
      (try inotify_rm_watch fd wd with _ -> ())
    ) t.wd_to_dir;
    (try Unix.close fd with _ -> ());
    t.inotify_fd <- None

let add_directory_inotify t fd dir =
  let all_dirs = list_dirs_recursive dir in
  List.iter (fun d ->
    let wd = try inotify_add_watch fd d inotify_watch_mask
             with Unix.Unix_error _ -> -1 in
    if wd >= 0 then
      Hashtbl.replace t.wd_to_dir wd d
    (* wd = -1 means ENOSPC, silently skip — polling will cover *)
  ) all_dirs

let add_directory_polling t dir =
  t.poll_dirs := dir :: !(t.poll_dirs);
  (* Take initial mtime snapshot *)
  let mtimes = scan_mtimes t.extensions [dir] in
  Hashtbl.iter (fun k v -> Hashtbl.replace t.last_mtimes k v) mtimes

let add_directory t dir =
  match t.backend, t.inotify_fd with
  | Inotify, Some fd -> add_directory_inotify t fd dir
  | _ -> add_directory_polling t dir

let add_directories t dirs =
  List.iter (add_directory t) dirs

(* --- Polling backend --- *)

let poll_events t =
  let current = scan_mtimes t.extensions !(t.poll_dirs) in
  let events = ref [] in
  (* Check for modifications and creations *)
  Hashtbl.iter (fun path mtime ->
    match Hashtbl.find_opt t.last_mtimes path with
    | None ->
      events := { path; kind = `Created } :: !events
    | Some old_mtime ->
      if mtime > old_mtime then
        events := { path; kind = `Modified } :: !events
  ) current;
  (* Check for deletions *)
  Hashtbl.iter (fun path _mtime ->
    if not (Hashtbl.mem current path) then
      events := { path; kind = `Deleted } :: !events
  ) t.last_mtimes;
  (* Update snapshot *)
  Hashtbl.reset t.last_mtimes;
  Hashtbl.iter (fun k v -> Hashtbl.replace t.last_mtimes k v) current;
  !events

(* --- Inotify backend --- *)

let inotify_events t =
  match t.inotify_fd with
  | None -> []
  | Some fd ->
    let raw = try inotify_read fd with Unix.Unix_error _ -> [] in
    let events = ref [] in
    List.iter (fun (wd, mask, name) ->
      (* New subdirectory — add a watch for it *)
      if Int64.logand mask mask_isdir <> 0L
         && Int64.logand mask mask_create <> 0L then begin
        match Hashtbl.find_opt t.wd_to_dir wd with
        | Some dir ->
          let new_dir = Filename.concat dir name in
          add_directory_inotify t fd new_dir
        | None -> ()
      end;
      if name <> "" && has_matching_ext t.extensions name then begin
        let dir = match Hashtbl.find_opt t.wd_to_dir wd with
          | Some d -> d
          | None -> "."
        in
        let path = Filename.concat dir name in
        let kind =
          if Int64.logand mask mask_modify <> 0L
             || Int64.logand mask mask_moved_to <> 0L then `Modified
          else if Int64.logand mask mask_create <> 0L then `Created
          else if Int64.logand mask mask_delete <> 0L
                  || Int64.logand mask mask_moved_from <> 0L then `Deleted
          else `Modified  (* fallback *)
        in
        events := { path; kind } :: !events
      end
    ) raw;
    List.rev !events

(* --- Public API --- *)

let poll t =
  let events = match t.backend with
    | Polling -> poll_events t
    | Inotify -> inotify_events t
  in
  dedup_events events

let wait t =
  match t.backend with
  | Polling ->
    let interval_s = float_of_int t.poll_interval_ms /. 1000.0 in
    let rec loop () =
      Unix.sleepf interval_s;
      let events = poll_events t in
      if events = [] then loop ()
      else dedup_events events
    in
    loop ()
  | Inotify ->
    match t.inotify_fd with
    | None -> [] (* unreachable: backend=Inotify implies fd=Some *)
    | Some fd ->
      let rec loop () =
        let ready, _, _ = Unix.select [fd] [] [] 1.0 in
        if ready = [] then loop ()
        else begin
          (* Small debounce: wait a bit then drain all events *)
          Unix.sleepf 0.05;
          let events = inotify_events t in
          if events = [] then loop ()
          else dedup_events events
        end
      in
      loop ()

let fd t = t.inotify_fd

let backend t = t.backend
