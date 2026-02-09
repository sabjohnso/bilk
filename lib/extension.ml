exception Extension_error of string

let extension_error msg = raise (Extension_error msg)

(* ---- Static registry ---- *)

(* Process-global table of statically-linked extensions.
   This is an intentional exception to the "no module-level mutable globals"
   rule: extensions register themselves at OCaml module-init time, before
   any Instance exists. *)
let static_registry : (string, Instance.t -> unit) Hashtbl.t =
  Hashtbl.create 16

let register_static name init_fn =
  Hashtbl.replace static_registry name init_fn

let init_static inst name =
  match Hashtbl.find_opt static_registry name with
  | Some init_fn -> init_fn inst; true
  | None -> false

(* ---- File search ---- *)

let find_file ~search_dirs ~sld_dir basename =
  let try_dir dir =
    let path = Filename.concat dir basename in
    if Sys.file_exists path then Some path else None
  in
  let result = match sld_dir with
    | Some dir -> try_dir dir
    | None -> None
  in
  match result with
  | Some _ -> result
  | None ->
    let rec search = function
      | [] -> None
      | dir :: rest ->
        match try_dir dir with
        | Some _ as found -> found
        | None -> search rest
    in
    search search_dirs

(* ---- Dynlink loading (.cmxs) ---- *)

let load_cmxs inst name path =
  (try Dynlink.loadfile path
   with Dynlink.Error e ->
     extension_error (Printf.sprintf "failed to load %s: %s"
       path (Dynlink.error_message e)));
  if not (init_static inst name) then
    extension_error (Printf.sprintf
      "extension %s loaded from %s but did not call register_static" name path)

(* ---- C extension loading (.so) ---- *)

external ext_dlopen : string -> nativeint = "wile_ext_dlopen"
external ext_dlsym : nativeint -> string -> nativeint = "wile_ext_dlsym"
external ext_dlclose : nativeint -> unit = "wile_ext_dlclose"
external ext_call_init : nativeint -> int -> unit = "wile_ext_call_init"

(* Forward references filled in by Wile_c_api to break the dependency cycle:
   Extension -> Instance (OK)
   Extension -/-> Wile_c_api (would cause: Wile_c_api -> Instance -> Extension -> Wile_c_api) *)
let c_temporary_handle_ref : (Instance.t -> int) ref =
  ref (fun _inst -> extension_error "C extension support not initialized")
let c_release_handle_ref : (int -> unit) ref =
  ref (fun _ih -> ())

let load_c inst path =
  let handle =
    try ext_dlopen path
    with Failure msg ->
      extension_error (Printf.sprintf "dlopen %s: %s" path msg)
  in
  let fn_ptr =
    try ext_dlsym handle "wile_ext_init"
    with Failure msg ->
      (try ext_dlclose handle with _ -> ());
      extension_error (Printf.sprintf "dlsym wile_ext_init in %s: %s" path msg)
  in
  let ih = !c_temporary_handle_ref inst in
  Fun.protect ~finally:(fun () ->
    !c_release_handle_ref ih;
    (try ext_dlclose handle with _ -> ()))
    (fun () ->
      try ext_call_init fn_ptr ih
      with Failure msg ->
        extension_error (Printf.sprintf "wile_ext_init in %s failed: %s"
          path msg))

(* ---- Main entry point ---- *)

let load_native inst ~search_dirs ~sld_dir name =
  (* 1. Try static registry *)
  if init_static inst name then ()
  else begin
    let cmxs_name = "wile_" ^ name ^ ".cmxs" in
    let so_name = "wile_" ^ name ^ ".so" in
    (* 2. Try .cmxs *)
    match find_file ~search_dirs ~sld_dir cmxs_name with
    | Some path -> load_cmxs inst name path
    | None ->
      (* 3. Try .so (C extension) *)
      match find_file ~search_dirs ~sld_dir so_name with
      | Some path -> load_c inst path
      | None ->
        extension_error (Printf.sprintf
          "extension not found: %s (searched for %s and %s)"
          name cmxs_name so_name)
  end

(* Wire up the forward reference in Instance *)
let () =
  Instance.load_native_ref := load_native
