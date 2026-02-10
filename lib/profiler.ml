exception Profiler_error of string

type proc_key = {
  pk_name : string;
  pk_loc : Loc.t;
}

type proc_stats = {
  mutable call_count : int;
  mutable total_time : float;
  mutable self_time : float;
}

type trace_event = {
  te_ph : char;
  te_name : string;
  te_cat : string;
  te_ts : float;
}

type stack_entry = {
  se_name : string;
  se_loc : Loc.t;
  se_enter : float;
  mutable se_children : float;
  se_depth : int;
}

type t = {
  stats : (string, proc_stats) Hashtbl.t;
  mutable stack : stack_entry list;
  mutable trace : trace_event list;  (* reversed *)
  flame_stacks : (string, float) Hashtbl.t;
  mutable start_time : float;
}

(* Build a unique key string for a procedure *)
let make_key name (loc : Loc.t) =
  Printf.sprintf "%s\x00%s:%d:%d" name loc.file loc.line loc.col

(* Get or create stats entry *)
let get_stats t name loc =
  let key = make_key name loc in
  match Hashtbl.find_opt t.stats key with
  | Some s -> s
  | None ->
    let s = { call_count = 0; total_time = 0.0; self_time = 0.0 } in
    Hashtbl.replace t.stats key s;
    s

(* Record timing for a completed stack entry *)
let record_entry t se now =
  let elapsed = now -. se.se_enter in
  let self = elapsed -. se.se_children in
  let stats = get_stats t se.se_name se.se_loc in
  stats.total_time <- stats.total_time +. elapsed;
  stats.self_time <- stats.self_time +. self;
  (* Update parent's children time *)
  (match t.stack with
   | parent :: _ -> parent.se_children <- parent.se_children +. elapsed
   | [] -> ());
  (* Emit trace end event *)
  let ts = (now -. t.start_time) *. 1e6 in
  t.trace <- { te_ph = 'E'; te_name = se.se_name;
               te_cat = "scheme"; te_ts = ts } :: t.trace;
  (* Record flame stack *)
  let path = String.concat ";" (List.rev_map (fun e -> e.se_name)
    (se :: t.stack)) in
  let self_us = self *. 1e6 in
  let prev = match Hashtbl.find_opt t.flame_stacks path with
    | Some v -> v | None -> 0.0 in
  Hashtbl.replace t.flame_stacks path (prev +. self_us)

(* Extract procedure name and location from a datum *)
let proc_info (proc : Datum.t) =
  match proc with
  | Datum.Closure clos ->
    let loc =
      if Array.length clos.clos_code.source_map > 0 then
        clos.clos_code.source_map.(0)
      else Loc.none
    in
    (clos.clos_name, loc, true)
  | Datum.Primitive prim ->
    (prim.prim_name, Loc.none, false)
  | _ -> ("<unknown>", Loc.none, false)

let create () =
  { stats = Hashtbl.create 64;
    stack = [];
    trace = [];
    flame_stacks = Hashtbl.create 64;
    start_time = 0.0 }

let install t inst =
  if !(inst.Instance.on_call) <> None then
    raise (Profiler_error "hooks already installed on instance");
  let ds = Vm.make_debug_state () in
  inst.debug_state := Some ds;
  t.start_time <- Unix.gettimeofday ();
  inst.on_call := Some (fun _loc proc _args ->
    let now = Unix.gettimeofday () in
    let (name, ploc, is_closure) = proc_info proc in
    (* Count every call *)
    let stats = get_stats t name ploc in
    stats.call_count <- stats.call_count + 1;
    if is_closure then begin
      (* Get frame depth from debug state *)
      let frame_depth = List.length ds.dbg_frames in
      (* Pop tail-replaced entries: entries with depth >= current frame_depth
         were from closures that were tail-replaced *)
      let rec pop_tail_replaced () =
        match t.stack with
        | se :: _ when se.se_depth >= frame_depth ->
          t.stack <- List.tl t.stack;
          record_entry t se now;
          pop_tail_replaced ()
        | _ -> ()
      in
      pop_tail_replaced ();
      (* Push new entry *)
      let se = {
        se_name = name;
        se_loc = ploc;
        se_enter = now;
        se_children = 0.0;
        se_depth = frame_depth;
      } in
      t.stack <- se :: t.stack;
      (* Emit trace begin event *)
      let ts = (now -. t.start_time) *. 1e6 in
      t.trace <- { te_ph = 'B'; te_name = name;
                   te_cat = "scheme"; te_ts = ts } :: t.trace
    end);
  inst.on_return := Some (fun _loc _value ->
    let now = Unix.gettimeofday () in
    match t.stack with
    | se :: rest ->
      t.stack <- rest;
      record_entry t se now
    | [] -> ())

let uninstall inst =
  inst.Instance.on_call := None;
  inst.Instance.on_return := None;
  inst.Instance.debug_state := None

let finalize t =
  let now = Unix.gettimeofday () in
  let rec flush () =
    match t.stack with
    | se :: rest ->
      t.stack <- rest;
      record_entry t se now;
      flush ()
    | [] -> ()
  in
  flush ()

let entries t =
  Hashtbl.fold (fun key stats acc ->
    (* Parse key back into proc_key *)
    match String.split_on_char '\x00' key with
    | [name; loc_str] ->
      let pk_loc =
        match String.split_on_char ':' loc_str with
        | file :: line_s :: col_s :: _ ->
          (try Loc.make file (int_of_string line_s) (int_of_string col_s)
           with _ -> Loc.none)
        | _ -> Loc.none
      in
      ({ pk_name = name; pk_loc }, stats) :: acc
    | _ ->
      ({ pk_name = key; pk_loc = Loc.none }, stats) :: acc
  ) t.stats []

let trace_events t =
  List.rev t.trace

let flame_stacks t =
  Hashtbl.fold (fun path time acc -> (path, time) :: acc)
    t.flame_stacks []
