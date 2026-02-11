exception Debug_error of string

type breakpoint = {
  bp_id : int;
  bp_file : string;
  bp_line : int;
  bp_verified : bool;
}

type stop_reason =
  | Breakpoint of int
  | Step
  | Pause_request
  | Entry

type step_mode =
  | Run
  | Step_in
  | Step_over
  | Step_out
  | Paused

type stack_frame = {
  sf_id : int;
  sf_name : string;
  sf_file : string;
  sf_line : int;
  sf_col : int;
}

type scope = {
  sc_name : string;
  sc_ref : int;
}

type variable = {
  var_name : string;
  var_value : string;
  var_type : string;
  var_ref : int;
}

type var_ref_kind =
  | Scope_locals of Datum.frame
  | Scope_globals of Datum.frame
  | Compound_pair of Datum.t * Datum.t
  | Compound_vector of Datum.t array
  | Compound_hash of (Datum.t * Datum.t) list

type t = {
  inst : Instance.t;
  debug_st : Vm.debug_state;
  breakpoints : (string, breakpoint list) Hashtbl.t;
  mutable next_bp_id : int;
  mutable mode : step_mode;
  mutable call_depth : int;
  mutable paused_depth : int;
  mutable pause_requested : bool;
  var_handles : (int, var_ref_kind) Hashtbl.t;
  mutable next_var_handle : int;
  sym_id_to_name : (int, string) Hashtbl.t;
}

let create inst =
  let ds = Vm.make_debug_state () in
  inst.Instance.debug_state := Some ds;
  { inst;
    debug_st = ds;
    breakpoints = Hashtbl.create 16;
    next_bp_id = 1;
    mode = Run;
    call_depth = 0;
    paused_depth = 0;
    pause_requested = false;
    var_handles = Hashtbl.create 32;
    next_var_handle = 1000;
    sym_id_to_name = Hashtbl.create 256 }

let alloc_handle t kind =
  let h = t.next_var_handle in
  t.next_var_handle <- h + 1;
  Hashtbl.replace t.var_handles h kind;
  h

let set_breakpoints t file lines =
  let bps = List.map (fun line ->
    let bp = { bp_id = t.next_bp_id; bp_file = file;
               bp_line = line; bp_verified = true } in
    t.next_bp_id <- t.next_bp_id + 1;
    bp
  ) lines in
  Hashtbl.replace t.breakpoints file bps;
  bps

let clear_breakpoints t file =
  Hashtbl.remove t.breakpoints file

let check_breakpoint t loc =
  let file = loc.Loc.file in
  let line = loc.Loc.line in
  match Hashtbl.find_opt t.breakpoints file with
  | None -> None
  | Some bps ->
    let rec check = function
      | [] -> None
      | bp :: rest ->
        if bp.bp_line = line then Some (Breakpoint bp.bp_id)
        else check rest
    in
    check bps

let on_call t loc _proc _args =
  let depth_before = t.call_depth in
  t.call_depth <- t.call_depth + 1;
  if t.pause_requested then begin
    t.pause_requested <- false;
    t.paused_depth <- t.call_depth;
    Some Pause_request
  end else
    match check_breakpoint t loc with
    | Some _ as r ->
      t.paused_depth <- t.call_depth;
      r
    | None ->
      match t.mode with
      | Step_in ->
        t.paused_depth <- t.call_depth;
        Some Step
      | Step_over when depth_before <= t.paused_depth ->
        t.paused_depth <- t.call_depth;
        Some Step
      | Step_out when depth_before < t.paused_depth ->
        t.paused_depth <- t.call_depth;
        Some Step
      | _ -> None

let on_return t _loc _value =
  t.call_depth <- max 0 (t.call_depth - 1)

let continue t =
  t.mode <- Run

let step_in t =
  t.mode <- Step_in

let step_over t =
  t.paused_depth <- t.call_depth;
  t.mode <- Step_over

let step_out t =
  t.paused_depth <- t.call_depth;
  t.mode <- Step_out

let request_pause t =
  t.pause_requested <- true

let reset_handles t =
  Hashtbl.clear t.var_handles;
  t.next_var_handle <- 1000

(* --- Inspection --- *)

let build_sym_map t =
  Hashtbl.clear t.sym_id_to_name;
  let syms = Symbol.all t.inst.symbols in
  List.iter (fun sym ->
    Hashtbl.replace t.sym_id_to_name (Symbol.id sym) (Symbol.name sym)
  ) syms

let sym_name t id =
  match Hashtbl.find_opt t.sym_id_to_name id with
  | Some n -> n
  | None -> Printf.sprintf "sym#%d" id

let datum_type d =
  match d with
  | Datum.Fixnum _ -> "integer"
  | Datum.Rational _ -> "rational"
  | Datum.Flonum _ -> "float"
  | Datum.Complex _ -> "complex"
  | Datum.Bool _ -> "boolean"
  | Datum.Char _ -> "char"
  | Datum.Str _ -> "string"
  | Datum.Symbol _ -> "symbol"
  | Datum.Pair _ -> "pair"
  | Datum.Nil -> "null"
  | Datum.Vector _ -> "vector"
  | Datum.Bytevector _ -> "bytevector"
  | Datum.Closure _ -> "procedure"
  | Datum.Primitive _ -> "primitive"
  | Datum.Void -> "void"
  | Datum.Port _ -> "port"
  | Datum.Hash_table _ -> "hash-table"
  | Datum.Char_set _ -> "char-set"
  | Datum.Regexp _ -> "regexp"
  | Datum.Promise _ -> "promise"
  | Datum.Continuation _ -> "continuation"
  | Datum.Values _ -> "values"
  | Datum.Error_object _ -> "error"
  | Datum.Eof -> "eof"

let is_expandable d =
  match d with
  | Datum.Pair _ | Datum.Vector _ | Datum.Hash_table _ -> true
  | _ -> false

let stack_trace t =
  build_sym_map t;
  let ds = t.debug_st in
  (* Current frame *)
  let cur_loc =
    if Array.length ds.dbg_code.source_map > 0 then
      ds.dbg_code.source_map.(min ds.dbg_pc
        (Array.length ds.dbg_code.source_map - 1))
    else Loc.none
  in
  let cur_frame = {
    sf_id = 0;
    sf_name = (if ds.dbg_code.name = "" then "<top-level>" else ds.dbg_code.name);
    sf_file = cur_loc.Loc.file;
    sf_line = cur_loc.Loc.line;
    sf_col = cur_loc.Loc.col;
  } in
  (* Saved call frames *)
  let saved = List.mapi (fun i (cf : Datum.call_frame) ->
    let loc =
      if Array.length cf.saved_code.source_map > 0 then
        cf.saved_code.source_map.(min (max 0 (cf.saved_pc - 1))
          (Array.length cf.saved_code.source_map - 1))
      else Loc.none
    in
    { sf_id = i + 1;
      sf_name = (if cf.saved_code.name = "" then "<top-level>" else cf.saved_code.name);
      sf_file = loc.Loc.file;
      sf_line = loc.Loc.line;
      sf_col = loc.Loc.col }
  ) ds.dbg_frames in
  cur_frame :: saved

let scopes t frame_id =
  let ds = t.debug_st in
  let env = match frame_id with
    | 0 -> ds.dbg_env
    | n ->
      if n > 0 && n <= List.length ds.dbg_frames then
        let cf = List.nth ds.dbg_frames (n - 1) in
        cf.Datum.saved_env
      else ds.dbg_env
  in
  match env with
  | [] -> []
  | local_frame :: rest ->
    let local_h = alloc_handle t (Scope_locals local_frame) in
    let scopes = [{ sc_name = "Locals"; sc_ref = local_h }] in
    (* Add globals scope from outermost frame *)
    let rec last = function
      | [] -> None
      | [f] -> Some f
      | _ :: tl -> last tl
    in
    (match last rest with
     | Some global_frame when global_frame != local_frame ->
       let global_h = alloc_handle t (Scope_globals global_frame) in
       scopes @ [{ sc_name = "Globals"; sc_ref = global_h }]
     | _ -> scopes)

let make_variable t name value =
  let ref_h = if is_expandable value then
    let kind = match value with
      | Datum.Pair { car; cdr } -> Compound_pair (car, cdr)
      | Datum.Vector arr -> Compound_vector arr
      | Datum.Hash_table ht ->
        let entries = ref [] in
        Array.iter (fun bucket ->
          List.iter (fun (k, v) -> entries := (k, v) :: !entries) bucket
        ) ht.Datum.ht_data;
        Compound_hash !entries
      | _ -> Compound_pair (Datum.Void, Datum.Void)
    in
    alloc_handle t kind
  else 0
  in
  { var_name = name;
    var_value = Datum.to_string value;
    var_type = datum_type value;
    var_ref = ref_h }

let variables t ref_h =
  build_sym_map t;
  match Hashtbl.find_opt t.var_handles ref_h with
  | None -> []
  | Some (Scope_locals frame | Scope_globals frame) ->
    let bindings = Env.frame_bindings frame in
    List.map (fun (id, value) ->
      make_variable t (sym_name t id) value
    ) (List.sort (fun (_, _) (_, _) -> 0) bindings)
  | Some (Compound_pair (car, cdr)) ->
    [ make_variable t "car" car;
      make_variable t "cdr" cdr ]
  | Some (Compound_vector arr) ->
    Array.to_list (Array.mapi (fun i v ->
      make_variable t (string_of_int i) v
    ) arr)
  | Some (Compound_hash entries) ->
    List.mapi (fun i (k, v) ->
      let name = Printf.sprintf "[%s]" (Datum.to_string k) in
      ignore i;
      make_variable t name v
    ) entries

let evaluate t expr frame_id =
  let env = match frame_id with
    | Some 0 -> t.debug_st.dbg_env
    | Some n when n > 0 && n <= List.length t.debug_st.dbg_frames ->
      let cf = List.nth t.debug_st.dbg_frames (n - 1) in
      cf.Datum.saved_env
    | _ -> t.inst.global_env
  in
  try
    let port = Port.of_string expr in
    let sx = Reader.read_syntax t.inst.readtable port in
    let gensym () =
      let n = !(t.inst.gensym_counter) in
      t.inst.gensym_counter := n + 1;
      Printf.sprintf "%%g%d" n
    in
    let expanded = Expander.expand
      ~syn_env:t.inst.syn_env
      ~gensym
      ~features:t.inst.features
      ~has_library:(fun _name -> false)
      ~read_include:(fun ~fold_case:_ _path -> failwith "include not supported in evaluate")
      sx in
    let code = Compiler.compile t.inst.symbols expanded in
    let result = Vm.execute ~winds:t.inst.winds env code in
    Datum.to_string result
  with
  | Vm.Runtime_error msg -> Printf.sprintf "Error: %s" msg
  | Compiler.Compile_error (_, msg) -> Printf.sprintf "Error: %s" msg
  | Reader.Read_error (_, msg) -> Printf.sprintf "Error: %s" msg
  | Failure msg -> Printf.sprintf "Error: %s" msg

(* --- DAP session --- *)

let stop_reason_string = function
  | Breakpoint _ -> "breakpoint"
  | Step -> "step"
  | Pause_request -> "pause"
  | Entry -> "entry"

let stop_reason_bp_id = function
  | Breakpoint id -> Some id
  | _ -> None

let send_stopped oc seq_ref reason t =
  let body_fields = [
    ("reason", `String (stop_reason_string reason));
    ("threadId", `Int 1);
    ("allThreadsStopped", `Bool true);
  ] in
  let body_fields = match stop_reason_bp_id reason with
    | Some id -> body_fields @ [("hitBreakpointIds", `List [`Int id])]
    | None -> body_fields
  in
  ignore t;
  let evt = Dap.make_event seq_ref "stopped" (`Assoc body_fields) in
  Dap.write_event oc seq_ref evt

let send_output oc seq_ref category text =
  let evt = Dap.make_event seq_ref "output" (`Assoc [
    ("category", `String category);
    ("output", `String text)
  ]) in
  Dap.write_event oc seq_ref evt

let handle_stack_trace t req oc seq_ref =
  let frames = stack_trace t in
  let frames_json = List.map (fun sf ->
    `Assoc [
      ("id", `Int sf.sf_id);
      ("name", `String sf.sf_name);
      ("source", `Assoc [("path", `String sf.sf_file)]);
      ("line", `Int sf.sf_line);
      ("column", `Int sf.sf_col);
    ]
  ) frames in
  let resp = Dap.make_response seq_ref req true
    (`Assoc [("stackFrames", `List frames_json);
             ("totalFrames", `Int (List.length frames))]) in
  Dap.write_response oc seq_ref resp

let handle_scopes t req oc seq_ref =
  let frame_id = match req.Dap.arguments with
    | `Assoc assoc ->
      (match List.assoc_opt "frameId" assoc with
       | Some (`Int n) -> n
       | _ -> 0)
    | _ -> 0
  in
  let scs = scopes t frame_id in
  let scopes_json = List.map (fun sc ->
    `Assoc [
      ("name", `String sc.sc_name);
      ("variablesReference", `Int sc.sc_ref);
      ("expensive", `Bool (sc.sc_name = "Globals"));
    ]
  ) scs in
  let resp = Dap.make_response seq_ref req true
    (`Assoc [("scopes", `List scopes_json)]) in
  Dap.write_response oc seq_ref resp

let handle_variables t req oc seq_ref =
  let ref_h = match req.Dap.arguments with
    | `Assoc assoc ->
      (match List.assoc_opt "variablesReference" assoc with
       | Some (`Int n) -> n
       | _ -> 0)
    | _ -> 0
  in
  let vars = variables t ref_h in
  let vars_json = List.map (fun v ->
    `Assoc [
      ("name", `String v.var_name);
      ("value", `String v.var_value);
      ("type", `String v.var_type);
      ("variablesReference", `Int v.var_ref);
    ]
  ) vars in
  let resp = Dap.make_response seq_ref req true
    (`Assoc [("variables", `List vars_json)]) in
  Dap.write_response oc seq_ref resp

let handle_evaluate t req oc seq_ref =
  let expr = match req.Dap.arguments with
    | `Assoc assoc ->
      (match List.assoc_opt "expression" assoc with
       | Some (`String s) -> s
       | _ -> "")
    | _ -> ""
  in
  let frame_id = match req.Dap.arguments with
    | `Assoc assoc ->
      (match List.assoc_opt "frameId" assoc with
       | Some (`Int n) -> Some n
       | _ -> None)
    | _ -> None
  in
  let result = evaluate t expr frame_id in
  let resp = Dap.make_response seq_ref req true
    (`Assoc [("result", `String result);
             ("variablesReference", `Int 0)]) in
  Dap.write_response oc seq_ref resp

let handle_set_breakpoints t req oc seq_ref =
  let source_path = match req.Dap.arguments with
    | `Assoc assoc ->
      (match List.assoc_opt "source" assoc with
       | Some (`Assoc src) ->
         (match List.assoc_opt "path" src with
          | Some (`String p) -> p
          | _ -> "")
       | _ -> "")
    | _ -> ""
  in
  let lines = match req.Dap.arguments with
    | `Assoc assoc ->
      (match List.assoc_opt "breakpoints" assoc with
       | Some (`List bps) ->
         List.filter_map (fun bp ->
           match bp with
           | `Assoc bp_assoc ->
             (match List.assoc_opt "line" bp_assoc with
              | Some (`Int n) -> Some n
              | _ -> None)
           | _ -> None
         ) bps
       | _ -> [])
    | _ -> []
  in
  let bps = set_breakpoints t source_path lines in
  let bps_json = List.map (fun bp ->
    `Assoc [
      ("id", `Int bp.bp_id);
      ("verified", `Bool bp.bp_verified);
      ("line", `Int bp.bp_line);
      ("source", `Assoc [("path", `String bp.bp_file)]);
    ]
  ) bps in
  let resp = Dap.make_response seq_ref req true
    (`Assoc [("breakpoints", `List bps_json)]) in
  Dap.write_response oc seq_ref resp

(* Paused command loop — processes DAP commands while stopped.
   Returns when a resume command is received. *)
type resume_action = Continue | StepIn | StepOver | StepOut | Disconnect

let paused_loop t dap_in oc seq_ref reason =
  send_stopped oc seq_ref reason t;
  let action = ref None in
  while !action = None do
    let msg = Dap.read_message dap_in in
    match msg with
    | Dap.Request req ->
      (match req.command with
       | "continue" ->
         let resp = Dap.make_response seq_ref req true (`Assoc [("allThreadsContinued", `Bool true)]) in
         Dap.write_response oc seq_ref resp;
         action := Some Continue
       | "next" ->
         let resp = Dap.make_response seq_ref req true `Null in
         Dap.write_response oc seq_ref resp;
         action := Some StepOver
       | "stepIn" ->
         let resp = Dap.make_response seq_ref req true `Null in
         Dap.write_response oc seq_ref resp;
         action := Some StepIn
       | "stepOut" ->
         let resp = Dap.make_response seq_ref req true `Null in
         Dap.write_response oc seq_ref resp;
         action := Some StepOut
       | "disconnect" ->
         let resp = Dap.make_response seq_ref req true `Null in
         Dap.write_response oc seq_ref resp;
         action := Some Disconnect
       | "threads" ->
         let resp = Dap.make_response seq_ref req true
           (`Assoc [("threads", `List [
             `Assoc [("id", `Int 1); ("name", `String "main")]
           ])]) in
         Dap.write_response oc seq_ref resp
       | "stackTrace" -> handle_stack_trace t req oc seq_ref
       | "scopes" -> handle_scopes t req oc seq_ref
       | "variables" -> handle_variables t req oc seq_ref
       | "evaluate" -> handle_evaluate t req oc seq_ref
       | "setBreakpoints" -> handle_set_breakpoints t req oc seq_ref
       | cmd ->
         let resp = { (Dap.make_response seq_ref req false `Null)
                      with message = Some (Printf.sprintf "unsupported: %s" cmd) } in
         Dap.write_response oc seq_ref resp)
    | _ -> ()
  done;
  match !action with
  | Some a -> a
  | None -> Continue

let flush_output oc seq_ref port category flushed_ref =
  let s = Port.get_output_string port in
  let len = String.length s in
  if len > !flushed_ref then begin
    let new_text = String.sub s !flushed_ref (len - !flushed_ref) in
    send_output oc seq_ref category new_text;
    flushed_ref := len
  end

let run_session t dap_in dap_out program_path _args =
  let seq_ref = ref 1 in
  let disconnected = ref false in
  (* Phase 1: Initialize *)
  let rec wait_initialize () =
    match Dap.read_message dap_in with
    | Dap.Request req when req.command = "initialize" ->
      let caps = `Assoc [
        ("supportsConfigurationDoneRequest", `Bool true);
        ("supportsFunctionBreakpoints", `Bool false);
        ("supportsConditionalBreakpoints", `Bool false);
        ("supportsEvaluateForHovers", `Bool true);
      ] in
      let resp = Dap.make_response seq_ref req true caps in
      Dap.write_response dap_out seq_ref resp;
      let evt = Dap.make_event seq_ref "initialized" `Null in
      Dap.write_event dap_out seq_ref evt
    | _ -> wait_initialize ()
  in
  wait_initialize ();
  (* Phase 2: Read launch and configuration *)
  let rec wait_config () =
    match Dap.read_message dap_in with
    | Dap.Request req when req.command = "launch" ->
      let resp = Dap.make_response seq_ref req true `Null in
      Dap.write_response dap_out seq_ref resp;
      wait_config ()
    | Dap.Request req when req.command = "setBreakpoints" ->
      handle_set_breakpoints t req dap_out seq_ref;
      wait_config ()
    | Dap.Request req when req.command = "configurationDone" ->
      let resp = Dap.make_response seq_ref req true `Null in
      Dap.write_response dap_out seq_ref resp
    | Dap.Request req ->
      let resp = Dap.make_response seq_ref req true `Null in
      Dap.write_response dap_out seq_ref resp;
      wait_config ()
    | _ -> wait_config ()
  in
  wait_config ();
  (* Phase 3: Execute with debugging *)
  let stdout_port = Port.open_output_string () in
  let stderr_port = Port.open_output_string () in
  let saved_out = !(t.inst.current_output) in
  let saved_err = !(t.inst.current_error) in
  t.inst.current_output := stdout_port;
  t.inst.current_error := stderr_port;
  let stdout_flushed = ref 0 in
  let stderr_flushed = ref 0 in
  (* Install hooks *)
  t.inst.on_call := Some (fun loc proc args ->
    (* Flush any pending output *)
    flush_output dap_out seq_ref stdout_port "stdout" stdout_flushed;
    flush_output dap_out seq_ref stderr_port "stderr" stderr_flushed;
    (* Check for async pause via non-blocking read *)
    (try
       let fd = Unix.descr_of_in_channel dap_in in
       let ready, _, _ = Unix.select [fd] [] [] 0.0 in
       if ready <> [] then begin
         let msg = Dap.read_message dap_in in
         match msg with
         | Dap.Request req when req.command = "pause" ->
           let resp = Dap.make_response seq_ref req true `Null in
           Dap.write_response dap_out seq_ref resp;
           request_pause t
         | Dap.Request req when req.command = "disconnect" ->
           let resp = Dap.make_response seq_ref req true `Null in
           Dap.write_response dap_out seq_ref resp;
           disconnected := true
         | _ -> ()
       end
     with Unix.Unix_error _ -> ());
    if !disconnected then raise Exit;
    match on_call t loc proc args with
    | Some reason ->
      t.mode <- Paused;
      let action = paused_loop t dap_in dap_out seq_ref reason in
      reset_handles t;
      (match action with
       | Continue -> continue t
       | StepIn -> step_in t
       | StepOver -> step_over t
       | StepOut -> step_out t
       | Disconnect ->
         disconnected := true;
         raise Exit)
    | None -> ());
  t.inst.on_return := Some (fun loc value ->
    on_return t loc value);
  (try
     let port = Port.of_file program_path in
     ignore (Instance.eval_port t.inst port)
   with
   | Exit -> ()
   | exn ->
     let msg = Printexc.to_string exn in
     send_output dap_out seq_ref "stderr" (msg ^ "\n"));
  (* Flush remaining output *)
  flush_output dap_out seq_ref stdout_port "stdout" stdout_flushed;
  flush_output dap_out seq_ref stderr_port "stderr" stderr_flushed;
  (* Restore ports *)
  t.inst.current_output := saved_out;
  t.inst.current_error := saved_err;
  t.inst.on_call := None;
  t.inst.on_return := None;
  (* Send terminated and exited *)
  if not !disconnected then begin
    let evt = Dap.make_event seq_ref "terminated" `Null in
    Dap.write_event dap_out seq_ref evt;
    let evt = Dap.make_event seq_ref "exited" (`Assoc [("exitCode", `Int 0)]) in
    Dap.write_event dap_out seq_ref evt;
    (* Wait for disconnect *)
    (try
       let rec wait_disconnect () =
         match Dap.read_message dap_in with
         | Dap.Request req when req.command = "disconnect" ->
           let resp = Dap.make_response seq_ref req true `Null in
           Dap.write_response dap_out seq_ref resp
         | _ -> wait_disconnect ()
       in
       wait_disconnect ()
     with Dap.Dap_error _ | End_of_file -> ())
  end
