exception Runtime_error of string

type debug_state = {
  mutable dbg_env : Datum.env;
  mutable dbg_frames : Datum.call_frame list;
  mutable dbg_code : Datum.code;
  mutable dbg_pc : int;
}

let make_debug_state () =
  let empty_code : Datum.code = {
    instructions = [||];
    source_map = [||];
    constants = [||];
    symbols = [||];
    children = [||];
    params = [||];
    variadic = false;
    name = "";
  } in
  { dbg_env = [];
    dbg_frames = [];
    dbg_code = empty_code;
    dbg_pc = 0 }

(* Internal frame types for multi-step intrinsic operations.
   Standard frames are for normal calls/returns.
   The DW_* and CWV_* variants track state for dynamic-wind and call-with-values. *)
type vm_frame =
  | Standard of Datum.call_frame
  | CWV_pending of Datum.call_frame * Datum.t     (* consumer *)
  | DW_before of Datum.call_frame * Datum.t * Datum.t * Datum.t  (* before, thunk, after *)
  | DW_thunk of Datum.call_frame * Datum.t * Datum.t   (* before, after *)
  | DW_after of Datum.call_frame * Datum.t         (* saved result *)

let rec execute ?(winds : Datum.wind list ref option)
    ?(on_call : (Loc.t -> Datum.t -> Datum.t list -> unit) option)
    ?(on_return : (Loc.t -> Datum.t -> unit) option)
    ?(debug_state : debug_state option)
    (env : Datum.env) (code : Datum.code) : Datum.t =
  let stack_size = 1024 in
  let stack = Array.make stack_size Datum.Void in
  let acc = ref Datum.Void in
  let sp = ref 0 in
  let pc = ref 0 in
  let cur_code = ref code in
  let cur_env = ref env in
  let frames = ref ([] : vm_frame list) in
  let winds = match winds with Some r -> r | None -> ref [] in

  let runtime_error msg = raise (Runtime_error msg) in

  let push v =
    if !sp >= stack_size then runtime_error "stack overflow";
    stack.(!sp) <- v;
    incr sp
  in

  let pop () =
    if !sp <= 0 then runtime_error "stack underflow";
    decr sp;
    stack.(!sp)
  in

  let pop_n n =
    let args = Array.make n Datum.Void in
    for i = n - 1 downto 0 do
      args.(i) <- pop ()
    done;
    Array.to_list args
  in

  let bind_params (child_code : Datum.code) args env =
    let params = child_code.params in
    let nparams = Array.length params in
    if child_code.variadic then begin
      let nfixed = nparams - 1 in
      let nargs = List.length args in
      if nargs < nfixed then
        runtime_error (Printf.sprintf "%s: expected at least %d arguments, got %d"
                         child_code.name nfixed nargs);
      let bindings = ref [] in
      let args_ref = ref args in
      for i = 0 to nfixed - 1 do
        bindings := (params.(i), List.hd !args_ref) :: !bindings;
        args_ref := List.tl !args_ref
      done;
      let rest = List.fold_right (fun x acc -> Datum.Pair { car = x; cdr = acc }) !args_ref Datum.Nil in
      bindings := (params.(nfixed), rest) :: !bindings;
      Env.extend env (List.rev !bindings)
    end else begin
      let nargs = List.length args in
      if nargs <> nparams then
        runtime_error (Printf.sprintf "%s: expected %d arguments, got %d"
                         child_code.name nparams nargs);
      let bindings = List.combine (Array.to_list params) args in
      Env.extend env bindings
    end
  in

  let extract_call_frame = function
    | Standard f | CWV_pending (f, _) | DW_before (f, _, _, _)
    | DW_thunk (f, _, _) | DW_after (f, _) -> f
  in

  let vm_frame_to_cont_frame = function
    | Standard f -> Datum.CF_standard f
    | CWV_pending (f, consumer) -> Datum.CF_cwv_pending (f, consumer)
    | DW_before (f, before, thunk, after) -> Datum.CF_dw_before (f, before, thunk, after)
    | DW_thunk (f, before, after) -> Datum.CF_dw_thunk (f, before, after)
    | DW_after (f, result) -> Datum.CF_dw_after (f, result)
  in

  let cont_frame_to_vm_frame = function
    | Datum.CF_standard f -> Standard f
    | Datum.CF_cwv_pending (f, consumer) -> CWV_pending (f, consumer)
    | Datum.CF_dw_before (f, before, thunk, after) -> DW_before (f, before, thunk, after)
    | Datum.CF_dw_thunk (f, before, after) -> DW_thunk (f, before, after)
    | Datum.CF_dw_after (f, result) -> DW_after (f, result)
  in

  let capture_continuation () =
    let saved_stack = Array.sub stack 0 !sp in
    let saved_frames = List.map vm_frame_to_cont_frame !frames in
    Datum.Continuation {
      cont_stack = saved_stack;
      cont_sp = !sp;
      cont_frames = saved_frames;
      cont_code = !cur_code;
      cont_pc = !pc;
      cont_env = !cur_env;
      cont_winds = !winds;
    }
  in

  let call_thunk_nested thunk =
    let wrapper_code : Datum.code = {
      instructions = [| Opcode.Const 0; Opcode.Call 0; Opcode.Halt |];
      source_map = Array.make 3 Loc.none;
      constants = [| thunk |];
      symbols = [||];
      children = [||];
      params = [||];
      variadic = false;
      name = "<wind-thunk>";
    } in
    ignore (execute ~winds:(ref !winds) env wrapper_code)
  in

  let do_wind_switch current_winds target_winds =
    let rec common_tail l1 n1 l2 n2 =
      if n1 > n2 then common_tail (List.tl l1) (n1 - 1) l2 n2
      else if n2 > n1 then common_tail l1 n1 (List.tl l2) (n2 - 1)
      else if l1 == l2 then n1
      else common_tail (List.tl l1) (n1 - 1) (List.tl l2) (n2 - 1)
    in
    let n_cur = List.length current_winds in
    let n_tgt = List.length target_winds in
    let n_common = common_tail current_winds n_cur target_winds n_tgt in
    let n_unwind = n_cur - n_common in
    let n_rewind = n_tgt - n_common in
    let to_unwind = List.filteri (fun i _ -> i < n_unwind) current_winds in
    List.iter (fun (w : Datum.wind) -> call_thunk_nested w.wind_after) to_unwind;
    let to_rewind = List.filteri (fun i _ -> i < n_rewind) target_winds in
    List.iter (fun (w : Datum.wind) -> call_thunk_nested w.wind_before) (List.rev to_rewind)
  in

  let restore_continuation (cont : Datum.continuation) value =
    if !winds != cont.cont_winds then
      do_wind_switch !winds cont.cont_winds;
    Array.blit cont.cont_stack 0 stack 0 cont.cont_sp;
    sp := cont.cont_sp;
    frames := List.map cont_frame_to_vm_frame cont.cont_frames;
    cur_code := cont.cont_code;
    pc := cont.cont_pc;
    cur_env := cont.cont_env;
    winds := cont.cont_winds;
    acc := value
  in

  let restore_frame (frame : Datum.call_frame) =
    cur_code := frame.saved_code;
    pc := frame.saved_pc;
    cur_env := frame.saved_env;
    sp := frame.saved_sp
  in

  let make_call_frame () =
    Datum.({
      saved_code = !cur_code;
      saved_pc = !pc;
      saved_env = !cur_env;
      saved_sp = !sp;
    })
  in

  let fire_on_call proc args =
    (match debug_state with
     | Some ds ->
       ds.dbg_env <- !cur_env;
       ds.dbg_frames <- List.filter_map (function
         | Standard cf -> Some cf | _ -> None) !frames;
       ds.dbg_code <- !cur_code;
       ds.dbg_pc <- max 0 (!pc - 1)
     | None -> ());
    match on_call with
    | Some f -> f (!cur_code).source_map.(max 0 (!pc - 1)) proc args
    | None -> ()
  in

  (* Enter a thunk (0-arg procedure). Returns true if the call was to a
     closure (deferred — will eventually Return), or false if it was a
     primitive (immediate — result is already in acc). *)
  let enter_thunk thunk =
    fire_on_call thunk [];
    match thunk with
    | Datum.Closure clos ->
      let new_env = bind_params clos.clos_code [] clos.clos_env in
      cur_code := clos.clos_code;
      pc := 0;
      cur_env := new_env;
      true
    | Datum.Primitive p when p.prim_intrinsic = None ->
      acc := p.prim_fn [];
      false
    | _ -> runtime_error "expected a thunk (0-argument procedure)"
  in

  let perform_call proc args ~is_tail =
    fire_on_call proc args;
    match proc with
    | Datum.Primitive prim when prim.prim_intrinsic = None ->
      acc := prim.prim_fn args
    | Datum.Closure clos ->
      let new_env = bind_params clos.clos_code args clos.clos_env in
      if is_tail then begin
        (match !frames with
         | frame :: _ -> sp := (extract_call_frame frame).saved_sp
         | [] -> sp := 0);
        cur_code := clos.clos_code;
        pc := 0;
        cur_env := new_env
      end else begin
        let frame = make_call_frame () in
        frames := Standard frame :: !frames;
        cur_code := clos.clos_code;
        pc := 0;
        cur_env := new_env
      end
    | Datum.Continuation cont ->
      let value = match args with
        | [v] -> v
        | _ -> runtime_error (Printf.sprintf
                 "continuation: expected 1 argument, got %d" (List.length args))
      in
      restore_continuation cont value
    | _ ->
      runtime_error (Printf.sprintf "not a procedure: %s" (Datum.to_string proc))
  in

  let rec handle_intrinsic prim args ~is_tail =
    ignore handle_intrinsic;  (* suppress unused warning for rec *)
    match prim.Datum.prim_intrinsic with
    | Some Datum.Intrinsic_apply ->
      let rec flatten_apply = function
        | [] -> runtime_error "apply: expected at least 2 arguments"
        | [last] ->
          let rec datum_to_list = function
            | Datum.Nil -> []
            | Datum.Pair { car = a; cdr = d } -> a :: datum_to_list d
            | _ -> runtime_error "apply: last argument must be a list"
          in
          datum_to_list last
        | x :: rest -> x :: flatten_apply rest
      in
      (match args with
       | [] | [_] -> runtime_error "apply: expected at least 2 arguments"
       | proc :: rest ->
         let flat_args = flatten_apply rest in
         perform_call proc flat_args ~is_tail)
    | Some Datum.Intrinsic_call_cc ->
      (match args with
       | [proc] ->
         if is_tail then begin
           let cont = capture_continuation () in
           perform_call proc [cont] ~is_tail
         end else begin
           (* Capture continuation BEFORE pushing frame.  The continuation
              represents "resume after call/cc returns".  The frame is only
              needed for the normal return path (lambda returns without
              calling k).  Including the frame in the continuation would
              cause double-return when k is invoked and the result is
              tail-called (e.g. the ((call/cc ...)) pattern). *)
           let cont = capture_continuation () in
           let frame = make_call_frame () in
           frames := Standard frame :: !frames;
           fire_on_call proc [cont];
           (match proc with
            | Datum.Closure clos ->
              let new_env = bind_params clos.clos_code [cont] clos.clos_env in
              cur_code := clos.clos_code;
              pc := 0;
              cur_env := new_env
            | Datum.Primitive p when p.prim_intrinsic = None ->
              acc := p.prim_fn [cont];
              frames := List.tl !frames;
              restore_frame frame
            | Datum.Continuation cont2 ->
              restore_continuation cont2 cont
            | _ ->
              runtime_error (Printf.sprintf "call/cc: not a procedure: %s"
                              (Datum.to_string proc)))
         end
       | _ -> runtime_error (Printf.sprintf
                "call/cc: expected 1 argument, got %d" (List.length args)))
    | Some Datum.Intrinsic_call_with_values ->
      (match args with
       | [producer; consumer] ->
         let frame = make_call_frame () in
         if is_tail then begin
           (match !frames with
            | Standard top :: rest ->
              sp := top.saved_sp;
              frames := CWV_pending (frame, consumer) :: rest
            | _ -> frames := CWV_pending (frame, consumer) :: !frames)
         end else
           frames := CWV_pending (frame, consumer) :: !frames;
         if not (enter_thunk producer) then
           ignore (process_intrinsic_return ())
       | _ -> runtime_error (Printf.sprintf
                "call-with-values: expected 2 arguments, got %d" (List.length args)))
    | Some Datum.Intrinsic_dynamic_wind ->
      (match args with
       | [before; thunk; after] ->
         let frame = make_call_frame () in
         if is_tail then begin
           (match !frames with
            | Standard top :: rest ->
              sp := top.saved_sp;
              frames := DW_before (frame, before, thunk, after) :: rest
            | _ -> frames := DW_before (frame, before, thunk, after) :: !frames)
         end else
           frames := DW_before (frame, before, thunk, after) :: !frames;
         if not (enter_thunk before) then
           ignore (process_intrinsic_return ())
       | _ -> runtime_error (Printf.sprintf
                "dynamic-wind: expected 3 arguments, got %d" (List.length args)))
    | None -> assert false

  and process_intrinsic_return () =
    match !frames with
    | CWV_pending (frame, consumer) :: rest ->
      frames := rest;
      let result_args = match !acc with
        | Datum.Values vs -> vs
        | v -> [v]
      in
      restore_frame frame;
      perform_call consumer result_args ~is_tail:false;
      true
    | DW_before (_frame, before, thunk, after) :: rest ->
      winds := { wind_before = before; wind_after = after } :: !winds;
      frames := DW_thunk (_frame, before, after) :: rest;
      if enter_thunk thunk then
        true
      else
        process_intrinsic_return ()
    | DW_thunk (_frame, _before, after) :: rest ->
      let result = !acc in
      winds := List.tl !winds;
      frames := DW_after (_frame, result) :: rest;
      if enter_thunk after then
        true
      else
        process_intrinsic_return ()
    | DW_after (frame, result) :: rest ->
      frames := rest;
      acc := result;
      restore_frame frame;
      false
    | _ ->
      false
  in

  let running = ref true in
  while !running do
    let instr = (!cur_code).instructions.(!pc) in
    pc := !pc + 1;
    match instr with
    | Opcode.Halt ->
      running := false

    | Opcode.Const i ->
      acc := (!cur_code).constants.(i)

    | Opcode.Lookup i ->
      let sym = (!cur_code).symbols.(i) in
      (match Env.lookup !cur_env sym with
       | Some v -> acc := v
       | None -> runtime_error (Printf.sprintf "unbound variable: %s" (Symbol.name sym)))

    | Opcode.Define i ->
      let sym = (!cur_code).symbols.(i) in
      Env.define !cur_env sym !acc;
      acc := Datum.Void

    | Opcode.SetBang i ->
      let sym = (!cur_code).symbols.(i) in
      (try Env.set !cur_env sym !acc
       with Env.Unbound_variable s ->
         runtime_error (Printf.sprintf "set!: unbound variable: %s" (Symbol.name s)));
      acc := Datum.Void

    | Opcode.Push ->
      push !acc

    | Opcode.Jump target ->
      pc := target

    | Opcode.JumpFalse target ->
      (match !acc with
       | Datum.Bool false -> pc := target
       | _ -> ())

    | Opcode.Call n ->
      let args = pop_n n in
      let proc = !acc in
      (match proc with
       | Datum.Primitive prim when prim.prim_intrinsic <> None ->
         fire_on_call proc args;
         handle_intrinsic prim args ~is_tail:false
       | Datum.Continuation cont ->
         fire_on_call proc args;
         let value = match args with
           | [v] -> v
           | _ -> runtime_error (Printf.sprintf
                    "continuation: expected 1 argument, got %d" (List.length args))
         in
         restore_continuation cont value
       | _ ->
         (* on_call fires inside perform_call *)
         perform_call proc args ~is_tail:false)

    | Opcode.TailCall n ->
      let args = pop_n n in
      let proc = !acc in
      (match proc with
       | Datum.Primitive prim when prim.prim_intrinsic <> None ->
         fire_on_call proc args;
         handle_intrinsic prim args ~is_tail:true
       | Datum.Continuation cont ->
         fire_on_call proc args;
         let value = match args with
           | [v] -> v
           | _ -> runtime_error (Printf.sprintf
                    "continuation: expected 1 argument, got %d" (List.length args))
         in
         restore_continuation cont value
       | _ ->
         (* on_call fires inside perform_call *)
         perform_call proc args ~is_tail:true)

    | Opcode.Return ->
      (match !frames with
       | Standard frame :: rest ->
         let return_loc = (!cur_code).source_map.(max 0 (!pc - 1)) in
         frames := rest;
         restore_frame frame;
         (match on_return with
          | Some f -> f return_loc !acc
          | None -> ())
       | (CWV_pending _ | DW_before _ | DW_thunk _ | DW_after _) :: _ ->
         ignore (process_intrinsic_return ())
       | [] ->
         running := false)

    | Opcode.MakeClosure i ->
      let child_code = (!cur_code).children.(i) in
      acc := Datum.Closure {
        clos_name = child_code.name;
        clos_code = child_code;
        clos_env = !cur_env;
      }
  done;
  !acc
