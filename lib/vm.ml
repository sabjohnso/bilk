exception Runtime_error of string

type call_frame = {
  saved_code : Datum.code;
  saved_pc : int;
  saved_env : Datum.env;
  saved_sp : int;
}

let execute (env : Datum.env) (code : Datum.code) : Datum.t =
  let stack_size = 1024 in
  let stack = Array.make stack_size Datum.Void in
  let acc = ref Datum.Void in
  let sp = ref 0 in
  let pc = ref 0 in
  let cur_code = ref code in
  let cur_env = ref env in
  let frames = ref ([] : call_frame list) in

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
      (* Last param is rest arg *)
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
      (* Collect rest into a list *)
      let rest = List.fold_right (fun x acc -> Datum.Pair (x, acc)) !args_ref Datum.Nil in
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
      (match !acc with
       | Datum.Primitive prim ->
         acc := prim.prim_fn args
       | Datum.Closure clos ->
         let new_env = bind_params clos.clos_code args clos.clos_env in
         let frame = {
           saved_code = !cur_code;
           saved_pc = !pc;
           saved_env = !cur_env;
           saved_sp = !sp;
         } in
         frames := frame :: !frames;
         cur_code := clos.clos_code;
         pc := 0;
         cur_env := new_env
       | other ->
         runtime_error (Printf.sprintf "not a procedure: %s" (Datum.to_string other)))

    | Opcode.TailCall n ->
      let args = pop_n n in
      (match !acc with
       | Datum.Primitive prim ->
         acc := prim.prim_fn args
       | Datum.Closure clos ->
         let new_env = bind_params clos.clos_code args clos.clos_env in
         (* Don't push frame — reuse current *)
         (match !frames with
          | frame :: _ -> sp := frame.saved_sp
          | [] -> sp := 0);
         cur_code := clos.clos_code;
         pc := 0;
         cur_env := new_env
       | other ->
         runtime_error (Printf.sprintf "not a procedure: %s" (Datum.to_string other)))

    | Opcode.Return ->
      (match !frames with
       | frame :: rest ->
         frames := rest;
         cur_code := frame.saved_code;
         pc := frame.saved_pc;
         cur_env := frame.saved_env;
         sp := frame.saved_sp
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
