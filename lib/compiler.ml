exception Compile_error of Loc.t * string

let compile_error loc msg = raise (Compile_error (loc, msg))

(* --- Mutable compilation state --- *)

type state = {
  sym_table : Symbol.table;
  mutable instructions : Opcode.t list;  (* reversed *)
  mutable source_locs : Loc.t list;      (* reversed, parallel to instructions *)
  mutable constants : Datum.t list;      (* reversed *)
  mutable symbols : Symbol.t list;       (* reversed *)
  mutable children : Datum.code list;    (* reversed *)
  mutable current_loc : Loc.t;           (* set by compile_* functions *)
}

let create_state sym_table =
  { sym_table;
    instructions = [];
    source_locs = [];
    constants = [];
    symbols = [];
    children = [];
    current_loc = Loc.none }

let emit st op =
  st.instructions <- op :: st.instructions;
  st.source_locs <- st.current_loc :: st.source_locs

let current_pc st =
  List.length st.instructions

let add_constant st v =
  let idx = List.length st.constants in
  st.constants <- v :: st.constants;
  idx

let add_symbol st name =
  let sym = Symbol.intern st.sym_table name in
  let idx = List.length st.symbols in
  st.symbols <- sym :: st.symbols;
  idx

let add_child st code =
  let idx = List.length st.children in
  st.children <- code :: st.children;
  idx

(* Patch a jump instruction at position [pc] with a new target. *)
let patch_jump st pc target =
  let arr = Array.of_list (List.rev st.instructions) in
  let patched = match arr.(pc) with
    | Opcode.Jump _ -> Opcode.Jump target
    | Opcode.JumpFalse _ -> Opcode.JumpFalse target
    | _ -> failwith "patch_jump: not a jump instruction"
  in
  arr.(pc) <- patched;
  st.instructions <- Array.to_list arr |> List.rev

(* --- Syntax list helpers --- *)

let rec syntax_list_to_list s =
  match s.Syntax.datum with
  | Syntax.Nil -> []
  | Syntax.Pair (car, cdr) -> car :: syntax_list_to_list cdr
  | _ -> compile_error s.loc "expected proper list"

let rec syntax_to_params loc s =
  match s.Syntax.datum with
  | Syntax.Nil -> ([], false)
  | Syntax.Symbol name -> ([name], true)  (* rest param *)
  | Syntax.Pair (car, cdr) ->
    (match car.datum with
     | Syntax.Symbol name ->
       let (rest, variadic) = syntax_to_params loc cdr in
       (name :: rest, variadic)
     | _ -> compile_error car.loc "parameter must be a symbol")
  | _ -> compile_error s.loc "malformed parameter list"

(* --- Self-evaluating test --- *)

let is_self_evaluating (s : Syntax.t) =
  match s.datum with
  | Syntax.Bool _ | Syntax.Fixnum _ | Syntax.Bignum _ | Syntax.Rational _
  | Syntax.Flonum _ | Syntax.Complex _ | Syntax.Char _ | Syntax.Str _ -> true
  | _ -> false

(* --- Main compilation --- *)

let rec compile_expr st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  if is_self_evaluating s then begin
    let v = Syntax.to_datum s in
    let idx = add_constant st v in
    emit st (Opcode.Const idx)
  end else
  match s.datum with
  | Syntax.Symbol name ->
    let idx = add_symbol st name in
    emit st (Opcode.Lookup idx)

  | Syntax.Nil ->
    compile_error s.loc "empty application ()"

  | Syntax.Pair (head, _rest) ->
    compile_form st s head ~tail

  | Syntax.Vector _ ->
    let v = Syntax.to_datum s in
    let idx = add_constant st v in
    emit st (Opcode.Const idx)

  | Syntax.Bytevector _ ->
    let v = Syntax.to_datum s in
    let idx = add_constant st v in
    emit st (Opcode.Const idx)

  | Syntax.Eof ->
    compile_error s.loc "unexpected eof"

  | _ ->
    (* Bool, Fixnum, etc. already handled by is_self_evaluating *)
    compile_error s.loc "unexpected expression"

and compile_form st (s : Syntax.t) (head : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  match head.datum with
  | Syntax.Symbol "quote" ->
    compile_quote st s

  | Syntax.Symbol "if" ->
    compile_if st s ~tail

  | Syntax.Symbol "lambda" ->
    compile_lambda st s

  | Syntax.Symbol "define" ->
    compile_define st s

  | Syntax.Symbol "set!" ->
    compile_set_bang st s

  | Syntax.Symbol "begin" ->
    compile_begin st s ~tail

  | Syntax.Symbol "let" ->
    compile_let st s ~tail

  | Syntax.Symbol "let*" ->
    compile_let_star st s ~tail

  | Syntax.Symbol "letrec" ->
    compile_letrec_star st s ~tail

  | Syntax.Symbol "letrec*" ->
    compile_letrec_star st s ~tail

  | Syntax.Symbol "cond" ->
    compile_cond st s ~tail

  | Syntax.Symbol "case" ->
    compile_case st s ~tail

  | Syntax.Symbol "do" ->
    compile_do st s ~tail

  | Syntax.Symbol "and" ->
    compile_and st s ~tail

  | Syntax.Symbol "or" ->
    compile_or st s ~tail

  | Syntax.Symbol "when" ->
    compile_when st s ~tail

  | Syntax.Symbol "unless" ->
    compile_unless st s ~tail

  | _ ->
    compile_call st s ~tail

and compile_quote st (s : Syntax.t) =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  (match args with
   | [_; datum] ->
     let v = Syntax.to_datum datum in
     let idx = add_constant st v in
     emit st (Opcode.Const idx)
   | _ -> compile_error s.loc "quote expects exactly 1 argument")

and compile_if st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | [_; test; consequent; alternate] ->
    compile_expr st test ~tail:false;
    let jf_pc = current_pc st in
    emit st (Opcode.JumpFalse 0);  (* placeholder *)
    compile_expr st consequent ~tail;
    let jump_pc = current_pc st in
    emit st (Opcode.Jump 0);  (* placeholder *)
    let alt_target = current_pc st in
    patch_jump st jf_pc alt_target;
    compile_expr st alternate ~tail;
    let end_target = current_pc st in
    patch_jump st jump_pc end_target
  | [_; test; consequent] ->
    compile_expr st test ~tail:false;
    let jf_pc = current_pc st in
    emit st (Opcode.JumpFalse 0);
    compile_expr st consequent ~tail;
    let jump_pc = current_pc st in
    emit st (Opcode.Jump 0);
    let alt_target = current_pc st in
    patch_jump st jf_pc alt_target;
    let void_idx = add_constant st Datum.Void in
    emit st (Opcode.Const void_idx);
    let end_target = current_pc st in
    patch_jump st jump_pc end_target
  | _ ->
    compile_error s.loc "if expects 2 or 3 arguments"

and compile_lambda st (s : Syntax.t) =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | _ :: params_syntax :: body when body <> [] ->
    let (param_names, variadic) = syntax_to_params s.loc params_syntax in
    let child_st = create_state st.sym_table in
    compile_body child_st body;
    let child_code = package_code child_st param_names variadic "<lambda>" in
    let idx = add_child st child_code in
    emit st (Opcode.MakeClosure idx)
  | _ ->
    compile_error s.loc "lambda expects parameters and at least one body expression"

and compile_define st (s : Syntax.t) =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | [_; name_syn; expr] when (match name_syn.datum with Syntax.Symbol _ -> true | _ -> false) ->
    (* (define x expr) *)
    let name = match name_syn.datum with Syntax.Symbol n -> n | _ -> assert false in
    compile_expr st expr ~tail:false;
    st.current_loc <- s.loc;
    let idx = add_symbol st name in
    emit st (Opcode.Define idx)
  | _ :: name_syn :: _body when (match name_syn.datum with Syntax.Pair _ -> true | _ -> false) ->
    (* (define (f params...) body...) → desugar to (define f (lambda (params...) body...)) *)
    (match name_syn.datum with
     | Syntax.Pair (fname, params) ->
       (match fname.datum with
        | Syntax.Symbol name ->
          let body = List.tl (List.tl args) in  (* skip 'define' and '(f params...)' *)
          let (param_names, variadic) = syntax_to_params s.loc params in
          let child_st = create_state st.sym_table in
          compile_body child_st body;
          let child_code = package_code child_st param_names variadic name in
          let child_idx = add_child st child_code in
          st.current_loc <- s.loc;
          emit st (Opcode.MakeClosure child_idx);
          let sym_idx = add_symbol st name in
          emit st (Opcode.Define sym_idx)
        | _ -> compile_error fname.loc "define: expected symbol as function name")
     | _ -> assert false)
  | _ ->
    compile_error s.loc "malformed define"

and compile_set_bang st (s : Syntax.t) =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | [_; name_syn; expr] ->
    (match name_syn.datum with
     | Syntax.Symbol name ->
       compile_expr st expr ~tail:false;
       let idx = add_symbol st name in
       emit st (Opcode.SetBang idx)
     | _ -> compile_error name_syn.loc "set!: expected symbol")
  | _ -> compile_error s.loc "set! expects exactly 2 arguments"

and compile_begin st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | [_] ->
    (* (begin) — empty begin, produce void *)
    let idx = add_constant st Datum.Void in
    emit st (Opcode.Const idx)
  | _ :: exprs ->
    compile_seq st exprs ~tail
  | [] ->
    compile_error s.loc "impossible: empty list"

and compile_seq st exprs ~tail =
  match exprs with
  | [] -> ()
  | [last] -> compile_expr st last ~tail
  | e :: rest ->
    compile_expr st e ~tail:false;
    compile_seq st rest ~tail

(* Parse let bindings: ((x init) ...) → (names, inits) *)
and parse_bindings loc bindings_syntax =
  let bindings = syntax_list_to_list bindings_syntax in
  List.map (fun binding ->
    let parts = syntax_list_to_list binding in
    match parts with
    | [name_syn; init] ->
      (match name_syn.Syntax.datum with
       | Syntax.Symbol name -> (name, init)
       | _ -> compile_error name_syn.loc "let: binding name must be a symbol")
    | _ -> compile_error binding.Syntax.loc "let: binding must be (name init)"
  ) bindings
  |> List.split
  |> fun (names, inits) -> (names, inits, loc)

and compile_let st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | _ :: bindings_syn :: body when body <> [] ->
    (* Check for named let: (let name ((x init) ...) body...) *)
    (match bindings_syn.datum with
     | Syntax.Symbol name ->
       compile_named_let st s name ~tail
     | _ ->
       let (names, inits, _) = parse_bindings s.loc bindings_syn in
       (* Compile as ((lambda (names) body...) inits...) *)
       List.iter (fun init ->
         compile_expr st init ~tail:false;
         emit st Opcode.Push
       ) inits;
       let child_st = create_state st.sym_table in
       compile_body child_st body;
       let child_code = package_code child_st names false "<let>" in
       let idx = add_child st child_code in
       emit st (Opcode.MakeClosure idx);
       let n = List.length inits in
       if tail then emit st (Opcode.TailCall n)
       else emit st (Opcode.Call n))
  | _ -> compile_error s.loc "let expects bindings and at least one body expression"

and compile_named_let st (s : Syntax.t) name ~tail =
  st.current_loc <- s.loc;
  (* (let name ((x init) ...) body...)
     Desugar to: (letrec* ((name (lambda (x ...) body...))) (name init ...)) *)
  let args = syntax_list_to_list s in
  match args with
  | _ :: _name_syn :: bindings_syn :: body when body <> [] ->
    let (param_names, inits, _) = parse_bindings s.loc bindings_syn in
    (* Build the letrec* child code:
       params = [name], variadic = false
       body = set! name to closure(param_names, body), then call name with inits *)
    let outer_st = create_state st.sym_table in
    (* Compile the inner lambda *)
    let inner_st = create_state st.sym_table in
    compile_body inner_st body;
    let inner_code = package_code inner_st param_names false name in
    let inner_idx = add_child outer_st inner_code in
    emit outer_st (Opcode.MakeClosure inner_idx);
    let name_idx = add_symbol outer_st name in
    emit outer_st (Opcode.SetBang name_idx);
    (* Now call: push inits, lookup name, call *)
    List.iter (fun init ->
      compile_expr outer_st init ~tail:false;
      emit outer_st Opcode.Push
    ) inits;
    let name_idx2 = add_symbol outer_st name in
    emit outer_st (Opcode.Lookup name_idx2);
    let n = List.length inits in
    emit outer_st (Opcode.TailCall n);
    emit outer_st Opcode.Return;
    let outer_code = package_code outer_st [name] false "<named-let>" in
    (* Push void for the name param, then call *)
    let void_idx = add_constant st Datum.Void in
    emit st (Opcode.Const void_idx);
    emit st Opcode.Push;
    let child_idx = add_child st outer_code in
    emit st (Opcode.MakeClosure child_idx);
    if tail then emit st (Opcode.TailCall 1)
    else emit st (Opcode.Call 1)
  | _ -> compile_error s.loc "malformed named let"

and compile_let_star st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | _ :: bindings_syn :: body when body <> [] ->
    let bindings = syntax_list_to_list bindings_syn in
    (match bindings with
     | [] ->
       (* (let* () body...) → compile body directly *)
       compile_seq st body ~tail
     | _ ->
       (* Desugar to nested lets *)
       compile_nested_lets st bindings body ~tail)
  | _ -> compile_error s.loc "let* expects bindings and at least one body expression"

and compile_nested_lets st bindings body ~tail =
  match bindings with
  | [] -> compile_seq st body ~tail
  | binding :: rest ->
    let parts = syntax_list_to_list binding in
    (match parts with
     | [name_syn; init] ->
       (match name_syn.Syntax.datum with
        | Syntax.Symbol name ->
          (* Compile as ((lambda (name) <rest>) init) *)
          compile_expr st init ~tail:false;
          emit st Opcode.Push;
          let child_st = create_state st.sym_table in
          compile_nested_lets child_st rest body ~tail:true;
          emit child_st Opcode.Return;
          let child_code = package_code child_st [name] false "<let*>" in
          let idx = add_child st child_code in
          emit st (Opcode.MakeClosure idx);
          if tail then emit st (Opcode.TailCall 1)
          else emit st (Opcode.Call 1)
        | _ -> compile_error name_syn.loc "let*: binding name must be a symbol")
     | _ -> compile_error binding.Syntax.loc "let*: binding must be (name init)")

and compile_letrec_star st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | _ :: bindings_syn :: body when body <> [] ->
    let (names, inits, _) = parse_bindings s.loc bindings_syn in
    let n = List.length names in
    (* Push void for each binding *)
    let void_idx = add_constant st Datum.Void in
    for _ = 1 to n do
      emit st (Opcode.Const void_idx);
      emit st Opcode.Push
    done;
    (* Build child: set! each binding, then body *)
    let child_st = create_state st.sym_table in
    List.iter2 (fun name init ->
      compile_expr child_st init ~tail:false;
      let idx = add_symbol child_st name in
      emit child_st (Opcode.SetBang idx)
    ) names inits;
    compile_seq child_st body ~tail:true;
    emit child_st Opcode.Return;
    let child_code = package_code child_st names false "<letrec*>" in
    let idx = add_child st child_code in
    emit st (Opcode.MakeClosure idx);
    if tail then emit st (Opcode.TailCall n)
    else emit st (Opcode.Call n)
  | _ -> compile_error s.loc "letrec* expects bindings and at least one body expression"

and compile_cond st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | _ :: clauses ->
    compile_cond_clauses st s.loc clauses ~tail
  | _ -> compile_error s.loc "cond expects at least one clause"

and compile_cond_clauses st loc clauses ~tail =
  let end_jumps = ref [] in
  let rec go = function
    | [] ->
      (* No matching clause → void *)
      let void_idx = add_constant st Datum.Void in
      emit st (Opcode.Const void_idx)
    | [clause] ->
      let parts = syntax_list_to_list clause in
      (match parts with
       | [{ Syntax.datum = Syntax.Symbol "else"; _ }] ->
         compile_error clause.loc "cond: else clause must have at least one expression"
       | { Syntax.datum = Syntax.Symbol "else"; _ } :: body ->
         compile_seq st body ~tail
       | [test] ->
         (* (cond (test)) → return test value *)
         compile_expr st test ~tail
       | [test; { Syntax.datum = Syntax.Symbol "=>"; _ }; proc_expr] ->
         compile_cond_arrow st test proc_expr end_jumps ~tail;
         let void_idx = add_constant st Datum.Void in
         emit st (Opcode.Const void_idx)
       | test :: body ->
         compile_expr st test ~tail:false;
         let jf_pc = current_pc st in
         emit st (Opcode.JumpFalse 0);
         compile_seq st body ~tail;
         let jump_pc = current_pc st in
         emit st (Opcode.Jump 0);
         end_jumps := jump_pc :: !end_jumps;
         let next_pc = current_pc st in
         patch_jump st jf_pc next_pc;
         (* No more clauses → void *)
         let void_idx = add_constant st Datum.Void in
         emit st (Opcode.Const void_idx)
       | [] -> compile_error clause.loc "cond: empty clause")
    | clause :: rest ->
      let parts = syntax_list_to_list clause in
      (match parts with
       | { Syntax.datum = Syntax.Symbol "else"; _ } :: _ when rest <> [] ->
         compile_error clause.loc "cond: else clause must be last"
       | { Syntax.datum = Syntax.Symbol "else"; _ } :: body ->
         compile_seq st body ~tail
       | [test] ->
         (* (cond (test) ...) → if test is truthy, return test value *)
         compile_expr st test ~tail:false;
         let jf_pc = current_pc st in
         emit st (Opcode.JumpFalse 0);
         let jump_pc = current_pc st in
         emit st (Opcode.Jump 0);
         end_jumps := jump_pc :: !end_jumps;
         let next_pc = current_pc st in
         patch_jump st jf_pc next_pc;
         go rest
       | [test; { Syntax.datum = Syntax.Symbol "=>"; _ }; proc_expr] ->
         compile_cond_arrow st test proc_expr end_jumps ~tail;
         go rest
       | test :: body ->
         compile_expr st test ~tail:false;
         let jf_pc = current_pc st in
         emit st (Opcode.JumpFalse 0);
         compile_seq st body ~tail;
         let jump_pc = current_pc st in
         emit st (Opcode.Jump 0);
         end_jumps := jump_pc :: !end_jumps;
         let next_pc = current_pc st in
         patch_jump st jf_pc next_pc;
         go rest
       | [] -> compile_error clause.loc "cond: empty clause")
  in
  go clauses;
  let end_pc = current_pc st in
  List.iter (fun pc -> patch_jump st pc end_pc) !end_jumps;
  ignore loc

and compile_cond_arrow st test proc_expr end_jumps ~tail =
  (* (cond (test => proc) ...) — compile test, if truthy call proc with test value *)
  compile_expr st test ~tail:false;
  let jf_pc = current_pc st in
  emit st (Opcode.JumpFalse 0);
  (* Test was truthy — push test value, build closure (lambda (%v) (proc %v)), call it *)
  emit st Opcode.Push;
  let child_st = create_state st.sym_table in
  let val_name = "%cond-val" in
  let val_idx = add_symbol child_st val_name in
  emit child_st (Opcode.Lookup val_idx);
  emit child_st Opcode.Push;
  compile_expr child_st proc_expr ~tail:false;
  emit child_st (Opcode.TailCall 1);
  emit child_st Opcode.Return;
  let child_code = package_code child_st [val_name] false "<cond-arrow>" in
  let idx = add_child st child_code in
  emit st (Opcode.MakeClosure idx);
  if tail then emit st (Opcode.TailCall 1)
  else emit st (Opcode.Call 1);
  let jump_pc = current_pc st in
  emit st (Opcode.Jump 0);
  end_jumps := jump_pc :: !end_jumps;
  let next_pc = current_pc st in
  patch_jump st jf_pc next_pc

and compile_case st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | _ :: key_expr :: clauses when clauses <> [] ->
    (* Bind key in a let, then chain eqv? tests *)
    compile_expr st key_expr ~tail:false;
    emit st Opcode.Push;
    let child_st = create_state st.sym_table in
    let key_name = "%case-key" in
    compile_case_clauses child_st s.loc key_name clauses;
    emit child_st Opcode.Return;
    let child_code = package_code child_st [key_name] false "<case>" in
    let idx = add_child st child_code in
    emit st (Opcode.MakeClosure idx);
    if tail then emit st (Opcode.TailCall 1)
    else emit st (Opcode.Call 1)
  | _ -> compile_error s.loc "case expects a key expression and at least one clause"

and compile_case_clauses st loc key_name clauses =
  let end_jumps = ref [] in
  let rec go = function
    | [] ->
      let void_idx = add_constant st Datum.Void in
      emit st (Opcode.Const void_idx)
    | [clause] ->
      let parts = syntax_list_to_list clause in
      (match parts with
       | { Syntax.datum = Syntax.Symbol "else"; _ } :: body ->
         compile_seq st body ~tail:true
       | datums_syn :: { Syntax.datum = Syntax.Symbol "=>"; _ } :: [proc_expr] ->
         let datums = syntax_list_to_list datums_syn in
         compile_case_datums_arrow st key_name datums proc_expr end_jumps;
         let void_idx = add_constant st Datum.Void in
         emit st (Opcode.Const void_idx)
       | datums_syn :: body when body <> [] ->
         let datums = syntax_list_to_list datums_syn in
         compile_case_datums st key_name datums body end_jumps;
         let void_idx = add_constant st Datum.Void in
         emit st (Opcode.Const void_idx)
       | _ -> compile_error clause.loc "case: malformed clause")
    | clause :: rest ->
      let parts = syntax_list_to_list clause in
      (match parts with
       | { Syntax.datum = Syntax.Symbol "else"; _ } :: _ when rest <> [] ->
         compile_error clause.loc "case: else clause must be last"
       | { Syntax.datum = Syntax.Symbol "else"; _ } :: body ->
         compile_seq st body ~tail:true
       | datums_syn :: { Syntax.datum = Syntax.Symbol "=>"; _ } :: [proc_expr] ->
         let datums = syntax_list_to_list datums_syn in
         compile_case_datums_arrow st key_name datums proc_expr end_jumps;
         go rest
       | datums_syn :: body when body <> [] ->
         let datums = syntax_list_to_list datums_syn in
         compile_case_datums st key_name datums body end_jumps;
         go rest
       | _ -> compile_error clause.loc "case: malformed clause")
  in
  go clauses;
  let end_pc = current_pc st in
  List.iter (fun pc -> patch_jump st pc end_pc) !end_jumps;
  ignore loc

and compile_case_datums st key_name datums body end_jumps =
  (* For each datum, emit: lookup key; push; const datum; push; lookup eqv?; call 2; jumpfalse next *)
  let body_jumps = ref [] in
  List.iter (fun d ->
    let key_idx = add_symbol st key_name in
    emit st (Opcode.Lookup key_idx);
    emit st Opcode.Push;
    let v = Syntax.to_datum d in
    let c_idx = add_constant st v in
    emit st (Opcode.Const c_idx);
    emit st Opcode.Push;
    let eqv_idx = add_symbol st "eqv?" in
    emit st (Opcode.Lookup eqv_idx);
    emit st (Opcode.Call 2);
    let jf_pc = current_pc st in
    emit st (Opcode.JumpFalse 0);
    let jump_pc = current_pc st in
    emit st (Opcode.Jump 0);
    body_jumps := jump_pc :: !body_jumps;
    let next_pc = current_pc st in
    patch_jump st jf_pc next_pc
  ) datums;
  (* If none matched, jump to next clause *)
  let skip_pc = current_pc st in
  emit st (Opcode.Jump 0);
  (* Patch body jumps to here *)
  let body_target = current_pc st in
  List.iter (fun pc -> patch_jump st pc body_target) !body_jumps;
  compile_seq st body ~tail:true;
  let end_pc = current_pc st in
  end_jumps := end_pc :: !end_jumps;
  emit st (Opcode.Jump 0);
  end_jumps := (current_pc st - 1) :: (List.tl !end_jumps);
  (* Patch skip jump to after body *)
  let after_body = current_pc st in
  patch_jump st skip_pc after_body

and compile_case_datums_arrow st key_name datums proc_expr end_jumps =
  (* Like compile_case_datums but calls proc with key instead of compiling body *)
  let body_jumps = ref [] in
  List.iter (fun d ->
    let key_idx = add_symbol st key_name in
    emit st (Opcode.Lookup key_idx);
    emit st Opcode.Push;
    let v = Syntax.to_datum d in
    let c_idx = add_constant st v in
    emit st (Opcode.Const c_idx);
    emit st Opcode.Push;
    let eqv_idx = add_symbol st "eqv?" in
    emit st (Opcode.Lookup eqv_idx);
    emit st (Opcode.Call 2);
    let jf_pc = current_pc st in
    emit st (Opcode.JumpFalse 0);
    let jump_pc = current_pc st in
    emit st (Opcode.Jump 0);
    body_jumps := jump_pc :: !body_jumps;
    let next_pc = current_pc st in
    patch_jump st jf_pc next_pc
  ) datums;
  (* If none matched, jump to next clause *)
  let skip_pc = current_pc st in
  emit st (Opcode.Jump 0);
  (* Patch body jumps to here: push key (arg), compile proc (acc), tail-call *)
  let body_target = current_pc st in
  List.iter (fun pc -> patch_jump st pc body_target) !body_jumps;
  let key_idx = add_symbol st key_name in
  emit st (Opcode.Lookup key_idx);
  emit st Opcode.Push;
  compile_expr st proc_expr ~tail:false;
  emit st (Opcode.TailCall 1);
  let end_pc = current_pc st in
  end_jumps := end_pc :: !end_jumps;
  emit st (Opcode.Jump 0);
  end_jumps := (current_pc st - 1) :: (List.tl !end_jumps);
  (* Patch skip jump to after body *)
  let after_body = current_pc st in
  patch_jump st skip_pc after_body

and compile_do st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  (* (do ((var init step) ...) (test expr ...) body ...)
     Desugar to named let:
     (let loop ((var init) ...) (if test (begin expr ...) (begin body ... (loop step ...)))) *)
  let args = syntax_list_to_list s in
  match args with
  | _ :: var_clauses_syn :: test_clause_syn :: body ->
    let var_clauses = syntax_list_to_list var_clauses_syn in
    let test_parts = syntax_list_to_list test_clause_syn in
    let loc = s.loc in
    (* Parse variable clauses *)
    let parsed_vars = List.map (fun vc ->
      let parts = syntax_list_to_list vc in
      match parts with
      | [name_syn; init; step] ->
        (match name_syn.datum with
         | Syntax.Symbol name -> (name, init, Some step)
         | _ -> compile_error name_syn.loc "do: variable name must be a symbol")
      | [name_syn; init] ->
        (match name_syn.datum with
         | Syntax.Symbol name -> (name, init, None)
         | _ -> compile_error name_syn.loc "do: variable name must be a symbol")
      | _ -> compile_error vc.loc "do: malformed variable clause"
    ) var_clauses in
    let param_names = List.map (fun (n, _, _) -> n) parsed_vars in
    let inits = List.map (fun (_, i, _) -> i) parsed_vars in
    let test = match test_parts with
      | t :: _ -> t
      | [] -> compile_error test_clause_syn.loc "do: test clause must have a test"
    in
    let result_exprs = match test_parts with
      | _ :: rest -> rest
      | [] -> []
    in
    (* Build the loop body:
       (if test
         (begin result_exprs...)  ; or void if empty
         (begin body... (loop step...))) *)
    let loop_name = "%do-loop" in
    let steps = List.map (fun (name, _, step) ->
      match step with
      | Some s -> s
      | None -> { Syntax.datum = Syntax.Symbol name; loc }
    ) parsed_vars in
    (* Build as letrec* pattern: similar to named let *)
    let outer_st = create_state st.sym_table in
    let inner_st = create_state st.sym_table in
    (* Inner lambda body: if test then result else body+recurse *)
    compile_expr inner_st test ~tail:false;
    let jf_pc = current_pc inner_st in
    emit inner_st (Opcode.JumpFalse 0);
    (* Test is true → evaluate result exprs *)
    (match result_exprs with
     | [] ->
       let void_idx = add_constant inner_st Datum.Void in
       emit inner_st (Opcode.Const void_idx)
     | _ -> compile_seq inner_st result_exprs ~tail:true);
    emit inner_st Opcode.Return;
    (* Test is false → evaluate body, then recurse *)
    let false_target = current_pc inner_st in
    patch_jump inner_st jf_pc false_target;
    List.iter (fun b -> compile_expr inner_st b ~tail:false) body;
    (* Push steps, lookup loop, tail-call *)
    List.iter (fun step ->
      compile_expr inner_st step ~tail:false;
      emit inner_st Opcode.Push
    ) steps;
    let loop_idx = add_symbol inner_st loop_name in
    emit inner_st (Opcode.Lookup loop_idx);
    let n = List.length steps in
    emit inner_st (Opcode.TailCall n);
    emit inner_st Opcode.Return;
    let inner_code = package_code inner_st param_names false loop_name in
    (* Outer: set! loop to closure, then call loop with inits *)
    let inner_child_idx = add_child outer_st inner_code in
    emit outer_st (Opcode.MakeClosure inner_child_idx);
    let loop_sym_idx = add_symbol outer_st loop_name in
    emit outer_st (Opcode.SetBang loop_sym_idx);
    List.iter (fun init ->
      compile_expr outer_st init ~tail:false;
      emit outer_st Opcode.Push
    ) inits;
    let loop_lookup_idx = add_symbol outer_st loop_name in
    emit outer_st (Opcode.Lookup loop_lookup_idx);
    emit outer_st (Opcode.TailCall n);
    emit outer_st Opcode.Return;
    let outer_code = package_code outer_st [loop_name] false "<do>" in
    let void_idx = add_constant st Datum.Void in
    emit st (Opcode.Const void_idx);
    emit st Opcode.Push;
    let child_idx = add_child st outer_code in
    emit st (Opcode.MakeClosure child_idx);
    if tail then emit st (Opcode.TailCall 1)
    else emit st (Opcode.Call 1)
  | _ -> compile_error s.loc "do expects variable clauses and a test clause"

and compile_and st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | [_] ->
    (* (and) → #t *)
    let idx = add_constant st (Datum.Bool true) in
    emit st (Opcode.Const idx)
  | [_; e] ->
    (* (and e) → e *)
    compile_expr st e ~tail
  | _ :: exprs ->
    (* (and e1 e2 ... en) → chain of JumpFalse *)
    let jumps = ref [] in
    let rec go = function
      | [] -> ()
      | [last] -> compile_expr st last ~tail
      | e :: rest ->
        compile_expr st e ~tail:false;
        let jf_pc = current_pc st in
        emit st (Opcode.JumpFalse 0);
        jumps := jf_pc :: !jumps;
        go rest
    in
    go exprs;
    let end_pc = current_pc st in
    List.iter (fun pc -> patch_jump st pc end_pc) !jumps
  | [] -> compile_error s.loc "impossible: empty list"

and compile_or st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | [_] ->
    (* (or) → #f *)
    let idx = add_constant st (Datum.Bool false) in
    emit st (Opcode.Const idx)
  | [_; e] ->
    (* (or e) → e *)
    compile_expr st e ~tail
  | _ :: exprs ->
    (* (or e1 e2 ... en) *)
    let end_jumps = ref [] in
    let rec go = function
      | [] -> ()
      | [last] -> compile_expr st last ~tail
      | e :: rest ->
        compile_expr st e ~tail:false;
        let jf_pc = current_pc st in
        emit st (Opcode.JumpFalse 0);  (* if false, try next *)
        let jump_pc = current_pc st in
        emit st (Opcode.Jump 0);  (* if true, jump to end *)
        end_jumps := jump_pc :: !end_jumps;
        let next_pc = current_pc st in
        patch_jump st jf_pc next_pc;
        go rest
    in
    go exprs;
    let end_pc = current_pc st in
    List.iter (fun pc -> patch_jump st pc end_pc) !end_jumps
  | [] -> compile_error s.loc "impossible: empty list"

and compile_when st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | _ :: test :: body when body <> [] ->
    compile_expr st test ~tail:false;
    let jf_pc = current_pc st in
    emit st (Opcode.JumpFalse 0);
    compile_seq st body ~tail;
    let jump_pc = current_pc st in
    emit st (Opcode.Jump 0);
    let skip_target = current_pc st in
    patch_jump st jf_pc skip_target;
    let void_idx = add_constant st Datum.Void in
    emit st (Opcode.Const void_idx);
    let end_target = current_pc st in
    patch_jump st jump_pc end_target
  | _ -> compile_error s.loc "when expects a test and at least one body expression"

and compile_unless st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | _ :: test :: body when body <> [] ->
    compile_expr st test ~tail:false;
    let jf_pc = current_pc st in
    emit st (Opcode.JumpFalse 0);
    (* test was truthy → skip to void *)
    let void_idx = add_constant st Datum.Void in
    emit st (Opcode.Const void_idx);
    let jump_pc = current_pc st in
    emit st (Opcode.Jump 0);
    (* test was false → execute body *)
    let body_target = current_pc st in
    patch_jump st jf_pc body_target;
    compile_seq st body ~tail;
    let end_target = current_pc st in
    patch_jump st jump_pc end_target
  | _ -> compile_error s.loc "unless expects a test and at least one body expression"

and compile_call st (s : Syntax.t) ~tail =
  st.current_loc <- s.loc;
  let args = syntax_list_to_list s in
  match args with
  | [] -> compile_error s.loc "empty application"
  | proc :: operands ->
    (* Evaluate arguments left-to-right, push each *)
    List.iter (fun arg ->
      compile_expr st arg ~tail:false;
      emit st Opcode.Push
    ) operands;
    (* Evaluate operator last (into acc) *)
    compile_expr st proc ~tail:false;
    let n = List.length operands in
    st.current_loc <- s.loc;
    if tail then
      emit st (Opcode.TailCall n)
    else
      emit st (Opcode.Call n)

and compile_body st body =
  match body with
  | [] -> failwith "compile_body: empty body (impossible)"
  | _ ->
    (* Scan for leading internal defines *)
    let (defs, rest) = scan_internal_defines body in
    (match defs with
     | [] ->
       compile_seq st body ~tail:true;
       emit st Opcode.Return
     | _ ->
       (* Wrap in letrec*: push voids, create child with set! inits + body *)
       let names = List.map fst defs in
       let inits = List.map snd defs in
       let n = List.length names in
       let void_idx = add_constant st Datum.Void in
       for _ = 1 to n do
         emit st (Opcode.Const void_idx);
         emit st Opcode.Push
       done;
       let child_st = create_state st.sym_table in
       List.iter2 (fun name init ->
         compile_expr child_st init ~tail:false;
         let idx = add_symbol child_st name in
         emit child_st (Opcode.SetBang idx)
       ) names inits;
       compile_seq child_st rest ~tail:true;
       emit child_st Opcode.Return;
       let child_code = package_code child_st names false "<letrec*>" in
       let idx = add_child st child_code in
       emit st (Opcode.MakeClosure idx);
       emit st (Opcode.TailCall n);
       emit st Opcode.Return)

and scan_internal_defines forms =
  let rec go acc = function
    | [] -> (List.rev acc, [])
    | (form :: rest) as all ->
      let parts = try syntax_list_to_list form with _ -> [] in
      (match parts with
       | { Syntax.datum = Syntax.Symbol "define"; _ } :: name_syn :: _ ->
         (match name_syn.datum with
          | Syntax.Symbol name ->
            (* (define x expr) *)
            (match parts with
             | [_; _; init] -> go ((name, init) :: acc) rest
             | _ -> (List.rev acc, all))
          | Syntax.Pair (fname, params) ->
            (* (define (f params...) body...) *)
            (match fname.datum with
             | Syntax.Symbol name ->
               let body = List.tl (List.tl parts) in
               (* Build lambda syntax *)
               let loc = form.Syntax.loc in
               let lambda_syn = make_lambda_syntax loc params body in
               go ((name, lambda_syn) :: acc) rest
             | _ -> (List.rev acc, all))
          | _ -> (List.rev acc, all))
       | _ -> (List.rev acc, all))
  in
  go [] forms

and make_lambda_syntax loc params body =
  (* Construct (lambda (params...) body...) as Syntax.t *)
  let lambda_sym = { Syntax.datum = Syntax.Symbol "lambda"; loc } in
  let body_list = List.fold_right (fun b acc ->
    { Syntax.datum = Syntax.Pair (b, acc); loc }
  ) body { Syntax.datum = Syntax.Nil; loc } in
  let inner = { Syntax.datum = Syntax.Pair (params, body_list); loc } in
  { Syntax.datum = Syntax.Pair (lambda_sym, inner); loc }

and package_code st param_names variadic name =
  let params = Array.of_list
    (List.map (Symbol.intern st.sym_table) param_names) in
  { Datum.instructions = Array.of_list (List.rev st.instructions);
    source_map = Array.of_list (List.rev st.source_locs);
    constants = Array.of_list (List.rev st.constants);
    symbols = Array.of_list (List.rev st.symbols);
    children = Array.of_list (List.rev st.children);
    params;
    variadic;
    name }

let compile sym_table (s : Syntax.t) : Datum.code =
  let st = create_state sym_table in
  st.current_loc <- s.loc;
  compile_expr st s ~tail:false;
  emit st Opcode.Halt;
  package_code st [] false "<top-level>"
