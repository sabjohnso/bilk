exception Compile_error of Loc.t * string

let compile_error loc msg = raise (Compile_error (loc, msg))

(* --- Mutable compilation state --- *)

type state = {
  sym_table : Symbol.table;
  mutable instructions : Opcode.t list;  (* reversed *)
  mutable constants : Datum.t list;      (* reversed *)
  mutable symbols : Symbol.t list;       (* reversed *)
  mutable children : Datum.code list;    (* reversed *)
}

let create_state sym_table =
  { sym_table;
    instructions = [];
    constants = [];
    symbols = [];
    children = [] }

let emit st op =
  st.instructions <- op :: st.instructions

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
  | Syntax.Bool _ | Syntax.Fixnum _ | Syntax.Flonum _
  | Syntax.Char _ | Syntax.Str _ -> true
  | _ -> false

(* --- Main compilation --- *)

let rec compile_expr st (s : Syntax.t) ~tail =
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

  | _ ->
    compile_call st s ~tail

and compile_quote st (s : Syntax.t) =
  let args = syntax_list_to_list s in
  (match args with
   | [_; datum] ->
     let v = Syntax.to_datum datum in
     let idx = add_constant st v in
     emit st (Opcode.Const idx)
   | _ -> compile_error s.loc "quote expects exactly 1 argument")

and compile_if st (s : Syntax.t) ~tail =
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
  let args = syntax_list_to_list s in
  match args with
  | [_; name_syn; expr] when (match name_syn.datum with Syntax.Symbol _ -> true | _ -> false) ->
    (* (define x expr) *)
    let name = match name_syn.datum with Syntax.Symbol n -> n | _ -> assert false in
    compile_expr st expr ~tail:false;
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
          emit st (Opcode.MakeClosure child_idx);
          let sym_idx = add_symbol st name in
          emit st (Opcode.Define sym_idx)
        | _ -> compile_error fname.loc "define: expected symbol as function name")
     | _ -> assert false)
  | _ ->
    compile_error s.loc "malformed define"

and compile_set_bang st (s : Syntax.t) =
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

and compile_call st (s : Syntax.t) ~tail =
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
    if tail then
      emit st (Opcode.TailCall n)
    else
      emit st (Opcode.Call n)

and compile_body st body =
  match body with
  | [] -> failwith "compile_body: empty body (impossible)"
  | _ -> compile_seq st body ~tail:true;
         emit st Opcode.Return

and package_code st param_names variadic name =
  let params = Array.of_list
    (List.map (Symbol.intern st.sym_table) param_names) in
  { Datum.instructions = Array.of_list (List.rev st.instructions);
    constants = Array.of_list (List.rev st.constants);
    symbols = Array.of_list (List.rev st.symbols);
    children = Array.of_list (List.rev st.children);
    params;
    variadic;
    name }

let compile sym_table (s : Syntax.t) : Datum.code =
  let st = create_state sym_table in
  compile_expr st s ~tail:false;
  emit st Opcode.Halt;
  package_code st [] false "<top-level>"
