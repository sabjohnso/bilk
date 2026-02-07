let compile_error loc msg = raise (Compiler.Compile_error (loc, msg))

(* --- Syntactic environment types --- *)

type binding =
  | Var
  | Core of string
  | Macro of transformer

and transformer = {
  literals : string list;
  rules : rule list;
  def_env : env_frame list;
}

and rule = {
  pattern : Syntax.t;
  template : Syntax.t;
}

and env_frame = (string, binding) Hashtbl.t

type syn_env = env_frame list

(* Ensure all constructors are considered used *)
let _ = Var
let _ = fun t -> t.def_env

(* --- Syntactic environment operations --- *)

let syn_lookup (env : syn_env) name =
  let rec go = function
    | [] -> None
    | frame :: rest ->
      (match Hashtbl.find_opt frame name with
       | Some b -> Some b
       | None -> go rest)
  in
  go env

let core_forms = [
  "if"; "lambda"; "define"; "set!"; "begin"; "quote";
  "let"; "let*"; "letrec"; "letrec*";
  "cond"; "case"; "do"; "and"; "or"; "when"; "unless";
  "define-syntax"; "let-syntax"; "letrec-syntax";
  "quasiquote"; "guard"; "define-record-type"; "syntax-error";
  "cond-expand"; "include"; "include-ci";
]

let core_env () =
  let frame = Hashtbl.create 32 in
  List.iter (fun name -> Hashtbl.replace frame name (Core name)) core_forms;
  [frame]

(* --- Syntax list helpers --- *)

let rec syntax_list_to_list s =
  match s.Syntax.datum with
  | Syntax.Nil -> []
  | Syntax.Pair (car, cdr) -> car :: syntax_list_to_list cdr
  | _ -> []

let list_to_syntax loc elts =
  List.fold_right (fun elt acc ->
    { Syntax.datum = Syntax.Pair (elt, acc); loc }
  ) elts { Syntax.datum = Syntax.Nil; loc }

(* --- Pattern matching types --- *)

type pattern =
  | Pat_var of string
  | Pat_underscore
  | Pat_literal of string
  | Pat_const of Syntax.datum
  | Pat_pair of pattern * pattern
  | Pat_ellipsis of pattern list * pattern * pattern list
  | Pat_ellipsis_dot of pattern list * pattern * pattern list * pattern
  | Pat_vector of pattern list
  | Pat_vector_ellipsis of pattern list * pattern * pattern list
  | Pat_nil

type match_val =
  | Single of Syntax.t
  | Repeated of match_val list

type match_env = (string, match_val) Hashtbl.t

(* --- Pattern parsing --- *)

let is_ellipsis s =
  match s.Syntax.datum with
  | Syntax.Symbol "..." -> true
  | _ -> false

let rec parse_pattern ~literals (s : Syntax.t) : pattern =
  match s.datum with
  | Syntax.Symbol "_" when not (List.mem "_" literals) -> Pat_underscore
  | Syntax.Symbol name ->
    if List.mem name literals then Pat_literal name
    else Pat_var name
  | Syntax.Bool _ | Syntax.Fixnum _ | Syntax.Char _ | Syntax.Str _ ->
    Pat_const s.datum
  | Syntax.Nil -> Pat_nil
  | Syntax.Pair _ ->
    (* Check for improper list *)
    let rec check_proper s = match s.Syntax.datum with
      | Syntax.Nil -> true
      | Syntax.Pair (_, cdr) -> check_proper cdr
      | _ -> false
    in
    if check_proper s then
      let elts = syntax_list_to_list s in
      parse_list_pattern ~literals elts s.loc
    else begin
      (* Improper list — collect proper elements and dotted tail *)
      let rec collect_improper s =
        match s.Syntax.datum with
        | Syntax.Pair (car, cdr) ->
          let (elts, tail) = collect_improper cdr in
          (car :: elts, tail)
        | _ -> ([], s)
      in
      let (elts, tail) = collect_improper s in
      let has_ell = List.exists is_ellipsis elts in
      if has_ell then
        parse_dotted_ellipsis_pattern ~literals elts tail s.loc
      else
        (* No ellipsis — build nested pairs ending with tail *)
        List.fold_right (fun elt acc ->
          Pat_pair (parse_pattern ~literals elt, acc)
        ) elts (parse_pattern ~literals tail)
    end
  | Syntax.Vector elts ->
    let pats = Array.to_list (Array.map (parse_pattern ~literals) elts) in
    (* Check for ellipsis in vector *)
    let has_ell = Array.exists is_ellipsis elts in
    if has_ell then
      parse_vector_ellipsis_pattern ~literals pats
    else
      Pat_vector pats
  | _ -> Pat_const s.datum

and parse_list_pattern ~literals elts loc =
  (* Check if there's an ellipsis *)
  let has_ell = List.exists is_ellipsis
    (List.map (fun e -> e) elts) in
  if not has_ell then
    (* Simple list → nested pairs *)
    List.fold_right (fun elt acc ->
      Pat_pair (parse_pattern ~literals elt, acc)
    ) elts Pat_nil
  else begin
    (* Find ellipsis position *)
    let rec find_ell pre = function
      | [] -> assert false
      | elt :: rest ->
        if is_ellipsis elt then
          (* Previous element is the repeated pattern.
             pre is in reverse order, so head is the element just before ... *)
          (match pre with
           | [] ->
             compile_error loc "ellipsis must follow a pattern"
           | last :: before ->
             (List.rev before, last, rest))
        else
          find_ell (elt :: pre) rest
    in
    let (before, repeated_syn, after) = find_ell [] elts in
    let pre_pats = List.map (parse_pattern ~literals) before in
    let rep_pat = parse_pattern ~literals repeated_syn in
    let post_pats = List.map (parse_pattern ~literals) after in
    Pat_ellipsis (pre_pats, rep_pat, post_pats)
  end

and parse_vector_ellipsis_pattern ~literals:_ pats =
  (* pats includes Pat_var "..." for the ellipsis — find it and split *)
  let is_ell_pat = function Pat_var "..." -> true | _ -> false in
  let rec find_ell pre = function
    | [] -> failwith "internal: no ellipsis found in vector pattern"
    | p :: rest ->
      if is_ell_pat p then
        (match pre with
         | [] -> failwith "ellipsis must follow a pattern in vector"
         | last :: before -> (List.rev before, last, rest))
      else
        find_ell (p :: pre) rest
  in
  let (pre, rep, post) = find_ell [] pats in
  Pat_vector_ellipsis (pre, rep, post)

and parse_dotted_ellipsis_pattern ~literals elts tail loc =
  let rec find_ell pre = function
    | [] -> assert false
    | elt :: rest ->
      if is_ellipsis elt then
        (match pre with
         | [] ->
           compile_error loc "ellipsis must follow a pattern"
         | last :: before ->
           (List.rev before, last, rest))
      else
        find_ell (elt :: pre) rest
  in
  let (before, repeated_syn, after) = find_ell [] elts in
  let pre_pats = List.map (parse_pattern ~literals) before in
  let rep_pat = parse_pattern ~literals repeated_syn in
  let post_pats = List.map (parse_pattern ~literals) after in
  let tail_pat = parse_pattern ~literals tail in
  Pat_ellipsis_dot (pre_pats, rep_pat, post_pats, tail_pat)

(* --- Pattern matching --- *)

let rec match_pattern (pat : pattern) (s : Syntax.t) (env : match_env) : bool =
  match pat with
  | Pat_underscore -> true
  | Pat_var name ->
    Hashtbl.replace env name (Single s); true
  | Pat_literal name ->
    (match s.datum with
     | Syntax.Symbol sname -> String.equal name sname
     | _ -> false)
  | Pat_const expected ->
    Syntax.equal_datum { Syntax.datum = expected; loc = s.loc } s
  | Pat_nil ->
    (match s.datum with Syntax.Nil -> true | _ -> false)
  | Pat_pair (pcar, pcdr) ->
    (match s.datum with
     | Syntax.Pair (car, cdr) ->
       match_pattern pcar car env && match_pattern pcdr cdr env
     | _ -> false)
  | Pat_ellipsis (pre, rep, post) ->
    match_ellipsis pre rep post s env
  | Pat_ellipsis_dot (pre, rep, post, tail) ->
    match_ellipsis_dot pre rep post tail s env
  | Pat_vector pats ->
    (match s.datum with
     | Syntax.Vector elts ->
       if Array.length elts <> List.length pats then false
       else
         List.for_all2 (fun p e -> match_pattern p e env) pats (Array.to_list elts)
     | _ -> false)
  | Pat_vector_ellipsis (pre, rep, post) ->
    (match s.datum with
     | Syntax.Vector elts ->
       let n_pre = List.length pre in
       let n_post = List.length post in
       let n_total = Array.length elts in
       if n_total < n_pre + n_post then false
       else begin
         let elts_list = Array.to_list elts in
         (* Match prefix *)
         let pre_elts = List.filteri (fun i _ -> i < n_pre) elts_list in
         let ok = List.for_all2 (fun p e -> match_pattern p e env) pre pre_elts in
         if not ok then false
         else begin
           (* Match suffix *)
           let post_elts = List.filteri (fun i _ -> i >= n_total - n_post) elts_list in
           let ok2 = List.for_all2 (fun p e -> match_pattern p e env) post post_elts in
           if not ok2 then false
           else begin
             (* Match repeated elements *)
             let rep_elts = List.filteri (fun i _ ->
               i >= n_pre && i < n_total - n_post) elts_list in
             let rep_vars = collect_pattern_vars rep in
             List.iter (fun name ->
               if not (Hashtbl.mem env name) then
                 Hashtbl.replace env name (Repeated [])
             ) rep_vars;
             let sub_envs = List.map (fun elt ->
               let sub = Hashtbl.create 8 in
               let ok = match_pattern rep elt sub in
               (ok, sub)
             ) rep_elts in
             let all_ok = List.for_all fst sub_envs in
             if not all_ok then false
             else begin
               List.iter (fun name ->
                 let vals = List.map (fun (_, sub) ->
                   match Hashtbl.find_opt sub name with
                   | Some v -> v
                   | None -> Single { Syntax.datum = Syntax.Nil; loc = Loc.none }
                 ) sub_envs in
                 Hashtbl.replace env name (Repeated vals)
               ) rep_vars;
               true
             end
           end
         end
       end
     | _ -> false)

and match_ellipsis pre rep post (s : Syntax.t) env =
  (* Collect all elements into a list *)
  let rec collect s =
    match s.Syntax.datum with
    | Syntax.Nil -> []
    | Syntax.Pair (car, cdr) -> car :: collect cdr
    | _ -> [s]  (* improper tail *)
  in
  let elts = collect s in
  let n_pre = List.length pre in
  let n_post = List.length post in
  let n_total = List.length elts in
  if n_total < n_pre + n_post then false
  else begin
    (* Match prefix *)
    let pre_elts = List.filteri (fun i _ -> i < n_pre) elts in
    let ok = List.for_all2 (fun p e -> match_pattern p e env) pre pre_elts in
    if not ok then false
    else begin
      (* Match suffix *)
      let post_elts = List.filteri (fun i _ -> i >= n_total - n_post) elts in
      let ok2 = List.for_all2 (fun p e -> match_pattern p e env) post post_elts in
      if not ok2 then false
      else begin
        (* Match repeated elements *)
        let rep_elts = List.filteri (fun i _ -> i >= n_pre && i < n_total - n_post) elts in
        (* Collect pattern variable names from repeated pattern *)
        let rep_vars = collect_pattern_vars rep in
        (* Initialize repeated bindings *)
        List.iter (fun name ->
          if not (Hashtbl.mem env name) then
            Hashtbl.replace env name (Repeated [])
        ) rep_vars;
        (* Match each repeated element *)
        let sub_envs = List.map (fun elt ->
          let sub = Hashtbl.create 8 in
          let ok = match_pattern rep elt sub in
          (ok, sub)
        ) rep_elts in
        let all_ok = List.for_all fst sub_envs in
        if not all_ok then false
        else begin
          (* Collect repeated values *)
          List.iter (fun name ->
            let vals = List.map (fun (_, sub) ->
              match Hashtbl.find_opt sub name with
              | Some v -> v
              | None -> Single { Syntax.datum = Syntax.Nil; loc = Loc.none }
            ) sub_envs in
            Hashtbl.replace env name (Repeated vals)
          ) rep_vars;
          true
        end
      end
    end
  end

and match_ellipsis_dot pre rep post tail (s : Syntax.t) env =
  (* Collect proper elements and the improper tail *)
  let rec collect s =
    match s.Syntax.datum with
    | Syntax.Pair (car, cdr) ->
      let (elts, tl) = collect cdr in
      (car :: elts, tl)
    | _ -> ([], s)
  in
  let (elts, tail_syn) = collect s in
  let n_pre = List.length pre in
  let n_post = List.length post in
  let n_total = List.length elts in
  if n_total < n_pre + n_post then false
  else begin
    let pre_elts = List.filteri (fun i _ -> i < n_pre) elts in
    let ok = List.for_all2 (fun p e -> match_pattern p e env) pre pre_elts in
    if not ok then false
    else begin
      let post_elts = List.filteri (fun i _ -> i >= n_total - n_post) elts in
      let ok2 = List.for_all2 (fun p e -> match_pattern p e env) post post_elts in
      if not ok2 then false
      else begin
        let rep_elts = List.filteri (fun i _ ->
          i >= n_pre && i < n_total - n_post) elts in
        let rep_vars = collect_pattern_vars rep in
        List.iter (fun name ->
          if not (Hashtbl.mem env name) then
            Hashtbl.replace env name (Repeated [])
        ) rep_vars;
        let sub_envs = List.map (fun elt ->
          let sub = Hashtbl.create 8 in
          let ok = match_pattern rep elt sub in
          (ok, sub)
        ) rep_elts in
        let all_ok = List.for_all fst sub_envs in
        if not all_ok then false
        else begin
          List.iter (fun name ->
            let vals = List.map (fun (_, sub) ->
              match Hashtbl.find_opt sub name with
              | Some v -> v
              | None -> Single { Syntax.datum = Syntax.Nil; loc = Loc.none }
            ) sub_envs in
            Hashtbl.replace env name (Repeated vals)
          ) rep_vars;
          (* Match the dotted tail *)
          match_pattern tail tail_syn env
        end
      end
    end
  end

and collect_pattern_vars pat =
  match pat with
  | Pat_var name -> [name]
  | Pat_pair (a, b) ->
    collect_pattern_vars a @ collect_pattern_vars b
  | Pat_ellipsis (pre, rep2, post) ->
    List.concat_map collect_pattern_vars pre
    @ collect_pattern_vars rep2
    @ List.concat_map collect_pattern_vars post
  | Pat_ellipsis_dot (pre, rep2, post, tail) ->
    List.concat_map collect_pattern_vars pre
    @ collect_pattern_vars rep2
    @ List.concat_map collect_pattern_vars post
    @ collect_pattern_vars tail
  | Pat_vector pats ->
    List.concat_map collect_pattern_vars pats
  | Pat_vector_ellipsis (pre, rep2, post) ->
    List.concat_map collect_pattern_vars pre
    @ collect_pattern_vars rep2
    @ List.concat_map collect_pattern_vars post
  | _ -> []

(* --- Template types --- *)

type template =
  | Tmpl_var of string
  | Tmpl_const of Syntax.datum
  | Tmpl_pair of template * template
  | Tmpl_ellipsis_pair of template * template
  | Tmpl_vector of template list
  | Tmpl_vector_ellipsis of template list * template * template list
  | Tmpl_nil
  | Tmpl_id of string

(* --- Template parsing --- *)

(* pat_vars: map from variable name to ellipsis depth *)
let rec parse_template ~pat_vars (s : Syntax.t) : template =
  match s.datum with
  | Syntax.Symbol name ->
    (match List.assoc_opt name pat_vars with
     | Some _ -> Tmpl_var name
     | None -> Tmpl_id name)
  | Syntax.Bool _ | Syntax.Fixnum _ | Syntax.Flonum _
  | Syntax.Char _ | Syntax.Str _ ->
    Tmpl_const s.datum
  | Syntax.Nil -> Tmpl_nil
  | Syntax.Pair _ ->
    parse_template_list ~pat_vars s
  | Syntax.Vector elts ->
    parse_template_vector ~pat_vars (Array.to_list elts)
  | _ -> Tmpl_const s.datum

and parse_template_vector ~pat_vars elts =
  (* Scan for ... among vector template elements *)
  let rec find_ell pre = function
    | [] -> None
    | elt :: rest ->
      if is_ellipsis elt then
        (match pre with
         | [] -> compile_error elt.Syntax.loc "ellipsis must follow a template in vector"
         | last :: before -> Some (List.rev before, last, rest))
      else
        find_ell (elt :: pre) rest
  in
  match find_ell [] elts with
  | None -> Tmpl_vector (List.map (parse_template ~pat_vars) elts)
  | Some (before, repeated, after) ->
    let pre = List.map (parse_template ~pat_vars) before in
    let rep = parse_template ~pat_vars repeated in
    let post = List.map (parse_template ~pat_vars) after in
    Tmpl_vector_ellipsis (pre, rep, post)

and parse_template_list ~pat_vars (s : Syntax.t) =
  match s.datum with
  | Syntax.Pair (car, cdr) ->
    (* Check if cdr starts with ellipsis *)
    (match cdr.datum with
     | Syntax.Pair (ell, rest) when is_ellipsis ell ->
       let tmpl_car = parse_template ~pat_vars car in
       let tmpl_rest = parse_template ~pat_vars rest in
       Tmpl_ellipsis_pair (tmpl_car, tmpl_rest)
     | _ ->
       let tmpl_car = parse_template ~pat_vars car in
       let tmpl_cdr = parse_template ~pat_vars cdr in
       Tmpl_pair (tmpl_car, tmpl_cdr))
  | _ -> parse_template ~pat_vars s

(* --- Template instantiation --- *)

let rec instantiate (env : match_env) ~gensym ~rename loc (tmpl : template) : Syntax.t =
  match tmpl with
  | Tmpl_var name ->
    (match Hashtbl.find_opt env name with
     | Some (Single s) -> s
     | Some (Repeated _) ->
       compile_error loc
         (Printf.sprintf "template variable %s has ellipsis depth mismatch" name)
     | None ->
       compile_error loc
         (Printf.sprintf "unbound template variable %s" name))
  | Tmpl_const datum ->
    { Syntax.datum; loc }
  | Tmpl_nil ->
    { Syntax.datum = Syntax.Nil; loc }
  | Tmpl_id name ->
    (* Only rename identifiers that are introduced bindings *)
    let renamed = rename name in
    { Syntax.datum = Syntax.Symbol renamed; loc }
  | Tmpl_pair (a, b) ->
    let sa = instantiate env ~gensym ~rename loc a in
    let sb = instantiate env ~gensym ~rename loc b in
    { Syntax.datum = Syntax.Pair (sa, sb); loc }
  | Tmpl_ellipsis_pair (rep, rest) ->
    (* Find a repeated variable in rep to determine count *)
    let rep_vars = collect_template_vars rep in
    let count = List.fold_left (fun acc name ->
      match Hashtbl.find_opt env name with
      | Some (Repeated vs) ->
        let n = List.length vs in
        (match acc with None -> Some n | Some m ->
          if m <> n then
            compile_error loc "ellipsis count mismatch"
          else Some n)
      | _ -> acc
    ) None rep_vars in
    let n = match count with
      | Some n -> n
      | None -> 0  (* no repeated vars, zero repetitions *)
    in
    let rest_syntax = instantiate env ~gensym ~rename loc rest in
    (* Build repeated elements in reverse *)
    let result = ref rest_syntax in
    for i = n - 1 downto 0 do
      (* Create sub-env for this iteration *)
      let sub = Hashtbl.copy env in
      List.iter (fun name ->
        match Hashtbl.find_opt env name with
        | Some (Repeated vs) ->
          (match List.nth_opt vs i with
           | Some v -> Hashtbl.replace sub name v
           | None -> ())
        | _ -> ()
      ) rep_vars;
      let elt = instantiate sub ~gensym ~rename loc rep in
      result := { Syntax.datum = Syntax.Pair (elt, !result); loc }
    done;
    !result
  | Tmpl_vector elts ->
    let arr = Array.of_list (List.map (instantiate env ~gensym ~rename loc) elts) in
    { Syntax.datum = Syntax.Vector arr; loc }
  | Tmpl_vector_ellipsis (pre, rep, post) ->
    let pre_elts = List.map (instantiate env ~gensym ~rename loc) pre in
    let post_elts = List.map (instantiate env ~gensym ~rename loc) post in
    (* Determine repetition count from repeated template vars *)
    let rep_vars = collect_template_vars rep in
    let count = List.fold_left (fun acc name ->
      match Hashtbl.find_opt env name with
      | Some (Repeated vs) ->
        let n = List.length vs in
        (match acc with None -> Some n | Some m ->
          if m <> n then
            compile_error loc "ellipsis count mismatch in vector template"
          else Some n)
      | _ -> acc
    ) None rep_vars in
    let n = match count with Some n -> n | None -> 0 in
    let rep_elts = List.init n (fun i ->
      let sub = Hashtbl.copy env in
      List.iter (fun name ->
        match Hashtbl.find_opt env name with
        | Some (Repeated vs) ->
          (match List.nth_opt vs i with
           | Some v -> Hashtbl.replace sub name v
           | None -> ())
        | _ -> ()
      ) rep_vars;
      instantiate sub ~gensym ~rename loc rep
    ) in
    let all = pre_elts @ rep_elts @ post_elts in
    { Syntax.datum = Syntax.Vector (Array.of_list all); loc }

and collect_template_vars tmpl =
  match tmpl with
  | Tmpl_var name -> [name]
  | Tmpl_pair (a, b) -> collect_template_vars a @ collect_template_vars b
  | Tmpl_ellipsis_pair (a, b) -> collect_template_vars a @ collect_template_vars b
  | Tmpl_vector elts -> List.concat_map collect_template_vars elts
  | Tmpl_vector_ellipsis (pre, rep, post) ->
    List.concat_map collect_template_vars pre
    @ collect_template_vars rep
    @ List.concat_map collect_template_vars post
  | _ -> []

(* --- Transformer application --- *)

(* Collect identifiers that appear in binding positions within a syntax template.
   These are identifiers introduced by the macro that need hygienic renaming. *)
let collect_introduced_bindings (tmpl : Syntax.t) (pat_var_names : string list) : string list =
  let bindings = Hashtbl.create 8 in
  let is_pat_var name = List.mem name pat_var_names in
  let rec scan_binding_names s =
    match s.Syntax.datum with
    | Syntax.Symbol name ->
      if not (is_pat_var name) then Hashtbl.replace bindings name true
    | Syntax.Pair (car, cdr) ->
      scan_binding_names car; scan_binding_names cdr
    | _ -> ()
  in
  let rec walk s =
    match s.Syntax.datum with
    | Syntax.Pair (head, rest) ->
      (match head.Syntax.datum with
       | Syntax.Symbol "let" | Syntax.Symbol "let*"
       | Syntax.Symbol "letrec" | Syntax.Symbol "letrec*" ->
         let args = syntax_list_to_list rest in
         (match args with
          | bindings_syn :: body ->
            (* Check for named let *)
            (match bindings_syn.datum with
             | Syntax.Symbol name when not (is_pat_var name) ->
               Hashtbl.replace bindings name true;
               (match body with
                | real_bindings :: real_body ->
                  let binding_list = syntax_list_to_list real_bindings in
                  List.iter (fun b ->
                    let parts = syntax_list_to_list b in
                    match parts with
                    | [name_syn; init] ->
                      scan_binding_names name_syn; walk init
                    | _ -> ()
                  ) binding_list;
                  List.iter walk real_body
                | _ -> ())
             | _ ->
               let binding_list = syntax_list_to_list bindings_syn in
               List.iter (fun b ->
                 let parts = syntax_list_to_list b in
                 match parts with
                 | [name_syn; init] ->
                   scan_binding_names name_syn; walk init
                 | _ -> ()
               ) binding_list;
               List.iter walk body)
          | _ -> ())
       | Syntax.Symbol "lambda" ->
         let args = syntax_list_to_list rest in
         (match args with
          | params :: body ->
            scan_binding_names params;
            List.iter walk body
          | _ -> ())
       | Syntax.Symbol "define" ->
         let args = syntax_list_to_list rest in
         (match args with
          | name_syn :: _ ->
            (match name_syn.datum with
             | Syntax.Symbol name when not (is_pat_var name) ->
               Hashtbl.replace bindings name true
             | Syntax.Pair (fname, params) ->
               scan_binding_names fname;
               scan_binding_names params
             | _ -> ());
            List.iter walk (List.tl args)
          | _ -> ())
       | Syntax.Symbol "do" ->
         (* (do ((var init step) ...) (test expr ...) body ...) *)
         let args = syntax_list_to_list rest in
         (match args with
          | var_clauses :: test_clause :: body ->
            let clauses = syntax_list_to_list var_clauses in
            List.iter (fun vc ->
              let parts = syntax_list_to_list vc in
              (match parts with
               | name_syn :: inits -> scan_binding_names name_syn; List.iter walk inits
               | _ -> ())
            ) clauses;
            List.iter walk (syntax_list_to_list test_clause);
            List.iter walk body
          | _ -> ())
       | Syntax.Symbol "guard" ->
         (* (guard (var clause ...) body ...) *)
         let args = syntax_list_to_list rest in
         (match args with
          | clause_syn :: body ->
            let parts = syntax_list_to_list clause_syn in
            (match parts with
             | var_syn :: clauses ->
               scan_binding_names var_syn;
               List.iter (fun c -> List.iter walk (syntax_list_to_list c)) clauses
             | _ -> ());
            List.iter walk body
          | _ -> ())
       | _ ->
         List.iter walk (syntax_list_to_list s))
    | _ -> ()
  in
  walk tmpl;
  Hashtbl.fold (fun name _ acc -> name :: acc) bindings []

let apply_transformer (tf : transformer) (s : Syntax.t) ~gensym loc : Syntax.t option =
  (* Strip the keyword (first element) via direct cdr access to preserve
     dotted-pair structure — syntax_list_to_list would drop improper tails. *)
  let input_without_kw = match s.datum with
    | Syntax.Pair (_, cdr) -> cdr
    | _ -> { Syntax.datum = Syntax.Nil; loc }
  in
  let rec try_rules = function
    | [] -> None
    | rule :: rest ->
      let env = Hashtbl.create 16 in
      (* Pattern also skips keyword (first element of pattern is _) *)
      let pat_without_kw = match rule.pattern.datum with
        | Syntax.Pair (_, cdr) -> cdr
        | _ -> { Syntax.datum = Syntax.Nil; loc = rule.pattern.loc }
      in
      let pat = parse_pattern ~literals:tf.literals pat_without_kw in
      if match_pattern pat input_without_kw env then begin
        (* Collect pattern variable depths *)
        let rec match_val_depth = function
          | Single _ -> 0
          | Repeated [] -> 1
          | Repeated (first :: _) -> 1 + match_val_depth first
        in
        let pat_vars = Hashtbl.fold (fun name v acc ->
          let depth = match_val_depth v in
          (name, depth) :: acc
        ) env [] in
        let pat_var_names = List.map fst pat_vars in
        let tmpl = parse_template ~pat_vars rule.template in
        (* Determine which template ids need hygienic renaming *)
        let introduced = collect_introduced_bindings rule.template pat_var_names in
        let introduced_set = Hashtbl.create 8 in
        List.iter (fun name -> Hashtbl.replace introduced_set name true) introduced;
        (* Build rename function for hygiene — only rename introduced bindings *)
        let rename_table = Hashtbl.create 8 in
        let rename name =
          if Hashtbl.mem introduced_set name then
            match Hashtbl.find_opt rename_table name with
            | Some n -> n
            | None ->
              let fresh = gensym () in
              Hashtbl.replace rename_table name fresh;
              fresh
          else
            name  (* pass through free references unchanged *)
        in
        Some (instantiate env ~gensym ~rename loc tmpl)
      end else
        try_rules rest
  in
  try_rules tf.rules

(* --- Quasiquote expansion --- *)

let rec expand_quasiquote ~expand_fn loc (s : Syntax.t) depth : Syntax.t =
  match s.datum with
  | Syntax.Pair (car, cdr) ->
    (match car.datum with
     | Syntax.Symbol "unquote" when depth = 0 ->
       let args = syntax_list_to_list cdr in
       (match args with
        | [expr] -> expand_fn expr
        | _ -> compile_error loc "unquote expects 1 argument")
     | Syntax.Symbol "quasiquote" ->
       let args = syntax_list_to_list cdr in
       (match args with
        | [inner] ->
          let expanded = expand_quasiquote ~expand_fn loc inner (depth + 1) in
          let qq_sym = { Syntax.datum = Syntax.Symbol "list"; loc } in
          let quoted_qq = { Syntax.datum = Syntax.Pair (
            { Syntax.datum = Syntax.Symbol "quote"; loc },
            { Syntax.datum = Syntax.Pair (
              { Syntax.datum = Syntax.Symbol "quasiquote"; loc },
              { Syntax.datum = Syntax.Nil; loc }); loc }); loc } in
          { Syntax.datum = Syntax.Pair (qq_sym,
            { Syntax.datum = Syntax.Pair (quoted_qq,
              { Syntax.datum = Syntax.Pair (expanded,
                { Syntax.datum = Syntax.Nil; loc }); loc }); loc }); loc }
        | _ -> compile_error loc "quasiquote expects 1 argument")
     | Syntax.Symbol "unquote" when depth > 0 ->
       let args = syntax_list_to_list cdr in
       (match args with
        | [inner] ->
          let expanded = expand_quasiquote ~expand_fn loc inner (depth - 1) in
          let qq_sym = { Syntax.datum = Syntax.Symbol "list"; loc } in
          let quoted_uq = { Syntax.datum = Syntax.Pair (
            { Syntax.datum = Syntax.Symbol "quote"; loc },
            { Syntax.datum = Syntax.Pair (
              { Syntax.datum = Syntax.Symbol "unquote"; loc },
              { Syntax.datum = Syntax.Nil; loc }); loc }); loc } in
          { Syntax.datum = Syntax.Pair (qq_sym,
            { Syntax.datum = Syntax.Pair (quoted_uq,
              { Syntax.datum = Syntax.Pair (expanded,
                { Syntax.datum = Syntax.Nil; loc }); loc }); loc }); loc }
        | _ -> compile_error loc "unquote expects 1 argument")
     | _ ->
       expand_quasiquote_list ~expand_fn loc s depth)
  | Syntax.Vector elts ->
    let list_form = Array.fold_right (fun elt acc ->
      { Syntax.datum = Syntax.Pair (elt, acc); loc }
    ) elts { Syntax.datum = Syntax.Nil; loc } in
    let expanded_list = expand_quasiquote ~expand_fn loc list_form depth in
    let l2v = { Syntax.datum = Syntax.Symbol "list->vector"; loc } in
    { Syntax.datum = Syntax.Pair (l2v,
      { Syntax.datum = Syntax.Pair (expanded_list,
        { Syntax.datum = Syntax.Nil; loc }); loc }); loc }
  | _ ->
    (* Self-evaluating or symbol → quote it *)
    { Syntax.datum = Syntax.Pair (
      { Syntax.datum = Syntax.Symbol "quote"; loc },
      { Syntax.datum = Syntax.Pair (s,
        { Syntax.datum = Syntax.Nil; loc }); loc }); loc }

and expand_quasiquote_list ~expand_fn loc (s : Syntax.t) depth : Syntax.t =
  (* Process a list, handling unquote-splicing *)
  match s.datum with
  | Syntax.Nil ->
    { Syntax.datum = Syntax.Pair (
      { Syntax.datum = Syntax.Symbol "quote"; loc },
      { Syntax.datum = Syntax.Pair (
        { Syntax.datum = Syntax.Nil; loc },
        { Syntax.datum = Syntax.Nil; loc }); loc }); loc }
  | Syntax.Pair (car, cdr) ->
    (* Check for (unquote-splicing expr) in car position *)
    (match car.datum with
     | Syntax.Pair (uqs, args_cdr) when
       (match uqs.datum with Syntax.Symbol "unquote-splicing" -> true | _ -> false)
       && depth = 0 ->
       let args = syntax_list_to_list args_cdr in
       (match args with
        | [expr] ->
          let expanded_expr = expand_fn expr in
          let rest = expand_quasiquote_list ~expand_fn loc cdr depth in
          let app_sym = { Syntax.datum = Syntax.Symbol "append"; loc } in
          { Syntax.datum = Syntax.Pair (app_sym,
            { Syntax.datum = Syntax.Pair (expanded_expr,
              { Syntax.datum = Syntax.Pair (rest,
                { Syntax.datum = Syntax.Nil; loc }); loc }); loc }); loc }
        | _ -> compile_error loc "unquote-splicing expects 1 argument")
     | Syntax.Pair (uqs, args_cdr) when
       (match uqs.datum with Syntax.Symbol "unquote-splicing" -> true | _ -> false)
       && depth > 0 ->
       let args = syntax_list_to_list args_cdr in
       (match args with
        | [inner] ->
          let expanded = expand_quasiquote ~expand_fn loc inner (depth - 1) in
          let rest = expand_quasiquote_list ~expand_fn loc cdr depth in
          (* Reconstruct (unquote-splicing expanded) as car element *)
          let splice_form =
            let qq_sym = { Syntax.datum = Syntax.Symbol "list"; loc } in
            let quoted_uqs = { Syntax.datum = Syntax.Pair (
              { Syntax.datum = Syntax.Symbol "quote"; loc },
              { Syntax.datum = Syntax.Pair (
                { Syntax.datum = Syntax.Symbol "unquote-splicing"; loc },
                { Syntax.datum = Syntax.Nil; loc }); loc }); loc } in
            { Syntax.datum = Syntax.Pair (qq_sym,
              { Syntax.datum = Syntax.Pair (quoted_uqs,
                { Syntax.datum = Syntax.Pair (expanded,
                  { Syntax.datum = Syntax.Nil; loc }); loc }); loc }); loc }
          in
          let cons_sym = { Syntax.datum = Syntax.Symbol "cons"; loc } in
          { Syntax.datum = Syntax.Pair (cons_sym,
            { Syntax.datum = Syntax.Pair (splice_form,
              { Syntax.datum = Syntax.Pair (rest,
                { Syntax.datum = Syntax.Nil; loc }); loc }); loc }); loc }
        | _ -> compile_error loc "unquote-splicing expects 1 argument")
     | _ ->
       let expanded_car = expand_quasiquote ~expand_fn loc car depth in
       let expanded_cdr = expand_quasiquote_list ~expand_fn loc cdr depth in
       let cons_sym = { Syntax.datum = Syntax.Symbol "cons"; loc } in
       { Syntax.datum = Syntax.Pair (cons_sym,
         { Syntax.datum = Syntax.Pair (expanded_car,
           { Syntax.datum = Syntax.Pair (expanded_cdr,
             { Syntax.datum = Syntax.Nil; loc }); loc }); loc }); loc })
  | _ ->
    (* Dotted tail *)
    expand_quasiquote ~expand_fn loc s depth

(* --- Guard expansion --- *)

let expand_guard ~expand_fn ~gensym loc (s : Syntax.t) : Syntax.t =
  let args = syntax_list_to_list s in
  match args with
  | _ :: clause_syn :: body when body <> [] ->
    let clause_parts = syntax_list_to_list clause_syn in
    (match clause_parts with
     | var_syn :: cond_clauses when cond_clauses <> [] ->
       (match var_syn.datum with
        | Syntax.Symbol var_name ->
          let guard_k = gensym () in
          let handler_k = gensym () in
          let condition = gensym () in
          let args_var = gensym () in
          let mk_sym n = { Syntax.datum = Syntax.Symbol n; loc } in
          (* R7RS §4.2.7 / §7.3 double-call/cc pattern.
             The inner handler-k continuation captures the dynamic
             environment of the original raise.  When no clause matches,
             (handler-k thunk) jumps back to that environment before
             re-raising.  The body uses call-with-values to propagate
             multiple return values. *)
          let cond_with_reraise =
            let reraise =
              list_to_syntax loc [mk_sym handler_k;
                list_to_syntax loc [mk_sym "lambda";
                  { Syntax.datum = Syntax.Nil; loc };
                  (* dynamic-wind re-entry pushes the guard handler back;
                     pop it so raise-continuable finds the outer handler *)
                  list_to_syntax loc [mk_sym "begin";
                    list_to_syntax loc [mk_sym "%pop-handler!"];
                    list_to_syntax loc [mk_sym "raise-continuable";
                      mk_sym condition]]]]
            in
            let rec build_clauses = function
              | [] -> reraise
              | clause :: rest ->
                let parts = syntax_list_to_list clause in
                let fallthrough = build_clauses rest in
                (match parts with
                 | { Syntax.datum = Syntax.Symbol "else"; _ } :: exprs when exprs <> [] ->
                   list_to_syntax loc (mk_sym "begin" :: exprs)
                 | [test; { Syntax.datum = Syntax.Symbol "=>"; _ }; proc] ->
                   let v = gensym () in
                   list_to_syntax loc [mk_sym "let";
                     list_to_syntax loc [
                       list_to_syntax loc [mk_sym v; test]];
                     list_to_syntax loc [mk_sym "if"; mk_sym v;
                       list_to_syntax loc [proc; mk_sym v];
                       fallthrough]]
                 | [test] ->
                   let v = gensym () in
                   list_to_syntax loc [mk_sym "let";
                     list_to_syntax loc [
                       list_to_syntax loc [mk_sym v; test]];
                     list_to_syntax loc [mk_sym "if"; mk_sym v;
                       mk_sym v;
                       fallthrough]]
                 | test :: exprs when exprs <> [] ->
                   list_to_syntax loc [mk_sym "if"; test;
                     list_to_syntax loc (mk_sym "begin" :: exprs);
                     fallthrough]
                 | _ -> fallthrough)
            in
            build_clauses cond_clauses
          in
          (* Handler thunk passed to guard-k: evaluates clauses *)
          let handler_inner_thunk = list_to_syntax loc [
            mk_sym "lambda";
            { Syntax.datum = Syntax.Nil; loc };
            list_to_syntax loc [mk_sym "let";
              list_to_syntax loc [
                list_to_syntax loc [mk_sym var_name; mk_sym condition]];
              cond_with_reraise]]
          in
          (* Handler: (lambda (condition)
               ((call/cc (lambda (handler-k)
                  (guard-k handler-inner-thunk))))) *)
          let handler = list_to_syntax loc [
            mk_sym "lambda";
            list_to_syntax loc [mk_sym condition];
            list_to_syntax loc [
              list_to_syntax loc [mk_sym "call/cc";
                list_to_syntax loc [mk_sym "lambda";
                  list_to_syntax loc [mk_sym handler_k];
                  list_to_syntax loc [mk_sym guard_k;
                    handler_inner_thunk]]]]]
          in
          (* Body thunk: (lambda ()
               (call-with-values
                 (lambda () e1 e2 ...)
                 (lambda args
                   (guard-k (lambda () (apply values args)))))) *)
          let body_thunk = list_to_syntax loc [
            mk_sym "lambda";
            { Syntax.datum = Syntax.Nil; loc };
            list_to_syntax loc [
              mk_sym "call-with-values";
              list_to_syntax loc [
                mk_sym "lambda";
                { Syntax.datum = Syntax.Nil; loc };
                list_to_syntax loc (mk_sym "begin" :: body)];
              list_to_syntax loc [
                mk_sym "lambda";
                mk_sym args_var;
                list_to_syntax loc [mk_sym guard_k;
                  list_to_syntax loc [mk_sym "lambda";
                    { Syntax.datum = Syntax.Nil; loc };
                    list_to_syntax loc [mk_sym "apply";
                      mk_sym "values"; mk_sym args_var]]]]]]
          in
          (* Full: ((call/cc (lambda (guard-k)
                      (with-exception-handler handler body-thunk)))) *)
          let weh = list_to_syntax loc [
            mk_sym "with-exception-handler";
            handler;
            body_thunk
          ] in
          let callcc = list_to_syntax loc [mk_sym "call/cc";
            list_to_syntax loc [mk_sym "lambda";
              list_to_syntax loc [mk_sym guard_k];
              weh]]
          in
          (* ((call/cc ...)) — invoke the returned thunk *)
          let result = list_to_syntax loc [callcc] in
          expand_fn result
        | _ -> compile_error loc "guard: expected variable name")
     | _ -> compile_error loc "guard: expected (var clause ...) body ...")
  | _ -> compile_error loc "guard: expected (guard (var clause ...) body ...)"

(* --- define-record-type expansion --- *)

let expand_define_record_type ~expand_fn ~gensym loc (s : Syntax.t) : Syntax.t =
  let args = syntax_list_to_list s in
  match args with
  | _ :: name_syn :: ctor_syn :: pred_syn :: field_specs ->
    (* Parse constructor: (ctor-name field ...) *)
    let ctor_parts = syntax_list_to_list ctor_syn in
    let (ctor_name, ctor_fields) = match ctor_parts with
      | name :: fields ->
        let cn = (match name.datum with
          | Syntax.Symbol n -> n
          | _ -> compile_error loc "define-record-type: expected constructor name") in
        let fs = List.map (fun f -> match f.Syntax.datum with
          | Syntax.Symbol n -> n
          | _ -> compile_error loc "define-record-type: expected field name") fields in
        (cn, fs)
      | [] -> compile_error loc "define-record-type: empty constructor"
    in
    let type_name = match name_syn.datum with
      | Syntax.Symbol n -> n
      | _ -> compile_error loc "define-record-type: expected type name"
    in
    let pred_name = match pred_syn.datum with
      | Syntax.Symbol n -> n
      | _ -> compile_error loc "define-record-type: expected predicate name"
    in
    (* Parse field specs: (field accessor) or (field accessor mutator) *)
    let fields = List.map (fun spec ->
      let parts = syntax_list_to_list spec in
      match parts with
      | [fname; acc] ->
        let fn = (match fname.datum with Syntax.Symbol n -> n
          | _ -> compile_error loc "define-record-type: expected field name") in
        let an = (match acc.datum with Syntax.Symbol n -> n
          | _ -> compile_error loc "define-record-type: expected accessor name") in
        (fn, an, None)
      | [fname; acc; mut] ->
        let fn = (match fname.datum with Syntax.Symbol n -> n
          | _ -> compile_error loc "define-record-type: expected field name") in
        let an = (match acc.datum with Syntax.Symbol n -> n
          | _ -> compile_error loc "define-record-type: expected accessor name") in
        let mn = (match mut.datum with Syntax.Symbol n -> n
          | _ -> compile_error loc "define-record-type: expected mutator name") in
        (fn, an, Some mn)
      | _ -> compile_error loc "define-record-type: malformed field spec"
    ) field_specs in
    (* All field names in order *)
    let all_field_names = List.map (fun (n, _, _) -> n) fields in
    (* Generate tag *)
    let tag_name = gensym () in
    let mk_sym n = { Syntax.datum = Syntax.Symbol n; loc } in
    (* Constructor: (define (ctor f1 f2 ...) (vector tag f1 f2 ...)) *)
    let ctor_params = list_to_syntax loc (List.map mk_sym ctor_fields) in
    let ctor_header = { Syntax.datum = Syntax.Pair (mk_sym ctor_name, ctor_params); loc } in
    let vector_args = mk_sym tag_name :: List.map (fun fn ->
      if List.mem fn ctor_fields then mk_sym fn
      else (* uninitialized field — unspecified per R7RS *)
        list_to_syntax loc [mk_sym "if"; { Syntax.datum = Syntax.Bool false; loc };
                            { Syntax.datum = Syntax.Bool false; loc }]
    ) all_field_names in
    let ctor_body = list_to_syntax loc (mk_sym "vector" :: vector_args) in
    let ctor_def = list_to_syntax loc [mk_sym "define"; ctor_header; ctor_body] in
    (* Tag definition: (define tag (quote tag-gensym)) *)
    let tag_def = list_to_syntax loc [mk_sym "define"; mk_sym tag_name;
      list_to_syntax loc [mk_sym "quote"; mk_sym tag_name]] in
    (* Predicate: (define (pred obj) (and (vector? obj) (> (vector-length obj) 0) (eq? (vector-ref obj 0) tag))) *)
    let obj = gensym () in
    let pred_def = list_to_syntax loc [
      mk_sym "define";
      { Syntax.datum = Syntax.Pair (mk_sym pred_name,
        list_to_syntax loc [mk_sym obj]); loc };
      list_to_syntax loc [mk_sym "and";
        list_to_syntax loc [mk_sym "vector?"; mk_sym obj];
        list_to_syntax loc [mk_sym ">";
          list_to_syntax loc [mk_sym "vector-length"; mk_sym obj];
          { Syntax.datum = Syntax.Fixnum 0; loc }];
        list_to_syntax loc [mk_sym "eq?";
          list_to_syntax loc [mk_sym "vector-ref"; mk_sym obj;
            { Syntax.datum = Syntax.Fixnum 0; loc }];
          mk_sym tag_name]]]
    in
    (* Accessors and mutators *)
    let accessor_defs = List.mapi (fun i (_, acc_name, mut_opt) ->
      let idx = i + 1 in  (* +1 for tag *)
      let obj2 = gensym () in
      let acc_def = list_to_syntax loc [
        mk_sym "define";
        { Syntax.datum = Syntax.Pair (mk_sym acc_name,
          list_to_syntax loc [mk_sym obj2]); loc };
        list_to_syntax loc [mk_sym "vector-ref"; mk_sym obj2;
          { Syntax.datum = Syntax.Fixnum idx; loc }]]
      in
      match mut_opt with
      | None -> [acc_def]
      | Some mut_name ->
        let obj3 = gensym () in
        let val_name = gensym () in
        let mut_def = list_to_syntax loc [
          mk_sym "define";
          { Syntax.datum = Syntax.Pair (mk_sym mut_name,
            list_to_syntax loc [mk_sym obj3; mk_sym val_name]); loc };
          list_to_syntax loc [mk_sym "vector-set!"; mk_sym obj3;
            { Syntax.datum = Syntax.Fixnum idx; loc };
            mk_sym val_name]]
        in
        [acc_def; mut_def]
    ) fields |> List.concat in
    (* Bind record type name to tag per R7RS §5.5 *)
    let name_def = list_to_syntax loc [mk_sym "define"; mk_sym type_name;
      list_to_syntax loc [mk_sym "quote"; mk_sym tag_name]] in
    let all_defs = tag_def :: name_def :: ctor_def :: pred_def :: accessor_defs in
    let begin_form = list_to_syntax loc (mk_sym "begin" :: all_defs) in
    expand_fn begin_form
  | _ -> compile_error loc "define-record-type: malformed"

(* --- Parse syntax-rules transformer --- *)

let parse_syntax_rules (env : syn_env) loc (s : Syntax.t) : transformer =
  let args = syntax_list_to_list s in
  match args with
  | _ :: lits_syn :: rule_list when rule_list <> [] ->
    let literals = List.map (fun l ->
      match l.Syntax.datum with
      | Syntax.Symbol name -> name
      | _ -> compile_error loc "syntax-rules: literal must be a symbol"
    ) (syntax_list_to_list lits_syn) in
    let rules = List.map (fun r ->
      let parts = syntax_list_to_list r in
      match parts with
      | [pattern; template] -> { pattern; template }
      | _ -> compile_error loc "syntax-rules: each rule must be (pattern template)"
    ) rule_list in
    { literals; rules; def_env = env }
  | _ -> compile_error loc "syntax-rules: expected (syntax-rules (literals ...) rule ...)"

(* --- Main expansion --- *)

type expand_ctx = {
  features : string list;
  has_library : string list -> bool;
  read_include : fold_case:bool -> string -> Syntax.t list;
}

let default_read_include ~fold_case:_ _ =
  compile_error (Loc.make "<include>" 1 1) "include: not available in this context"

let rec expand_impl ~syn_env ~gensym ~ctx (s : Syntax.t) : Syntax.t =
  match s.datum with
  | Syntax.Bool _ | Syntax.Fixnum _ | Syntax.Flonum _
  | Syntax.Char _ | Syntax.Str _ | Syntax.Bytevector _ | Syntax.Eof ->
    s  (* self-evaluating *)

  | Syntax.Nil -> s

  | Syntax.Symbol name ->
    (* Look up in syntactic environment *)
    (match syn_lookup syn_env name with
     | Some (Core _) | Some Var | None -> s
     | Some (Macro _) ->
       compile_error s.loc
         (Printf.sprintf "macro %s used as a variable" name))

  | Syntax.Vector elts ->
    let expand_e = expand_impl ~syn_env ~gensym ~ctx in
    { s with datum = Syntax.Vector (Array.map expand_e elts) }

  | Syntax.Pair (head, _) ->
    (match head.datum with
     | Syntax.Symbol name ->
       (match syn_lookup syn_env name with
        | Some (Core core_name) ->
          expand_core ~syn_env ~gensym ~ctx core_name s
        | Some (Macro tf) ->
          let expanded = apply_transformer tf s ~gensym s.loc in
          (match expanded with
           | Some result -> expand_impl ~syn_env ~gensym ~ctx result
           | None ->
             compile_error s.loc
               (Printf.sprintf "no matching pattern for macro %s" name))
        | Some Var | None ->
          expand_application ~syn_env ~gensym ~ctx s)
     | _ ->
       expand_application ~syn_env ~gensym ~ctx s)

and expand_core ~syn_env ~gensym ~ctx name (s : Syntax.t) : Syntax.t =
  let loc = s.loc in
  match name with
  | "quote" -> s  (* don't expand inside quote *)

  | "if" ->
    let args = syntax_list_to_list s in
    (match args with
     | [kw; test; conseq; alt] ->
       let test' = expand_impl ~syn_env ~gensym ~ctx test in
       let conseq' = expand_impl ~syn_env ~gensym ~ctx conseq in
       let alt' = expand_impl ~syn_env ~gensym ~ctx alt in
       list_to_syntax loc [kw; test'; conseq'; alt']
     | [kw; test; conseq] ->
       let test' = expand_impl ~syn_env ~gensym ~ctx test in
       let conseq' = expand_impl ~syn_env ~gensym ~ctx conseq in
       list_to_syntax loc [kw; test'; conseq']
     | _ -> s)

  | "lambda" ->
    let args = syntax_list_to_list s in
    (match args with
     | kw :: params :: body when body <> [] ->
       let body_env = Hashtbl.create 8 :: syn_env in
       let body' = expand_body ~syn_env:body_env ~gensym ~ctx body in
       list_to_syntax loc (kw :: params :: body')
     | _ -> s)

  | "define" ->
    let args = syntax_list_to_list s in
    (match args with
     | [kw; name_syn; expr] when
       (match name_syn.datum with Syntax.Symbol _ -> true | _ -> false) ->
       let expr' = expand_impl ~syn_env ~gensym ~ctx expr in
       list_to_syntax loc [kw; name_syn; expr']
     | kw :: name_syn :: body when
       (match name_syn.datum with Syntax.Pair _ -> true | _ -> false)
       && body <> [] ->
       let body_env = Hashtbl.create 8 :: syn_env in
       let body' = expand_body ~syn_env:body_env ~gensym ~ctx body in
       list_to_syntax loc (kw :: name_syn :: body')
     | _ -> s)

  | "set!" ->
    let args = syntax_list_to_list s in
    (match args with
     | [kw; name_syn; expr] ->
       let expr' = expand_impl ~syn_env ~gensym ~ctx expr in
       list_to_syntax loc [kw; name_syn; expr']
     | _ -> s)

  | "begin" ->
    let args = syntax_list_to_list s in
    (match args with
     | kw :: exprs ->
       let exprs' = expand_body ~syn_env ~gensym ~ctx exprs in
       list_to_syntax loc (kw :: exprs')
     | _ -> s)

  | "let" ->
    let args = syntax_list_to_list s in
    (match args with
     | kw :: bindings_syn :: body when body <> [] ->
       (match bindings_syn.datum with
        | Syntax.Symbol _ ->
          (* Named let: (let name ((x init) ...) body ...) *)
          (match body with
           | bindings_syn2 :: body2 when body2 <> [] ->
             let bindings' = expand_bindings ~syn_env ~gensym ~ctx bindings_syn2 in
             let body_env = Hashtbl.create 8 :: syn_env in
             let body2' = expand_body ~syn_env:body_env ~gensym ~ctx body2 in
             list_to_syntax loc (kw :: bindings_syn :: bindings' :: body2')
           | _ -> s)
        | _ ->
          let bindings' = expand_bindings ~syn_env ~gensym ~ctx bindings_syn in
          let body_env = Hashtbl.create 8 :: syn_env in
          let body' = expand_body ~syn_env:body_env ~gensym ~ctx body in
          list_to_syntax loc (kw :: bindings' :: body'))
     | _ -> s)

  | "let*" | "letrec" | "letrec*" ->
    let args = syntax_list_to_list s in
    (match args with
     | kw :: bindings_syn :: body when body <> [] ->
       let bindings' = expand_bindings ~syn_env ~gensym ~ctx bindings_syn in
       let body_env = Hashtbl.create 8 :: syn_env in
       let body' = expand_body ~syn_env:body_env ~gensym ~ctx body in
       list_to_syntax loc (kw :: bindings' :: body')
     | _ -> s)

  | "cond" ->
    let args = syntax_list_to_list s in
    (match args with
     | kw :: clauses ->
       let clauses' = List.map (fun clause ->
         let parts = syntax_list_to_list clause in
         let parts' = List.map (expand_impl ~syn_env ~gensym ~ctx) parts in
         list_to_syntax clause.loc parts'
       ) clauses in
       list_to_syntax loc (kw :: clauses')
     | _ -> s)

  | "case" ->
    let args = syntax_list_to_list s in
    (match args with
     | kw :: key :: clauses when clauses <> [] ->
       let key' = expand_impl ~syn_env ~gensym ~ctx key in
       let clauses' = List.map (fun clause ->
         let parts = syntax_list_to_list clause in
         match parts with
         | datums :: body ->
           let body' = List.map (expand_impl ~syn_env ~gensym ~ctx) body in
           list_to_syntax clause.loc (datums :: body')
         | _ -> clause
       ) clauses in
       list_to_syntax loc (kw :: key' :: clauses')
     | _ -> s)

  | "do" ->
    let args = syntax_list_to_list s in
    (match args with
     | kw :: var_clauses :: test_clause :: body ->
       let var_clauses' = expand_do_vars ~syn_env ~gensym ~ctx var_clauses in
       let test_parts = syntax_list_to_list test_clause in
       let test_parts' = List.map (expand_impl ~syn_env ~gensym ~ctx) test_parts in
       let test_clause' = list_to_syntax test_clause.loc test_parts' in
       let body' = List.map (expand_impl ~syn_env ~gensym ~ctx) body in
       list_to_syntax loc (kw :: var_clauses' :: test_clause' :: body')
     | _ -> s)

  | "and" | "or" ->
    let args = syntax_list_to_list s in
    (match args with
     | kw :: exprs ->
       let exprs' = List.map (expand_impl ~syn_env ~gensym ~ctx) exprs in
       list_to_syntax loc (kw :: exprs')
     | _ -> s)

  | "when" | "unless" ->
    let args = syntax_list_to_list s in
    (match args with
     | kw :: test :: body when body <> [] ->
       let test' = expand_impl ~syn_env ~gensym ~ctx test in
       let body' = List.map (expand_impl ~syn_env ~gensym ~ctx) body in
       list_to_syntax loc (kw :: test' :: body')
     | _ -> s)

  | "define-syntax" ->
    let args = syntax_list_to_list s in
    (match args with
     | [_; name_syn; transformer_syn] ->
       (match name_syn.datum with
        | Syntax.Symbol macro_name ->
          (match transformer_syn.datum with
           | Syntax.Pair ({ datum = Syntax.Symbol "syntax-rules"; _ }, _) ->
             let tf = parse_syntax_rules syn_env loc transformer_syn in
             (* Bind in the top-level frame of syn_env *)
             (match syn_env with
              | frame :: _ -> Hashtbl.replace frame macro_name (Macro tf)
              | [] -> compile_error loc "define-syntax: empty syntactic environment");
             (* Return void *)
             { Syntax.datum = Syntax.Pair (
               { Syntax.datum = Syntax.Symbol "begin"; loc },
               { Syntax.datum = Syntax.Nil; loc }); loc }
           | _ -> compile_error loc "define-syntax: expected syntax-rules")
        | _ -> compile_error loc "define-syntax: expected symbol name")
     | _ -> compile_error loc "define-syntax: expected (define-syntax name transformer)")

  | "let-syntax" ->
    expand_let_syntax ~syn_env ~gensym ~ctx ~recursive:false s

  | "letrec-syntax" ->
    expand_let_syntax ~syn_env ~gensym ~ctx ~recursive:true s

  | "quasiquote" ->
    let args = syntax_list_to_list s in
    (match args with
     | [_; expr] ->
       let expand_fn e = expand_impl ~syn_env ~gensym ~ctx e in
       expand_quasiquote ~expand_fn loc expr 0
     | _ -> compile_error loc "quasiquote expects 1 argument")

  | "guard" ->
    let expand_fn e = expand_impl ~syn_env ~gensym ~ctx e in
    expand_guard ~expand_fn ~gensym loc s

  | "define-record-type" ->
    let expand_fn e = expand_impl ~syn_env ~gensym ~ctx e in
    expand_define_record_type ~expand_fn ~gensym loc s

  | "syntax-error" ->
    let args = syntax_list_to_list s in
    (match args with
     | _ :: msg_syn :: rest ->
       let msg = match msg_syn.datum with
         | Syntax.Str m -> m
         | _ -> Syntax.to_string msg_syn
       in
       let irritants = List.map Syntax.to_string rest in
       let full_msg = if irritants = [] then msg
         else msg ^ " " ^ String.concat " " irritants in
       compile_error loc full_msg
     | _ -> compile_error loc "syntax-error: expected message")

  | "cond-expand" ->
    expand_cond_expand ~syn_env ~gensym ~ctx loc s

  | "include" ->
    expand_include ~syn_env ~gensym ~ctx ~fold_case:false loc s

  | "include-ci" ->
    expand_include ~syn_env ~gensym ~ctx ~fold_case:true loc s

  | _ -> expand_application ~syn_env ~gensym ~ctx s

and expand_application ~syn_env ~gensym ~ctx (s : Syntax.t) : Syntax.t =
  (* Walk pairs directly to preserve improper tails from macro output *)
  let rec walk s =
    match s.Syntax.datum with
    | Syntax.Nil -> s
    | Syntax.Pair (car, cdr) ->
      let car' = expand_impl ~syn_env ~gensym ~ctx car in
      let cdr' = walk cdr in
      { s with datum = Syntax.Pair (car', cdr') }
    | _ -> expand_impl ~syn_env ~gensym ~ctx s
  in
  walk s

and expand_cond_expand ~syn_env ~gensym ~ctx loc
    (s : Syntax.t) : Syntax.t =
  let features = ctx.features in
  let has_library = ctx.has_library in
  let args = syntax_list_to_list s in
  let clauses = match args with
    | _ :: rest -> rest  (* skip "cond-expand" keyword *)
    | [] -> compile_error loc "cond-expand: expected clauses"
  in
  let rec eval_feature_req (r : Syntax.t) =
    match r.datum with
    | Syntax.Symbol name -> List.mem name features
    | Syntax.Pair _ ->
      let parts = syntax_list_to_list r in
      (match parts with
       | [{ datum = Syntax.Symbol "and"; _ }] -> true
       | { datum = Syntax.Symbol "and"; _ } :: reqs ->
         List.for_all eval_feature_req reqs
       | [{ datum = Syntax.Symbol "or"; _ }] -> false
       | { datum = Syntax.Symbol "or"; _ } :: reqs ->
         List.exists eval_feature_req reqs
       | [{ datum = Syntax.Symbol "not"; _ }; req] ->
         not (eval_feature_req req)
       | [{ datum = Syntax.Symbol "library"; _ }; name] ->
         let lib_name = syntax_to_lib_name name in
         has_library lib_name
       | _ -> compile_error r.loc "cond-expand: malformed feature requirement")
    | _ -> compile_error r.loc "cond-expand: malformed feature requirement"
  in
  let rec try_clauses = function
    | [] -> compile_error loc "cond-expand: no matching clause"
    | clause :: rest ->
      let parts = syntax_list_to_list clause in
      (match parts with
       | { datum = Syntax.Symbol "else"; _ } :: body when rest = [] ->
         let body' = List.map (expand_impl ~syn_env ~gensym ~ctx) body in
         list_to_syntax loc
           ({ Syntax.datum = Syntax.Symbol "begin"; loc } :: body')
       | req :: body ->
         if eval_feature_req req then begin
           let body' = List.map (expand_impl ~syn_env ~gensym ~ctx) body in
           list_to_syntax loc
             ({ Syntax.datum = Syntax.Symbol "begin"; loc } :: body')
         end else
           try_clauses rest
       | [] -> compile_error clause.loc "cond-expand: empty clause")
  in
  try_clauses clauses

and syntax_to_lib_name (s : Syntax.t) : string list =
  let parts = syntax_list_to_list s in
  List.map (fun p ->
    match p.Syntax.datum with
    | Syntax.Symbol name -> name
    | Syntax.Fixnum n -> string_of_int n
    | _ -> compile_error p.loc "cond-expand: library name: expected identifier or integer"
  ) parts

and expand_include ~syn_env ~gensym ~ctx ~fold_case loc
    (s : Syntax.t) : Syntax.t =
  let read_include = ctx.read_include in
  let args = syntax_list_to_list s in
  let filenames = match args with
    | _ :: rest -> rest  (* skip "include"/"include-ci" keyword *)
    | [] -> compile_error loc "include: expected filename"
  in
  let all_forms = List.concat_map (fun fn_syn ->
    match fn_syn.Syntax.datum with
    | Syntax.Str filename -> read_include ~fold_case filename
    | _ -> compile_error fn_syn.Syntax.loc "include: expected string filename"
  ) filenames in
  let expanded = List.map (expand_impl ~syn_env ~gensym ~ctx) all_forms in
  list_to_syntax loc
    ({ Syntax.datum = Syntax.Symbol "begin"; loc } :: expanded)

and expand_bindings ~syn_env ~gensym ~ctx bindings_syn =
  let bindings = syntax_list_to_list bindings_syn in
  let bindings' = List.map (fun b ->
    let parts = syntax_list_to_list b in
    match parts with
    | [name; init] ->
      let init' = expand_impl ~syn_env ~gensym ~ctx init in
      list_to_syntax b.loc [name; init']
    | _ -> b
  ) bindings in
  list_to_syntax bindings_syn.loc bindings'

and expand_do_vars ~syn_env ~gensym ~ctx var_clauses_syn =
  let clauses = syntax_list_to_list var_clauses_syn in
  let clauses' = List.map (fun vc ->
    let parts = syntax_list_to_list vc in
    match parts with
    | [name; init; step] ->
      let init' = expand_impl ~syn_env ~gensym ~ctx init in
      let step' = expand_impl ~syn_env ~gensym ~ctx step in
      list_to_syntax vc.loc [name; init'; step']
    | [name; init] ->
      let init' = expand_impl ~syn_env ~gensym ~ctx init in
      list_to_syntax vc.loc [name; init']
    | _ -> vc
  ) clauses in
  list_to_syntax var_clauses_syn.loc clauses'

and expand_body ~syn_env ~gensym ~ctx forms =
  match forms with
  | [] -> []
  | form :: rest ->
    (match form.datum with
     | Syntax.Pair ({ datum = Syntax.Symbol name; _ }, _) ->
       (match syn_lookup syn_env name with
        | Some (Core "define-syntax") ->
          let expanded = expand_core ~syn_env ~gensym ~ctx "define-syntax" form in
          (* The define-syntax was processed, skip it in output if it's a (begin) *)
          let rest' = expand_body ~syn_env ~gensym ~ctx rest in
          expanded :: rest'
        | _ ->
          let form' = expand_impl ~syn_env ~gensym ~ctx form in
          let rest' = expand_body ~syn_env ~gensym ~ctx rest in
          form' :: rest')
     | _ ->
       let form' = expand_impl ~syn_env ~gensym ~ctx form in
       let rest' = expand_body ~syn_env ~gensym ~ctx rest in
       form' :: rest')

and expand_let_syntax ~syn_env ~gensym ~ctx ~recursive (s : Syntax.t) : Syntax.t =
  let loc = s.loc in
  let args = syntax_list_to_list s in
  match args with
  | _ :: bindings_syn :: body when body <> [] ->
    let bindings = syntax_list_to_list bindings_syn in
    (* Create new frame *)
    let new_frame = Hashtbl.create 8 in
    let inner_env = new_frame :: syn_env in
    let eval_env = if recursive then inner_env else syn_env in
    List.iter (fun b ->
      let parts = syntax_list_to_list b in
      match parts with
      | [name_syn; transformer_syn] ->
        (match name_syn.datum with
         | Syntax.Symbol macro_name ->
           (match transformer_syn.datum with
            | Syntax.Pair ({ datum = Syntax.Symbol "syntax-rules"; _ }, _) ->
              let tf = parse_syntax_rules eval_env loc transformer_syn in
              Hashtbl.replace new_frame macro_name (Macro tf)
            | _ -> compile_error loc "let-syntax: expected syntax-rules")
         | _ -> compile_error loc "let-syntax: expected symbol name")
      | _ -> compile_error loc "let-syntax: malformed binding"
    ) bindings;
    (* Expand body in inner env, wrap in begin *)
    let body' = List.map (fun e -> expand_impl ~syn_env:inner_env ~gensym ~ctx e) body in
    let begin_form = list_to_syntax loc
      ({ Syntax.datum = Syntax.Symbol "begin"; loc } :: body') in
    begin_form
  | _ -> compile_error loc "let-syntax/letrec-syntax: expected bindings and body"

let expand ~syn_env ~gensym
    ?(features=[]) ?(has_library=fun _ -> false)
    ?(read_include=default_read_include) s =
  let ctx = { features; has_library; read_include } in
  expand_impl ~syn_env ~gensym ~ctx s

(* --- Binding API for library system --- *)

let lookup_binding (env : syn_env) name = syn_lookup env name

let define_binding (env : syn_env) name b =
  match env with
  | [] -> failwith "define_binding: empty syn_env (impossible)"
  | frame :: _ -> Hashtbl.replace frame name b

let var_binding = Var

let binding_names (env : syn_env) =
  List.fold_left (fun acc frame ->
    Hashtbl.fold (fun k _ acc -> if List.mem k acc then acc else k :: acc)
      frame acc
  ) [] env
