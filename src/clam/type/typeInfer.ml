open TypeState
open TypeConstrain
open TypeSolve

(* VALIDATE *)

let validate type' =
  let* ctx = get_context in
  return (TypeValidate.validate ctx type')

let validate_proper type' =
  let* ctx = get_context in
  return (TypeValidate.validate_proper ctx type')

(* TYPE INFERENCE *)

let return_type pos type' parent =
  constrain pos type' parent

let return_constrain pos f parent =
  let* type' = f in
  constrain pos type' parent

let rec infer (expr: Abt.expr) =
  with_var (fun var ->
    let* () = infer_with expr var in
    return var
  )

and infer_with (expr: Abt.expr) =
  match expr with
  | ExprUnit unit ->
    infer_unit unit
  | ExprBool bool ->
    infer_bool bool
  | ExprInt int ->
    infer_int int
  | ExprString string ->
    infer_string string
  | ExprBind bind ->
    infer_bind bind
  | ExprTuple tuple ->
    infer_tuple tuple
  | ExprRecord record ->
    infer_record record
  | ExprElem elem ->
    infer_elem elem
  | ExprAttr attr ->
    infer_attr attr
  | ExprIf if' ->
    infer_if if'
  | ExprAbs abs ->
    infer_abs abs
  | ExprApp app ->
    infer_app app
  | ExprTypeAbs abs ->
    infer_abs_type abs
  | ExprTypeApp app ->
    infer_app_type app
  | ExprAscr ascr ->
    infer_ascr ascr
  | ExprStmt stmt ->
    infer_stmt stmt

and infer_unit expr  =
  return_type expr.pos Type.unit

and infer_bool expr =
  return_type expr.pos Type.bool

and infer_int expr =
  return_type expr.pos Type.int

and infer_string expr =
  return_type expr.pos Type.string

and infer_bind expr =
  return_constrain expr.pos (
    let bind = Option.get !(expr.bind) in
    let* type' = get_expr_type bind in
    match type' with
    | Some type' ->
      return type'
    | None ->
      let* def = get_def bind in
      let* type' = infer_def def in
      return type'
  )

and infer_tuple expr =
  return_constrain expr.pos (
    let* elems = map_list infer expr.elems in
    return (Type.tuple elems)
  )

and infer_record expr =
  return_constrain expr.pos (
    let* attrs = map_list infer_record_attr expr.attrs in
    let attrs = List.fold_left (fun map (attr: Type.attr) -> Utils.NameMap.add attr.name attr map) Utils.NameMap.empty attrs in
    return (Type.record attrs)
  )

and infer_record_attr attr =
  let* type' = infer attr.expr in
  return { Type.name = attr.name; type' }

and infer_elem expr =
  return_constrain expr.pos (
    let* tuple = infer expr.expr in
    let* type' = TypeSearch2.search_proj (infer_elem_base expr.index) tuple in
    match type' with
    | Some type' ->
      return type'
    | None ->
      TypeError.infer_elem expr tuple
  )

and infer_elem_base index tuple =
  match tuple with
  | Tuple tuple ->
    List.nth_opt tuple.elems index
  | _ ->
    None

and infer_attr expr parent =
  let* _ = with_var (fun ret ->
    let record = Type.record (Utils.NameMap.singleton expr.name { Type.name = expr.name; type' = ret }) in
    let* () = infer_with expr.expr record in
    let* () = constrain expr.pos ret parent in
    return ret
  ) in
  return ()

and infer_abs expr =
  return_constrain expr.pos (
    match expr.param.type' with
    | Some type' ->
      let* type' = validate_proper type' in
      let* ret = with_expr expr.param.bind type'
        (infer expr.body) in
      return (Type.abs_expr type' ret)
    | None ->
      with_var (fun param ->
        with_var (fun ret ->
          let* () = with_expr expr.param.bind param
            (infer_with expr.body ret) in
          return (Type.abs_expr param ret)
        )
      )
  )

and infer_app expr parent =
  let* _ = with_var (fun param ->
    with_var (fun ret ->
      let abs = Type.abs_expr param ret in
      let* () = infer_with expr.expr abs in
      let* () = infer_with expr.arg param in
      let* () = constrain expr.pos ret parent in
      return ret
    )
  ) in
  return ()

and infer_abs_type expr =
  return_constrain expr.pos (
    let* bound = validate expr.param.bound in
    with_var (fun var ->
      let type' = Type.abs_type_expr { bind = expr.param.bind; bound } var in
      with_type expr.param.bind bound (
        let* () = infer_with expr.body var in
        return type'
      )
    )
  )

and infer_app_type expr =
  return_constrain expr.pos (
    let* abs = infer expr.expr in
    let* type' = TypeSearch2.search_app_type infer_app_type_base abs in
    match type' with
    | Some { param; ret } ->
      let* arg = validate expr.arg in
      let* ctx = get_context in
      if not (TypeSystem.isa ctx arg param.bound) then
        TypeError.infer_type_app_type expr arg param.bound
      else
      let ret = TypeSystem.substitute_arg ctx param.bind arg ret in
      return ret
    | None ->
      TypeError.infer_type_app_kind expr abs
  )

and infer_app_type_base abs =
  match abs with
  | AbsTypeExpr abs ->
    Some { param = abs.param; ret = abs.ret }
  | _ ->
    None

and infer_ascr expr =
  return_constrain expr.pos (
    let* type' = validate_proper expr.type' in
    let* () = infer_with expr.expr type' in
    return type'
  )

and infer_if expr =
  return_constrain expr.pos (
    let* () = infer_with expr.cond Type.bool in
    let* then' = infer expr.then' in
    let* else' = infer expr.else' in
    let* ctx = get_context in
    let type' = TypeSystem.join ctx then' else' in
    return type'
  )

and infer_stmt expr =
  infer_stmt_body expr.pos expr.stmt (infer expr.expr)

and infer_stmt_body pos stmt f =
  match stmt with
  | StmtExpr expr ->
    return_constrain pos (
      let* _ = infer expr in
      f
    )
  | StmtVar (bind, type', expr) ->
    return_constrain pos (
      let* body = match type' with
      | Some type' ->
        let* type' = validate_proper type' in
        let* () = infer_with expr type' in
        return type'
      | None ->
        infer expr
      in
      with_expr bind body f
    )

and infer_def def =
  let* () = remove_def def.bind in
  let* type' = infer_def_type def in
  let* () = add_expr def.bind type' in
  return type'

and infer_def_type def =
  with_level (
    match def.type' with
    | Some def_type ->
      let* def_type = validate_proper def_type in
      let* _ = with_expr def.bind def_type
        (infer_with def.expr def_type) in
      return def_type
    | None ->
      with_var (fun var ->
        let* () = with_expr def.bind var
          (infer_with def.expr var) in
        return var
      )
  )

let rec check_defs state =
  match state.defs with
  | entry :: _ ->
    let _, state = infer_def entry.def state in
    check_defs state
  | [] ->
    state

let check_defs defs primitives =
  let primitives = List.map (fun primitive -> { bind = fst primitive; type' = snd primitive }) primitives in
  let state = make_state defs primitives in
  let state = check_defs state in
  print_endline("");
  let types = List.filter (fun (e: entry_expr) -> not(List.exists (fun (p: entry_expr) -> p.bind.name = e.bind.name) primitives)) state.exprs in
  List.iter (fun (e: entry_expr) -> print_endline(e.bind.name ^ ": " ^ TypeDisplay.display e.type')) types

let check_types types =
  let _ = List.map (TypeValidate.validate TypeContext.empty) types in
  ()
