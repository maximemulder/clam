open TypeState
open TypeConstrain
open TypeSolve

(* TYPE INFERENCE *)

let rec infer_parent (expr: Abt.expr) parent =
  let* type' = infer expr in
  let span = Abt.expr_span expr in
  constrain span type' parent

and infer (expr: Abt.expr) =
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

and infer_unit _ =
  return Type.unit

and infer_bool _ =
  return Type.bool

and infer_int _ =
  return Type.int

and infer_string _ =
  return Type.string

and infer_bind expr =
  let bind = Option.get !(expr.bind) in
  let* type' = get_expr_type bind in
  match type' with
  | Some type' ->
    return type'
  | None ->
    let* def = get_def bind in
    infer_def def

and infer_tuple expr =
  let* elems = map_list infer expr.elems in
  return (Type.tuple elems)

and infer_record expr =
  let* attrs = map_list infer_record_attr expr.attrs in
  let attrs = List.fold_left (fun map (attr: Type.attr) -> Utils.NameMap.add attr.name attr map) Utils.NameMap.empty attrs in
  return (Type.record attrs)

and infer_record_attr attr =
  let* type' = infer attr.expr in
  return { Type.name = attr.name; type' }

and infer_elem expr =
  let* tuple = infer expr.tuple in
  let* type' = TypeSearch.search_proj (infer_elem_base expr.index) tuple in
  match type' with
  | Some type' ->
    return type'
  | None ->
    TypeError.infer_elem expr tuple

and infer_elem_base index tuple =
  match tuple with
  | Tuple tuple ->
    List.nth_opt tuple.elems index
  | _ ->
    None

and infer_attr expr =
  with_var (fun ret ->
    let record = Type.record (Utils.NameMap.singleton expr.label { Type.name = expr.label; type' = ret }) in
    let* () = infer_parent expr.record record in
    return ret
  )

and infer_abs expr =
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
          (infer_parent expr.body ret) in
        return (Type.abs_expr param ret)
      )
    )

and infer_app expr =
  with_var (fun param ->
    with_var (fun ret ->
      let abs = Type.abs_expr param ret in
      let* () = infer_parent expr.abs abs in
      let* () = infer_parent expr.arg param in
      return ret
    )
  )

and infer_abs_type expr =
  let* bound = validate expr.param.bound in
  with_var (fun var ->
    let type' = Type.abs_type_expr { bind = expr.param.bind; bound } var in
    let* () = with_type expr.param.bind bound
      (infer_parent expr.body var) in
    return type'
  )

and infer_app_type expr =
  let* abs = infer expr.abs in
  let* type' = TypeSearch.search_app_type infer_app_type_base abs in
  match type' with
  | Some { param; ret } ->
    let* arg = validate expr.arg in
    let* sub = isa arg param.bound in
    if not sub then
      TypeError.infer_type_app_type expr arg param.bound
    else
    substitute param.bind arg ret
  | None ->
    TypeError.infer_type_app_kind expr abs

and infer_app_type_base abs =
  match abs with
  | AbsTypeExpr abs ->
    Some { param = abs.param; ret = abs.ret }
  | _ ->
    None

and infer_ascr expr =
  let* type' = validate_proper expr.type' in
  let* () = infer_parent expr.expr type' in
  return type'

and infer_if expr =
  let* () = infer_parent expr.cond Type.bool in
  let* then' = infer expr.then' in
  let* else' = infer expr.else' in
  join then' else'

and infer_def def =
  let* () = remove_def def.bind in
  let* type' = infer_def_type def in
  let* () = add_expr def.bind type' in
  return type'

and infer_def_type def =
  match def.type' with
  | Some def_type ->
    let* def_type = validate_proper def_type in
    let* () = with_expr def.bind def_type
      (infer_parent def.expr def_type) in
    return def_type
  | None ->
    with_var (fun var ->
      let* () = with_expr def.bind var
        (infer_parent def.expr var) in
      return var
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
  List.map (fun (entry: entry_expr) -> entry.bind, entry.type') state.exprs

let check_types types =
  let _ = List.map (TypeValidate.validate TypeContext.empty) types in
  ()
