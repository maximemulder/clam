open Utils
open Abt

module DefKey = struct
  type t = def_expr
  let compare x y = Stdlib.compare x.id y.id
end

module DefSet = Set.Make(DefKey)

module BindKey = struct
  type t = bind_expr

  let compare x y  = Stdlib.compare (bind_expr_id x) (bind_expr_id y)
end

module BindMap = Map.Make(BindKey)

type expr_context = {
  remains: DefSet.t;
  currents: DefSet.t;
  dones: Type.type' BindMap.t;
}

type state = {
  type_ctx: TypeContext.context;
  expr_ctx: expr_context;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let get_type_ctx state =
  state.type_ctx, state

let get_expr_ctx state =
  state.expr_ctx, state

let with_bind_type bind type' f state =
  let ctx = state.type_ctx in
  let ctx_new = TypeContext.add_bind_type ctx bind type' in
  let state = { state with type_ctx = ctx_new } in
  let other, state = f state in
  other, { state with type_ctx = ctx }

let make_state defs =
  (* TODO: Dependency injection for primitives *)
  let dones = BindMap.of_list Primitive.types in
  let remains = List.fold_left (fun set def -> DefSet.add def set) DefSet.empty defs in
  let expr_ctx = { remains; currents = DefSet.empty; dones } in
  let type_ctx = TypeContext.empty in
  { expr_ctx; type_ctx }

let start_progress state def =
  let ctx = state.expr_ctx in
  let remains = DefSet.remove def ctx.remains in
  let currents = DefSet.add def ctx.currents in
  { state with expr_ctx = { ctx with remains; currents } }

let end_progress state def type' =
  let ctx = state.expr_ctx in
  let currents = DefSet.remove def ctx.currents in
  let dones = BindMap.add (BindExprDef def) type' ctx.dones in
  { state with expr_ctx = { ctx with currents; dones } }

let add_expr_bind bind type' state =
  let ctx = state.expr_ctx in
  let dones = BindMap.add bind type' ctx.dones in
  ((), { state with expr_ctx = { ctx with dones } })

let return_def (def: def_expr) =
  fun type' state -> (type', end_progress state def type')

let return_abs param returner =
  fun ret -> returner (Type.base (Type.AbsExpr { param; ret }))

let validate_type type' =
  let* ctx = get_type_ctx in
  return (TypeValidate.validate ctx type')

let validate_type_proper type' =
  let* ctx = get_type_ctx in
  return (TypeValidate.validate_proper ctx type')

let rec check expr (constr: Type.type') =
  check_union expr constr

and check_union expr constr =
  if List.length constr.union == 1 then
    check_inter expr (List.nth constr.union 0)
  else
    check_infer expr constr

and check_inter expr constr =
  if List.length constr.inter == 1 then
    check_base expr (List.nth constr.inter 0)
  else
    check_infer expr { Type.union = [constr] }

and check_base expr constr =
  match expr with
  | ExprTuple tuple ->
    check_tuple tuple constr
  | ExprRecord record ->
    check_record record constr
  | ExprIf if' ->
    check_if if' constr
  | ExprAbs abs ->
    check_abs abs constr
  | ExprTypeAbs abs ->
    check_type_abs abs constr
  | _ ->
    let constr = Type.base constr in
    check_infer expr constr

and check_infer expr constr =
  let* type' = infer_none expr in
  let* ctx = get_type_ctx in
  if not (TypeSystem.isa ctx type' constr) then
    TypeError.check_type expr type' constr
  else
    return ()

and check_tuple tuple constr =
  match constr with
  | Type.Tuple constr_tuple ->
    if List.compare_lengths tuple.elems constr_tuple.elems != 0 then
      TypeError.check_tuple_arity tuple constr_tuple
    else
    iter_list2 check tuple.elems constr_tuple.elems
  | _ ->
    TypeError.check_tuple tuple constr

and check_record record constr =
  match constr with
  | Type.Record constr_record ->
    iter_map (check_record_attr record) constr_record.attrs
  | _ ->
    TypeError.check_record record constr

and check_record_attr record constr_attr =
  match List.find_opt (fun (attr: attr_expr) -> attr.name = constr_attr.name) record.attrs with
  | Some attr -> check attr.expr constr_attr.type'
  | None -> TypeError.check_record_attr record constr_attr

and check_if if' constr =
  let* _ = check if'.cond TypePrimitive.bool in
  let* _ = check if'.then' (Type.base constr) in
  let* _ = check if'.else' (Type.base constr) in
  return ()

and check_abs abs constr =
  match constr with
  | Type.AbsExpr constr_abs ->
    let* _ = check_abs_param abs.param constr_abs.param in
    let* _ = check abs.body constr_abs.ret in
    return ()
  | _ ->
    TypeError.check_abs abs constr

and check_abs_param param constr =
  let* type' = match param.type' with
  | Some type' ->
    let* param' = validate_type_proper type' in
    let* ctx = get_type_ctx in
    if TypeSystem.isa ctx constr param' then
      return param'
    else
      TypeError.check_abs_param param param' constr
  | None ->
    return constr
  in
  add_expr_bind (BindExprParam param) type'

and check_type_abs abs constr =
  match constr with
  | Type.AbsTypeExpr constr_abs ->
    let* param = check_type_abs_param abs constr_abs.param in
    let* ctx = get_type_ctx in
    let constr_ret = TypeSystem.substitute_body ctx param constr_abs.param constr_abs.ret in
    with_bind_type param.bind param.bound
      (check abs.body constr_ret)
  | _ ->
    TypeError.check_type_abs abs constr

and check_type_abs_param abs constr_param =
  let* bound = validate_type abs.param.bound in
  let* ctx = get_type_ctx in
  if TypeSystem.is ctx bound constr_param.bound then
    return { Type.bind = abs.param.bind; bound }
  else
    TypeError.check_type_abs_param abs bound constr_param

and infer expr returner =
  match expr with
  | ExprUnit unit ->
    infer_unit unit returner
  | ExprBool bool ->
    infer_bool bool returner
  | ExprInt int ->
    infer_int int returner
  | ExprChar char ->
    infer_char char returner
  | ExprString string ->
    infer_string string returner
  | ExprBind bind ->
    infer_bind bind returner
  | ExprTuple tuple ->
    infer_tuple tuple returner
  | ExprRecord record ->
    infer_record record returner
  | ExprElem elem ->
    infer_elem elem returner
  | ExprAttr attr ->
    infer_attr attr returner
  | ExprAscr ascr ->
    infer_ascr ascr returner
  | ExprIf if' ->
    infer_if if' returner
  | ExprAbs abs ->
    infer_abs abs returner
  | ExprApp app ->
    infer_app app returner
  | ExprTypeAbs abs ->
    infer_type_abs abs returner
  | ExprTypeApp app ->
    infer_type_app app returner
  | ExprStmt stmt ->
    infer_stmt stmt returner

and infer_none expr =
  infer expr return

and infer_unit _ returner =
  returner (Type.base Type.Unit)

and infer_bool _ returner =
  returner (Type.base Type.Bool)

and infer_int _ returner =
  returner (Type.base Type.Int)

and infer_char _ returner =
  returner (Type.base Type.Char)

and infer_string _ returner =
  returner (Type.base Type.String)

and infer_bind bind returner =
  let* ctx = get_expr_ctx in
  let bind = Option.get !(bind.bind) in
  match bind with
  | BindExprDef def when DefSet.mem def ctx.remains ->
    let* type' = check_def def in
    returner type'
  | BindExprDef def when DefSet.mem def ctx.currents ->
    TypeError.infer_recursive def
  | _ ->
    returner (BindMap.find bind ctx.dones)

and infer_tuple tuple returner =
  let* elems = map_list infer_none tuple.elems in
  returner (Type.base (Type.Tuple { elems }))

and infer_record record returner =
  let attrs = record.attrs in
  let attrs = List.fold_left (fun map (attr: attr_expr) -> NameMap.add attr.name attr map) NameMap.empty attrs in
  let* attrs = map_map infer_record_attr attrs in
  returner (Type.base (Type.Record { attrs }))

and infer_record_attr attr =
  let* type' = infer_none attr.expr in
  return ({ Type.name = attr.name; type' })

and infer_elem elem returner =
  let* tuple = infer_none elem.expr in
  let* ctx = get_type_ctx in
  let type' = TypeSearch.search_proj ctx (infer_elem_base elem.index) tuple in
  match type' with
  | Some type' ->
    returner type'
  | None ->
    TypeError.infer_elem elem tuple

and infer_elem_base index type' =
  match type' with
  | Type.Tuple tuple ->
    List.nth_opt tuple.elems index
  | _ ->
    None

and infer_attr attr returner =
  let* record = infer_none attr.expr in
  let* ctx = get_type_ctx in
  let type' = TypeSearch.search_proj ctx (infer_attr_base attr.name) record in
  match type' with
  | Some type' ->
    returner type'
  | None ->
    TypeError.infer_attr attr record

and infer_attr_base name type' =
  match type' with
  | Type.Record record ->
    Option.map (fun (attr: Type.attr) -> attr.type') (NameMap.find_opt name record.attrs)
  | _ ->
    None

and infer_app app returner =
  let* abs = infer_none app.expr in
  let* ctx = get_type_ctx in
  let type' = TypeSearch.search_app ctx infer_app_base abs in
  match type' with
  | Some { param; ret } ->
    let* () = check app.arg param in
    returner ret
  | _ ->
    TypeError.infer_app_kind app abs

and infer_app_base type' =
  match type' with
  | Type.AbsExpr abs ->
    Some { param = abs.param; ret = abs.ret }
  | _ ->
    None

and infer_type_app app returner =
  let* abs = infer_none app.expr in
  let* ctx = get_type_ctx in
  let type' = TypeSearch.search_app_type ctx infer_type_app_base abs in
  match type' with
  | Some { param; ret } ->
    let* arg = validate_type app.arg in
    let* ctx = get_type_ctx in
    if not (TypeSystem.isa ctx arg param.bound) then
      TypeError.infer_type_app_type app arg param.bound
    else
    let ret = TypeSystem.substitute_arg ctx param.bind arg ret in
    returner ret
  | None ->
    TypeError.infer_type_app_kind app abs

and infer_type_app_base type' =
  match type' with
  | Type.AbsTypeExpr abs ->
    Some { param = abs.param; ret = abs.ret }
  | _ ->
    None

and infer_ascr ascr returner =
  let* type' = validate_type ascr.type' in
  let* returned = returner type' in
  let* _ = check ascr.expr type' in
  return returned

and infer_if if' returner =
  let* _ = check if'.cond TypePrimitive.bool in
  let* then' = infer_none if'.then' in
  let* else' = infer_none if'.else' in
  let* ctx = get_type_ctx in
  returner (TypeSystem.join ctx then' else')

and infer_abs abs returner =
  let* param = infer_abs_param abs.param in
  let returner = return_abs param returner in
  infer abs.body returner

and infer_abs_param param =
  let* type' = match param.type' with
  | Some type' -> validate_type_proper type'
  | None -> TypeError.infer_abs_param param
  in
  let bind = BindExprParam param in
  let* _ = add_expr_bind bind type' in
  return type'

and infer_type_abs abs returner =
  let* (param: Type.param) = infer_type_param abs.param in
  with_bind_type param.bind param.bound (
    let* ret = infer_none abs.body in
    returner (Type.base (Type.AbsTypeExpr { param; ret }))
  )

and infer_type_param param =
  let* bound = validate_type param.bound in
  return { Type.bind = param.bind; bound }

and infer_stmt stmt returner =
  let* _ = infer_stmt_body stmt.stmt in
  infer stmt.expr returner

and infer_stmt_body body =
  match body with
  | StmtVar (var, type', expr) ->
    let* type' = match type' with
    | Some type' ->
      let* type' = validate_type type' in
      let* _ = check expr type' in
      return type'
    | None ->
      infer_none expr
    in
    let* _ = add_expr_bind (BindExprVar var) type' in
    return ()
  | StmtExpr expr ->
    let* _ = infer_none expr in
    return ()

and check_def def state =
  let state = start_progress state def in
  match def.type' with
  | Some type' ->
    let (type', state) = validate_type_proper type' state in
    let state = end_progress state def type' in
    let (_, state) = check def.expr type' state in
    (type', state)
  | None ->
    let (type', state) = infer def.expr (return_def def) state in
    (type', state)

let rec progress_defs state =
  match DefSet.choose_opt state.expr_ctx.remains with
  | None -> state
  | Some def ->
    let (_, state) = check_def def state in
    progress_defs state

let check_exprs defs =
  progress_defs (make_state defs)

let check_types types =
  let _ = List.map (TypeValidate.validate TypeContext.empty) types in
  ()
