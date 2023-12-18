open Utils
open Model

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

let make_state defs =
  let dones = BindMap.of_list Primitive2.types in
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

let return_abs (abs: expr_abs) param returner =
  fun ret -> returner (Type.base (Type.AbsExpr { pos = abs.pos; param; ret }))

module type INFERER = sig
  type t
  val bot: t
  val meet: TypeContext.context -> t -> t -> t
  val join: TypeContext.context -> t -> t -> t
end

module Inferer(I: INFERER) = struct
  let rec infer ctx f (type': Type.type') =
    infer_union ctx f type'

  and infer_union ctx f union =
    let types = List.map (infer_inter ctx f) union.union in
    Utils.list_option_meet types (I.join ctx)

  and infer_inter ctx f inter =
    let types = List.map (infer_base ctx f) inter.inter in
    Utils.list_option_join types (I.meet ctx)

  and infer_base ctx f type' =
    match type' with
    | Type.Bot _ ->
      Some I.bot
    | Type.Var var ->
      let bound = TypeContext.get_bind_type ctx var.bind in
      infer ctx f bound
    | Type.App _ ->
      (* TODO: Recurse on applied bounds ? *)
      None
    | _ ->
      f type'
end

module InfererProj = struct
  type t = Type.type'
  let bot = Primitive2.bot
  let join = Typing2.join
  let meet = Typing2.meet
end

module InfererProj2 = Inferer(InfererProj)

module InfererApp = struct
  type t = { arg: Type.type'; ret: Type.type' }

  let bot = { arg = Primitive2.top; ret = Primitive2.bot }

  let join ctx left right =
    let arg = Typing2.meet ctx left.arg right.arg in
    let ret = Typing2.join ctx left.ret right.ret in
    { arg; ret }

  let meet ctx left right =
    let arg = Typing2.join ctx left.arg right.arg in
    let ret = Typing2.meet ctx left.ret right.ret in
    { arg; ret }
end

module InfererApp2 = Inferer(InfererApp)

module InfererAppType = struct
  type t = { arg: Type.param; ret: Type.type' }

  let bot = { arg = { bind = { name = "_" }; bound = Primitive2.top }; ret = Primitive2.bot }

  let join ctx left right =
    let bound = Typing2.meet ctx left.arg.bound right.arg.bound in
    let arg = { Type.bind = { name = "_" }; bound } in
    let entry = TypeContext.entry_param (Type.pos_type left.ret) arg left.arg in
    let left_ret = Typing2.substitute ctx entry left.ret in
    let entry = TypeContext.entry_param (Type.pos_type right.ret) arg right.arg in
    let right_ret = Typing2.substitute ctx entry right.ret in
    let ret = Typing2.join ctx left_ret right_ret in
    { arg; ret }

  let meet ctx left right =
    if not (Typing2.is_param ctx left.arg right.arg) then
      bot
    else
    let entry = TypeContext.entry_param (Type.pos_type right.ret) left.arg right.arg in
    let right_ret = Typing2.substitute ctx entry right.ret in
    let ret = Typing2.meet ctx left.ret right_ret in
    { arg = left.arg; ret }
end

module InfererAppType2 = Inferer(InfererAppType)

let validate_type type' =
  let* ctx = get_type_ctx in
  return (TypingValidate2.validate ctx type')

let validate_type_proper type' =
  let* ctx = get_type_ctx in
  return (TypingValidate2.validate_proper ctx type')

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
  if not (Typing2.isa ctx type' constr) then
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
  let* _ = check if'.cond Primitive2.bool in
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
    if Typing2.isa ctx param' constr then
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
    let entry = TypeContext.entry_param abs.pos param constr_abs.param in
    let* ctx = get_type_ctx in
    let constr_body = Typing2.substitute ctx entry constr_abs.ret in
    let* _ = check abs.body constr_body in
    return ()
  | _ ->
    TypeError.check_type_abs abs constr

and check_type_abs_param abs constr_param =
  let* bound = validate_type abs.param.bound in
  let* ctx = get_type_ctx in
  if Typing2.is ctx bound constr_param.bound then
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

and infer_unit unit returner =
  returner (Type.base (Type.Unit { pos = unit.pos }))

and infer_bool bool returner =
  returner (Type.base (Type.Bool { pos = bool.pos }))

and infer_int int returner =
  returner (Type.base (Type.Int { pos = int.pos }))

and infer_char char returner =
  returner (Type.base (Type.Char { pos = char.pos }))

and infer_string string returner =
  returner (Type.base (Type.String { pos = string.pos }))

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
  returner (Type.base (Type.Tuple { pos = tuple.pos; elems }))

and infer_record record returner =
  let attrs = record.attrs in
  let attrs = List.fold_left (fun map (attr: attr_expr) -> NameMap.add attr.name attr map) NameMap.empty attrs in
  let* attrs = map_map infer_record_attr attrs in
  returner (Type.base (Type.Record { pos = record.pos; attrs }))

and infer_record_attr attr =
  let* type' = infer_none attr.expr in
  return ({ Type.pos = attr.pos; name = attr.name; type' })

and infer_elem elem returner =
  let* tuple = infer_none elem.expr in
  let* ctx = get_type_ctx in
  let type' = InfererProj2.infer ctx (infer_elem_base elem.index) tuple in
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
  let type' = InfererProj2.infer ctx (infer_attr_base attr.name) record in
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
  let type' = InfererApp2.infer ctx infer_app_base abs in
  match type' with
  | Some { arg; ret } ->
    let* () = check app.arg arg in
    returner ret
  | _ ->
    TypeError.infer_app_kind app abs

and infer_app_base type' =
  match type' with
  | Type.AbsExpr abs ->
    Some { InfererApp.arg = abs.param; ret = abs.ret }
  | _ ->
    None

and infer_type_app app returner =
  let* abs = infer_none app.expr in
  let* ctx = get_type_ctx in
  let type' = InfererAppType2.infer ctx infer_type_app_base abs in
  match type' with
  | Some { arg; ret } ->
    let param = arg in
    let* arg = validate_type app.arg in
    let* ctx = get_type_ctx in
    if Typing2.isa ctx arg param.bound then
      TypeError.infer_type_app_type app arg param.bound
    else
    let entry = TypeContext.entry param.bind arg in
    let ret = Typing2.substitute ctx entry ret in
    returner ret
  | None ->
    TypeError.infer_type_app_kind app abs

and infer_type_app_base type' =
  match type' with
  | Type.AbsTypeExpr abs ->
    Some { InfererAppType.arg = abs.param; ret = abs.ret }
  | _ ->
    None

and infer_ascr ascr returner =
  let* type' = validate_type ascr.type' in
  let* returned = returner type' in
  let* _ = check ascr.expr type' in
  return returned

and infer_if if' returner =
  let* _ = check if'.cond Primitive2.bool in
  let* then' = infer_none if'.then' in
  let* else' = infer_none if'.else' in
  let* ctx = get_type_ctx in
  returner (Typing2.join ctx then' else')

and infer_abs abs returner =
  let* param = infer_abs_param abs.param in
  let returner = return_abs abs param returner in
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
  let* bound = validate_type abs.param.bound in
  let param = { Type.bind = abs.param.bind; bound } in
  (* TODO: with type bound ? *)
  let* ret = infer_none abs.body in
  returner (Type.base (Type.AbsTypeExpr { pos = abs.pos; param = param; ret }))

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
  List.iter TypingValidate.validate types
