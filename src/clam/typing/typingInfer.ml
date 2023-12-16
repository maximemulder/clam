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

type state = {
  remains: DefSet.t;
  currents: DefSet.t;
  dones: type' BindMap.t;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let make_progress defs =
  let dones = BindMap.of_list Primitive.types in
  let remains = List.fold_left (fun set def -> DefSet.add def set) DefSet.empty defs in
  { remains; currents = DefSet.empty; dones }

let start_progress progress def =
  let remains = DefSet.remove def progress.remains in
  let currents = DefSet.add def progress.currents in
  { progress with remains; currents }

let end_progress progress def type' =
  let currents = DefSet.remove def progress.currents in
  let dones = BindMap.add (BindExprDef def) type' progress.dones in
  { progress with currents; dones }

let add_bind bind type' state =
  let dones = BindMap.add bind type' state.dones in
  ((), { state with dones })

let return_def (def: def_expr) =
  fun type' state -> (type', end_progress state def type')

let return_abs (abs: expr_abs) param returner =
  fun body -> returner (TypeAbsExpr { pos = abs.pos; param; body })

module type INFERER = sig
  type t
  val bot: t
  val meet: t -> t -> t
  val join: t -> t -> t
end

module Inferer(I: INFERER) = struct
  let rec infer f type' =
    let type' = Typing.normalize type' in
    match type' with
    | TypeBot _ ->
      Some I.bot
    | TypeVar var ->
      infer f var.param.bound
    | TypeInter inter ->
      let left = infer f inter.left in
      let right = infer f inter.right in
      Utils.join_option2 left right I.meet
    | TypeUnion union ->
      let left = infer f union.left in
      let right = infer f union.right in
      Utils.map_option2 left right I.join
    | TypeApp app ->
      let type' = Typing.simplify_app app in
      infer f type'
    | _ ->
      f type'
end

module InfererProj = struct
  type t = type'
  let bot = Primitive.bot
  let join = Typing.join
  let meet = Typing.meet
end

module InfererProj2 = Inferer(InfererProj)

module InfererApp = struct
  type t = { arg: type'; ret: type' }

  let bot = { arg = Primitive.top; ret = Primitive.bot }

  let join left right =
    let arg = Typing.meet left.arg right.arg in
    let ret = Typing.join left.ret right.ret in
    { arg; ret }

  let meet left right =
    let arg = Typing.join left.arg right.arg in
    let ret = Typing.meet left.ret right.ret in
    { arg; ret }
end

module InfererApp2 = Inferer(InfererApp)

module InfererAppType = struct
  type t = { arg: param_type; ret: type' }

  let bot = { arg = { bind = { name = "_" }; bound = Primitive.top }; ret = Primitive.bot }

  let join left right =
    let bound = Typing.meet left.arg.bound right.arg.bound in
    let arg = { bind = { name = "_" }; bound } in
    let entry = TypingApp.entry_param arg left.arg (type_pos left.ret) in
    let left_ret = TypingApp.apply left.ret entry in
    let entry = TypingApp.entry_param arg right.arg (type_pos right.ret) in
    let right_ret = TypingApp.apply right.ret entry in
    let ret = Typing.join left_ret right_ret in
    { arg; ret }

  let meet left right =
    if not (Typing.is_param left.arg right.arg) then
      bot
    else
    let entry = TypingApp.entry_param right.arg left.arg (type_pos right.ret) in
    let right_ret = TypingApp.apply right.ret entry in
    let ret = Typing.meet left.ret right_ret in
    { arg = left.arg; ret }
end

module InfererAppType2 = Inferer(InfererAppType)

let rec check expr constr =
  let constr = Typing.normalize constr in
  match constr with
  | TypeUnion _ | TypeInter _ ->
    check_infer expr constr
  | _ ->
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
    check_infer expr constr

and check_infer expr constr =
  let* type' = infer_none expr in
  if not (Typing.isa type' constr) then
    TypingErrors.raise_expr_constraint expr type' constr
  else
    return ()

and check_tuple tuple constr =
  match constr with
  | TypeTuple constr_tuple ->
    if List.compare_lengths tuple.elems constr_tuple.elems != 0 then
      TypingErrors.raise_check_tuple_arity tuple constr_tuple
    else
    iter_list2 check tuple.elems constr_tuple.elems
  | _ ->
    TypingErrors.raise_check_tuple tuple constr

and check_record record constr =
  match constr with
  | TypeRecord constr_record ->
    iter_map (check_record_attr record) constr_record.attrs
  | _ ->
    TypingErrors.raise_check_record record constr

and check_record_attr record constr_attr =
  match List.find_opt (fun (attr: attr_expr) -> attr.name = constr_attr.name) record.attrs with
  | Some attr -> check attr.expr constr_attr.type'
  | None -> TypingErrors.raise_check_record_attr record constr_attr

and check_if if' constr =
  let* _ = check if'.cond Primitive.bool in
  let* _ = check if'.then' constr in
  let* _ = check if'.else' constr in
  return ()

and check_abs abs constr =
  match constr with
  | TypeAbsExpr constr_abs ->
    let* _ = check_abs_param abs.param constr_abs.param in
    let* _ = check abs.body constr_abs.body in
    return ()
  | _ ->
    TypingErrors.raise_check_abs abs constr

and check_abs_param param constr =
  let* type' = match param.type' with
  | Some type' ->
    TypingValidate.validate_proper type';
    TypingValidate.validate_suptype type' constr;
    return type'
  | None ->
    return constr
  in
  add_bind (BindExprParam param) type'

and check_type_abs abs constr =
  match constr with
  | TypeAbsExprType constr_abs ->
    let* _ = check_type_abs_param abs constr_abs.param in
    let constr_body = TypingApp.apply_abs_expr_param constr_abs abs.param in
    let* _ = check abs.body constr_body in
    return ()
  | _ ->
    TypingErrors.raise_check_type_abs abs constr

and check_type_abs_param abs constr_param =
  TypingValidate.validate abs.param.bound;
  if Typing.is abs.param.bound constr_param.bound then
    return ()
  else
    TypingErrors.raise_check_type_abs_param abs constr_param

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
  returner (TypeUnit { pos = unit.pos })

and infer_bool bool returner =
  returner (TypeBool { pos = bool.pos })

and infer_int int returner =
  returner (TypeInt { pos = int.pos })

and infer_char char returner =
  returner (TypeChar { pos = char.pos })

and infer_string string returner =
  returner (TypeString { pos = string.pos })

and infer_bind bind returner state =
  let bind = Option.get !(bind.bind) in
  match bind with
  | BindExprDef def when DefSet.mem def state.remains ->
    let (type', state) = check_def def state in
    returner type' state
  | BindExprDef def when DefSet.mem def state.currents ->
    TypingErrors.raise_expr_recursive def
  | _ ->
    returner (BindMap.find bind state.dones) state

and infer_tuple tuple returner =
  let* elems = map_list infer_none tuple.elems in
  returner (TypeTuple { pos = tuple.pos; elems })

and infer_record record returner =
  let attrs = record.attrs in
  let attrs = List.fold_left (fun map (attr: attr_expr) -> NameMap.add attr.name attr map) NameMap.empty attrs in
  let* attrs = map_map infer_record_attr attrs in
  returner (TypeRecord { pos = record.pos; attrs })

and infer_record_attr attr =
  let* type' = infer_none attr.expr in
  return ({ pos = attr.pos; name = attr.name; type' })

and infer_elem elem returner =
  let* tuple = infer_none elem.expr in
  let type' = InfererProj2.infer (infer_elem_base elem.index) tuple in
  match type' with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_elem elem tuple

and infer_elem_base index type' =
  match type' with
  | TypeTuple tuple ->
    List.nth_opt tuple.elems index
  | _ ->
    None

and infer_attr attr returner =
  let* record = infer_none attr.expr in
  let type' = InfererProj2.infer (infer_attr_base attr.name) record in
  match type' with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_attr attr record

and infer_attr_base name type' =
  match type' with
  | TypeRecord record ->
    Option.map (fun (attr: attr_type) -> attr.type') (NameMap.find_opt name record.attrs)
  | _ ->
    None

and infer_app app returner =
  let* abs = infer_none app.expr in
  let type' = InfererApp2.infer infer_app_base abs in
  match type' with
  | Some { arg; ret } ->
    let* () = check app.arg arg in
    returner ret
  | _ ->
    TypingErrors.raise_expr_app_kind app abs

and infer_app_base type' =
  match type' with
  | TypeAbsExpr abs ->
    Some { InfererApp.arg = abs.param; ret = abs.body }
  | _ ->
    None

and infer_type_app app returner =
  let* abs = infer_none app.expr in
  let type' = InfererAppType2.infer infer_type_app_base abs in
  match type' with
  | Some { arg; ret } ->
    TypingValidate.validate_subtype app.arg arg.bound;
    let entry = TypingApp.entry arg app.arg in
    let ret = TypingApp.apply ret entry in
    returner ret
  | None ->
    TypingErrors.raise_expr_type_app_kind app abs

and infer_type_app_base type' =
  match type' with
  | TypeAbsExprType abs ->
    Some { InfererAppType.arg = abs.param; ret = abs.body }
  | _ ->
    None

and infer_ascr ascr returner =
  let type' = ascr.type' in
  TypingValidate.validate type';
  let* returned = returner type' in
  let* _ = check ascr.expr type' in
  return returned

and infer_if if' returner =
  let* _ = check if'.cond Primitive.bool in
  let* then' = infer_none if'.then' in
  let* else' = infer_none if'.else' in
  returner (Typing.join then' else')

and infer_abs abs returner =
  let* param = infer_abs_param abs.param in
  let returner = return_abs abs param returner in
  infer abs.body returner

and infer_abs_param param =
  let type' = match param.type' with
  | Some type' -> type'
  | None -> TypingErrors.raise_param param in
  TypingValidate.validate_proper type';
  let bind = BindExprParam param in
  let* _ = add_bind bind type' in
  return type'

and infer_type_abs abs returner =
  TypingValidate.validate abs.param.bound;
  let* body = infer_none abs.body in
  returner (TypeAbsExprType { pos = abs.pos; param = abs.param; body })

and infer_stmt stmt returner =
  let* _ = infer_stmt_body stmt.stmt in
  infer stmt.expr returner

and infer_stmt_body body =
  match body with
  | StmtVar (var, type', expr) ->
    let* type' = match type' with
    | Some type' ->
      TypingValidate.validate type';
      let* _ = check expr type' in
      return type'
    | None ->
      infer_none expr
    in
    let* _ = add_bind (BindExprVar var) type' in
    return ()
  | StmtExpr expr ->
    let* _ = infer_none expr in
    return ()

and check_def def state =
  let state = start_progress state def in
  match def.type' with
  | Some type' ->
    TypingValidate.validate_proper type';
    let state = end_progress state def type' in
    let (_, state) = check def.expr type' state in
    (type', state)
  | None ->
    let (type', state) = infer def.expr (return_def def) state in
    (type', state)

let rec progress_defs progress =
  match DefSet.choose_opt progress.remains with
  | None -> progress
  | Some def ->
    let (_, progress) = check_def def progress in
    progress_defs progress

let check_exprs defs =
  progress_defs (make_progress defs)

let check_types types =
  List.iter TypingValidate.validate types
