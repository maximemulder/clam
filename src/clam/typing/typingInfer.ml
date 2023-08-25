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

type progress = {
  remains: DefSet.t;
  currents: DefSet.t;
  dones: type' BindMap.t;
}

type state = {
  progress: progress;
  context: TypingContext.context;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let make_progress defs =
  let remains = List.fold_left (fun set def -> DefSet.add def set) DefSet.empty defs in
  { remains; currents = DefSet.empty; dones = BindMap.empty }

let make_state progress =
  { progress; context = TypingContext.context_empty }

let get_context state =
  (state.context, state)

let start_progress progress def =
  let remains = DefSet.remove def progress.remains in
  let currents = DefSet.add def progress.currents in
  { progress with remains; currents }

let end_progress progress def type' =
  let currents = DefSet.remove def progress.currents in
  let dones = BindMap.add (BindExprDef def) type' progress.dones in
  { progress with currents; dones }

let add_bind bind type' state =
  let dones = BindMap.add bind type' state.progress.dones in
  ((), { state with progress = { state.progress with dones } })

let with_entries call entries state =
  let parent = state.context in
  let context = TypingContext.context_child parent entries in
  let state = { state with context } in
  let res = call state in
  (res, { state with context = parent })

let return_def (def: def_expr) =
  fun type' state -> (type', { state with progress = end_progress state.progress def type' })

let return_abs (abs: expr_abs) params returner =
  fun body -> returner (TypeAbsExpr { pos = abs.pos; params; body })

let preop_types =
  [
    ("+", (prim_int, prim_int));
    ("-", (prim_int, prim_int));
    ("!", (prim_bool, prim_bool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let binop_types =
  [
    ("+",  ((prim_int, prim_int), prim_int));
    ("-",  ((prim_int, prim_int), prim_int));
    ("*",  ((prim_int, prim_int), prim_int));
    ("/",  ((prim_int, prim_int), prim_int));
    ("%",  ((prim_int, prim_int), prim_int));
    ("++", ((prim_string, prim_string), prim_string));
    ("==", ((prim_top, prim_top), prim_bool));
    ("!=", ((prim_top, prim_top), prim_bool));
    ("<",  ((prim_int, prim_int), prim_bool));
    (">",  ((prim_int, prim_int), prim_bool));
    ("<=", ((prim_int, prim_int), prim_bool));
    (">=", ((prim_int, prim_int), prim_bool));
    ("|",  ((prim_bool, prim_bool), prim_bool));
    ("&",  ((prim_bool, prim_bool), prim_bool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let print_type =
  TypeAbsExpr { pos = prim_pos; params = [prim_top]; body = prim_unit }

let validate type' =
  let* context = get_context in
  let () = TypingValidate.validate type' context in
  return ()

let validate_proper type' =
  let* context = get_context in
  let () = TypingValidate.validate_proper type' context in
  return ()

let validate_subtype type' constr =
  let* context = get_context in
  let () = TypingValidate.validate_subtype type' constr context in
  return ()

let rec check expr constr =
  match constr with
  | TypeUnion _ | TypeInter _ ->
    check_infer expr constr
  | TypeApp app ->
    let constr = TypingApply.apply_app app in
    check expr constr
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
  let* context = get_context in
  if Bool.not (TypingSub.is_subtype type' constr context) then
    TypingErrors.raise_expr_constraint expr type' constr
  else
    return ()

and check_error expr constr =
  let* type' = infer_none expr in
  TypingErrors.raise_expr_constraint expr type' constr

and check_tuple tuple constr =
  let expr = ExprTuple tuple in
  let elems = tuple.elems in
  match constr with
  | TypeTuple constr_tuple ->
    let constr_elems = constr_tuple.elems in
    if List.compare_lengths elems constr_elems != 0 then
      check_error expr constr
    else
    let* _ = map_list2 check elems constr_elems in
    return ()
  | _ ->
    check_error expr constr

and check_record record constr =
  let expr = ExprRecord record in
  let attrs = record.attrs in
  match constr with
  | TypeRecord constr_record ->
    let* _ = map_map (fun (constr_attr: attr_type) -> match List.find_opt (fun (attr: attr_expr) -> attr.name = constr_attr.name) attrs with
    | Some attr -> check attr.expr constr_attr.type'
    | None ->
      check_error expr constr
    ) constr_record.attrs in
    return ()
  | _ ->
    check_error expr constr

and check_if if' constr =
  let* _ = check if'.cond prim_bool in
  let* _ = check if'.then' constr in
  let* _ = check if'.else' constr in
  return ()

and check_abs abs constr =
  let expr = ExprAbs abs in
  match constr with
  | TypeAbsExpr constr_abs ->
    let* _ = check_abs_params abs constr constr_abs.params in
    let* _ = check abs.body constr_abs.body in
    return ()
  | _ ->
    check_error expr constr

and check_abs_params abs constr constr_params =
  let expr = ExprAbs abs in
  let params = abs.params in
  if List.compare_lengths params constr_params != 0 then
    check_error expr constr
  else
  let* _ = map_list2 check_abs_param params constr_params in
  return ()

and check_abs_param param constr =
  let* type' = match param.type' with
  | Some type' ->
    let* () = validate_proper type' in
    let* () = validate_subtype constr type' in
    return type'
  | None ->
    return constr
  in
  add_bind (BindExprParam param) type'

and check_type_abs abs constr =
  let expr = ExprTypeAbs abs in
  match constr with
  | TypeAbsExprType constr_abs ->
    let* _ = check_type_abs_params abs constr constr_abs.params in
    let* _ = check abs.body constr_abs.body in
    return ()
  | _ ->
    check_error expr constr

and check_type_abs_params abs constr constr_params =
  let expr = ExprTypeAbs abs in
  let params = abs.params in
  if List.compare_lengths params constr_params != 0 then
    check_error expr constr
  else
  let* _ = map_list2 (check_type_abs_param expr constr) params constr_params in
  return ()

and check_type_abs_param expr constr param constr_param state =
  let (_, state) = validate param.type' state in
  if TypingEqual.is_type param.type' constr_param.type' then
    (() ,state)
  else
    check_error expr constr state

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
  | ExprPreop preop ->
    infer_preop preop returner
  | ExprBinop binop ->
    infer_binop binop returner
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
  | BindExprPrint ->
    returner print_type state
  | BindExprDef def when DefSet.mem def state.progress.remains ->
    let (type', progress) = check_def def state.progress in
    returner type' { state with progress }
  | BindExprDef def when DefSet.mem def state.progress.currents ->
    TypingErrors.raise_expr_recursive def
  | _ ->
    returner (BindMap.find bind state.progress.dones) state

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
  let* type' = infer_none elem.expr in
  let* context = get_context in
  match infer_elem_type type' elem.index context with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_elem elem type'

and infer_elem_type type' index context =
  match type' with
  | TypeTuple tuple ->
    List.nth_opt tuple.elems index
  | TypeVar var ->
    let type' = TypingPromote.promote_var var in
    infer_elem_type type' index context
  | TypeApp app ->
    let type' = TypingApply.apply_app app in
    infer_elem_type type' index context
  | TypeUnion union ->
    let left = infer_elem_type union.left index context in
    let right = infer_elem_type union.right index context in
    Utils.map_option2 left right
      (fun left right -> TypingJoin.join left right)
  | TypeInter inter ->
    let left = infer_elem_type inter.left index context in
    let right = infer_elem_type inter.right index context in
    Utils.join_option2 left right
      (fun left right -> TypingMeet.meet left right)
  | _ -> None

and infer_attr attr returner =
  let* type' = infer_none attr.expr in
  let* context = get_context in
  match infer_attr_type type' attr.name context with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_attr attr type'

and infer_attr_type type' name context =
  match type' with
  | TypeRecord record ->
    Option.map (fun (attr: attr_type) -> attr.type') (NameMap.find_opt name record.attrs)
  | TypeVar var ->
    let type' = TypingPromote.promote_var var in
    infer_attr_type type' name context
  | TypeApp app ->
    let type' = TypingApply.apply_app app in
    infer_attr_type type' name context
  | TypeUnion union ->
    let left = infer_attr_type union.left name context in
    let right = infer_attr_type union.right name context in
    Utils.map_option2 left right
      (fun left right -> TypingJoin.join left right)
  | TypeInter inter ->
    let left = infer_attr_type inter.left name context in
    let right = infer_attr_type inter.right name context in
    Utils.join_option2 left right
      (fun left right -> TypingMeet.meet left right)
  | _ -> None

and infer_app app returner =
  let* type' = infer_none app.expr in
  let* context = get_context in
  match infer_app_type app type' context with
  | Some abs -> infer_app_abs app abs returner
  | None -> TypingErrors.raise_expr_app_kind app type'

and infer_app_type app type' context =
  match type' with
  | TypeVar var ->
    let type' = TypingPromote.promote_var var in
    infer_app_type app type' context
  | TypeApp type_app ->
    let type' = TypingApply.apply_app type_app in
    infer_app_type app type' context
  | TypeAbsExpr abs ->
    Some abs
  | _ -> None

and infer_app_abs app abs returner =
  let params = abs.params in
  let args = app.args in
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_app_arity app params
  else
  let* _ = map_list2 check args params in
  returner abs.body

and infer_preop preop returner =
  let entry = NameMap.find_opt preop.op preop_types in
  match entry with
  | Some (type_operand, type_result) ->
    let operand = preop.expr in
    let* _ = returner type_result in
    let* _ = check operand type_operand in
    return type_result
  | None ->
    TypingErrors.raise_unexpected

and infer_binop binop returner =
  let entry = NameMap.find_opt binop.op binop_types in
  match entry with
  | Some ((type_left, type_right), type_result) ->
    let left = binop.left in
    let right = binop.right in
    let* _ = returner type_result in
    let* _ = check left type_left in
    let* _ = check right type_right in
    return type_result
  | None ->
    TypingErrors.raise_unexpected

and infer_ascr ascr returner =
  let type' = ascr.type' in
  let* () = validate type' in
  let* _ = returner type' in
  let* _ = check ascr.expr type' in
  return type'

and infer_if if' returner =
  let* _ = check if'.cond prim_bool in
  let* then' = infer_none if'.then' in
  let* else' = infer_none if'.else' in
  returner (TypingJoin.join then' else')

and infer_abs abs returner =
  let* params = infer_abs_params abs.params in
  let returner = return_abs abs params returner in
  let* body = infer abs.body returner in
  return (TypeAbsExpr { pos = abs.pos; params; body })

and infer_abs_params params =
  let types = List.map (fun (param: param_expr) -> match param.type' with
  | Some type' -> type'
  | None -> TypingErrors.raise_param param
  ) params in
  let* () = iter_list validate_proper types in
  let params = List.map (fun param -> BindExprParam param) params in
  let* _ = map_list2 add_bind params types in
  return types

and infer_type_abs abs returner =
  let params = abs.params in
  let body = abs.body in
  let _ = List.map (fun (param: param_type) -> TypingValidate.validate param.type') params in
  let* body = infer_none body in
  returner (TypeAbsExprType { pos = abs.pos; params; body })

and infer_type_app app returner =
  let* type' = infer_none app.expr in
  match type' with
  | TypeAbsExprType abs ->
    infer_type_app_abs app abs returner
  | _ -> TypingErrors.raise_expr_type_app_kind app type'

and infer_type_app_abs app abs returner =
  let params = abs.params in
  let args = app.args in
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_type_app_arity app params
  else
  let* _ = iter_list2 (fun (param: param_type) arg -> validate_subtype arg param.type') params args in
  let entries = List.combine params args in
  let body = TypingApply.apply abs.body entries in
  returner body

and infer_stmt stmt returner =
  let* _ = infer_stmt_body stmt.stmt in
  infer stmt.expr returner

and infer_stmt_body body =
  match body with
  | StmtVar (var, type', expr) ->
    let* type' = match type' with
    | Some type' ->
      let* () = validate type' in
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

and check_def def progress =
  let progress = start_progress progress def in
  match def.type' with
  | Some type' ->
    TypingValidate.validate_proper type' TypingContext.context_empty;
    let progress = end_progress progress def type' in
    let state = make_state progress in
    let (_, state) = check def.expr type' state in
    (type', state.progress)
  | None ->
    let state = make_state progress in
    let (type', state) = infer def.expr (return_def def) state in
    (type', state.progress)

let rec progress_defs progress =
  match DefSet.choose_opt progress.remains with
  | None -> progress
  | Some def ->
    let (_, progress) = check_def def progress in
    progress_defs progress

let check_exprs defs =
  progress_defs (make_progress defs)

let check_types types =
  List.iter (fun type' -> TypingValidate.validate type' TypingContext.context_empty) types
