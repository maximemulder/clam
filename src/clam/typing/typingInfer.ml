open Utils
open Model
open TypingContext

module DefKey = struct
  type t = def_expr
  let compare x y = Stdlib.compare x.def_expr_id y.def_expr_id
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
  context: context;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let make_progress defs =
  let remains = List.fold_left (fun set def -> DefSet.add def set) DefSet.empty defs in
  { remains; currents = DefSet.empty; dones = BindMap.empty }

let make_state progress =
  { progress; context = empty_context }

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

let with_params call params state =
  let context = state.context in
  let state = { state with context = { parent = Some context; params } } in
  let res = call state in
  (res, { state with context })

let return_def def =
  fun type' state -> (type', { state with progress = end_progress state.progress def type' })

let make_type expr type_data =
  (expr_pos expr, type_data)

let preop_types =
  [
    ("+", (TypeInt, TypeInt));
    ("-", (TypeInt, TypeInt));
    ("!", (TypeBool, TypeBool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let binop_types =
  [
    ("+",  ((TypeInt, TypeInt), TypeInt));
    ("-",  ((TypeInt, TypeInt), TypeInt));
    ("*",  ((TypeInt, TypeInt), TypeInt));
    ("/",  ((TypeInt, TypeInt), TypeInt));
    ("%",  ((TypeInt, TypeInt), TypeInt));
    ("++", ((TypeString, TypeString), TypeString));
    ("==", ((TypeAny, TypeAny), TypeBool));
    ("!=", ((TypeAny, TypeAny), TypeBool));
    ("<",  ((TypeInt, TypeInt), TypeBool));
    (">",  ((TypeInt, TypeInt), TypeBool));
    ("<=", ((TypeInt, TypeInt), TypeBool));
    (">=", ((TypeInt, TypeInt), TypeBool));
    ("|",  ((TypeBool, TypeBool), TypeBool));
    ("&",  ((TypeBool, TypeBool), TypeBool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let check_type type' context =
  TypingCheck.check type';
  TypingApply.apply type' context

let check_type_proper type' context =
  let type' = check_type type' context in
  match snd type' with
  | TypeAbs _ -> TypingErrors.raise_type_proper type'
  | _ -> type'

let rec check expr constr =
  match snd constr with
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
  if Bool.not (TypingSub.is_subtype type' constr) then
    TypingErrors.raise_expr_constraint expr type' constr
  else
    return ()

and check_error expr constr =
  let* type' = infer_none expr in
  TypingErrors.raise_expr_constraint expr type' constr

and check_tuple tuple constr =
  let expr = ExprTuple tuple in
  let exprs = tuple.expr_tuple_exprs in
  match snd constr with
  | TypeTuple constr_exprs ->
    if List.compare_lengths exprs constr_exprs != 0 then
      check_error expr constr
    else
    let* _ = map_list2 check exprs constr_exprs in
    return ()
  | _ ->
    check_error expr constr

and check_record record constr =
  let expr = ExprRecord record in
  let attrs = record.expr_record_attrs in
  match snd constr with
  | TypeRecord constr_attrs ->
    let* _ = map_map (fun constr_attr -> match List.find_opt (fun attr -> attr.attr_expr_name = constr_attr.attr_type_name) attrs with
    | Some attr -> check attr.attr_expr constr_attr.attr_type
    | None -> check_error expr constr
    ) constr_attrs in
    return ()
  | _ ->
    check_error expr constr

and check_if if' constr =
  let expr = ExprIf if' in
  let* _ = check if'.expr_if_cond (make_type expr TypeBool) in
  let* _ = check if'.expr_if_then constr in
  let* _ = check if'.expr_if_then constr in
  return ()

and check_abs abs constr =
  let expr = ExprAbs abs in
  match snd constr with
  | TypeAbsExpr (constr_params, constr_ret) ->
    let* _ = check_abs_params abs constr constr_params in
    let* ret = check_abs_ret abs.expr_abs_ret constr_ret in
    let* _ = check abs.expr_abs_body ret in
    return ()
  | _ ->
    check_error expr constr

and check_abs_params abs constr constr_params =
  let expr = ExprAbs abs in
  let params = abs.expr_abs_params in
  if List.compare_lengths params constr_params != 0 then
    check_error expr constr
  else
  let* _ = map_list2 check_abs_param params constr_params in
  return ()

and check_abs_param param constr =
  let* type' = match param.param_expr_type with
  | Some type' ->
    let* context = get_context in
    let type' = check_type_proper type' context in
    TypingCheck.check_subtype constr type';
    return type'
  | None ->
    return constr
  in
  add_bind (BindExprParam param) type'

and check_abs_ret type' constr =
  match type' with
  | Some type' ->
    let* context = get_context in
    let type' = check_type_proper type' context in
    TypingCheck.check_subtype type' constr;
    return type'
  | None ->
    return constr

and check_type_abs abs constr =
  let expr = ExprTypeAbs abs in
  match snd constr with
  | TypeAbsExprType (constr_params, constr_ret) ->
    let* _ = check_type_abs_params abs constr constr_params in
    let* _ = check abs.expr_type_abs_body constr_ret in
    return ()
  | _ ->
    check_error expr constr

and check_type_abs_params abs constr constr_params =
  let expr = ExprTypeAbs abs in
  let params = abs.expr_type_abs_params in
  if List.compare_lengths params constr_params != 0 then
    check_error expr constr
  else
  let* _ = map_list2 (check_type_abs_param expr constr) params constr_params in
  return ()

and check_type_abs_param expr constr param constr_param state =
  let type' = check_type param.param_type state.context in
  if TypingSub.is_type type' constr_param.param_type then
    (() ,state)
  else
    check_error expr constr state

and infer expr returner =
  match expr with
  | ExprVoid void ->
    infer_void void returner
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
  | ExprBlock block ->
    infer_block block returner
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

and infer_none expr =
  infer expr return

and infer_void void returner =
  returner (void.expr_void_pos, TypeVoid)

and infer_bool bool returner =
  returner (bool.expr_bool_pos, TypeBool)

and infer_int int returner =
  returner (int.expr_int_pos, TypeInt)

and infer_char char returner =
  returner (char.expr_char_pos, TypeChar)

and infer_string string returner =
  returner (string.expr_string_pos, TypeString)

and infer_bind bind returner state =
  let pos = bind.expr_bind_pos in
  let bind = Option.get !(bind.expr_bind) in
  match bind with
  | BindExprPrint ->
    returner (pos, TypeAbsExpr ([(pos, TypeAny)], (pos, TypeVoid))) state
  | BindExprDef def when DefSet.mem def state.progress.remains ->
    let (type', progress) = check_def def state.progress in
    returner type' { state with progress }
  | BindExprDef def when DefSet.mem def state.progress.currents ->
    TypingErrors.raise_expr_recursive def
  | _ ->
    returner (BindMap.find bind state.progress.dones) state

and infer_tuple tuple returner =
  let* types = map_list infer_none tuple.expr_tuple_exprs in
  returner (tuple.expr_tuple_pos, TypeTuple types)

and infer_record record returner =
  let attrs = record.expr_record_attrs in
  let attrs = List.fold_left (fun map attr -> NameMap.add attr.attr_expr_name attr map) NameMap.empty attrs in
  let* attrs = map_map infer_record_attr attrs in
  returner (record.expr_record_pos, TypeRecord attrs)

and infer_record_attr attr =
  let* type' = infer_none attr.attr_expr in
  return (make_attr_type attr.attr_expr_pos attr.attr_expr_name type')

and infer_elem elem returner =
  let* type' = infer_none elem.expr_elem_expr in
  let* context = get_context in
  match infer_elem_type type' elem.expr_elem_index context with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_elem elem type'

and infer_elem_type type' index context =
  let type' = TypingPromote.promote type' in
  let type' = TypingApply.apply type' context in
  match snd type' with
  | TypeTuple types ->
    List.nth_opt types index
  | TypeUnion (left, right) ->
    let left = infer_elem_type left index context in
    let right = infer_elem_type right index context in
    Utils.map_option2 left right
      (fun left right -> (fst type', (TypingJoin.join left right)))
  | TypeInter (left, right) ->
    let left = infer_elem_type left index context in
    let right = infer_elem_type right index context in
    Utils.join_option2 left right
      (fun left right -> (fst type', (TypingMeet.meet left right)))
  | _ -> None

and infer_attr attr returner =
  let* type' = infer_none attr.expr_attr_expr in
  let* context = get_context in
  match infer_attr_type type' attr.expr_attr_name context with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_attr attr type'

and infer_attr_type type' name context =
  let type' = TypingPromote.promote type' in
  let type' = TypingApply.apply type' context in
  match snd type' with
  | TypeRecord attrs ->
    Option.map (fun attr -> attr.attr_type) (NameMap.find_opt name attrs)
  | TypeUnion (left, right) ->
    let left = infer_attr_type left name context in
    let right = infer_attr_type right name context in
    Utils.map_option2 left right
      (fun left right -> (fst type', (TypingJoin.join left right)))
  | TypeInter (left, right) ->
    let left = infer_attr_type left name context in
    let right = infer_attr_type right name context in
    Utils.join_option2 left right
      (fun left right -> (fst type', (TypingMeet.meet left right)))
  | _ -> None

and infer_preop preop returner =
  let entry = NameMap.find_opt preop.expr_preop_op preop_types in
  match entry with
  | Some (type_operand, type_result) ->
    let operand = preop.expr_preop_expr in
    let type_operand = make_type operand type_operand in
    let type_result = (preop.expr_preop_pos, type_result) in
    let* _ = returner type_result in
    let* _ = check operand type_operand in
    return type_result
  | None ->
    TypingErrors.raise_unexpected

and infer_binop binop returner =
  let entry = NameMap.find_opt binop.expr_binop_op binop_types in
  match entry with
  | Some ((type_left, type_right), type_result) ->
    let left = binop.expr_binop_left in
    let right = binop.expr_binop_right in
    let type_left = make_type left type_left in
    let type_right = make_type right type_right in
    let type_result = (binop.expr_binop_pos, type_result) in
    let* _ = returner type_result in
    let* _ = check left type_left in
    let* _ = check right type_right in
    return type_result
  | None ->
    TypingErrors.raise_unexpected

and infer_ascr ascr returner =
  let* context = get_context in
  let type' = check_type_proper ascr.expr_ascr_type context in
  let* _ = returner type' in
  let* _ = check ascr.expr_ascr_expr type' in
  return type'

and infer_if if' returner =
  let* _ = check if'.expr_if_cond (expr_pos if'.expr_if_cond, TypeBool) in
  let* then' = infer_none if'.expr_if_then in
  let* else' = infer_none if'.expr_if_else in
  returner (if'.expr_if_pos, TypingJoin.join then' else')

and infer_block block returner =
  let* _ = map_list infer_block_stmt block.expr_block_stmts in
  match block.expr_block_expr with
  | Some expr -> infer expr returner
  | None -> returner (block.expr_block_pos, TypeVoid)

and infer_block_stmt stmt =
  match stmt with
  | StmtVar (var, type', expr) ->
    let* type' = match type' with
    | Some type' ->
      let* context = get_context in
      let type' = check_type_proper type' context in
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

and infer_abs abs returner =
  let* params = infer_abs_params abs.expr_abs_params in
  match abs.expr_abs_ret with
  | Some type' ->
    let* context = get_context in
    let type' = check_type_proper type' context in
    let signature = (abs.expr_abs_pos, TypeAbsExpr (params, type')) in
    let* _ = returner signature in
    let* _ = check abs.expr_abs_body type' in
    return signature
  | None ->
    let* type' = infer_none abs.expr_abs_body in
    returner (abs.expr_abs_pos, TypeAbsExpr (params, type'))

and infer_abs_params params =
  let types = List.map (fun param -> match param.param_expr_type with
  | Some type' -> type'
  | None -> TypingErrors.raise_param param
  ) params in
  let* context = get_context in
  let types = List.map (fun type' -> check_type_proper type' context) types in
  let params = List.map (fun param -> BindExprParam param) params in
  let* _ = map_list2 add_bind params types in
  return types

and infer_app app returner =
  let* type' = infer_none app.expr_app_expr in
  match snd type' with
  | TypeAbsExpr (params, type') ->
    infer_app_abs params type' app returner
  | _ -> TypingErrors.raise_expr_app_kind app type'

and infer_app_abs params type' app returner =
  let args = app.expr_app_args in
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_app_arity app params
  else
  let* _ = map_list2 check args params in
  returner type'

and infer_type_abs abs returner =
  let params = abs.expr_type_abs_params in
  let body = abs.expr_type_abs_body in
  let* context = get_context in
  let _ = List.map (fun param -> check_type param.param_type context) params in
  let* type' = infer_none body in
  returner (abs.expr_type_abs_pos, TypeAbsExprType (params, type'))

and infer_type_app app returner =
  let* type' = infer_none app.expr_type_app_expr in
  match snd type' with
  | TypeAbsExprType (params, type') ->
    infer_type_app_abs params type' app returner
  | _ -> TypingErrors.raise_expr_type_app_kind app type'

and infer_type_app_abs params type' app returner state =
  let args = app.expr_type_app_args in
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_type_app_arity app params
  else
  let pairs = List.combine params args in
  List.iter (fun (param, arg) -> let _ = check_type arg state.context in TypingCheck.check_subtype arg param.param_type; ) pairs;
  let context = { parent = Some state.context; params = pairs } in
  let body = TypingApply.apply type' context in
  returner body state

and check_def def progress =
  let progress = start_progress progress def in
  match def.def_expr_type with
  | Some type' ->
    let type' = check_type_proper type' empty_context in
    let progress = end_progress progress def type' in
    let state = make_state progress in
    let (_, state) = check def.def_expr type' state in
    (type', state.progress)
  | None ->
    let state = make_state progress in
    let (type', state) = infer def.def_expr (return_def def) state in
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
  List.iter (fun type' -> TypingCheck.check type') types
