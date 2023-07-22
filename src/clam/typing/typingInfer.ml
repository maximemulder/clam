open Utils
open Model

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

let return_def def =
  fun type' state -> (type', { state with progress = end_progress state.progress def type' })

let return_abs abs params returner =
  fun body -> returner (TypeAbsExpr {
    type_abs_expr_pos = abs.expr_abs_pos;
    type_abs_expr_params = params;
    type_abs_expr_body = body;
  })

let preop_types =
  [
    ("+", (type_int, type_int));
    ("-", (type_int, type_int));
    ("!", (type_bool, type_bool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let binop_types =
  [
    ("+",  ((type_int, type_int), type_int));
    ("-",  ((type_int, type_int), type_int));
    ("*",  ((type_int, type_int), type_int));
    ("/",  ((type_int, type_int), type_int));
    ("%",  ((type_int, type_int), type_int));
    ("++", ((type_string, type_string), type_string));
    ("==", ((type_top, type_top), type_bool));
    ("!=", ((type_top, type_top), type_bool));
    ("<",  ((type_int, type_int), type_bool));
    (">",  ((type_int, type_int), type_bool));
    ("<=", ((type_int, type_int), type_bool));
    (">=", ((type_int, type_int), type_bool));
    ("|",  ((type_bool, type_bool), type_bool));
    ("&",  ((type_bool, type_bool), type_bool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let print_type =
  TypeAbsExpr {
    type_abs_expr_pos = primitive_pos;
    type_abs_expr_params = [type_top];
    type_abs_expr_body = type_unit;
  }

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
  let exprs = tuple.expr_tuple_exprs in
  match constr with
  | TypeTuple constr_tuple ->
    let constr_params = constr_tuple.type_tuple_types in
    if List.compare_lengths exprs constr_params != 0 then
      check_error expr constr
    else
    let* _ = map_list2 check exprs constr_params in
    return ()
  | _ ->
    check_error expr constr

and check_record record constr =
  let expr = ExprRecord record in
  let attrs = record.expr_record_attrs in
  match constr with
  | TypeRecord constr_record ->
    let* _ = map_map (fun constr_attr -> match List.find_opt (fun attr -> attr.attr_expr_name = constr_attr.attr_type_name) attrs with
    | Some attr -> check attr.attr_expr constr_attr.attr_type
    | None ->
      check_error expr constr
    ) constr_record.type_record_attrs in
    return ()
  | _ ->
    check_error expr constr

and check_if if' constr =
  let* _ = check if'.expr_if_cond type_bool in
  let* _ = check if'.expr_if_then constr in
  let* _ = check if'.expr_if_then constr in
  return ()

and check_abs abs constr =
  let expr = ExprAbs abs in
  match constr with
  | TypeAbsExpr constr_abs ->
    let* _ = check_abs_params abs constr constr_abs.type_abs_expr_params in
    let* _ = check abs.expr_abs_body constr_abs.type_abs_expr_body in
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
    let* _ = check_type_abs_params abs constr constr_abs.type_abs_expr_type_params in
    let* _ = check abs.expr_type_abs_body constr_abs.type_abs_expr_type_body in
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
  let (_, state) = validate param.param_type state in
  if TypingEqual.is_type param.param_type constr_param.param_type then
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

and infer_unit unit returner =
  returner (TypeUnit {
    type_unit_pos = unit.expr_unit_pos;
  })

and infer_bool bool returner =
  returner (TypeBool {
    type_bool_pos = bool.expr_bool_pos;
  })

and infer_int int returner =
  returner (TypeInt {
    type_int_pos = int.expr_int_pos;
  })

and infer_char char returner =
  returner (TypeChar {
    type_char_pos = char.expr_char_pos;
  })

and infer_string string returner =
  returner (TypeString {
    type_string_pos = string.expr_string_pos;
  })

and infer_bind bind returner state =
  let bind = Option.get !(bind.expr_bind) in
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
  let* types = map_list infer_none tuple.expr_tuple_exprs in
  returner (TypeTuple {
    type_tuple_pos = tuple.expr_tuple_pos;
    type_tuple_types = types;
  })

and infer_record record returner =
  let attrs = record.expr_record_attrs in
  let attrs = List.fold_left (fun map attr -> NameMap.add attr.attr_expr_name attr map) NameMap.empty attrs in
  let* attrs = map_map infer_record_attr attrs in
  returner (TypeRecord {
    type_record_pos = record.expr_record_pos;
    type_record_attrs = attrs;
  })

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
  match type' with
  | TypeTuple tuple ->
    List.nth_opt tuple.type_tuple_types index
  | TypeVar var ->
    let type' = TypingPromote.promote_var var in
    infer_elem_type type' index context
  | TypeApp app ->
    let type' = TypingApply.apply_app app in
    infer_elem_type type' index context
  | TypeUnion union ->
    let left = infer_elem_type union.type_union_left index context in
    let right = infer_elem_type union.type_union_right index context in
    Utils.map_option2 left right
      (fun left right -> TypingJoin.join left right)
  | TypeInter inter ->
    let left = infer_elem_type inter.type_inter_left index context in
    let right = infer_elem_type inter.type_inter_right index context in
    Utils.join_option2 left right
      (fun left right -> TypingMeet.meet left right)
  | _ -> None

and infer_attr attr returner =
  let* type' = infer_none attr.expr_attr_expr in
  let* context = get_context in
  match infer_attr_type type' attr.expr_attr_name context with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_attr attr type'

and infer_attr_type type' name context =
  match type' with
  | TypeRecord record ->
    Option.map (fun attr -> attr.attr_type) (NameMap.find_opt name record.type_record_attrs)
  | TypeVar var ->
    let type' = TypingPromote.promote_var var in
    infer_attr_type type' name context
  | TypeApp app ->
    let type' = TypingApply.apply_app app in
    infer_attr_type type' name context
  | TypeUnion union ->
    let left = infer_attr_type union.type_union_left name context in
    let right = infer_attr_type union.type_union_right name context in
    Utils.map_option2 left right
      (fun left right -> TypingJoin.join left right)
  | TypeInter inter ->
    let left = infer_attr_type inter.type_inter_left name context in
    let right = infer_attr_type inter.type_inter_right name context in
    Utils.join_option2 left right
      (fun left right -> TypingMeet.meet left right)
  | _ -> None

and infer_app app returner =
  let* type' = infer_none app.expr_app_expr in
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
  let params = abs.type_abs_expr_params in
  let args = app.expr_app_args in
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_app_arity app params
  else
  let* _ = map_list2 check args params in
  returner abs.type_abs_expr_body

and infer_preop preop returner =
  let entry = NameMap.find_opt preop.expr_preop_op preop_types in
  match entry with
  | Some (type_operand, type_result) ->
    let operand = preop.expr_preop_expr in
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
    let* _ = returner type_result in
    let* _ = check left type_left in
    let* _ = check right type_right in
    return type_result
  | None ->
    TypingErrors.raise_unexpected

and infer_ascr ascr returner =
  let type' = ascr.expr_ascr_type in
  let* () = validate type' in
  let* _ = returner type' in
  let* _ = check ascr.expr_ascr_expr type' in
  return type'

and infer_if if' returner =
  let* _ = check if'.expr_if_cond type_bool in
  let* then' = infer_none if'.expr_if_then in
  let* else' = infer_none if'.expr_if_else in
  returner (TypingJoin.join then' else')

and infer_block block returner =
  let* _ = map_list infer_block_stmt block.expr_block_stmts in
  match block.expr_block_expr with
  | Some expr -> infer expr returner
  | None -> returner type_unit

and infer_block_stmt stmt =
  match stmt with
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

and infer_abs abs returner =
  let* params = infer_abs_params abs.expr_abs_params in
  let returner = return_abs abs params returner in
  let* body = infer abs.expr_abs_body returner in
  return (TypeAbsExpr {
    type_abs_expr_pos = abs.expr_abs_pos;
    type_abs_expr_params = params;
    type_abs_expr_body = body;
  })

and infer_abs_params params =
  let types = List.map (fun param -> match param.param_expr_type with
  | Some type' -> type'
  | None -> TypingErrors.raise_param param
  ) params in
  let* () = iter_list validate_proper types in
  let params = List.map (fun param -> BindExprParam param) params in
  let* _ = map_list2 add_bind params types in
  return types

and infer_type_abs abs returner =
  let params = abs.expr_type_abs_params in
  let body = abs.expr_type_abs_body in
  let _ = List.map (fun param -> TypingValidate.validate param.param_type) params in
  let* body = infer_none body in
  returner (TypeAbsExprType {
    type_abs_expr_type_pos = abs.expr_type_abs_pos;
    type_abs_expr_type_params = params;
    type_abs_expr_type_body = body;
  })

and infer_type_app app returner =
  let* type' = infer_none app.expr_type_app_expr in
  match type' with
  | TypeAbsExprType abs ->
    infer_type_app_abs app abs returner
  | _ -> TypingErrors.raise_expr_type_app_kind app type'

and infer_type_app_abs app abs returner state =
  let params = abs.type_abs_expr_type_params in
  let args = app.expr_type_app_args in
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_type_app_arity app params
  else
  let pairs = List.combine params args in
  let (_, state ) = iter_list (fun (param, arg) -> validate_subtype arg param.param_type) pairs state in
  let body = TypingApply.apply abs.type_abs_expr_type_body pairs in
  returner body state

and check_def def progress =
  let progress = start_progress progress def in
  match def.def_expr_type with
  | Some type' ->
    TypingValidate.validate_proper type' TypingContext.context_empty;
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
  List.iter (fun type' -> TypingValidate.validate type' TypingContext.context_empty) types
