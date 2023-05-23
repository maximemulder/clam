open Utils
open Model
open TypingApp
open TypingCheck
open TypingContext
open TypingSub

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

let start_progress progress def =
  let remains = DefSet.remove def progress.remains in
  let currents = DefSet.add def progress.currents in
  { progress with remains; currents }

let end_progress progress def type' =
  let currents = DefSet.remove def progress.currents in
  let dones = BindMap.add (BindExprDef def) type' progress.dones in
  { progress with currents; dones }

let make_state progress =
  { progress; context = empty_context }

let add_bind bind type' state =
  let dones = BindMap.add bind type' state.progress.dones in
  { state with progress = { state.progress with dones } }

let rec check_expr expr constraint' state =
  let (type', state) = infer expr state in
  if Bool.not (is_subtype type' constraint')
    then TypingErrors.raise_expr_constraint expr type' constraint'
    else ((), state)

and infer expr state =
  let (type_data, state) = infer_data expr state in
  let type' = (fst expr, type_data) in
  let type' = apply type' state.context in
  (type', state)

and infer_data expr =
  match snd expr with
  | ExprVoid ->
    return TypeVoid
  | ExprBool _ ->
    return TypeBool
  | ExprInt _ ->
    return TypeInt
  | ExprChar _ ->
    return TypeChar
  | ExprString _ ->
    return TypeString
  | ExprBind bind ->
    infer_bind expr (Option.get bind.bind_expr)
  | ExprTuple types ->
    let* types = list_map infer types in
    return (TypeTuple types)
  | ExprRecord attrs ->
    let* attrs = infer_attrs attrs in
    return (TypeRecord attrs)
  | ExprVariant (expr, index) ->
    infer_variant expr index
  | ExprAttr (expr, attr) ->
    infer_attr expr attr
  | ExprPreop (op, expr) ->
    infer_preop op expr
  | ExprBinop (left, op, right) ->
    infer_binop left op right
  | ExprAscr (expr, type') ->
    let* _ = check_expr expr type' in
    return (snd type')
  | ExprIf (if', then', else') ->
    let* _ = check_expr if' (fst expr, TypeBool) in
    let* then' = infer then' in
    let* else' = infer else' in
    return (TypeUnion (then', else'))
  | ExprBlock block ->
    infer_block block
  | ExprAbs (params, type', expr) ->
    let* params = infer_abs_params params in
    let* type' = infer_abs_return type' expr in
    return (TypeAbsExpr (params, type'))
  | ExprApp (expr, args) ->
    infer_app expr args
  | ExprTypeAbs (params, expr) ->
    let _ = List.iter (fun param -> check param.param_type) params in
    let* type' = infer expr in
    return (TypeAbsExprType (params, type'))
  | ExprTypeApp (expr, args) ->
    infer_type_app expr args

and infer_variant expr index =
  let* type' = infer expr in
  match snd type' with
  | TypeTuple types ->
    (match List.nth_opt types index with
    | Some type' -> return (snd type')
    | None -> TypingErrors.raise_expr_tuple_index expr type' index)
  | _ -> TypingErrors.raise_expr_tuple_kind expr type'

and infer_attr expr attr =
  let* type' = infer expr in
  match snd type' with
  | TypeRecord attrs ->
    (match NameMap.find_opt attr attrs with
    | Some attr -> return (snd attr.attr_type)
    | None -> TypingErrors.raise_expr_record_attr expr type' attr)
  | _ -> TypingErrors.raise_expr_record_kind expr type'

and infer_bind expr bind state =
  match bind with
  | BindExprPrint ->
    (TypeAbsExpr ([(fst expr, TypeAny)], (fst expr, TypeVoid)), state)
  | BindExprDef def when DefSet.mem def state.progress.remains ->
    let (type', progress) = check_def def state.progress in
    (type', { state with progress })
  | BindExprDef def when DefSet.mem def state.progress.currents ->
    TypingErrors.raise_expr_recursive def
  | _ -> (snd (BindMap.find bind state.progress.dones), state)

and infer_block block =
  let* _ = list_map infer_stmt block.block_stmts in
  match block.block_expr with
  | Some expr -> infer_data expr
  | None -> return TypeVoid

and infer_stmt stmt state =
  match stmt with
  | StmtVar (var, type', expr) ->
    let (type', state) = match type' with
    | Some type' ->
      let (_, state) = check_expr expr type' state in
      (type', state)
    | None ->
      infer expr state
    in
    let state = add_bind (BindExprVar var) type' state in
    ((), state)
  | StmtExpr expr ->
    let _ = infer expr state in
    ((), state)

and infer_attrs attrs =
  let attrs = List.fold_left (fun map attr -> NameMap.add attr.attr_expr_name attr map) NameMap.empty attrs in
  let mapper = (fun attr ->
    let* type' = infer attr.attr_expr in
    return (make_attr_type (fst attr.attr_expr) attr.attr_expr_name type')
  ) in
  map_map mapper attrs

and infer_abs_params params state =
  let types = List.map (fun param -> match param.param_expr_type with
  | Some type' -> type'
  | None -> TypingErrors.raise_param param
  ) params in
  let params = List.map (fun param -> BindExprParam param) params in
  let pairs = List.combine params types in
  let state = List.fold_left (fun state pair -> add_bind (fst pair) (snd pair) state) state pairs in
  (types, state)

and infer_abs_return type' expr =
  match type' with
  | Some type' ->
    let* _ = check_expr expr type' in
    return type'
  | None ->
    infer expr

and infer_app expr args =
  let* type' = infer expr in
  match snd type' with
  | TypeAbsExpr (params, type') ->
    infer_app_abs expr type' params args
  | _ -> TypingErrors.raise_expr_app_kind expr type'

and infer_app_abs expr type' params args =
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_app_arity expr params args
  else
  let pairs = List.combine params args in
  let* _ = list_map (fun (param, arg) -> check_expr arg param) pairs in
  return (snd type')

and infer_type_app expr args =
  let* type' = infer expr in
  match snd type' with
  | TypeAbsExprType (params, type') ->
    infer_type_app_abs expr params args type'
  | _ -> TypingErrors.raise_expr_type_app_kind expr type'

and infer_type_app_abs expr params args type' state =
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_type_app_arity expr params args
  else
  let pairs = List.combine params args in
  List.iter (fun (param, arg) -> check arg; check_subtype arg param.param_type; ) pairs;
  let context = { parent = Some state.context; params = pairs } in
  let body = apply type' context in
  ((snd body), state)

and infer_preop op expr =
  match op with
  | "+" ->
    let* _ = check_expr expr (fst expr, TypeInt) in
    return TypeInt
  | "-" ->
    let* _ = check_expr expr (fst expr, TypeInt) in
    return TypeInt
  | "!" ->
    let* _ = check_expr expr (fst expr, TypeBool) in
    return TypeBool
  | _ -> TypingErrors.raise_unexpected

and infer_binop left op right =
  match op with
  | "+" ->
    let* _ = check_expr left (fst left, TypeInt) in
    let* _ = check_expr right (fst right, TypeInt) in
    return TypeInt
  | "-" ->
    let* _ = check_expr left (fst left, TypeInt) in
    let* _ = check_expr right (fst right, TypeInt) in
    return TypeInt
  | "*" ->
    let* _ = check_expr left (fst left, TypeInt) in
    let* _ = check_expr right (fst right, TypeInt) in
    return TypeInt
  | "/" ->
    let* _ = check_expr left (fst left, TypeInt) in
    let* _ = check_expr right (fst right, TypeInt) in
    return TypeInt
  | "%" ->
    let* _ = check_expr left (fst left, TypeInt) in
    let* _ = check_expr right (fst right, TypeInt) in
    return TypeInt
  | "++" ->
    let* _ = check_expr left (fst left, TypeString) in
    let* _ = check_expr right (fst right, TypeString) in
    return TypeString
  | "==" ->
    let* _ = check_expr left (fst left, TypeAny) in
    let* _ = check_expr right (fst right, TypeAny) in
    return TypeBool
  | "!=" ->
    let* _ = check_expr left (fst left, TypeAny) in
    let* _ = check_expr right (fst right, TypeAny) in
    return TypeBool
  | "<" ->
    let* _ = check_expr left (fst left, TypeInt) in
    let* _ = check_expr right (fst right, TypeInt) in
    return TypeBool
  | ">" ->
    let* _ = check_expr left (fst left, TypeInt) in
    let* _ = check_expr right (fst right, TypeInt) in
    return TypeBool
  | "<=" ->
    let* _ = check_expr left (fst left, TypeInt) in
    let* _ = check_expr right (fst right, TypeInt) in
    return TypeBool
  | ">=" ->
    let* _ = check_expr left (fst left, TypeInt) in
    let* _ = check_expr right (fst right, TypeInt) in
    return TypeBool
  | "|" ->
    let* _ = check_expr left (fst left, TypeBool) in
    let* _ = check_expr right (fst right, TypeBool) in
    return TypeBool
  | "&" ->
    let* _ = check_expr left (fst left, TypeBool) in
    let* _ = check_expr right (fst right, TypeBool) in
    return TypeBool
  | _ -> TypingErrors.raise_unexpected

and check_def def progress =
  let progress = start_progress progress def in
  match def.def_expr_type with
  | Some type' ->
    check type';
    let progress = end_progress progress def type' in
    let state = make_state progress in
    let (_, state) = check_expr def.def_expr type' state in
    (snd type', state.progress)
  | None ->
    let state = make_state progress in
    let (type', state) = infer def.def_expr state in
    let progress = end_progress state.progress def type' in
    (snd type', progress)

let rec progress_defs progress =
  match DefSet.choose_opt progress.remains with
  | None -> progress
  | Some def ->
    let (_, progress) = check_def def progress in
    progress_defs progress

let check_exprs defs =
  progress_defs (make_progress defs)

let check_types types =
  List.iter (fun type' -> check type') types
