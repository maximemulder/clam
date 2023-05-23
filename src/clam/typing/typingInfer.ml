open Utils
open Model
open TypingCheck
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

type state = {
  remains: DefSet.t;
  currents: DefSet.t;
  dones: type' BindMap.t;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let make_state defs =
  let remains = List.fold_left (fun set def -> DefSet.add def set) DefSet.empty defs in
  { remains; currents = DefSet.empty; dones = BindMap.empty }

let rec check_expr_with_constraint expr constraint' =
  let* type' = check_expr expr in
  if Bool.not (is_subtype type' constraint')
    then TypingErrors.raise_expr_constraint expr type' constraint'
    else return ()

and check_expr expr =
  let* type_data = check_expr_data expr in
  return (fst expr, type_data)

and check_expr_data expr =
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
    check_expr_bind expr (Option.get bind.bind_expr)
  | ExprTuple types ->
    let* types = list_map check_expr types in
    return (TypeTuple types)
  | ExprRecord attrs ->
    let* attrs = check_attrs_without_constraint attrs in
    return (TypeRecord attrs)
  | ExprVariant (expr, index) ->
    check_expr_variant expr index
  | ExprAttr (expr, attr) ->
    check_expr_attr expr attr
  | ExprPreop (op, expr) ->
    check_expr_preop op expr
  | ExprBinop (left, op, right) ->
    check_expr_binop left op right
  | ExprAscr (expr, type') ->
    let* _ = check_expr_with_constraint expr type' in
    return (snd type')
  | ExprIf (if', then', else') ->
    let* _ = check_expr_with_constraint if' (fst expr, TypeBool) in
    let* then' = check_expr then' in
    let* else' = check_expr else' in
    return (TypeUnion (then', else'))
  | ExprBlock block ->
    check_expr_block block
  | ExprAbs (params, type', expr) ->
    let* params = check_abs_params params in
    let* type' = check_abs_return type' expr in
    return (TypeAbsExpr (params, type'))
  | ExprApp (expr, args) ->
    check_expr_app expr args
  | ExprTypeAbs (params, expr) ->
    let _ = List.iter (fun param -> check param.param_type) params in
    let* type' = check_expr expr in
    return (TypeAbsExprType (params, type'))
  | ExprTypeApp (expr, args) ->
    check_type_app expr args

and check_expr_variant expr index =
  let* type' = check_expr expr in
  match snd type' with
  | TypeTuple types ->
    (match List.nth_opt types index with
    | Some type' -> return (snd type')
    | None -> TypingErrors.raise_expr_tuple_index expr type' index)
  | _ -> TypingErrors.raise_expr_tuple_kind expr type'

and check_expr_attr expr attr =
  let* type' = check_expr expr in
  match snd type' with
  | TypeRecord attrs ->
    (match NameMap.find_opt attr attrs with
    | Some attr -> return (snd attr.attr_type)
    | None -> TypingErrors.raise_expr_record_attr expr type' attr)
  | _ -> TypingErrors.raise_expr_record_kind expr type'

and check_expr_preop op expr =
  match op with
  | "+" ->
    let* _ = check_expr_with_constraint expr (fst expr, TypeInt) in
    return TypeInt
  | "-" ->
    let* _ = check_expr_with_constraint expr (fst expr, TypeInt) in
    return TypeInt
  | "!" ->
    let* _ = check_expr_with_constraint expr (fst expr, TypeBool) in
    return TypeBool
  | _ -> TypingErrors.raise_unexpected

and check_expr_binop left op right =
  match op with
  | "+" ->
    let* _ = check_expr_with_constraint left (fst left, TypeInt) in
    let* _ = check_expr_with_constraint right (fst right, TypeInt) in
    return TypeInt
  | "-" ->
    let* _ = check_expr_with_constraint left (fst left, TypeInt) in
    let* _ = check_expr_with_constraint right (fst right, TypeInt) in
    return TypeInt
  | "*" ->
    let* _ = check_expr_with_constraint left (fst left, TypeInt) in
    let* _ = check_expr_with_constraint right (fst right, TypeInt) in
    return TypeInt
  | "/" ->
    let* _ = check_expr_with_constraint left (fst left, TypeInt) in
    let* _ = check_expr_with_constraint right (fst right, TypeInt) in
    return TypeInt
  | "%" ->
    let* _ = check_expr_with_constraint left (fst left, TypeInt) in
    let* _ = check_expr_with_constraint right (fst right, TypeInt) in
    return TypeInt
  | "++" ->
    let* _ = check_expr_with_constraint left (fst left, TypeString) in
    let* _ = check_expr_with_constraint right (fst right, TypeString) in
    return TypeString
  | "==" ->
    let* _ = check_expr_with_constraint left (fst left, TypeAny) in
    let* _ = check_expr_with_constraint right (fst right, TypeAny) in
    return TypeBool
  | "!=" ->
    let* _ = check_expr_with_constraint left (fst left, TypeAny) in
    let* _ = check_expr_with_constraint right (fst right, TypeAny) in
    return TypeBool
  | "<" ->
    let* _ = check_expr_with_constraint left (fst left, TypeInt) in
    let* _ = check_expr_with_constraint right (fst right, TypeInt) in
    return TypeBool
  | ">" ->
    let* _ = check_expr_with_constraint left (fst left, TypeInt) in
    let* _ = check_expr_with_constraint right (fst right, TypeInt) in
    return TypeBool
  | "<=" ->
    let* _ = check_expr_with_constraint left (fst left, TypeInt) in
    let* _ = check_expr_with_constraint right (fst right, TypeInt) in
    return TypeBool
  | ">=" ->
    let* _ = check_expr_with_constraint left (fst left, TypeInt) in
    let* _ = check_expr_with_constraint right (fst right, TypeInt) in
    return TypeBool
  | "|" ->
    let* _ = check_expr_with_constraint left (fst left, TypeBool) in
    let* _ = check_expr_with_constraint right (fst right, TypeBool) in
    return TypeBool
  | "&" ->
    let* _ = check_expr_with_constraint left (fst left, TypeBool) in
    let* _ = check_expr_with_constraint right (fst right, TypeBool) in
    return TypeBool
  | _ -> TypingErrors.raise_unexpected

and check_expr_bind expr bind state =
  match bind with
  | BindExprPrint ->
    (TypeAbsExpr ([(fst expr, TypeAny)], (fst expr, TypeVoid)), state)
  | BindExprDef def when DefSet.mem def state.remains ->
    check_def def state
  | BindExprDef def when DefSet.mem def state.currents ->
    TypingErrors.raise_expr_recursive def
  | _ -> (snd (BindMap.find bind state.dones), state)

and check_expr_block block =
  let* _ = list_map check_stmt block.block_stmts in
  match block.block_expr with
  | Some expr -> check_expr_data expr
  | None -> return TypeVoid

and check_stmt stmt state =
  match stmt with
  | StmtVar (var, type', expr) ->
    let (type', state) = match type' with
    | Some type' ->
      let (_, state) = check_expr_with_constraint expr type' state in
      (type', state)
    | None ->
      check_expr expr state
    in
    let dones = BindMap.add (BindExprVar var) type' state.dones in
    ((), { state with dones })
  | StmtExpr expr ->
    let _ = check_expr expr state in
    ((), state)

and check_def def state =
  let remains = DefSet.remove def state.remains in
  let currents = DefSet.add def state.currents in
  let state = { state with remains; currents } in
  match def.def_expr_type with
  | Some type' ->
    check type';
    let currents = DefSet.remove def state.currents in
    let dones = BindMap.add (BindExprDef def) type' state.dones in
    let state = { state with currents; dones } in
    let (_, state) = check_expr_with_constraint def.def_expr type' state in
    (snd type', state)
  | None ->
    let (type', state) = check_expr def.def_expr state in
    let currents = DefSet.remove def state.currents in
    let dones = BindMap.add (BindExprDef def) type' state.dones in
    let state = { state with currents; dones } in
    (snd type', state)

and check_attrs_without_constraint attrs =
  let attrs = List.fold_left (fun map attr -> NameMap.add attr.attr_expr_name attr map) NameMap.empty attrs in
  let mapper = (fun attr ->
    let* type' = check_expr attr.attr_expr in
    return (make_attr_type (fst attr.attr_expr) attr.attr_expr_name type')
  ) in
  map_map mapper attrs

and check_abs_params params state =
  let types = List.map (fun param -> match param.param_expr_type with
  | Some type' -> type'
  | None -> TypingErrors.raise_param param
  ) params in
  let params = List.map (fun param -> BindExprParam param) params in
  let pairs = List.combine params types in
  let dones = List.fold_left (fun dones pair -> BindMap.add (fst pair) (snd pair) dones) state.dones pairs in
  let state = { state with dones } in
  (types, state)

and check_abs_return type' expr =
  match type' with
  | Some type' ->
    let* _ = check_expr_with_constraint expr type' in
    return type'
  | None -> check_expr expr

and check_expr_app expr args =
  let* type' = check_expr expr in
  match snd type' with
  | TypeAbsExpr (params, type') ->
    check_expr_app_abs expr type' params args
  | _ -> TypingErrors.raise_expr_app_kind expr type'

and check_expr_app_abs expr type' params args =
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_app_arity expr params args
  else
  let pairs = List.combine params args in
  let* _ = list_map (fun (param, arg) -> check_expr_with_constraint arg param) pairs in
  return (snd type')

and check_type_app expr args =
  let* type' = check_expr expr in
  match snd type' with
  | TypeAbsExprType (params, type') ->
    check_type_app_abs expr type' params args
  | _ -> TypingErrors.raise_expr_type_app_kind expr type'

and check_type_app_abs expr type' params args =
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_type_app_arity expr params args
  else
  let params = List.map (fun param -> param.param_type) params in
  let pairs = List.combine params args in
  let mapper = (fun (param, arg) ->
    let _ = check arg in
    let _ = check_subtype arg param in
    return ()
  ) in
  let* _ = list_map mapper pairs in
  return (snd type')

let check_expr expr constraint' =
  match constraint' with
  | Some constraint' -> check_expr_with_constraint expr constraint'
  | None             -> let* _ = check_expr expr in return ()

let rec check_exprs_state state =
  match DefSet.choose_opt state.remains with
  | None -> state
  | Some def ->
    let (_, state) = check_def def state in
    check_exprs_state state

let check_exprs defs =
  check_exprs_state (make_state defs)

let check_types types =
  List.iter (fun type' -> check type') types
