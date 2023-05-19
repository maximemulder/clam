open Collection
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
  let* type' = check_expr_without_constraint expr in
  if Bool.not (is_subtype_of type' constraint')
    then TypingErrors.raise_expr_constraint expr type' constraint'
    else return ()

and check_expr_without_constraint2 expr =
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
    check_bind_without_constraint (Option.get bind.bind_expr)
  | ExprTuple types ->
    let* types = list_map check_expr_without_constraint types in
    return (TypeTuple types)
  | ExprRecord attrs ->
    let* attrs = check_attrs_without_constraint attrs in
    return (TypeRecord attrs)
  | ExprPreop (_, expr) ->
    let* _ = check_expr_with_constraint expr (fst expr, TypeInt) in
    return TypeInt
  | ExprBinop (left, _, right) ->
    let* _ = check_expr_with_constraint left (fst expr, TypeInt) in
    let* _ = check_expr_with_constraint right (fst expr, TypeInt) in
    return TypeInt
  | ExprAscr (expr, type') ->
    let* _ = check_expr_with_constraint expr type' in
    return (snd type')
  | ExprIf (if', then', else') ->
    let* _ = check_expr_with_constraint if' (fst expr, TypeBool) in
    let* then' = check_expr_without_constraint then' in
    let* else' = check_expr_without_constraint else' in
    return (TypeUnion (then', else'))
  | ExprBlock { block_expr } ->
    check_expr_without_constraint2 block_expr
  | ExprAbs (params, type', expr) ->
    let* params = check_abs_params params in
    let* type' = check_abs_return type' expr in
    return (TypeAbsExpr (params, type'))
  | ExprApp (expr, args) ->
    check_expr_app expr args
  | ExprTypeAbs (params, expr) ->
    let _ = List.iter (fun param -> check param.param_type) params in
    let* type' = check_expr_without_constraint expr in
    return (TypeAbsExprType (params, type'))
  | ExprTypeApp (expr, args) ->
    check_type_app expr args

and check_expr_without_constraint expr =
  let* type_data = check_expr_without_constraint2 expr in
  return (fst expr, type_data)

and check_bind_without_constraint bind state =
  match bind with
  | BindExprDef def when DefSet.mem def state.remains ->
    check_def def state
  | BindExprDef def when DefSet.mem def state.currents ->
    TypingErrors.raise_recursive def
  | _ -> (snd (BindMap.find bind state.dones), state)

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
    let (type', state) = check_expr_without_constraint def.def_expr state in
    let currents = DefSet.remove def state.currents in
    let dones = BindMap.add (BindExprDef def) type' state.dones in
    let state = { state with currents; dones } in
    (snd type', state)

and check_attrs_without_constraint attrs =
  let attrs = List.fold_left (fun map attr -> NameMap.add attr.attr_expr_name attr map) NameMap.empty attrs in
  let mapper = (fun attr ->
    let* type' = check_expr_without_constraint attr.attr_expr in
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
  | None -> check_expr_without_constraint expr

and check_expr_app expr args =
  let* type' = check_expr_without_constraint expr in
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
  let* type' = check_expr_without_constraint expr in
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
  | None             -> let* _ = check_expr_without_constraint expr in return ()

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
