open Collection
open Display_type
open Typing_check

module DefKey = struct
  type t = Model.def_expr
  let compare x y = Stdlib.compare x.Model.def_expr_id y.Model.def_expr_id
end

module DefSet = Set.Make(DefKey)

module BindKey = struct
  type t = Model.bind_expr

  let compare x y  = Stdlib.compare (Model.bind_expr_id x) (Model.bind_expr_id y)
end

module BindMap = Map.Make(BindKey)

type state = {
  remains: DefSet.t;
  currents: DefSet.t;
  dones: Model.type' BindMap.t;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let make_state (defs: Model.def_expr list) =
  let remains = List.fold_left (fun set def -> DefSet.add def set) DefSet.empty defs in
  { remains; currents = DefSet.empty; dones = BindMap.empty }

let empty_context = { parent = None; binds = [] }

let check_type type' =
  let context = { parent = None; binds = [] } in
  let _ = check type' context in
  ()

let rec check_expr_with_constraint (expr: Model.expr) (constraint': Model.type') =
  let* type' = check_expr_without_constraint expr in
  let constraint' = Typing_check.check constraint' empty_context in
  return (Typing_check.check_subtype type' constraint')

and check_expr_without_constraint (expr: Model.expr) =
  match expr with
  | ExprVoid ->
    return Model.TypeVoid
  | ExprBool _ ->
    return Model.TypeBool
  | ExprInt _ ->
    return Model.TypeInt
  | ExprChar _ ->
    return Model.TypeChar
  | ExprString _ ->
    return Model.TypeString
  | ExprBind bind ->
    check_bind_without_constraint (Option.get bind.Model.bind_expr)
  | ExprTuple types ->
    let* types = map_list check_expr_without_constraint types in
    return (Model.TypeTuple types)
  | ExprRecord attrs ->
    let* attrs = check_attrs_without_constraint attrs in
    return (Model.TypeRecord attrs)
  | ExprPreop (_, expr) ->
    let* _ = check_expr_with_constraint expr Model.TypeInt in
    return Model.TypeInt
  | ExprBinop (left, _, right) ->
    let* _ = check_expr_with_constraint left Model.TypeInt in
    let* _ = check_expr_with_constraint right Model.TypeInt in
    return Model.TypeInt
  | ExprAscr (expr, type') ->
    let* _ = check_expr_with_constraint expr type' in
    return type'
  | ExprIf (if', then', else') ->
    let* _ = check_expr_with_constraint if' TypeBool in
    let* then' = check_expr_without_constraint then' in
    let* else' = check_expr_without_constraint else' in
    return (Model.TypeUnion (then', else'))
  | ExprBlock { block_expr } ->
    check_expr_without_constraint block_expr
  | ExprAbs (params, type', expr) ->
    let* params = check_abs_params params in
    let* type' = check_abs_return type' expr in
    return (Model.TypeAbsExpr (params, type'))
  | ExprApp (expr, args) ->
    check_app expr args
  | ExprTypeAbs (params, expr) ->
    let _ = List.iter (fun param -> check_type param.Model.type_param_type) params in
    let* type' = check_expr_without_constraint expr in
    return (Model.TypeAbsExprType (params, type'))
  | ExprTypeApp (expr, args) ->
    check_type_app expr args

and check_bind_without_constraint bind state =
  match bind with
  | BindExprDef def when DefSet.mem def state.remains ->
    check_def def state
  | BindExprDef def when DefSet.mem def state.currents ->
    Typing_errors.raise_recursive def
  | _ -> (BindMap.find bind state.dones, state)

and check_def def state =
  let remains = DefSet.remove def state.remains in
  let currents = DefSet.add def state.currents in
  let state = { state with remains; currents } in
  match def.def_expr_type with
  | Some type' ->
    let currents = DefSet.remove def state.currents in
    let dones = BindMap.add (Model.BindExprDef def) type' state.dones in
    let state = { state with currents; dones } in
    let (_, state) = check_expr_with_constraint def.Model.def_expr type' state in
    (type', state)
  | None ->
    let (type', state) = check_expr_without_constraint def.Model.def_expr state in
    let currents = DefSet.remove def state.currents in
    let dones = BindMap.add (Model.BindExprDef def) type' state.dones in
    let state = { state with currents; dones } in
    (type', state)

and check_attrs_without_constraint attrs =
  let attrs = List.fold_left (fun map attr -> NameMap.add attr.Model.attr_expr_name attr map) NameMap.empty attrs in
  let mapper = (fun attr ->
    let* type' = check_expr_without_constraint attr.Model.attr_expr in
    return (Model.make_attr_type attr.Model.attr_expr_name type')
  ) in
  map_map mapper attrs

and check_abs_params params state =
  let types = List.map (fun param -> match param.Model.param_expr_type with
  | Some type' -> type'
  | None -> Typing_errors.raise_param param
  ) params in
  let params = List.map (fun param -> Model.BindExprParam param) params in
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

and check_app expr args =
  let* type' = check_expr_without_constraint expr in
  match type' with
  | Model.TypeAbsExpr (params, type') ->
    let length_params = List.length params in
    let length_args = List.length args in
    if length_params == length_args
      then let pairs = List.combine params args in
      let mapper = (fun (param, arg) ->
        let* _ = check_expr_with_constraint arg param in
        return ()
      ) in
      let* _ = map_list mapper pairs in
      return type'
      else Typing_errors.raise_expr_app_arity length_params length_args
  | type' -> Typing_errors.raise_expr_app_kind type'

and check_type_app expr args =
  let* type' = check_expr_without_constraint expr in
  match type' with
  | Model.TypeAbsExprType (params, type') ->
    let length_params = List.length params in
    let length_args = List.length args in
    if length_params == length_args then
      let params = List.map (fun param -> param.Model.type_param_type) params in
      let pairs = List.combine params args in
      let mapper = (fun (param, arg) ->
        let arg = Typing_check.check arg empty_context in
        let _ = Typing_check.check_subtype arg param in
        return ()
      ) in
      let* _ = map_list mapper pairs in
      return type'
    else Typing_errors.raise_type_app_arity length_params length_args
  | type' -> Typing_errors.raise_type_app_kind type'

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
  List.iter (fun type' -> check_type type') types
