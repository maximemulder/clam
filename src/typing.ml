open Collection
open Display_type
open Typing_check

open Monad.Monad(Monad.ReaderMonad(Typing_check.Reader))

let check_type type' =
  let context = { parent = None; binds = [] } in
  let _ = check type' context in
  ()

let rec check_expr_with_constraint (expr: Model.expr) (constraint': Model.type') =
  let* type' = check_expr_without_constraint expr in
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
  | ExprBind name ->
    check_bind_without_constraint name
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
    let params = check_abs_params params in
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

and check_bind_without_constraint _name _context =
  Model.TypeAny

and check_attrs_without_constraint attrs =
  let attrs = List.fold_left (fun map attr -> NameMap.add attr.Model.attr_expr_name attr map) NameMap.empty attrs in
  let mapper = (fun attr ->
    let* type' = check_expr_without_constraint attr.Model.attr_expr in
    return (Model.make_attr_type attr.Model.attr_expr_name type')
  ) in
  map_map mapper attrs

and check_abs_params params =
  List.map (fun param -> match param.Model.param_expr_type with
  | Some type' -> type'
  | None -> Typing_errors.raise_param param
  ) params

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
        let _ = check_subtype arg param in
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

let check_exprs defs =
  List.iter (fun def -> check_expr def.Model.def_expr def.Model.def_expr_type { parent = None; binds = [] }) defs

let check_types types =
  List.iter (fun type' -> check_type type') types
