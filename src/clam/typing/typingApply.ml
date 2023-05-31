open Model
open TypingContext

module Reader = struct
  type r = context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec apply_type type' =
  let* type_data = apply_type_data type' in
  return (fst type', type_data)

and apply_type_data type' =
  match snd type' with
  | TypeAny ->
    return TypeAny
  | TypeVoid ->
    return TypeVoid
  | TypeBool ->
    return TypeBool
  | TypeInt ->
    return TypeInt
  | TypeChar ->
    return TypeChar
  | TypeString ->
    return TypeString
  | TypeVar var ->
    apply_var var
  | TypeAbsExpr abs ->
    let* params = map_list apply_type abs.type_abs_expr_params in
    let* ret = apply_type abs.type_abs_expr_ret in
    return (TypeAbsExpr {
      type_abs_expr_params = params;
      type_abs_expr_ret = ret;
    })
  | TypeAbsExprType (params, type') ->
    let* params = map_list apply_param params in
    let* type' = apply_type type' in
    return (TypeAbsExprType (params, type'))
  | TypeTuple types ->
    let* types = map_list apply_type types in
    return (TypeTuple types)
  | TypeRecord attrs ->
    let* attrs = map_map apply_attr attrs in
    return (TypeRecord attrs)
  | TypeInter (left, right) ->
    let* left = apply_type left in
    let* right = apply_type right in
    return (TypeInter (left, right))
  | TypeUnion (left, right) ->
    let* left = apply_type left in
    let* right = apply_type right in
    return (TypeUnion (left, right))
  | TypeAbs abs ->
    let* params = map_list apply_param abs.type_abs_params in
    let* body = apply_type abs.type_abs_body in
    return (TypeAbs {
      type_abs_params = params;
      type_abs_body = body;
    })
  | TypeApp app -> apply_app2 app

and apply_attr attr =
  let* type' = apply_type attr.attr_type in
  return { attr with attr_type = type' }

and apply_param param =
  let* type' = apply_type param.param_type in
  return { param with param_type = type' }

and apply_app2 app =
  let args = app.type_app_args in
  match snd app.type_app_type with
  | TypeAbs abs -> apply_app_abs abs.type_abs_body abs.type_abs_params args
  | TypeAbsExprType (params, type') -> apply_app_abs type' params args
  | _ -> TypingErrors.raise_unexpected ()

and apply_app_abs body params args context =
  let params = List.combine params args in
  let context = { parent = Some context; params } in
  snd (apply_type body context)

and apply_var var context =
  match List.find_opt (fun other -> (fst other) = var.type_var_param) context.params with
  | Some pair -> snd (apply_type (snd pair) context)
  | None ->
  match context.parent with
  | Some parent -> apply_var var parent
  | None -> TypeVar var

let apply type' context =
  apply_type type' context

let apply_app app context =
  (fst app.type_app_type, apply_app2 app context)
