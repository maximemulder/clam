open Model

type context = {
  parent: context option;
  binds: (param_type * type') list;
}

module Reader = struct
  type r = context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec apply_type type' =
  match type' with
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
  | TypeVar param ->
    apply_var param
  | TypeAbsExpr (params, type') ->
    let* params = list_map apply_type params in
    let* type' = apply_type type' in
    return (TypeAbsExpr (params, type'))
  | TypeAbsExprType (params, type') ->
    let* params = list_map apply_param params in
    let* type' = apply_type type' in
    return (TypeAbsExprType (params, type'))
  | TypeTuple types ->
    let* types = list_map apply_type types in
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
  | TypeAbs (params, type') ->
    let* params = list_map apply_param params in
    let* type' = apply_type type' in
    return (TypeAbs (params, type'))
  | TypeApp (type', args) -> apply_app_type type' args

and apply_attr attr =
  let* type' = apply_type attr.attr_type in
  return { attr with attr_type = type' }

and apply_param param =
  let* type' = apply_type param.param_type in
  return { param with param_type = type' }

and apply_app_type type' args =
  match type' with
  | TypeAbs (params, type') -> apply_app_abs type' params args
  | _ -> TypingErrors.raise_unexpected ()

and apply_app_abs type' params args context =
  let binds = List.combine params args in
  let context = { parent = Some context; binds } in
  apply_type type' context

and apply_var param context =
  match List.find_opt (fun other -> (fst other) == param) context.binds with
  | Some pair -> snd pair
  | None ->
  match context.parent with
  | Some parent -> apply_var param parent
  | None -> TypeVar param

let apply type' =
  apply_type type' { parent = None; binds = [] }
