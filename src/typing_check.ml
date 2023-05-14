open Typing_sub

type context = {
  parent: context option;
  binds: (Model.type_param * Model.type') list;
}

module Reader = struct
  type r = context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let check_subtype (type': Model.type') (constraint': Model.type') =
  if Bool.not (is_subtype_of type' constraint')
    then Typing_errors.raise_type type' constraint'
    else ()

let rec check (type': Model.type') =
  match type' with
  | TypeAny    -> return Model.TypeAny
  | TypeVoid   -> return Model.TypeVoid
  | TypeBool   -> return Model.TypeBool
  | TypeInt    -> return Model.TypeInt
  | TypeChar   -> return Model.TypeChar
  | TypeString -> return Model.TypeString
  | TypeVar param -> check_var param
  | TypeAbsExpr (params, expr) ->
    let* params = map_list check params in
    let* expr = check expr in
    return (Model.TypeAbsExpr (params, expr))
  | TypeAbsExprType (params, expr) ->
    let* params = map_list check_param params in
    let* expr =  check expr in
    return (Model.TypeAbsExprType (params, expr))
  | TypeTuple types ->
    let* types = map_list check types in
    return (Model.TypeTuple types)
  | TypeRecord attrs ->
    let* attrs = map_map check_attr attrs in
    return (Model.TypeRecord attrs)
  | TypeInter (left, right) ->
    let* left = check left in
    let* right = check right in
    return (Model.TypeInter (left, right))
  | TypeUnion (left, right) ->
    let* left = check left in
    let* right = check right in
    return (Model.TypeUnion (left, right))
  | TypeAbs (params, type') ->
    let* params = map_list check_param params in
    let* type' =  check type' in
    return (Model.TypeAbs (params, type'))
  | TypeApp (type', args) -> check_app type' args

and check_var param context =
  match List.find_opt (fun other -> (fst other) == param) context.binds with
  | Some pair -> snd pair
  | None ->
  match context.parent with
  | Some parent -> check_var param parent
  | None -> TypeVar param

and check_attr (attr: Model.attr_type) =
  let* type' = check attr.Model.attr_type in
  return { attr with Model.attr_type = type' }

and check_param (param: Model.type_param) =
  let* type' = check param.Model.type_param_type in
  return { param with Model.type_param_type = type' }

and check_app type' args context =
  match type' with
  | Model.TypeAbs (params, type') ->
    let length_params = List.length params in
    let length_args = List.length args in
    if length_params == length_args
      then let binds = List.combine params args in
      let _ = List.iter (fun (param, arg) -> check_subtype arg param.Model.type_param_type) binds in
      check type' { parent = Some context; binds }
      else Typing_errors.raise_app_arity length_params length_args
  | _ -> Typing_errors.raise_app_kind type'
