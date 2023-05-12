open Display_type

type context = {
  parent: context option;
  binds: (Model.type_param * Model.type') list;
}

module Context = struct
  type c = context
end

open Functor.Functor(Functor.ContextFunctor(Context))

let rec get_type param context =
  match List.find_opt (fun other -> (fst other) == param) context.binds with
  | Some pair -> snd pair
  | None -> get_type param (Option.get context.parent)

let rec is_subtype_of (type': Model.type') (other: Model.type') =
  if other == TypeAny then true else
  match type' with
  | TypeAny -> false
  | TypeVoid -> other == TypeVoid
  | TypeBool -> other == TypeBool
  | TypeInt -> other == TypeInt
  | TypeChar -> other == TypeChar
  | TypeString -> other == TypeString
  | TypeTuple types -> (match other with
    | TypeTuple others ->
      let length_types = List.length types in
      let length_others = List.length others in
      if length_types == length_others
        then
          let pairs = List.combine types others in
          List.for_all (fun (type', other) -> is_subtype_of type' other) pairs
        else false
    | _ -> false)
  | TypeRecord attrs -> (match other with
    | TypeRecord others ->
      let attrs = (List.sort (fun a b -> String.compare a.Model.attr_type_name b.Model.attr_type_name) attrs) in
      let others = (List.sort (fun a b -> String.compare a.Model.attr_type_name b.Model.attr_type_name) others) in
      let pairs = List.combine attrs others in
      List.for_all (fun (type', other) -> is_subtype_of type'.Model.attr_type other.Model.attr_type) pairs
    | _ -> false)
  | _ -> false

let require_subtype (type': Model.type') (constraint': Model.type') =
  if Bool.not (is_subtype_of type' constraint')
    then Typecheck_errors.raise_type type' constraint'
    else ()

let rec promote (type': Model.type') =
  match type' with
  | TypeVar param -> get_type param
  | TypeAbsExpr (params, expr) ->
    let+ params = map_list promote params in
    let+ expr = promote expr in
    return (Model.TypeAbsExpr (params, expr))
  | TypeTuple types ->
    let+ types = map_list promote types in
    return (Model.TypeTuple types)
  | TypeRecord attrs ->
    let attrs = (List.sort (fun a b -> String.compare a.Model.attr_type_name b.Model.attr_type_name) attrs) in
    let+ attrs = map_list promote_attr attrs in
    return (Model.TypeRecord attrs)
  | TypeInter (left, right) ->
    let+ left = promote left in
    let+ right = promote right in
    return (Model.TypeInter (left, right))
  | TypeUnion (left, right) ->
    let+ left = promote left in
    let+ right = promote right in
    return (Model.TypeUnion (left, right))
  | TypeAbs (params, type') ->
    let+ params = map_list promote_param params in
    let+ type' =  promote type' in
    return (Model.TypeAbs (params, type'))
  | TypeApp (type', args) -> promote_app type' args
  | _ -> return type'

and promote_app type' args context =
  match type' with
  | Model.TypeAbs (params, type') ->
    let length_params = List.length params in
    let length_args = List.length args in
    if length_params == length_args
      then let binds = List.combine params args in
      let _ = List.iter (fun (param, arg) -> require_subtype arg param.Model.type_param_type) binds in
      promote type' { parent = Some context; binds }
      else Typecheck_errors.raise_app_arity length_params length_args
  | _ -> Typecheck_errors.raise_app_kind type'

and promote_attr (attr: Model.attr_type) =
  let+ type' = promote attr.Model.attr_type in
  return { attr with Model.attr_type = type' }

and promote_param (param: Model.type_param) =
  let+ type' = promote param.Model.type_param_type in
  return { param with Model.type_param_type = type' }

let require_constraint constraint' type' =
  match constraint' with
  | Some constraint' ->
    let+ constraint' = promote constraint' in
    let+ type' = promote type' in
    return (require_subtype type' constraint')
  | None -> return ()

let rec get_expr_type expr =
  match expr with
  | Model.ExprVoid         -> Model.TypeVoid
  | Model.ExprBool   _     -> Model.TypeBool
  | Model.ExprInt    _     -> Model.TypeInt
  | Model.ExprChar   _     -> Model.TypeChar
  | Model.ExprString _     -> Model.TypeString
  | Model.ExprTuple exprs  -> Model.TypeTuple (List.map get_expr_type exprs)
  | Model.ExprRecord attrs -> Model.TypeRecord (List.map
    (fun attr -> { Model.attr_type_name = attr.Model.attr_expr_name; Model.attr_type = get_expr_type attr.Model.attr_expr })
    (List.sort (fun a b -> String.compare a.Model.attr_expr_name b.Model.attr_expr_name) attrs)
  )
  | _ -> Model.TypeAny

let check_expr expr constraint' =
  require_constraint constraint' (get_expr_type expr)

let check exprs =
  Modelize_state.NameMap.iter (fun _ done' -> check_expr done'.Modelize_exprs.done_expr done'.Modelize_exprs.done_type { parent = None; binds = [] }) exprs
