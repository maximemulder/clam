open Collection

let rec is_subtype_of (type': Model.type') (other: Model.type') =
  match (type', other) with
  | (_, TypeAny) -> true
  | (type', TypeInter (left, right)) ->
    is_subtype_of type' left && is_subtype_of type' right
  | (type', TypeUnion (left, right)) ->
    is_subtype_of type' left || is_subtype_of type' right
  | (TypeVoid, TypeVoid) -> true
  | (TypeBool, TypeBool) -> true
  | (TypeInt, TypeInt) -> true
  | (TypeChar, TypeChar) -> true
  | (TypeString, TypeString) -> true
  | (TypeVar param, other) ->
    other == (TypeVar param) || is_subtype_of param.Model.type_param_type other
  | (TypeAbsExpr (params, expr), TypeAbsExpr (other_params, other_expr)) ->
    compare_lists is_subtype_of other_params params && is_subtype_of expr other_expr
  | (TypeAbsExprType (params, expr), TypeAbsExprType (other_params, other_expr)) ->
    compare_lists (==) other_params params && is_subtype_of expr other_expr
  | (TypeTuple (types), TypeTuple (other_types)) ->
    compare_lists is_subtype_of types other_types
  | (TypeRecord (attrs), TypeRecord (other_attrs)) ->
    NameMap.for_all (fun name other -> match NameMap.find_opt name attrs with
    | Some attr -> is_subtype_of attr.Model.attr_type other.Model.attr_type
    | None -> false
    ) other_attrs
  | (TypeInter (left, right), other) ->
    is_subtype_of left other || is_subtype_of right other
  | (TypeUnion (left, right), other) ->
    is_subtype_of left other && is_subtype_of right other
  | (TypeAbs (params, type'), TypeAbs (other_params, other_type)) ->
    compare_lists (==) other_params params && is_subtype_of type' other_type
  | (TypeApp (type', args), TypeApp (other_type, other_args)) ->
    is_subtype_of type' other_type && compare_lists (==) args other_args
  | _ -> false
