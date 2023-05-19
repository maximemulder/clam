open Collection
open Model
open TypingApp

let rec is_subtype_of type' other =
  let type' = apply type' in
  let other = apply other in
  match (snd type', snd other) with
  | (_, TypeAny) -> true
  | (_, TypeInter (left, right)) ->
    is_subtype_of type' left && is_subtype_of type' right
  | (_, TypeUnion (left, right)) ->
    is_subtype_of type' left || is_subtype_of type' right
  | (TypeVoid, TypeVoid) -> true
  | (TypeBool, TypeBool) -> true
  | (TypeInt, TypeInt) -> true
  | (TypeChar, TypeChar) -> true
  | (TypeString, TypeString) -> true
  | (TypeVar param, _) ->
    (snd other) == (TypeVar param) || is_subtype_of param.param_type other
  | (TypeAbsExpr (params, expr), TypeAbsExpr (other_params, other_expr)) ->
    compare_lists is_subtype_of other_params params && is_subtype_of expr other_expr
  | (TypeAbsExprType (params, expr), TypeAbsExprType (other_params, other_expr)) ->
    compare_lists (=) other_params params && is_subtype_of expr other_expr
  | (TypeTuple (types), TypeTuple (other_types)) ->
    compare_lists is_subtype_of types other_types
  | (TypeRecord (attrs), TypeRecord (other_attrs)) ->
    NameMap.for_all (fun name other -> match NameMap.find_opt name attrs with
    | Some attr -> is_subtype_of attr.attr_type other.attr_type
    | None -> false
    ) other_attrs
  | (TypeInter (left, right), _) ->
    is_subtype_of left other || is_subtype_of right other
  | (TypeUnion (left, right), _) ->
    is_subtype_of left other && is_subtype_of right other
  | (TypeAbs (params, type'), TypeAbs (other_params, other_type)) ->
    compare_lists (=) other_params params && is_subtype_of type' other_type
  | _ -> false

let merge_union left right =
  if is_subtype_of left right then right else
  if is_subtype_of right left then left else
  (fst left, TypeUnion (left, right))

let merge_inter left right =
  if is_subtype_of left right then left else
  if is_subtype_of right left then right else
  (fst left, TypeInter (left, right))
