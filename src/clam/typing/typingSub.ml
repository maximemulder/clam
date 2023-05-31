open Utils
open Model
open TypingApply
open TypingContext

(* TODO: This function is not exact, notably with unions and intersections *)
let rec is_type type' other =
  let type' = apply type' empty_context in
  let other = apply other empty_context in
  match (snd type', snd other) with
  | (TypeAny, TypeAny) -> true
  | (TypeVoid, TypeVoid) -> true
  | (TypeBool, TypeBool) -> true
  | (TypeInt, TypeInt) -> true
  | (TypeChar, TypeChar) -> true
  | (TypeString, TypeString) -> true
  | (TypeVar param, TypeVar other_param) ->
    param = other_param
  | (TypeTuple types, TypeTuple other_types) ->
    compare_lists is_type types other_types
  | (TypeRecord attrs, TypeRecord other_attrs) ->
    compare_maps is_type_attr attrs other_attrs
  | (TypeInter (left, right), TypeInter (other_left, other_right)) ->
    is_type left other_left && is_type right other_right
  | (TypeUnion (left, right), TypeUnion (other_left, other_right)) ->
    is_type left other_left || is_type right other_right
  | (TypeAbsExpr abs, TypeAbsExpr other_abs) ->
    compare_lists is_type abs.type_abs_expr_params other_abs.type_abs_expr_params
    && is_type abs.type_abs_expr_ret other_abs.type_abs_expr_ret
  | (TypeAbsExprType (params, type'), TypeAbsExprType (other_params, other_type)) ->
    compare_lists is_type_param other_params params && is_type type' other_type
  | (TypeAbs abs, TypeAbs other_abs) ->
    compare_lists is_type_param abs.type_abs_params other_abs.type_abs_params
    && is_type abs.type_abs_body other_abs.type_abs_body
  | _ -> false

and is_type_param param other =
  is_type param.param_type other.param_type

and is_type_attr attr other =
  is_type attr.attr_type other.attr_type

let rec is_subtype type' other =
  let type' = apply type' empty_context in
  let other = apply other empty_context in
  match (snd type', snd other) with
  | (_, TypeAny) -> true
  | (TypeVoid, TypeVoid) -> true
  | (TypeBool, TypeBool) -> true
  | (TypeInt, TypeInt) -> true
  | (TypeChar, TypeChar) -> true
  | (TypeString, TypeString) -> true
  | (TypeVar param, _) ->
    is_subtype_var param other
  | (TypeAbsExpr abs, TypeAbsExpr other_abs) ->
    compare_lists is_subtype other_abs.type_abs_expr_params abs.type_abs_expr_params
    && is_subtype abs.type_abs_expr_ret other_abs.type_abs_expr_ret
  | (TypeAbsExprType (params, expr), TypeAbsExprType (other_params, other_expr)) ->
    compare_lists is_type_param other_params params && is_subtype expr other_expr
  | (TypeTuple (types), TypeTuple (other_types)) ->
    compare_lists is_subtype types other_types
  | (TypeRecord (attrs), TypeRecord (other_attrs)) ->
    NameMap.for_all (fun name other -> match NameMap.find_opt name attrs with
    | Some attr -> is_subtype attr.attr_type other.attr_type
    | None -> false
    ) other_attrs
  | (_, TypeInter (left, right)) ->
    is_subtype type' left && is_subtype type' right
  | (TypeInter (left, right), _) ->
    is_subtype left other || is_subtype right other
  | (TypeUnion (left, right), _) ->
    is_subtype left other && is_subtype right other
  | (_, TypeUnion (left, right)) ->
    is_subtype type' left || is_subtype type' right
  | (TypeAbs abs, TypeAbs other_abs) ->
    compare_lists is_type_param abs.type_abs_params other_abs.type_abs_params
      && is_subtype abs.type_abs_body other_abs.type_abs_body
  | _ -> false

and is_subtype_var var other =
  match snd other with
  | TypeVar other ->
    var.type_var_param = other.type_var_param || is_subtype var.type_var_param.param_type other.type_var_param.param_type
  | _ ->
    is_subtype var.type_var_param.param_type other
