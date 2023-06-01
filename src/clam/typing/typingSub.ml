open Utils
open Model
open TypingApply
open TypingContext

let rec is_type type' other =
  let type' = apply type' empty_context in
  let other = apply other empty_context in
  match (type', other) with
  | (TypeAny _, TypeAny _) -> true
  | (TypeVoid _, TypeVoid _) -> true
  | (TypeBool _, TypeBool _) -> true
  | (TypeInt _, TypeInt _) -> true
  | (TypeChar _, TypeChar _) -> true
  | (TypeString _, TypeString _) -> true
  | (TypeVar param, TypeVar other_param) ->
    param = other_param
  | (TypeTuple tuple, TypeTuple other_tuple) ->
    compare_lists is_type tuple.type_tuple_types other_tuple.type_tuple_types
  | (TypeRecord record, TypeRecord other_record) ->
    compare_maps is_type_attr record.type_record_attrs other_record.type_record_attrs
  | (TypeInter inter, TypeInter other_inter) ->
    is_type inter.type_inter_left other_inter.type_inter_left
    && is_type inter.type_inter_right other_inter.type_inter_right
  | (TypeUnion union, TypeUnion other_union) ->
    is_type union.type_union_left other_union.type_union_left
    || is_type union.type_union_right other_union.type_union_right
  | (TypeAbsExpr abs, TypeAbsExpr other_abs) ->
    compare_lists is_type abs.type_abs_expr_params other_abs.type_abs_expr_params
    && is_type abs.type_abs_expr_ret other_abs.type_abs_expr_ret
  | (TypeAbsExprType abs, TypeAbsExprType other_abs) ->
    compare_lists is_type_param abs.type_abs_expr_type_params other_abs.type_abs_expr_type_params
    && is_type abs.type_abs_expr_type_body other_abs.type_abs_expr_type_body
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
  match (type', other) with
  | (_, TypeAny _) -> true
  | (TypeVoid _, TypeVoid _) -> true
  | (TypeBool _, TypeBool _) -> true
  | (TypeInt _, TypeInt _) -> true
  | (TypeChar _, TypeChar _) -> true
  | (TypeString _, TypeString _) -> true
  | (TypeVar param, _) ->
    is_subtype_var param other
  | (TypeAbsExpr abs, TypeAbsExpr other_abs) ->
    compare_lists is_subtype other_abs.type_abs_expr_params abs.type_abs_expr_params
    && is_subtype abs.type_abs_expr_ret other_abs.type_abs_expr_ret
  | (TypeAbsExprType abs, TypeAbsExprType other_abs) ->
    compare_lists is_type_param other_abs.type_abs_expr_type_params abs.type_abs_expr_type_params
    && is_subtype abs.type_abs_expr_type_body other_abs.type_abs_expr_type_body
  | (TypeTuple tuple, TypeTuple other_tuple) ->
    compare_lists is_subtype tuple.type_tuple_types other_tuple.type_tuple_types
  | (TypeRecord record, TypeRecord other_record) ->
    NameMap.for_all (fun name other -> match NameMap.find_opt name record.type_record_attrs with
    | Some attr -> is_subtype attr.attr_type other.attr_type
    | None -> false
    ) other_record.type_record_attrs
  | (_, TypeInter inter) ->
    is_subtype type' inter.type_inter_left && is_subtype type' inter.type_inter_right
  | (TypeInter inter, _) ->
    is_subtype inter.type_inter_left other || is_subtype inter.type_inter_right other
  | (TypeUnion union, _) ->
    is_subtype union.type_union_left other && is_subtype union.type_union_right other
  | (_, TypeUnion union) ->
    is_subtype type' union.type_union_left || is_subtype type' union.type_union_right
  | (TypeAbs abs, TypeAbs other_abs) ->
    compare_lists is_type_param abs.type_abs_params other_abs.type_abs_params
      && is_subtype abs.type_abs_body other_abs.type_abs_body
  | _ -> false

and is_subtype_var var other =
  match other with
  | TypeVar other ->
    var.type_var_param = other.type_var_param || is_subtype var.type_var_param.param_type other.type_var_param.param_type
  | _ ->
    is_subtype var.type_var_param.param_type other
