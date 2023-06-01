open Model

let rec is_type type' other =
  match (type', other) with
  | (TypeAny _, TypeAny _) ->
    true
  | (TypeVoid _, TypeVoid _) ->
    true
  | (TypeBool _, TypeBool _) ->
    true
  | (TypeInt _, TypeInt _) ->
    true
  | (TypeChar _, TypeChar _) ->
    true
  | (TypeString _, TypeString _) ->
    true
  | (TypeVar var, TypeVar other_var) ->
    var.type_var_param = other_var.type_var_param
  | (TypeTuple tuple, TypeTuple other_tuple) ->
    Utils.compare_lists is_type tuple.type_tuple_types other_tuple.type_tuple_types
  | (TypeRecord record, TypeRecord other_record) ->
    Utils.compare_maps is_type_attr record.type_record_attrs other_record.type_record_attrs
  | (TypeInter inter, TypeInter other_inter) ->
    is_type inter.type_inter_left other_inter.type_inter_left
    && is_type inter.type_inter_right other_inter.type_inter_right
  | (TypeUnion union, TypeUnion other_union) ->
    is_type union.type_union_left other_union.type_union_left
    && is_type union.type_union_right other_union.type_union_right
  | (TypeAbsExpr abs, TypeAbsExpr other_abs) ->
    Utils.compare_lists is_type abs.type_abs_expr_params other_abs.type_abs_expr_params
    && is_type abs.type_abs_expr_ret other_abs.type_abs_expr_ret
  | (TypeAbsExprType abs, TypeAbsExprType other_abs) ->
    Utils.compare_lists is_type_param abs.type_abs_expr_type_params other_abs.type_abs_expr_type_params
    && is_type abs.type_abs_expr_type_body other_abs.type_abs_expr_type_body
  | (TypeAbs abs, TypeAbs other_abs) ->
    Utils.compare_lists is_type_param abs.type_abs_params other_abs.type_abs_params
    && is_type abs.type_abs_body other_abs.type_abs_body
  | (TypeApp app, _) ->
    is_type (TypingApply.apply_app app) other
  | (_, TypeApp app) ->
    is_type type' (TypingApply.apply_app app)
  | _ -> false

and is_type_param param other =
  is_type param.param_type other.param_type

and is_type_attr attr other =
  is_type attr.attr_type other.attr_type
