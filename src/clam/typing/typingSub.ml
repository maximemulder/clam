open Model

module Reader = struct
  type r = TypingContext.context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec is_subtype type' other =
  match (type', other) with
  | (_, TypeAny _) ->
    return true
  | (TypeVoid _, TypeVoid _) ->
    return true
  | (TypeBool _, TypeBool _) ->
    return true
  | (TypeInt _, TypeInt _) ->
    return true
  | (TypeChar _, TypeChar _) ->
    return true
  | (TypeString _, TypeString _) ->
    return true
  | (TypeVar var, _) ->
    is_subtype_var var other
  | (TypeTuple tuple, TypeTuple other_tuple) ->
    compare_list2 is_subtype tuple.type_tuple_types other_tuple.type_tuple_types
  | (TypeRecord record, TypeRecord other_record) ->
    is_subtype_record record other_record
  | (_, TypeInter inter) ->
    let* left = is_subtype type' inter.type_inter_left in
    let* right = is_subtype type' inter.type_inter_right in
    return (left && right)
  | (TypeInter inter, _) ->
    let* left = is_subtype inter.type_inter_left other in
    let* right = is_subtype inter.type_inter_right other in
    return (left || right)
  | (TypeUnion union, _) ->
    let* left = is_subtype union.type_union_left other in
    let* right = is_subtype union.type_union_right other in
    return (left && right)
  | (_, TypeUnion union) ->
    let* left = is_subtype type' union.type_union_left in
    let* right = is_subtype type' union.type_union_right in
    return (left || right)
  | (TypeAbsExpr abs, TypeAbsExpr other_abs) ->
    let* params = compare_list2 is_subtype other_abs.type_abs_expr_params abs.type_abs_expr_params in
    let* ret = is_subtype abs.type_abs_expr_ret other_abs.type_abs_expr_ret in
    return (params && ret)
  | (TypeAbsExprType abs, TypeAbsExprType other_abs) ->
    let params = Utils.compare_lists TypingEqual.is_type_param abs.type_abs_expr_type_params other_abs.type_abs_expr_type_params in
    let* ret = is_subtype abs.type_abs_expr_type_body other_abs.type_abs_expr_type_body in
    return (params && ret)
  | (TypeAbs abs, TypeAbs other_abs) ->
    let params = Utils.compare_lists TypingEqual.is_type_param abs.type_abs_params other_abs.type_abs_params in
    let* body = is_subtype abs.type_abs_body other_abs.type_abs_body in
    return (params && body)
  | (TypeApp app, _) ->
    is_subtype (TypingApply.apply_app app) other
  | (_, TypeApp app) ->
    is_subtype type' (TypingApply.apply_app app)
  | _ ->
    return false

and is_subtype_var var other =
  let* arg = TypingContext.find_arg var.type_var_param in
  match arg with
  | Some arg -> is_subtype arg other
  | None ->
  match other with
  | TypeVar other_var ->
    return (var.type_var_param = other_var.type_var_param)
  | _ ->
    return false

and is_subtype_record record other context =
  Utils.NameMap.for_all (fun _ other -> is_subtype_attr other record context) other.type_record_attrs

and is_subtype_attr other record =
  match Utils.NameMap.find_opt other.attr_type_name record.type_record_attrs with
    | Some attr ->
      is_subtype attr.attr_type other.attr_type
    | None ->
      return false
