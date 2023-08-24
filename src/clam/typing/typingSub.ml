open Model

module Reader = struct
  type r = TypingContext.context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec is_subtype (type': type') (other: type') =
  let type' = TypingBool.normalize type' in
  let other = TypingBool.normalize other in
  match (type', other) with
  | (_, TypeInter inter) ->
    let* left = is_subtype type' inter.left in
    let* right = is_subtype type' inter.right in
    return (left && right)
  | (TypeInter inter, _) ->
    let* left = is_subtype inter.left other in
    let* right = is_subtype inter.right other in
    return (left || right)
  | (TypeUnion union, _) ->
    let* left = is_subtype union.left other in
    let* right = is_subtype union.right other in
    return (left && right)
  | (_, TypeUnion union) ->
    let* left = is_subtype type' union.left in
    let* right = is_subtype type' union.right in
    return (left || right)
  | (_, TypeTop _) ->
    return true
  | (TypeBot _, _) ->
    return true
  | (TypeUnit _, TypeUnit _) ->
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
    compare_list2 is_subtype tuple.elems other_tuple.elems
  | (TypeRecord record, TypeRecord other_record) ->
    is_subtype_record record other_record
  | (TypeAbsExpr abs, TypeAbsExpr other_abs) ->
    let* params = compare_list2 is_subtype other_abs.params abs.params in
    let* body = is_subtype abs.body other_abs.body in
    return (params && body)
  | (TypeAbsExprType abs, TypeAbsExprType other_abs) ->
    let params = Utils.compare_lists TypingEqual.is_type_param abs.params other_abs.params in
    let* ret = is_subtype abs.body other_abs.body in
    return (params && ret)
  | (TypeAbs abs, TypeAbs other_abs) ->
    let params = Utils.compare_lists TypingEqual.is_type_param abs.params other_abs.params in
    let* body = is_subtype abs.body other_abs.body in
    return (params && body)
  | (TypeApp app, _) ->
    let app = TypingApply.apply_app app in
    is_subtype app other
  | (_, TypeApp app) ->
    let app = TypingApply.apply_app app in
    is_subtype type' app
  | _ ->
    return false

and is_subtype_var var other =
  let* arg = TypingContext.find_arg var.param in
  match arg with
  | Some arg -> is_subtype arg other
  | None ->
  match other with
  | TypeVar other_var ->
    return (var.param = other_var.param)
  | _ ->
    return false

and is_subtype_record record other context =
  Utils.NameMap.for_all (fun _ other -> is_subtype_attr other record context) other.attrs

and is_subtype_attr other record =
  match Utils.NameMap.find_opt other.name record.attrs with
    | Some attr ->
      is_subtype attr.type' other.type'
    | None ->
      return false
