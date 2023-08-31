open Model

module Reader = struct
  type r = TypingContext.context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec is_subtype (sub: type') (sup: type') =
  let sub = TypingSet.normalize sub in
  let sup = TypingSet.normalize sup in
  match (sub, sup) with
  | (           _, TypeTop    _) -> return true
  | (TypeBot    _,            _) -> return true
  | (TypeUnit   _, TypeUnit   _) -> return true
  | (TypeBool   _, TypeBool   _) -> return true
  | (TypeInt    _, TypeInt    _) -> return true
  | (TypeChar   _, TypeChar   _) -> return true
  | (TypeString _, TypeString _) -> return true
  | (_, TypeInter sup_inter) ->
    let* left = is_subtype sub sup_inter.left in
    let* right = is_subtype sub sup_inter.right in
    return (left || right)
  | (TypeInter sub_inter, _) ->
    let* left = is_subtype sub_inter.left sup in
    let* right = is_subtype sub_inter.right sup in
    return (left && right)
  | (TypeUnion sub_union, _) ->
    let* left = is_subtype sub_union.left sup in
    let* right = is_subtype sub_union.right sup in
    return (left && right)
  | (_, TypeUnion sup_union) ->
    let* left = is_subtype sub sup_union.left in
    let* right = is_subtype sub sup_union.right in
    return (left || right)
  | (TypeVar sub_var, _) ->
    is_subtype_var sub_var sup
  | (TypeTuple sub_tuple, TypeTuple sup_tuple) ->
    compare_list2 is_subtype sub_tuple.elems sup_tuple.elems
  | (TypeRecord sub_record, TypeRecord sup_record) ->
    is_subtype_record sub_record sup_record
  | (TypeAbsExpr sub_abs, TypeAbsExpr sup_abs) ->
    let* params = compare_list2 is_subtype sup_abs.params sub_abs.params in
    let* body = is_subtype sub_abs.body sup_abs.body in
    return (params && body)
  | (TypeAbsExprType sub_abs, TypeAbsExprType sup_abs) ->
    let params = Utils.compare_lists TypingEqual.is_type_param sub_abs.params sup_abs.params in
    let* ret = is_subtype sub_abs.body sup_abs.body in
    return (params && ret)
  | (TypeAbs sub_abs, TypeAbs sup_abs) ->
    let params = Utils.compare_lists TypingEqual.is_type_param sub_abs.params sup_abs.params in
    let* body = is_subtype sub_abs.body sup_abs.body in
    return (params && body)
  | (TypeApp sub_app, _) ->
    let sub = TypingApply.apply_app sub_app in
    is_subtype sub sup
  | (_, TypeApp sup_app) ->
    let sup = TypingApply.apply_app sup_app in
    is_subtype sub sup
  | _ ->
    return false

and is_subtype_var var sup =
  let* arg = TypingContext.find_arg var.param in
  match arg with
  | Some arg -> is_subtype arg sup
  | None ->
  match sup with
  | TypeVar sup_var ->
    return (var.param = sup_var.param)
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
