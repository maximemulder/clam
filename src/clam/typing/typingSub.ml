open Model

module Reader = struct
  type r = TypingContext.context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec isa (sub: type') (sup: type') =
  let sub = Typing.normalize sub in
  let sup = Typing.normalize sup in
  match (sub, sup) with
  | (           _, TypeTop    _) -> return true
  | (TypeBot    _,            _) -> return true
  | (TypeUnit   _, TypeUnit   _) -> return true
  | (TypeBool   _, TypeBool   _) -> return true
  | (TypeInt    _, TypeInt    _) -> return true
  | (TypeChar   _, TypeChar   _) -> return true
  | (TypeString _, TypeString _) -> return true
  | (TypeUnion sub_union, _) ->
    let* left = isa sub_union.left sup in
    let* right = isa sub_union.right sup in
    return (left && right)
  | (_, TypeUnion sup_union) ->
    let* left = isa sub sup_union.left in
    let* right = isa sub sup_union.right in
    return (left || right)
  | (_, TypeInter sup_inter) ->
    let* left = isa sub sup_inter.left in
    let* right = isa sub sup_inter.right in
    return (left && right)
  | (TypeInter sub_inter, _) ->
    let* left = isa sub_inter.left sup in
    let* right = isa sub_inter.right sup in
    return (left || right)
  | (TypeVar sub_var, _) ->
    isa_var sub_var sup
  | (TypeTuple sub_tuple, TypeTuple sup_tuple) ->
    isa_tuple sub_tuple sup_tuple
  | (TypeRecord sub_record, TypeRecord sup_record) ->
    isa_record sub_record sup_record
  | (TypeAbsExpr sub_abs, TypeAbsExpr sup_abs) ->
    let* params = compare_list2 isa sup_abs.params sub_abs.params in
    let* body = isa sub_abs.body sup_abs.body in
    return (params && body)
  | (TypeAbsExprType sub_abs, TypeAbsExprType sup_abs) ->
    let params = Utils.compare_lists Typing.is_param sub_abs.params sup_abs.params in
    let* ret = isa sub_abs.body sup_abs.body in
    return (params && ret)
  | (TypeAbs sub_abs, TypeAbs sup_abs) ->
    let params = Utils.compare_lists Typing.is_param sub_abs.params sup_abs.params in
    let* body = isa sub_abs.body sup_abs.body in
    return (params && body)
  | (TypeApp sub_app, _) ->
    let sub = TypingApply.apply_app sub_app in
    isa sub sup
  | (_, TypeApp sup_app) ->
    let sup = TypingApply.apply_app sup_app in
    isa sub sup
  | _ ->
    return false

and isa_var sub_var sup =
  let* sub_type = TypingContext.find_arg sub_var.param in
  match sub_type with
  | Some sub_type -> isa sub_type sup
  | None ->
  if Typing.var_is_bot sub_var then
    return true
  else
  match sup with
  | TypeVar sup_var ->
    return (sub_var.param = sup_var.param)
  | _ ->
    isa sub_var.param.type' sup

and isa_tuple sub_tuple sup_tuple =
  compare_list2 isa sub_tuple.elems sup_tuple.elems

and isa_record sub_record sup_record context =
  Utils.NameMap.for_all (fun _ sup_attr -> isa_record_attr sub_record sup_attr context) sup_record.attrs

and isa_record_attr sub_record sup_attr =
  match Utils.NameMap.find_opt sup_attr.name sub_record.attrs with
    | Some sub_attr ->
      isa sub_attr.type' sup_attr.type'
    | None ->
      return false
