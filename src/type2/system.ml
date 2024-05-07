open Context
open Context.Monad
open Node
open Kind
open Rename
open Split

let rec is left right =
  let* sub = isa left right in
  let* sup = isa right left in
  return (sub && sup)

and is_param (left: param) (right: param) =
  let* sub = is left.lower right.lower in
  let* sup = is left.upper right.upper in
  return (sub && sup)

and isa sub sup =
  match split_inter sup with
  | Some (left, right) ->
    let* left  = isa sub left  in
    let* right = isa sub right in
    return (left && right)
  | None ->
  match split_inter sub with
  | Some (left, right) ->
    let* sub = merge left right in (
    match sub with
    | Some sub ->
      isa sub sup
    | None ->
      let* left  = isa left  sup in
      let* right = isa right sup in
      return (left || right))
  | None ->
  match split_union sub with
  | Some (left, right) ->
    let* left  = isa left  sup in
    let* right = isa right sup in
    return (left && right)
  | None ->
  match split_union sup with
  | Some (left, right) ->
    let* left  = isa sub left  in
    let* right = isa sub right in
    return (left || right)
  | None ->
  match sub, sup with
  | sub, Top ->
    is_proper sub
  | Bot, sup ->
    is_proper sup
  | Unit, Unit | Bool, Bool | Int, Int | String, String ->
    return true
  | Tuple sub, Tuple sup ->
    isa_tuple sub sup
  | Record sub, Record sup ->
    isa_record sub sup
  | Lam sub, Lam sup ->
    isa_lam sub sup
  | Abs sub, Abs sup ->
    isa_abs sub sup
  | App sub, App sup ->
    isa_app sub sup
  | _, _ ->
    return false

and isa_tuple sub sup =
  if List.compare_lengths sub.elems sup.elems != 0 then
    return false
  else
    list_all2 (fun left right -> isa left right) sub.elems sup.elems

and isa_record sub sup =
  map_all (fun sup_attr -> isa_record_attr sub sup_attr) sup.attrs

and isa_record_attr sub_record sup_attr =
  match Util.NameMap.find_opt sup_attr.label sub_record.attrs with
  | Some sub_attr ->
    isa sub_attr.type' sup_attr.type'
  | None ->
    return false

and isa_lam sub sup =
  let* param = isa sup.param sub.param in
  let* ret   = isa sub.ret   sup.ret   in
  return (param && ret)

and isa_abs sub sup =
  let* param = is_param sub.param sup.param in
  let sup_body = rename sup.param.bind sub.param.bind sup.body in
  let* body = with_param_rigid sub.param (isa sub.body sup_body) in
  return (param && body)

and isa_app sub sup =
  let* abs = isa sub.abs sup.abs in
  let* arg = is  sub.arg sup.arg in
  return (abs && arg)

and join left right =
  let* sub = isa left right in
  let* sup = isa right left in
  match sub, sup with
  | true, true ->
    (* Both types are equal, any of them can be returned *)
    return right
  | true, false ->
    return right
  | false, true ->
    return left
  | false, false ->
    return (Union { left; right })

and meet left right =
  let* type' = merge left right in
  match type' with
  | Some type' ->
    return type'
  | None ->
    return (Inter { left; right })

and merge left right =
  let* sub = isa left right in
  let* sup = isa right left in
  match sub, sup with
  | true, true ->
    (* Both types are equal, any of them can be returned *)
    return (Some left)
  | true, false ->
    return (Some left)
  | false, true ->
    return (Some right)
  | false, false ->
  match left, right with
  | Tuple left, Tuple right ->
    if List.compare_lengths left.elems right.elems != 0 then
      return None
    else
    let* elems = list_map2 meet left.elems right.elems in
    return (Some (Tuple { elems }))
  | Lam left, Lam right ->
    let* param = is left.param right.param in
    let* ret   = is left.ret   right.ret   in (
    match param, ret with
    | false, false ->
      return None
    | _, _ ->
      let* param = join left.param right.param in
      let* ret   = meet left.ret   right.ret   in
      return (Some (Lam { param; ret })))
  | _, _ ->
    return None
