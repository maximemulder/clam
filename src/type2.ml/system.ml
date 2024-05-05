open Context
open Context.Monad
open Node
open Rename
open Split

let rec is left right =
  let* sub = isa left right in
  let* sup = isa right left in
  return (sub && sup)

and is_param left right =
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
  | _, Top | Bot, _ ->
    return true (* TODO: Kind *)
  | Unit, Unit | Bool, Bool | Int, Int | String, String ->
    return true
  | Tuple sub, Tuple sup ->
    if List.compare_lengths sub.elems sup.elems != 0 then
      return false
    else
      list_all2 (fun left right -> isa left right) sub.elems sup.elems
  | Lam sub, Lam sup ->
    let* param = isa sup.param sub.param in
    let* ret   = isa sub.ret   sup.ret   in
    return (param && ret)
  | Abs sub, Abs sup ->
    let* param = is_param sub.param sup.param in
    let sup_body = rename sup.param.bind sub.param.bind sup.body in
    let* body = with_param_rigid sub.param (isa sub.body sup_body) in
    return (param && body)
  | App sub, App sup ->
    let* abs = isa sub.abs sup.abs in
    let* arg = is  sub.arg sup.arg in
    return (abs && arg)
  | _, _ ->
    return false

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
