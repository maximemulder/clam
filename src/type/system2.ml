open Context2
open Context2.Monad
open Level
open Node
open Rename

let rec is left right =
  let* sub = isa left right in
  let* sup = isa right left in
  return (sub && sup)

and is_param left right =
  let* sub = is left.lower right.lower in
  let* sup = is left.upper right.upper in
  return (sub && sup)

and isa left right =
  isa_union left.dnf right.dnf

and isa_union left right =
  list_all (fun left -> list_any (isa_inter left) right) left

and isa_inter left right =
  list_all (fun right -> list_any (fun left -> isa_base left right) left) right

and isa_base sub sup =
  match sub, sup with
  (* TODO: Top and Bot kind *)
  | _, Top ->
    return true
  | Bot, _ ->
    return true
  | Unit, Unit | Bool, Bool | Int, Int | String, String ->
    return true
  | Univ univ, sup ->
    with_param_fresh univ.param (isa univ.ret (base sup))
  | sub, Univ univ ->
    with_param_rigid univ.param (isa (base sub) univ.ret)
  | Var sub, Var sup ->
    isa_var sub sup
  | Var sub, sup ->
    isa_var_sub sub (Node.base sup)
  | sub, Var sup ->
    isa_var_sup (Node.base sub) sup
  | Tuple left, Tuple right ->
    isa_tuple left right
  | Lam left, Lam right ->
    isa_lam left right
  | Abs sub_abs, Abs sup_abs ->
    isa_abs sub_abs sup_abs
  | App sub_app, App sup_app ->
    isa_app sub_app sup_app
  | _, _ ->
    return false

and isa_var sub sup =
  if sub.bind == sup.bind then
    return true
  else
  let* sub = get_var sub.bind in
  let* sup = get_var sup.bind in
  match sub, sup with
  | Fresh sub, Fresh sup ->
    let* level = cmp_level sub.bind sup.bind in
    if level then
      isa_var_sub { bind = sub.bind } (Node.var sup.bind)
    else
      isa_var_sup (Node.var sub.bind) { bind = sup.bind }
  | Fresh sub, Rigid sup ->
    isa_var_sub { bind = sub.bind } (Node.var sup.bind)
  | Rigid sub, Fresh sup ->
    isa_var_sup (Node.var sub.bind) { bind = sup.bind }
  | Rigid sub, Rigid sup ->
    (* TODO: Checking both lower and upper bounds result in infintie recursion,
    check what is the right approach *)
    let* lower = isa sub.upper (Node.var sup.bind) in
    (* let* upper = isa (Node.var sub.bind) sub.lower in *)
    return lower

and isa_var_sub var sup =
  let* var = get_var var.bind in
  match var with
  | Fresh var ->
    let* cond = isa var.lower sup in
    if not cond then
      return false
    else
    let* upper = meet var.upper sup in
    let var = { var with upper } in
    let* () = levelize var sup in
    let* () = update_fresh var in
    return true
  | Rigid var ->
    isa var.upper sup

and isa_var_sup sub var =
  let* var = get_var var.bind in
  match var with
  | Fresh var ->
    let* cond = isa sub var.upper in
    if not cond then
      return false
    else
    let* lower = join var.lower sub in
    let var = { var with lower } in
    let* () = levelize var sub in
    let* () = update_fresh var in
    return true
  | Rigid var ->
    isa sub var.lower

and isa_tuple sub sup =
  if List.compare_lengths sub.elems sup.elems != 0 then
    return false
  else
    let elems = List.combine sub.elems sup.elems in
    list_all (fun (left, right) -> isa left right) elems

and isa_lam sub sup =
  let* param = isa sup.param sub.param in
  let* ret = isa sub.ret sup.ret in
  return (param && ret)

and isa_abs sub sup =
  let* param = is_param sub.param sup.param in
  let sup_body = rename sup.body sup.param.bind sub.param.bind in
  let* body = with_param_rigid sub.param (isa sub.body sup_body) in
  return (param && body)

and isa_app sub sup =
  let* abs = isa sub.abs sup.abs in
  let* arg = is sub.arg sup.arg in
  return (abs && arg)

and join (left: Node.type') (right: Node.type') =
  let* sub = isa left right in
  let* sup = isa right left in
  match sub, sup with
  | true, true ->
    return left
  | true, false ->
    return right
  | false, true ->
    return left
  | false, false ->
    return { dnf = left.dnf @ right.dnf }

and join_inter ctx left right =
  let sub, ctx = isa_inter left right ctx in
  let sup, ctx = isa_inter left right ctx in
  match sub, sup with
  | true, true ->
    Some left
  | true, false ->
    Some right
  | false, true ->
    Some left
  | false, false ->
    None

and meet left right =
  let* sub = isa left right in
  let* sup = isa right left in
  match sub, sup with
  | true, true ->
    return left
  | true, false ->
    return left
  | false, true ->
    return right
  | false, false ->
    meet_2 left right

(* TODO: This is old stuff *)
and meet_2 left right ctx =
  let types = Util.list_product (meet_inter ctx) left.dnf right.dnf in
  { dnf = types }, ctx

and meet_inter ctx left right =
  Util.list_collapse (fun l r -> meet_base l r ctx |> fst) (left @ right)

and meet_base left right =
  match left, right with
  (* TODO: Handle variable subtyping *)
  | Var _, _ ->
    return None
  | _, Var _ ->
    return None
  | Tuple left, Tuple right ->
    meet_tuple left right
  | Record left, Record right ->
    meet_record left right
  | Lam left, Lam right ->
    meet_lam left right
  | Univ left, Univ right ->
    meet_univ left right
  | Abs left, Abs right ->
    meet_abs left right
  | _, _ ->
    return (Some Bot)

and meet_tuple left right =
  if List.compare_lengths left.elems right.elems != 0 then
    return (Some Bot)
  else
  let* elems = list_map2 meet left.elems right.elems in
  return (Some (Tuple { elems }))

and meet_record left right =
  let* attrs = map_join meet_record_attr left.attrs right.attrs in
  return (Some (Record { attrs }))

and meet_record_attr left right =
  let* type' = meet left.type' right.type' in
  return { left with type' }

and meet_lam left right =
  let* param = is left.param right.param in
  let* ret = is left.ret right.ret in
  match param, ret with
  | false, false ->
    return None
  | _, _ ->
    let* param = join left.param right.param in
    let* ret = meet left.ret right.ret in
    return (Some (Lam { param; ret }))

and meet_univ left right =
  let* param = is_param left.param right.param in
  if not param then
    return (Some Bot)
  else
  let right_ret = rename right.ret right.param.bind left.param.bind in
  let* ret = with_param_rigid left.param (meet left.ret right_ret) in
  return (Some (Univ { param = left.param; ret }))

and meet_abs left right =
  let* param = is_param left.param right.param in
  if not param then
    return (Some Bot)
  else
  let right_body = rename right.body right.param.bind left.param.bind in
  let* body = with_param_rigid left.param (meet left.body right_body) in
  return (Some (Abs { param = left.param; body }))
