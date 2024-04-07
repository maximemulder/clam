open Context
open Context.Monad
open Level
open Node
open Rename

(* CONSTRAIN EQUIVALENCE *)

let rec is left right =
  let* sub = isa left right in
  let* sup = isa right left in
  return (sub && sup)

and is_param left right =
  let* sub = is left.lower right.lower in
  let* sup = is left.upper right.upper in
  return (sub && sup)

(* CONSTRAIN SUBTYPE *)

and isa left right =
  isa_union left.dnf right.dnf

and isa_union left right =
  list_all (fun left -> list_any (isa_inter left) right) left

and isa_inter left right =
  list_all (fun right -> list_any (fun left -> isa_base left right) left) right

and isa_base sub sup =
  match sub, sup with
  | _, Top ->
    let* kind = Kind.get_kind_base sub in
    return (kind = Type)
  | Bot, _ ->
    let* kind = Kind.get_kind_base sup in
    return (kind = Type)
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
  | App sub, _ ->
    let* sub_abs = promote_upper sub.abs in
    let* sub = compute sub_abs sub.arg in
    isa sub (Node.base sup)
  | _, App sup ->
    let* sup_abs = promote_lower sup.abs in
    let* sup = compute sup_abs sup.arg in
    isa (Node.base sub) sup
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

(* TYPE JOIN *)

and join left right = with_freeze (join_freeze left right)

and join_freeze left right =
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
    join_disjoint left right

and join_disjoint left right =
    let* types = list_collapse join_inter (left.dnf @ right.dnf) in
    return { dnf = types }

and join_inter left right =
  let* sub = isa_inter left right in
  let* sup = isa_inter right left in
  match sub, sup with
  | true, true ->
    (* Both types work here. *)
    return (Some right)
  | true, false ->
    return (Some right)
  | false, true ->
    return (Some left)
  | false, false ->
    return None

(* TYPE MEET *)

and meet left right = with_freeze (meet_freeze left right)

and meet_freeze left right =
  let* sub = isa left right in
  let* sup = isa right left in
  match sub, sup with
  | true, true ->
    (* Both types work here. *)
    return left
  | true, false ->
    return left
  | false, true ->
    return right
  | false, false ->
    meet_disjoint left right

and meet_disjoint left right =
  let* types = list_product meet_inter left.dnf right.dnf in
  let* types = list_collapse join_inter types in
  return { dnf = types }

and meet_inter left right =
  list_collapse meet_base (left @ right)

and meet_base left right =
  match left, right with
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
  let* ret   = is left.ret   right.ret   in
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

(* MAP TYPE *)

and map_type f type' =
  map_union (map_inter f) type'.dnf

and map_union f types =
  let* types = list_map f types in
  list_reduce join types

and map_inter f types =
  let* types = list_map f types in
  list_reduce meet types

(* SUBSTITUTE *)

and substitute bind other (type': Node.type') =
  map_type (substitute_base bind other) type'

and substitute_base bind other (type': Node.base) =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return (base type')
  | Var var ->
    substitute_var bind other var
  | Tuple tuple ->
    let* elems = list_map (substitute bind other) tuple.elems in
    return (Node.tuple elems)
  | Record record ->
    let* attrs = map_map (substitute_attr bind other) record.attrs in
    return (Node.record attrs)
  | Lam lam ->
    let* param = substitute bind other lam.param in
    let* ret   = substitute bind other lam.ret   in
    return (Node.lam param ret)
  | Univ univ ->
    let* param = substitute_param bind other univ.param in
    let* ret = with_param_rigid param (substitute bind other univ.ret) in
    return (Node.univ param ret)
  | Abs abs ->
    let* param = substitute_param bind other abs.param in
    let* body = with_param_rigid param (substitute bind other abs.body) in
    return (Node.abs param body)
  | App app ->
    let* abs = substitute bind other app.abs in
    let* arg = substitute bind other app.arg in
    compute abs arg

and substitute_var bind other var =
  if var.bind == bind then
    return other
  else
    return (Node.var var.bind)

and substitute_param bind other param =
  let* lower = substitute bind other param.lower in
  let* upper = substitute bind other param.upper in
  return { param with lower; upper }

and substitute_attr bind other attr =
  let* type' = substitute bind other attr.type' in
  return { attr with type' }

(* TYPE COMPUTATION *)

and compute (abs: Node.type') (arg: Node.type'): type' t =
  map_type (Util.flip compute_base arg) abs

and compute_base (abs: Node.base) (arg: Node.type') =
  match abs with
  | Abs abs ->
    substitute abs.param.bind arg abs.body
  | _ ->
    return (Node.app (Node.base abs) arg)

(* TYPE PROMOTION *)

and promote_upper type' =
  map_type promote_upper_base type'

and promote_upper_base type' =
  match type' with
  | Var var ->
    let* var = get_var var.bind in
    (match var with
    | Fresh fresh ->
      promote_upper fresh.upper
    | Rigid rigid ->
      promote_upper rigid.upper
    )
  | _ ->
    return (Node.base type')

and promote_lower type' =
  map_type promote_lower_base type'

and promote_lower_base type' =
  match type' with
  | Var var ->
    let* var = get_var var.bind in
    (match var with
    | Fresh fresh ->
      promote_lower fresh.lower
    | Rigid rigid ->
      promote_lower rigid.lower)
  | _ ->
    return (Node.base type')

(* KIND EQUIVALENCE *)

let rec is_kind left right =
  match left, right with
  | Kind.Type, Kind.Type ->
    return true
  | Kind.Abs left_abs, Kind.Abs right_abs ->
    let* lower = is left_abs.lower right_abs.lower in
    let* upper = is left_abs.upper right_abs.upper in
    let* body = is_kind left_abs.body right_abs.body in
    return (lower && upper && body)
  | _, _ ->
    return false
