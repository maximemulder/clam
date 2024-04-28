open Context
open Context.Monad
open Misc
open Node
open Rename

(* EXTRACT FRESH TYPE VARIABLES *)

(* Extract sole fresh type variables from the type of a given side of a type
  inequation. *)

let get_fresh_sub sub =
  match sub with
  | [Var var] ->
    let* var = get_var var.bind in
    (match var with
    | Fresh fresh ->
      return (Some fresh)
    | Rigid _ ->
      return None)
  | _ ->
    return None

let get_fresh_sup sup =
  match sup with
  | Var var ->
    let* var = get_var var.bind in
    (match var with
    | Fresh fresh ->
      return (Some fresh)
    | Rigid _ ->
      return None)
  | _ ->
    return None

(* CONSTRAIN EQUIVALENCE *)

(* Constrain two types to be equal in a given context. *)

let rec is left right =
  let* sub = isa left right in
  let* sup = isa right left in
  return (sub && sup)

and is_param left right =
  let* sub = is left.lower right.lower in
  let* sup = is left.upper right.upper in
  return (sub && sup)

(* CONSTRAIN SUBTYPE *)

(** Constrain a type to, if possible, be a subtype of another type in a given
  context, and return the updated context for this subtyping to hold.

  There are several subtleties here.

  First, in the presence of existential type variables, and unions or intersections,
  there may be several solutions to a given subtyping inequation. For instance,
  considering the following inequation with two existential type variables A and B:

  {A, B} < {Int, String} | {String, Int}

  The solutions for this inequation are: A < Int, B < String OR A < String, B < Int
  To avoid such cases, the algorithm conservatively rejects cases where existential
  variables appear in inequations where unions or intersections are located on the
  wrong side of the inequation, respectively right and left, unless the opposite
  side is a sole existential variable.

  Additionally, the types constrained must be compared in the following order:
  1. Fresh type variables
  2. Universal types
  3. Other types (including rigid variables)

  Finally, cases where two type variables are compared to one another are treated
  separately to cases where a type variable is compared to another type.
*)
and isa sub sup =
  let* () = show
    !Global.show_constrain
    ("constrain " ^ Display.display sub ^ " < " ^ Display.display sup)
  in
  isa_nesting := !isa_nesting + 1;
  let* result = isa_union sub.dnf sup.dnf in
  isa_nesting := !isa_nesting - 1;
  let* () = show
    !Global.show_constrain
    ("= " ^ string_of_bool result)
  in
  return result

and isa_union sub sup =
  list_all (fun sub -> isa_union_hard sub sup) sub

and isa_union_hard sub sup =
  match sup with
  | [sup] ->
    isa_inter sub sup
  | _ ->
    let* fresh = get_fresh_sub sub in
    match fresh with
    | Some sub ->
      isa_fresh_sub sub { dnf = sup }
    | None ->
      let* fresh_sub = list_any appears_fresh_base sub in
      let* fresh_sup = list_any (list_any appears_fresh_base) sup in
      if not fresh_sub && not fresh_sup then
        list_any (isa_inter sub) sup
      else
        let* () = show !Global.show_constrain "maybe" in
        return false

and isa_inter sub sup =
  list_all (fun sup -> isa_inter_hard sub sup) sup

and isa_inter_hard sub sup =
  match sub with
  | [sub] ->
    isa_base_var sub sup
  | _ ->
    let* fresh = get_fresh_sup sup in
    match fresh with
    | Some sup ->
      isa_fresh_sup { dnf = [sub] } sup
    | None ->
      let* fresh_sub = list_any appears_fresh_base sub in
      let* fresh_sup = appears_fresh_base sup in
      if not fresh_sub && not fresh_sup then
        list_any (fun sub -> isa_base_var sub sup) sub
      else
        let* () = show !Global.show_constrain "maybe" in
        return false

and isa_base_var sub sup =
  match sub, sup with
  | Var sub, Var sup ->
    isa_var sub sup
  | Var sub, sup ->
    isa_var_sub sub sup
  | sub, Var sup ->
    isa_var_sup sub sup
  | sub, sup ->
    isa_base_univ sub sup

and isa_var sub sup =
  let* sub = get_var sub.bind in
  let* sup = get_var sup.bind in
  match sub, sup with
  | Fresh sub, Fresh sup ->
    isa_fresh sub sup
  | Fresh sub, Rigid sup ->
    isa_fresh_sub sub (Node.var sup.bind)
  | Rigid sub, Fresh sup ->
    isa_fresh_sup (Node.var sub.bind) sup
  | Rigid sub, Rigid sup ->
    isa_base_univ (Var { bind = sub.bind }) (Var { bind = sup.bind })

and isa_var_sub sub sup =
  let* sub = get_var sub.bind in
  match sub with
  | Fresh sub ->
    isa_fresh_sub sub (Node.base sup)
  | Rigid sub ->
    isa_base_univ (Var { bind = sub.bind }) sup

and isa_var_sup sub sup =
  let* sup = get_var sup.bind in
  match sup with
  | Fresh sup ->
    isa_fresh_sup (Node.base sub) sup
  | Rigid sup ->
    isa_base_univ sub (Var { bind = sup.bind })

and isa_base_univ sub sup =
  match sub, sup with
  | sub, Univ sup ->
    with_param_rigid sup.param (isa (base sub) sup.ret)
  | Univ sub, sup ->
    with_param_fresh sub.param sub.ret (fun ret -> isa ret (base sup))
  | sub, sup ->
    isa_base sub sup

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
  | Var sub, Var sup ->
    isa_rigid sub sup
  | Var sub, sup ->
    isa_rigid_sub sub sup
  | sub, Var var ->
    isa_rigid_sup sub var
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
  | App sub, sup ->
    let* sub_abs = promote_upper sub.abs in
    let* sub = compute sub_abs sub.arg in
    isa sub (Node.base sup)
  | sub, App sup ->
    let* sup_abs = promote_lower sup.abs in
    let* sup = compute sup_abs sup.arg in
    isa (Node.base sub) sup
  | _, _ ->
    return false

and isa_fresh sub sup =
  if sub.bind == sup.bind then
    return true
  else
  let* level = cmp_level sub.bind sup.bind in
  if level then
    isa_fresh_sub sub (Node.var sup.bind)
  else
    isa_fresh_sup (Node.var sub.bind) sup

and isa_fresh_sub sub sup =
  let* cond = isa sub.lower sup in
  if not cond then
    return false
  else
  let* upper = meet sub.upper sup in
  let sub = { sub with upper } in
  let* () = levelize sub sup in
  let* () = update_fresh sub in
  return true

and isa_fresh_sup sub sup =
  let* cond = isa sub sup.upper in
  if not cond then
    return false
  else
  let* lower = join sup.lower sub in
  let sup = { sup with lower } in
  let* () = levelize sup sub in
  let* () = update_fresh sup in
  return true

and isa_rigid sub sup =
  if sub.bind == sup.bind then
    return true
  else
  let* sub = get_var sub.bind in
  let* sup = get_var sup.bind in
  match sub, sup with
  | Rigid sub, Rigid sup ->
    isa_rigid_sub { bind = sub.bind } (Var { bind = sup.bind })
  | _, _ ->
    failwith "Unreachable"

and isa_rigid_sub sub sup =
  let* sub = get_var sub.bind in
  match sub with
  | Fresh _ ->
    failwith "Unreachable"
  | Rigid sub ->
    isa sub.upper (Node.base sup)

and isa_rigid_sup sub sup =
  let* sup = get_var sup.bind in
  match sup with
  | Fresh _ ->
    failwith "Unreachable"
  | Rigid sup ->
    isa (Node.base sub) sup.lower

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
  let sup_body = rename sup.param.bind sub.param.bind sup.body in
  let* body = with_param_rigid sub.param (isa sub.body sup_body) in
  return (param && body)

and isa_app sub sup =
  let* abs = isa sub.abs sup.abs in
  let* arg = is sub.arg sup.arg in
  return (abs && arg)

(* TYPE JOIN *)

and join left right =
  let* res = with_freeze (join_freeze left right) in
  let* () = show
    !Global.show_join
    ("join " ^ Display.display left ^ " " ^ Display.display right ^ " = " ^ Display.display res)
  in
  return res

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

and meet left right =
  let* res = with_freeze (meet_freeze left right) in
  let* () = show
    !Global.show_meet
    ("meet " ^ Display.display left ^ " " ^ Display.display right ^  " = " ^ Display.display res)
  in
  return res

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
  let right_ret = rename right.param.bind left.param.bind right.ret in
  let* ret = with_param_rigid left.param (meet left.ret right_ret) in
  return (Some (Univ { param = left.param; ret }))

and meet_abs left right =
  let* param = is_param left.param right.param in
  if not param then
    return (Some Bot)
  else
  let right_body = rename right.param.bind left.param.bind right.body in
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
