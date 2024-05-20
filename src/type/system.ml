open Context
open Context.Monad
open Kind
open Level
open Node
open Rename
open Split
open Trans_ctx

(* FIND TYPE VARIABLE *)

let get_type_fresh type' =
  match type' with
  | Var var ->
    let* var = get_var var.bind in (
    match var with
    | Fresh fresh ->
      return (Some fresh)
    | Rigid _ ->
      return None)
  | _ ->
    return None

let get_type_rigid type' =
  match type' with
  | Var var ->
    let* var = get_var var.bind in (
    match var with
    | Fresh _ ->
      return None
    | Rigid rigid ->
      return (Some rigid))
  | _ ->
    return None

(* TYPE MAP SET *)

let rec map_set f type' =
  match type' with
  | Union union ->
    let* left  = f union.left  in
    let* right = f union.right in
    join left right
  | Inter inter ->
    let* left  = f inter.left  in
    let* right = f inter.right in
    meet left right
  | type' ->
    f type'

(* TYPE SUBSTITUTION *)

and substitute bind other type' =
  map_set (substitute_base bind other) type'

and substitute_base bind other type' =
  match type' with
  | Var var when var.bind == bind ->
    return other
  | App app ->
    let* abs = substitute bind other app.abs in
    let* arg = substitute bind other app.arg in
    compute abs arg
  | type' ->
    map (substitute bind other) type'

(* TYPE COMPUTATION *)

and compute abs arg =
  map_set (compute_base arg) abs

and compute_base arg abs =
  match abs with
  | Abs abs ->
    substitute abs.param.bind arg abs.body
  | Var var ->
    return (App { abs = Var var; arg })
  | _ ->
    failwith "Ill-formed type application in `compute`."

(* TYPE PROMOTION LOWER *)

and promote_lower type' =
  map_set promote_lower_base type'

and promote_lower_base type' =
  match type' with
  | Var var ->
    promote_lower_var var
  | type' ->
    return type'

and promote_lower_var var =
  let* var = get_var var.bind in
  match var with
  | Rigid rigid ->
    promote_lower rigid.lower
  | Fresh _ ->
    failwith "Fresh type variable in `promote_lower_var`."

(* TYPE PROMOTE UPPER *)

and promote_upper type' =
  map_set promote_upper_base type'

and promote_upper_base type' =
  match type' with
  | Var var ->
    promote_upper_var var
  | type' ->
    return type'

and promote_upper_var var =
  let* var = get_var var.bind in
  match var with
  | Rigid rigid ->
    promote_upper rigid.upper
  | Fresh _ ->
    failwith "Fresh type variable in `promote_upper_var`."

(* CONSTRAIN EQUIVALENCE *)

and is left right =
  let* sub = isa left right in
  let* sup = isa right left in
  return (sub && sup)

and is_param left right =
  let* sub = is left.lower right.lower in
  let* sup = is left.upper right.upper in
  return (sub && sup)

(* CONSTRAIN SUBTYPE *)

and isa sub sup =
  let* () = show
    !Global.show_constrain
    ("constrain " ^ Display.display sub ^ " < " ^ Display.display sup)
  in
  Global.nesting := !Global.nesting + 1;
  let* res = isa_base sub sup in
  Global.nesting := !Global.nesting - 1;
  let* () = show
    !Global.show_constrain
    ("= " ^ string_of_bool res)
  in
  return res

and isa_base sub sup =
  match split_inter sup with
  | Some (left, right) ->
    isa_inter_sup sub left right
  | None ->
  match split_inter sub with
  | Some (left, right) ->
    isa_inter_sub left right sup
  | None ->
  match split_union sub with
  | Some (left, right) ->
    isa_union_sub left right sup
  | None ->
  match split_union sup with
  | Some (left, right) ->
    isa_union_sup sub left right
  | None ->
  let* fresh_sub = get_type_fresh sub in
  let* fresh_sup = get_type_fresh sup in
  match fresh_sub, fresh_sup with
  | Some fresh_sub, Some fresh_sup ->
    isa_fresh fresh_sub fresh_sup
  | Some fresh_sub, None ->
    isa_fresh_sub fresh_sub sup
  | None, Some fresh_sup ->
    isa_fresh_sup fresh_sup sub
  | None, None ->
  match sup with
  | Univ sup ->
    with_param_rigid sup.param (isa sub sup.ret)
  | _ ->
  match sub with
  | Univ sub ->
    with_param_fresh sub.param sub.ret (fun ret -> isa ret sup)
  | _ ->
  let* rigid_sub = get_type_rigid sub in
  let* rigid_sup = get_type_rigid sup in
  match rigid_sub, rigid_sup with
  | Some rigid_sub, Some rigid_sup ->
    isa_rigid rigid_sub rigid_sup
  | Some rigid_sub, None ->
    isa_rigid_sub rigid_sub sup
  | None, Some rigid_sup ->
    isa_rigid_sup rigid_sup sub
  | None, None ->
  match sub with
  | Bot ->
    is_proper sup
  | sub ->
  match sup with
  | Top ->
    is_proper sub
  | sup ->
  match sub, sup with
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
  | App abs, sup ->
    isa_app_sub abs sup
  | sub, App abs ->
    isa_app_sup abs sub
  | _, _ ->
    return false

and isa_fresh fresh_sub fresh_sup =
  if fresh_sub.bind == fresh_sup.bind then
    return true
  else
  let* level = cmp_level fresh_sub.bind fresh_sup.bind in
  if level then
    isa_fresh_sub fresh_sub (Var { bind = fresh_sup.bind })
  else
    isa_fresh_sup fresh_sup (Var { bind = fresh_sub.bind })

and isa_fresh_sub fresh_sub sup =
  let* cond = isa fresh_sub.lower sup in
  if not cond then
    return false
  else
  let* upper = meet fresh_sub.upper sup in
  let fresh_sub = { fresh_sub with upper } in
  let* () = levelize fresh_sub sup in
  let* () = update_fresh fresh_sub in
  return true

and isa_fresh_sup fresh_sup sub =
  let* cond = isa sub fresh_sup.upper in
  if not cond then
    return false
  else
  let* lower = join fresh_sup.lower sub in
  let fresh_sup = { fresh_sup with lower } in
  let* () = levelize fresh_sup sub in
  let* () = update_fresh fresh_sup in
  return true

and isa_inter_sup sub left right =
  let* left  = isa sub left  in
  let* right = isa sub right in
  return (left && right)

and isa_inter_sub left right sup =
  let* sub = with_freeze (meet_merge left right) in
  match sub with
  | Some sub ->
    isa sub sup
  | None ->
  let* fresh_sup = get_type_fresh sup in
  match fresh_sup with
  | Some fresh_sup ->
    isa_fresh_sup fresh_sup (Inter { left; right })
  | None ->
  let* left  = isa left  sup in
  let* right = isa right sup in
  return (left || right)

and isa_union_sub left right sup =
  let* left  = isa left  sup in
  let* right = isa right sup in
  return (left && right)

and isa_union_sup sub left right =
  let* fresh_sub = get_type_fresh sub in
  match fresh_sub with
  | Some fresh_sub ->
    isa_fresh_sub fresh_sub (Union { left; right })
  | None ->
  let* left  = isa sub left  in
  let* right = isa sub right in
  return (left || right)

and isa_rigid rigid_sub rigid_sup =
  if rigid_sub.bind == rigid_sup.bind then
    return true
  else
  let* lower = isa (Var { bind = rigid_sub.bind }) rigid_sup.lower in
  let* upper = isa rigid_sub.upper (Var { bind = rigid_sup.bind }) in
  return (lower || upper)

and isa_rigid_sub rigid_sub sup =
  isa rigid_sub.upper sup

and isa_rigid_sup rigid_sup sub =
  isa sub rigid_sup.lower

and isa_tuple sub sup =
  if List.compare_lengths sub.elems sup.elems != 0 then
    return false
  else
    list_all2 isa sub.elems sup.elems

and isa_record sub sup =
  map_all (isa_record_attr sub) sup.attrs

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

and isa_app_sub app sup =
  let* abs = promote_upper app.abs in
  let* sub = compute abs app.arg in
  isa sub sup

and isa_app_sup app sub =
  let* abs = promote_lower app.abs in
  let* sup = compute abs app.arg in
  isa sub sup

(* TYPE JOIN *)

and join left right =
  let* type' = join_base left right in
  let* () = show
    !Global.show_join
    ("join " ^ Display.display left ^ " " ^ Display.display right ^  " = " ^ Display.display type')
  in
  return type'

and join_base left right =
  with_freeze (join_freeze left right)

and join_freeze left right =
  let* type' = join_merge left right in
  match type' with
  | Some type' ->
    return type'
  | None ->
    return (Union { left; right })

and join_merge left right =
  let* sub = isa left right in
  let* sup = isa right left in
  match sub, sup with
  | true, true ->
    (* Both types are equivalent, any of them can be returned *)
    return (Some right)
  | true, false ->
    return (Some right)
  | false, true ->
    return (Some left)
  | false, false ->
    return None

(* TYPE MEET *)

and meet left right =
  let* type' = meet_base left right in
  let* () = show
    !Global.show_meet
    ("meet " ^ Display.display left ^ " " ^ Display.display right ^  " = " ^ Display.display type')
  in
  return type'

and meet_base left right =
  with_freeze (meet_freeze left right)

and meet_freeze left right =
  let* type' = meet_merge left right in
  match type' with
  | Some type' ->
    return type'
  | None ->
    return (Inter { left; right })

and meet_merge left right =
  let* sub = isa left right in
  let* sup = isa right left in
  match sub, sup with
  | true, true ->
    (* Both types are equivalent, any of them can be returned *)
    return (Some left)
  | true, false ->
    return (Some left)
  | false, true ->
    return (Some right)
  | false, false ->
    meet_merge_disjoint left right

and meet_merge_disjoint left right =
  match left, right with
  | Tuple left, Tuple right ->
    meet_merge_tuple left right
  | Record left, Record right ->
    meet_merge_record left right
  | Lam left, Lam right ->
    meet_merge_lam left right
  | Abs left, Abs right ->
    meet_merge_abs left right
  | _, _ ->
    return None

and meet_merge_tuple left right =
  if List.compare_lengths left.elems right.elems != 0 then
    return None
  else
  let* elems = list_map2 meet left.elems right.elems in
  return (Some (Tuple { elems }))

and meet_merge_record left right =
  let* attrs = map_join meet_merge_record_attr left.attrs right.attrs in
  return (Some (Record { attrs }))

and meet_merge_record_attr left right =
  let* type' = meet left.type' right.type' in
  return { left with type' }

and meet_merge_lam left right =
  let* param = is left.param right.param in
  let* ret   = is left.ret   right.ret   in
  match param, ret with
  | false, false ->
    return None
  | _, _ ->
    let* param = join left.param right.param in
    let* ret   = meet left.ret   right.ret   in
    return (Some (Lam { param; ret }))

and meet_merge_abs left right =
  let* param = is_param left.param right.param in
  if not param then
    return None
  else
  let right_body = rename right.param.bind left.param.bind right.body in
  let* body = with_param_rigid left.param (meet left.body right_body) in
  return (Some (Abs { param = left.param; body }))

(* TYPE KIND *)

and is_kind left right =
  match left, right with
  | Proper, Proper ->
    return true
  | Higher left, Higher right ->
    let* lower = is left.lower right.lower in
    let* upper = is left.upper right.upper in
    let* body = is_kind left.body right.body in
    return (lower && upper && body)
  | _, _ ->
    return false

and is_proper type' =
  let* kind = get_kind type' in
  is_kind kind Proper
