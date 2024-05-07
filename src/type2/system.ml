open Context
open Context.Monad
open Node
open Kind
open Rename
open Split

(** Type substitution entry *)
type entry = {
  bind: Abt.bind_type;
  other: type';
}

(* CONSTRAIN EQUIVALENCE *)

let rec is left right =
  let* sub = isa left right in
  let* sup = isa right left in
  return (sub && sup)

and is_param (left: param) (right: param) =
  let* sub = is left.lower right.lower in
  let* sup = is left.upper right.upper in
  return (sub && sup)

(* CONSTRAIN SUBTYPE *)

and isa sub sup =
  match split_inter sup with
  | Some (left, right) ->
    let* left  = isa sub left  in
    let* right = isa sub right in
    return (left && right)
  | None ->
  match split_inter sub with
  | Some (left, right) ->
    let* sub = meet_merge left right in (
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
  | App abs, sup ->
    isa_app_sub abs sup
  | sub, App abs ->
    isa_app_sup abs sub
  | _, _ ->
    return false

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

(* TYPE MAP *)

and map f type' =
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
  substitute_entry { bind; other } type'

and substitute_entry entry type' =
  map (substitute_base entry) type'

and substitute_base entry type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return type'
  | Var var ->
    substitute_var entry var
  | Tuple tuple ->
    substitute_tuple entry tuple
  | Record record ->
    substitute_record entry record
  | Lam lam ->
    substitute_lam entry lam
  | Univ univ ->
    substitute_univ entry univ
  | Abs abs ->
    substitute_abs entry abs
  | App app ->
    substitute_app entry app
  | _ ->
    failwith "Unreachable `System.substitute_base`"

and substitute_var entry var =
  if var.bind == entry.bind then
    return entry.other
  else
    return (Var var)

and substitute_tuple entry tuple =
  let* elems = list_map (substitute_entry entry) tuple.elems in
  return (Tuple { elems })

and substitute_record entry record =
  let* attrs = map_map (substitute_attr entry) record.attrs in
  return (Record { attrs })

and substitute_lam entry lam =
  let* param = substitute_entry entry lam.param in
  let* ret   = substitute_entry entry lam.ret   in
  return (Lam { param; ret })

and substitute_univ entry univ =
  let* param = substitute_param entry univ.param in
  let* ret = with_param_rigid param (substitute_entry entry univ.ret) in
  return (Univ { param; ret })

and substitute_param entry param =
  let* lower = substitute_entry entry param.lower in
  let* upper = substitute_entry entry param.upper in
  return { param with lower; upper }

and substitute_abs entry abs =
  let* param = substitute_param entry abs.param in
  let* body = with_param_rigid param (substitute_entry entry abs.body) in
  return (Abs { param; body })

and substitute_app entry app =
  let* abs = substitute_entry entry app.abs in
  let* arg = substitute_entry entry app.arg in
  compute abs arg

and substitute_attr entry attr =
  let* type' = substitute_entry entry attr.type' in
  return { attr with type' }

(* TYPE PROMOTION *)

and promote_lower type' =
  map promote_lower_base type'

and promote_lower_base type' =
  match type' with
  | Var var ->
    let* var = get_var var.bind in (
    match var with
    | Fresh fresh ->
      promote_lower fresh.lower
    | Rigid rigid ->
      promote_lower rigid.lower)
  | type' ->
    return type'

and promote_upper type' =
  map promote_upper_base type'

and promote_upper_base type' =
  match type' with
  | Var var ->
    let* var = get_var var.bind in (
    (match var with
    | Fresh fresh ->
      promote_upper fresh.upper
    | Rigid rigid ->
      promote_upper rigid.upper)
    )
  | type' ->
    return type'

(* TYPE COMPUTATION *)

and compute abs arg =
  map (fun abs -> compute_base abs arg) abs

and compute_base abs arg =
  match abs with
  | Abs abs ->
    substitute abs.param.bind arg abs.body
  | Var var ->
    return (App { abs = Var var; arg })
  | _ ->
    failwith "Ill-formed type application in `System.compute_base`."

(* TYPE JOIN *)

and join left right =
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
  | Univ left, Univ right ->
    meet_merge_univ left right
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

and meet_merge_univ left right =
  let* param = is_param left.param right.param in
  if not param then
    return None
  else
  let right_ret = rename right.param.bind left.param.bind right.ret in
  let* ret = with_param_rigid left.param (meet left.ret right_ret) in
  return (Some (Univ { param = left.param; ret }))

and meet_merge_abs left right =
  let* param = is_param left.param right.param in
  if not param then
    return None
  else
  let right_body = rename right.param.bind left.param.bind right.body in
  let* body = with_param_rigid left.param (meet left.body right_body) in
  return (Some (Abs { param = left.param; body }))
