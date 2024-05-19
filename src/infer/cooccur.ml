(** The algorithm to remove cooccurrences in a type. It is honestly not very good *)

open Type

(** The polarities at which a type variable occurs or cooccurs. *)
type cooccs = {
  neg: bool option;
  pos: bool option;
}

(** The empty cooccurrence, when a variable does not occur a type. *)
let cooccs_none = { neg = None; pos = None }

(** The polar cooccurrence, when a variable occurs or cooccurs at a given polarity in a type. *)
let cooccs_pol pol coocc =
  match pol with
  | Neg ->
    { neg = Some coocc; pos = None }
  | Pos ->
    { neg = None; pos = Some coocc }

let cooccs_cmp left right =
  match left, right with
  | Some true, Some true | Some true, None | None, Some true ->
    true
  | _, _ ->
    false

let cooccs_display cooccs =
  let f x = match x with
  | Some true ->
    "yes"
  | Some false ->
    "no"
  | None ->
    "none" in
  "neg:" ^ f cooccs.neg ^ " pos: " ^ f cooccs.pos

let merge_cooccs left right =
  let neg = Util.option_join left.neg right.neg (&&) in
  let pos = Util.option_join left.pos right.pos (&&) in
  { neg; pos }

let rec occurs bind type' =
  match type' with
  | Var var ->
    var.bind == bind
  | Union union ->
    (occurs bind union.left) || (occurs bind union.right)
  | Inter inter ->
    (occurs bind inter.left) || (occurs bind inter.right)
  | _ ->
    false

let rec occurs_neg bind type' =
  match type' with
  | Var var ->
    var.bind == bind
  | Union union ->
    (occurs_neg bind union.left) && (occurs_neg bind union.right)
  | Inter inter ->
    (occurs_neg bind inter.left) || (occurs_neg bind inter.right)
  | _ ->
    false

let rec occurs_pos bind type' =
  match type' with
  | Var var ->
    var.bind == bind
  | Union union ->
    (occurs_pos bind union.left) || (occurs_pos bind union.right)
  | Inter inter ->
    (occurs_pos bind inter.left) && (occurs_pos bind inter.right)
  | _ ->
    false

let rec cooccurs bind other pol type' =
  if occurs bind type' then
    match pol with
    | Neg ->
      cooccs_pol Neg (occurs_neg bind type' && occurs_neg other type')
    | Pos ->
      cooccs_pol Pos (occurs_pos bind type' && occurs_pos other type')
  else
  match type' with
  | Univ univ ->
    let param = cooccurs_param bind other univ.param in
    let ret   = cooccurs bind other pol univ.ret in
    merge_cooccs param ret
  | Abs abs ->
    let param = cooccurs_param bind other abs.param in
    let body  = cooccurs bind other pol abs.body in
    merge_cooccs param body
  | type' ->
    syn_fold_pol (cooccurs bind other) merge_cooccs cooccs_none pol type'

and cooccurs_param bind other param =
  if param.bind == other then
    cooccs_none
  else
  let lower = cooccurs bind other Pos param.lower in
  let upper = cooccurs bind other Neg param.upper in
  merge_cooccs lower upper

let join_coocc a b = Util.option_join a b (fun a _ -> a)

let rec fold_coocc bind orig pol type' =
  match type' with
  | Univ univ ->
    let a = cooccurs bind univ.param.bind Pos orig in
    let b = cooccurs univ.param.bind bind Pos orig in
    if (cooccs_cmp a.neg b.neg) || (cooccs_cmp a.pos b.pos) then
      Some univ.param.bind
    else
    let param = fold_coocc_param bind orig univ.param in
    let ret   = fold_coocc bind orig pol univ.ret in
    join_coocc param ret
  | type' ->
    syn_fold_pol (fold_coocc bind orig) join_coocc None pol type'

and fold_coocc_param bind orig param =
  let lower = fold_coocc bind orig Pos param.lower in
  let upper = fold_coocc bind orig Neg param.upper in
  join_coocc lower upper

let get_coocc bind pol type' =
  fold_coocc bind type' pol type'

open Type.Context
open Type.Context.Monad
open Type.System

let rec simplify_2 (fresh: fresh) orig pol type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    return type'
  | Tuple tuple ->
    let* elems = list_map (simplify_2 fresh orig pol) tuple.elems in
    return (Tuple { elems })
  | Record record ->
    let* attrs = map_map (simplify_attr fresh orig pol) record.attrs in
    return (Record { attrs })
  | Lam lam ->
    let* param = simplify_2 fresh orig (inv pol) lam.param in
    let* ret   = simplify_2 fresh orig pol lam.ret in
    return (Lam { param; ret })
  | Univ univ ->
    let* param = simplify_param fresh orig univ.param in
    with_param_rigid param (
      let* ret = simplify_2 fresh orig pol univ.ret in
      let* cond = simplify_univ_cond fresh orig univ in
      if cond then
        substitute param.bind (Var { bind = fresh.bind }) ret
      else
        return (Univ { param; ret })
    )
  | Abs abs ->
    let* param = simplify_param fresh orig abs.param in
    let* body  = simplify_2 fresh orig pol abs.body in
    return (Abs { param; body })
  | App app ->
    let* abs = simplify_2 fresh orig pol app.abs in
    let* arg = simplify_2 fresh orig pol app.arg in
    return (App { abs; arg })
  | _ ->
    Type.Transform.map (simplify_2 fresh orig pol) type'

and simplify_attr fresh orig pol attr =
  let* type' = simplify_2 fresh orig pol attr.type' in
  return { attr with type' }

and simplify_param fresh orig param =
  let* lower = simplify_2 fresh orig Pos param.lower in
  let* upper = simplify_2 fresh orig Neg param.upper in
  return { param with lower; upper }

and simplify_univ_cond fresh orig univ =
  let* lower = with_freeze (is univ.param.lower fresh.lower) in
  let* upper = with_freeze (is univ.param.upper fresh.upper) in
  let* sub = with_freeze (is (Var { bind = fresh.bind }) univ.param.lower) in
  let* sup = with_freeze (is univ.param.upper (Var { bind = fresh.bind })) in
  if not (sub || lower) || not (sup || upper) then
    return false
  else
  let fresh_param = cooccurs fresh.bind univ.param.bind Pos orig in
  let param_fresh = cooccurs univ.param.bind fresh.bind Pos orig in
  let param_fresh = {
    neg = if sup then Some true else param_fresh.neg;
    pos = if sub then Some true else param_fresh.pos;
  } in
  return ((cooccs_cmp fresh_param.neg param_fresh.neg) || (cooccs_cmp fresh_param.pos param_fresh.pos))

(* TODO: Better logging and clean recursion. *)
let rec simplify fresh pol type' =
  let* res = simplify_2 fresh type' pol type' in
  (* if type' <> res then
    print_endline ("simplify " ^ Type.display type' ^ " to " ^ Type.display res);*)
  if res <> type' then (
    (*print_endline ("try " ^ Type.display res);*)
    simplify fresh pol res)
  else
    return res
