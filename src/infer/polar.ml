open Type.System
open Type.Context
open Type.Context.Monad

(** Polarity of a type position. *)
type polarity = Neg | Pos

(** Invert a polarity. *)
let inv pol =
  match pol with
  | Neg -> Pos
  | Pos -> Neg

(** The polarities at which a type variable occurs. *)
type occs = {
  neg: bool;
  pos: bool;
}

(** The empty occurrence, when a variable does not occur in a type. *)
let occs_none = { neg = false; pos = false }

(** The polar occurrence, when a variable occurs at a given polarity in a type. *)
let occs_pol pol =
  match pol with
  | Neg ->
    { neg = true; pos = false }
  | Pos ->
    { neg = false; pos = true }

let merge_occs left right =
  let neg = left.neg || right.neg in
  let pos = left.pos || right.pos in
  return { neg; pos }

let rec occurs bind pol type' =
  let* types = list_map (fun types ->
    let* types = list_map (occurs_base bind pol) types in
    list_fold merge_occs occs_none types
  ) type'.Type.dnf in
  list_fold merge_occs occs_none types

and occurs_base bind pol type'  =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return occs_none
  | Var var ->
    if var.bind == bind then
      return (occs_pol pol)
    else
      return occs_none
  | Tuple tuple ->
    let* elems = list_map (occurs bind pol) tuple.elems in
    list_fold merge_occs occs_none elems
  | Record record ->
    let attrs = Util.NameMap.to_list record.attrs
    |> List.map snd in
    let* attrs = list_map (fun (attr: Type.attr) -> occurs_attr bind pol attr) attrs in
    list_fold merge_occs occs_none attrs
  | Lam lam ->
    let* param = occurs bind (inv pol) lam.param in
    let* ret   = occurs bind pol lam.ret in
    merge_occs param ret
  | Univ univ ->
    let* param = occurs_param bind univ.param in
    let* ret   = with_param_rigid univ.param (occurs bind pol univ.ret) in
    merge_occs param ret
  | Abs abs ->
    let* param = occurs_param bind abs.param in
    let* body  = with_param_rigid abs.param (occurs bind pol abs.body) in
    merge_occs param body
  | App app ->
    let* abs = occurs bind pol app.abs in
    let* arg = occurs bind pol app.arg in
    merge_occs abs arg

and occurs_attr bind pol attr =
  occurs bind pol attr.type'

and occurs_param bind param  =
  let* lower = occurs bind Pos param.lower in
  let* upper = occurs bind Neg param.upper in
  merge_occs lower upper

(* GET COOCCURRENCES *)

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

let merge_cooccs left right =
  let neg = Util.option_join left.neg right.neg (&&) in
  let pos = Util.option_join left.pos right.pos (&&) in
  return { neg; pos }

let occurs_neg bind type' =
  List.for_all (List.exists ((=) (Type.Var { bind }))) type'.Type.dnf

let occurs_pos bind type' =
  List.exists (List.for_all ((=) (Type.Var { bind }))) type'.Type.dnf

let rec cooccurs bind other pol type' =
  let cooccs_self = match pol with
  | Neg ->
    if occurs_neg bind type' then
      if occurs_neg other type' then
        cooccs_pol Neg true
      else
        cooccs_pol Neg false
    else
      cooccs_none
  | Pos ->
    if occurs_pos bind type' then
      if occurs_pos other type' then
        cooccs_pol Pos true
      else
        cooccs_pol Pos false
    else
      cooccs_none
  in
  let* cooccs = list_map (fun types ->
    let* cooccs = list_map (cooccurs_base bind other pol) types in
    list_fold merge_cooccs cooccs_none cooccs
  ) type'.dnf in
  let* cooccs_inner = list_fold merge_cooccs cooccs_none cooccs in
  merge_cooccs cooccs_self cooccs_inner

and cooccurs_base bind other pol type'  =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    return cooccs_none
  | Tuple tuple ->
    let* elems = list_map (cooccurs bind other pol) tuple.elems in
    list_fold merge_cooccs cooccs_none elems
  | Record record ->
    let attrs = Util.NameMap.to_list record.attrs
    |> List.map snd in
    let* attrs = list_map (cooccurs_attr bind other pol) attrs in
    list_fold merge_cooccs cooccs_none attrs
  | Lam lam ->
    let* param = cooccurs bind other (inv pol) lam.param in
    let* ret   = cooccurs bind other pol lam.ret in
    merge_cooccs param ret
  | Univ univ ->
    let* param = cooccurs_param bind other univ.param in
    let* ret   = with_param_rigid univ.param (cooccurs bind other pol univ.ret) in
    merge_cooccs param ret
  | Abs abs ->
    let* param = cooccurs_param bind other abs.param in
    let* body  = with_param_rigid abs.param (cooccurs bind other pol abs.body) in
    merge_cooccs param body
  | App app ->
    let* abs = cooccurs bind other pol app.abs in
    let* arg = cooccurs bind other pol app.arg in
    merge_cooccs abs arg

and cooccurs_attr bind other pol attr =
  cooccurs bind other pol attr.type'

and cooccurs_param bind other param  =
  let* lower = cooccurs bind other Pos param.lower in
  let* upper = cooccurs bind other Neg param.upper in
  merge_cooccs lower upper

(* SIMPLIFY COOCCURRENCES *)

let rec simplify (fresh: fresh) pol type' =
  map_type (simplify_base fresh pol) type'

and simplify_base fresh pol type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    return (Type.base type')
  | Lam lam ->
    let* param = simplify fresh (inv pol) lam.param in
    let* ret   = simplify fresh pol lam.ret in
    return (Type.lam param ret)
  | Tuple tuple ->
    let* elems = list_map (simplify fresh pol) tuple.elems in
    return (Type.tuple elems)
  | Record record ->
    let* attrs = map_map (simplify_attr fresh pol) record.attrs in
    return (Type.record attrs)
  | Univ univ ->
    let* param: Type.param = simplify_param fresh univ.param in
    let* cond = simplify_univ_cond fresh pol univ in
    if cond then
      let type' = Type.rename param.bind fresh.bind univ.ret in
      simplify fresh pol type'
    else
      let* ret = with_param_rigid param (simplify fresh pol univ.ret) in
      return (Type.univ param ret)
  | Abs abs ->
    let* param = simplify_param fresh abs.param in
    let* body  = with_param_rigid abs.param (simplify fresh pol abs.body) in
    return (Type.abs param body)
  | App app ->
    let* abs = simplify fresh pol app.abs in
    let* arg = simplify fresh pol app.arg in
    return (Type.app abs arg)

and simplify_attr fresh pol attr =
  let* type' = simplify fresh pol attr.type' in
  return { attr with type' }

and simplify_param fresh param =
  let* lower = simplify fresh Pos param.lower in
  let* upper = simplify fresh Neg param.upper in
  return { param with lower; upper }

and simplify_univ_cond fresh pol univ =
  let* lower = with_freeze (is univ.param.lower fresh.lower) in
  let* upper = with_freeze (is univ.param.upper fresh.upper) in
  let* sub = with_freeze (is (Type.var fresh.bind) univ.param.lower) in
  let* sup = with_freeze (is univ.param.upper (Type.var fresh.bind)) in
  if not (sub || lower) || not (upper || sup) then
    return false
  else
  if occurs_neg fresh.bind univ.param.lower || occurs_pos fresh.bind univ.param.upper then
    return true
  else
  let* cooccs = cooccurs univ.param.bind fresh.bind pol univ.ret in
  return (cooccs.neg = Some true || cooccs.pos = Some true)
