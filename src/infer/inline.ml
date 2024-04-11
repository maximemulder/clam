open Polar
open Type.Context
open Type.Context.Monad
open Type.System

type entry = {
  fresh: fresh;
  neg: Type.type';
  pos: Type.type';
}

let rec inline entry pol (type': Type.type') =
  inline_union entry pol type'.dnf

and inline_union entry pol types =
  let* types = list_map (inline_inter entry pol) types in
  list_fold join (Type.bot) types

and inline_inter entry pol types =
  let* types = list_map (inline_base entry pol) types in
  list_fold meet (Type.top) types

and inline_base entry pol type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return (Type.base type')
  | Var var when var.bind == entry.fresh.bind -> (
    match pol with
    | Neg ->
      meet entry.neg entry.fresh.upper
    | Pos ->
      join entry.pos entry.fresh.lower)
  | Var var ->
    return (Type.var var.bind)
  | Tuple tuple ->
    let* elems = list_map (inline entry pol) tuple.elems in
    return (Type.tuple elems)
  | Record record ->
    let* attrs = map_map (inline_attr entry pol) record.attrs in
    return (Type.record attrs)
  | Lam lam ->
    let* param = inline entry (inv pol) lam.param in
    let* ret = inline entry pol lam.ret in
    return (Type.lam param ret)
  | Univ univ ->
    let* param = inline_param entry (inv pol) univ.param in
    let* ret = with_param_rigid univ.param (inline entry pol univ.ret) in
    return (Type.univ param ret)
  | _ ->
    return (Type.base type')

and inline_attr entry pol attr =
  let* type' = inline entry pol attr.type' in
  return { attr with type' }

and inline_param entry _pol param =
  let* lower = inline entry Pos param.lower in
  let* upper = inline entry Neg param.upper in
  return { param with lower; upper }

let inline fresh neg pos pol type' =
  inline { fresh; neg; pos } pol type'
