open Abt.Type
open Context
open Context.Monad

(* TYPE MAP *)

let rec map f type' =
  match type' with
  | Top _ | Bot _ | Unit _ | Bool _ | Int _ | String _ | Var _ ->
    return type'
  | Tuple tuple ->
    let* elems = list_map f tuple.elems in
    return (Tuple { tuple with elems })
  | Record record ->
    let* attrs = map_map (map_attr f) record.attrs in
    return (Record { record with attrs })
  | Lam lam ->
    let* param = f lam.param in
    let* ret   = f lam.ret   in
    return (Lam { lam with param; ret })
  | Univ univ ->
    let* param = map_param f univ.param in
    let* ret   = with_param_rigid param (f univ.ret) in
    return (Univ { univ with param; ret })
  | Abs abs ->
    let* param = map_param f abs.param in
    let* body  = with_param_rigid param (f abs.body) in
    return (Abs { abs with param; body })
  | App app ->
    let* abs = f app.abs in
    let* arg = f app.arg in
    return (App { app with abs; arg })
  | Rec rec' ->
    let* body = f rec'.body in
    return (Rec { rec' with body })
  | Union union ->
    let* left  = f union.left  in
    let* right = f union.right in
    return (Union { union with left; right })
  | Inter inter ->
    let* left  = f inter.left  in
    let* right = f inter.right in
    return (Inter { inter with left; right })

and map_attr f attr =
  let* type' = f attr.type' in
  return { attr with type' }

and map_param f param =
  let* lower = f param.lower in
  let* upper = f param.upper in
  return { param with lower; upper }

(* POLAR TYPE MAP *)

open Pol

let rec map_pol f pol type' =
  match type' with
  | Lam lam ->
    let* param = f (inv pol) lam.param in
    let* ret   = f pol lam.ret   in
    return (Lam { lam with param; ret })
  | Univ univ ->
    let* param = map_pol_param f univ.param in
    let* ret   = with_param_rigid param (f pol univ.ret) in
    return (Univ { univ with param; ret })
  | Abs abs ->
    let* param = map_pol_param f abs.param in
    let* body  = with_param_rigid param (f pol abs.body) in
    return (Abs { abs with param; body })
  | type' ->
    map (f pol) type'

and map_pol_param f param =
  let* lower = f Pos param.lower in
  let* upper = f Neg param.upper in
  return { param with lower; upper }
