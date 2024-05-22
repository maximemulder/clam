open Node

(* TYPE MAP *)

let rec map f type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    type'
  | Tuple tuple ->
    let elems = List.map f tuple.elems in
    Tuple { elems }
  | Record record ->
    let attrs = Util.NameMap.map (map_attr f) record.attrs in
    Record { attrs }
  | Lam lam ->
    let param = f lam.param in
    let ret   = f lam.ret   in
    Lam { param; ret }
  | Univ univ ->
    let param = map_param f univ.param in
    let ret   = f univ.ret   in
    Univ { param; ret }
  | Abs abs ->
    let param = map_param f abs.param in
    let body  = f abs.body  in
    Abs { param; body }
  | App app ->
    let abs = f app.abs in
    let arg = f app.arg in
    App { abs; arg }
  | Rec rec' ->
    let body = f rec'.body in
    Rec { bind = rec'.bind; body }
  | Union union ->
    let left  = f union.left  in
    let right = f union.right in
    Union { left; right }
  | Inter inter ->
    let left  = f inter.left  in
    let right = f inter.right in
    Inter { left; right }

and map_attr f attr =
  let type' = f attr.type' in
  { attr with type' }

and map_param f param =
  let lower = f param.lower in
  let upper = f param.upper in
  { param with lower; upper }

(* TYPE FOLD *)

let rec fold f1 f2 acc type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    acc
  | Tuple tuple ->
    List.map f1 tuple.elems
    |> List.fold_left f2 acc
  | Record record ->
    Util.NameMap.map (fold_attr f1) record.attrs
    |> Util.NameMap.to_list
    |> List.map snd
    |> List.fold_left f2 acc
  | Lam lam ->
    let param = f1 lam.param in
    let ret   = f1 lam.ret   in
    f2 param ret
  | Univ univ ->
    let param = fold_param f1 f2 univ.param in
    let ret   = f1 univ.ret in
    f2 param ret
  | Abs abs ->
    let param = fold_param f1 f2 abs.param in
    let body  = f1 abs.body in
    f2 param body
  | App app ->
    let abs = f1 app.abs in
    let arg = f1 app.arg in
    f2 abs arg
  | Rec rec' ->
    f1 rec'.body
  | Union union ->
    let left  = f1 union.left  in
    let right = f1 union.right in
    f2 left right
  | Inter inter ->
    let left  = f1 inter.left  in
    let right = f1 inter.right in
    f2 left right

and fold_attr f1 attr =
  f1 attr.type'

and fold_param f1 f2 param =
  let lower = f1 param.lower in
  let upper = f1 param.upper in
  f2 lower upper

(* POLAR TYPE FOLD *)

open Pol

let rec fold_pol f1 f2 acc pol type' =
  match type' with
  | Lam lam ->
    let param = f1 (inv pol) lam.param in
    let ret   = f1 pol lam.ret   in
    f2 param ret
  | Univ univ ->
    let param = fold_pol_param f1 f2 univ.param in
    let ret   = f1 pol univ.ret in
    f2 param ret
  | Abs abs ->
    let param = fold_pol_param f1 f2 abs.param in
    let body  = f1 pol abs.body in
    f2 param body
  | type' ->
    fold (f1 pol) f2 acc type'

and fold_pol_param f1 f2 param =
  let lower = f1 Pos param.lower in
  let upper = f1 Neg param.upper in
  f2 lower upper
