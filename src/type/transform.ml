open Context
open Context.Monad
open Node

(* TYPE MAP *)

let rec map f type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    return type'
  | Tuple tuple ->
    let* elems = list_map f tuple.elems in
    return (Tuple { elems })
  | Record record ->
    let* attrs = map_map (map_attr f) record.attrs in
    return (Record { attrs })
  | Lam lam ->
    let* param = f lam.param in
    let* ret   = f lam.ret   in
    return (Lam { param; ret })
  | Univ univ ->
    let* param = map_param f univ.param in
    let* ret   = with_param_rigid param (f univ.ret) in
    return (Univ { param; ret })
  | Abs abs ->
    let* param = map_param f abs.param in
    let* body  = with_param_rigid param (f abs.body) in
    return (Abs { param; body })
  | App app ->
    let* abs = f app.abs in
    let* arg = f app.arg in
    return (App { abs; arg })
  | Union union ->
    let* left  = f union.left  in
    let* right = f union.right in
    return (Union { left; right })
  | Inter inter ->
    let* left  = f inter.left  in
    let* right = f inter.right in
    return (Inter { left; right })

and map_attr f attr =
  let* type' = f attr.type' in
  return { attr with type' }

and map_param f param =
  let* lower = f param.lower in
  let* upper = f param.upper in
  return { param with lower; upper }
