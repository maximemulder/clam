open Node
open Context
open Context.Monad

let rec appears bind type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return false
  | Var var ->
    return (var.bind == bind)
  | Tuple tuple ->
    list_any (appears bind) tuple.elems
  | Record record ->
    map_any (appears_attr bind) record.attrs
  | Lam lam ->
    let* param = appears bind lam.param in
    let* ret   = appears bind lam.ret   in
    return (param || ret)
  | Univ univ ->
    let* param = appears_param bind univ.param in
    let* ret   = with_param_rigid univ.param (appears bind univ.ret) in
    return (param || ret)
  | Abs abs ->
    let* param = appears_param bind abs.param in
    let* body  = with_param_rigid abs.param (appears bind abs.body) in
    return (param || body)
  | App app ->
    let* abs = appears bind app.abs in
    let* arg = appears bind app.arg in
    return (abs || arg)
  | Union union ->
    let* left  = appears bind union.left  in
    let* right = appears bind union.right in
    return (left || right)
  | Inter inter ->
    let* left  = appears bind inter.left  in
    let* right = appears bind inter.right in
    return (left || right)

and appears_attr bind attr =
  appears bind attr.type'

and appears_param bind param =
  let* lower = appears bind param.lower in
  let* upper = appears bind param.upper in
  return (lower || upper)
