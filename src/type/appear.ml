open Node

let rec appears bind type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    false
  | Var var ->
    var.bind == bind
  | Tuple tuple ->
    List.exists (appears bind) tuple.elems
  | Record record ->
    record.attrs
    |> Util.NameMap.to_list
    |> List.map snd
    |> List.exists (appears_attr bind)
  | Lam lam ->
    let param = appears bind lam.param in
    let ret   = appears bind lam.ret   in
    param || ret
  | Univ univ ->
    let param = appears_param bind univ.param in
    let ret   = appears       bind univ.ret   in
    param || ret
  | Abs abs ->
    let param = appears_param bind abs.param in
    let body  = appears       bind abs.body  in
    param || body
  | App app ->
    let abs = appears bind app.abs in
    let arg = appears bind app.arg in
    abs || arg
  | Union union ->
    let left  = appears bind union.left  in
    let right = appears bind union.right in
    left || right
  | Inter inter ->
    let left  = appears bind inter.left  in
    let right = appears bind inter.right in
    left || right

and appears_attr bind attr =
  appears bind attr.type'

and appears_param bind param =
  let lower = appears bind param.lower in
  let upper = appears bind param.upper in
  lower || upper
