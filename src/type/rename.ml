open Node

let rec rename bind other type' =
  rename_union bind other type'

and rename_union bind other union =
  { union = List.map (rename_inter bind other) union.union }

and rename_inter bind other inter =
  { inter = List.map (rename_base bind other) inter.inter }

and rename_base bind other type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    type'
  | Var var ->
    if var.bind == bind then
      Var { bind = other }
    else
      Var var
  | Tuple tuple ->
    let elems = List.map (rename bind other) tuple.elems in
    Tuple { elems }
  | Record record ->
    let attrs = Util.NameMap.map (rename_attr bind other) record.attrs in
    Record { attrs }
  | Lam lam ->
    let param = rename bind other lam.param in
    let ret = rename bind other lam.ret in
    Lam { param; ret }
  | Univ univ ->
    let param = rename_param bind other univ.param in
    let ret = rename bind other univ.ret in
    Univ { param; ret }
  | Abs abs ->
    let param = rename_param bind other abs.param in
    let body = rename bind other abs.body in
    Abs { param; body }
  | App app ->
    let abs = rename bind other app.abs in
    let arg = rename bind other app.arg in
    App { abs; arg }

and rename_attr bind other attr =
  { attr with type' = rename bind other attr.type' }

and rename_param bind other param =
  { param with bound = rename bind other param.bound }

let rename type' bind other =
  rename bind other type'
