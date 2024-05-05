open Node

type entry = {
  (* The original binding to substitute *)
  bind:  Abt.bind_type;
  (* The binding replacing the original binding *)
  other: Abt.bind_type;
}

let rec rename entry type' =
  match type' with
  | Var var when var.bind == entry.bind ->
    Var { bind = entry.other }
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    type'
  | Union union ->
    let left  = rename entry union.left  in
    let right = rename entry union.right in
    Union { left; right }
  | Inter inter ->
    let left  = rename entry inter.left  in
    let right = rename entry inter.right in
    Union { left; right }
  | Tuple tuple ->
    let elems = List.map (rename entry) tuple.elems in
    Tuple { elems }
  | Record record ->
    let attrs = Util.NameMap.map (rename_attr entry) record.attrs in
    Record { attrs }
  | Lam lam ->
    let param = rename entry lam.param in
    let ret   = rename entry lam.ret   in
    Lam { param; ret }
  | Univ univ ->
    let param = rename_param entry univ.param in
    let ret   = rename       entry univ.ret   in
    Univ { param; ret }
  | Abs abs ->
    let param = rename_param entry abs.param in
    let body  = rename       entry abs.body  in
    Abs { param; body }
  | App app ->
    let abs = rename entry app.abs in
    let arg = rename entry app.arg in
    App { abs; arg }

and rename_attr entry attr =
  { attr with type' = rename entry attr.type' }

and rename_param entry param =
  let lower = rename entry param.lower in
  let upper = rename entry param.upper in
  { param with lower; upper }

let rename bind other type' =
  rename { bind; other } type'
