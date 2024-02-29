open Type

let pattern = Str.regexp "'[0-9]+"

let rec index_to_name index =
  let quotient = index / 26 in
  let remainder = index mod 26 in
  let base = Char.code 'A' in
  let char = Char.chr (base + remainder) in
  let string = String.make 1 char in
  if quotient = 0 then
    string
  else
    index_to_name (quotient - 1) ^ string

let index_to_name index =
  "'" ^ index_to_name index

let rec rename index type' =
  rename_union index type'

and rename_union index union =
  { union = List.map (rename_inter index) union.union }

and rename_inter index inter =
  { inter = List.map (rename_base index) inter.inter }

and rename_base index type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    type'
  | Tuple tuple ->
    let elems = List.map (rename index) tuple.elems in
    Tuple { elems }
  | Record record ->
    let attrs = Util.NameMap.map (rename_attr index) record.attrs in
    Record { attrs }
  | Lam lam ->
    let param = rename index lam.param in
    let ret = rename index lam.ret in
    Lam { param; ret }
  | Univ univ ->
    if Str.string_match pattern univ.param.bind.name 0 then
      let param = rename_param (index + 1) univ.param in
      let ret = rename (index + 1) univ.ret in
      let bind = { Abt.name = index_to_name index } in
      let ret = Type.rename ret param.bind bind in
      let param = { param with bind } in
      Univ { param; ret }
    else
      let param = rename_param index univ.param in
      let ret = rename index univ.ret in
      Univ { param; ret }
  | Abs abs ->
    let param = rename_param index abs.param in
    let body = rename index abs.body in
    Abs { param; body }
  | App app ->
    let abs = rename index app.abs in
    let arg = rename index app.arg in
    App { abs; arg }

and rename_attr index attr =
  { attr with type' = rename index attr.type' }

and rename_param index param =
  { param with bound = rename index param.bound }

let rename type' =
  rename 0 type'
