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

open Util.Monad.StateMonad(struct
  type s = int
end)

(* TODO: Factorize *)
let rec rename type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    return type'
  | Tuple tuple ->
    let* elems = list_map rename tuple.elems in
    return (Tuple { elems })
  | Record record ->
    let* attrs = map_map rename_attr record.attrs in
    return (Record { attrs })
  | Lam lam ->
    let* param = rename lam.param in
    let* ret = rename lam.ret in
    return (Lam { param; ret })
  | Univ univ ->
    let* bind, ret = rename_infer univ.param.bind univ.ret in
    let univ = { param = { univ.param with bind }; ret } in
    let* param = rename_param univ.param in
    let* ret = rename univ.ret in
    return (Univ { param; ret })
  | Abs abs ->
    let* param = rename_param abs.param in
    let* body = rename abs.body in
    return (Abs { param; body })
  | App app ->
    let* abs = rename app.abs in
    let* arg = rename app.arg in
    return (App { abs; arg })
  | Rec rec' ->
    let* bind, body = rename_infer rec'.bind rec'.body in
    let rec' = { bind; body } in
    let* body = rename rec'.body in
    return (Rec { bind = rec'.bind; body })
  | Union union ->
    let* left  = rename union.left  in
    let* right = rename union.right in
    return (Union { left; right })
  | Inter inter ->
    let* left  = rename inter.left  in
    let* right = rename inter.right in
    return (Inter { left; right })

and rename_attr attr =
  let* type' = rename attr.type' in
  return { attr with type' }

and rename_param param =
  let* lower = rename param.lower in
  let* upper = rename param.upper in
  return { param with lower; upper }

and rename_infer bind type' i =
  if Str.string_match pattern bind.name 0 then
    let pretty = { Abt.name = index_to_name i } in
    let type' = Type.rename bind pretty type' in
    (pretty, type'), i + 1
  else
    (bind, type'), i

let rename type' =
  rename type' 0 |> fst
