open Type
open Type.Context
open Type.Context.Monad

(** The polarities at which a type variable occurs. *)
type occs = {
  neg: bool;
  pos: bool;
}

(** The empty occurrence, when a variable does not occur in a type. *)
let occs_none = { neg = false; pos = false }

(** The polar occurrence, when a variable occurs at a given polarity in a type. *)
let occs_pol pol =
  match pol with
  | Neg ->
    { neg = true; pos = false }
  | Pos ->
    { neg = false; pos = true }

let merge_occs left right =
  let neg = left.neg || right.neg in
  let pos = left.pos || right.pos in
  return { neg; pos }

let rec occurs bind pol type'  =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return occs_none
  | Var var ->
    if var.bind == bind then
      return (occs_pol pol)
    else
      return occs_none
  | Tuple tuple ->
    let* elems = list_map (occurs bind pol) tuple.elems in
    list_fold merge_occs occs_none elems
  | Record record ->
    let attrs = Util.NameMap.to_list record.attrs
    |> List.map snd in
    let* attrs = list_map (fun (attr: Type.attr) -> occurs_attr bind pol attr) attrs in
    list_fold merge_occs occs_none attrs
  | Lam lam ->
    let* param = occurs bind (inv pol) lam.param in
    let* ret   = occurs bind pol lam.ret in
    merge_occs param ret
  | Univ univ ->
    let* param = occurs_param bind univ.param in
    let* ret   = with_param_rigid univ.param (occurs bind pol univ.ret) in
    merge_occs param ret
  | Abs abs ->
    let* param = occurs_param bind abs.param in
    let* body  = with_param_rigid abs.param (occurs bind pol abs.body) in
    merge_occs param body
  | App app ->
    let* abs = occurs bind pol app.abs in
    let* arg = occurs bind pol app.arg in
    merge_occs abs arg
  | Union union ->
    let* left  = occurs bind pol union.left  in
    let* right = occurs bind pol union.right in
    merge_occs left right
  | Inter inter ->
    let* left  = occurs bind pol inter.left  in
    let* right = occurs bind pol inter.right in
    merge_occs left right

and occurs_attr bind pol attr =
  occurs bind pol attr.type'

and occurs_param bind param  =
  let* lower = occurs bind Pos param.lower in
  let* upper = occurs bind Neg param.upper in
  merge_occs lower upper
