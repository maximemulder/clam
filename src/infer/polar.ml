open Type.System
open Type.Context
open Type.Context.Monad

(** Polarity of a type position. *)
type polarity = Neg | Pos

(** Invert a polarity. *)
let inv pol =
  match pol with
  | Neg -> Pos
  | Pos -> Neg

(** The polarities in which a type variable occurs, along with its co-occurrences. *)
type pols = {
  neg: Type.type' option;
  pos: Type.type' option;
}

(** The none occurence, when a variable does not occur. *)
let none = { neg = None; pos = None }

let merge_pols a b =
  let* neg = option_join meet a.neg b.neg in
  let* pos = option_join join a.pos b.pos in
  return { neg; pos }

type entry = {
  bind: Abt.bind_type;
  lower: Type.type';
  upper: Type.type';
}

(* POLARITY *)

let occurs bind type' =
  List.exists (List.exists ((=) (Type.Var { bind }))) type'.Type.dnf

let get_pos_coocurrences bind type' base =
  let vars = List.filter (fun types -> match types with
    | [Type.Var var] when var.bind != bind ->
      true
    | _ ->
      false
  ) type'.Type.dnf
  |> List.map (fun types -> { Type.dnf = [types] }) in
  list_fold (fun type' var -> meet type' var) base vars

let get_neg_coocurrences bind type' base =
  let vars = List.map (List.filter (fun type' -> match type' with
    | Type.Var var when var.bind != bind ->
      true
    | _ ->
      false
  )) type'.Type.dnf
  |> List.flatten
  |> List.map Type.base in
  list_fold (fun type' var -> join type' var) base vars

(**
  Get the polarities at which an inference variable occurs in a type, as well
  as the other variables it co-occurs with.
*)
let rec get_pols entry pol type' =
  let* pols_1 = match pol with
  | Pos ->
    if occurs entry.bind type' then
      let* pos = get_pos_coocurrences entry.bind type' entry.lower in
      return { neg = None; pos = Some pos }
    else
      return none
  | Neg ->
    if occurs entry.bind type' then
      let* neg = get_neg_coocurrences entry.bind type' entry.upper in
      return { neg = Some neg; pos = None }
    else
      return none
  in
  let* tmp = list_map (fun types ->
    let* tmp = list_map (get_pols_base entry pol) types in
    list_fold (merge_pols) none tmp
  ) type'.dnf in
  let* pols_2 = list_fold merge_pols none tmp in
  merge_pols pols_1 pols_2

and get_pols_base entry pol type'  =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return none
  | Tuple tuple ->
    let* elems = list_map (get_pols entry pol) tuple.elems in
    list_fold merge_pols none elems
  | Record record ->
    let attrs = Util.NameMap.to_list record.attrs
    |> List.map snd in
    let* attrs = list_map (fun (attr: Type.attr) -> get_pols_attr entry pol attr) attrs in
    list_fold merge_pols none attrs
  | Lam lam ->
    let* param = get_pols entry (inv pol) lam.param in
    let* ret   = get_pols entry pol lam.ret in
    merge_pols param ret
  | Univ univ ->
    let* param = get_pols_param entry pol univ.param in
    let* ret   = with_param_rigid univ.param (get_pols entry pol univ.ret) in
    merge_pols param ret
  | _ ->
    return none

and get_pols_attr bind pol attr =
  get_pols bind pol attr.type'

and get_pols_param entry _pol param  =
  let* lower = get_pols entry Pos param.lower in
  let* upper = get_pols entry Neg param.upper in
  merge_pols lower upper

let get_pols (fresh: fresh) pol type' =
  let* lower = get_pos_coocurrences fresh.bind fresh.lower Type.top in
  let* upper = get_neg_coocurrences fresh.bind fresh.upper Type.bot in
  get_pols { bind = fresh.bind; lower; upper } pol type'
