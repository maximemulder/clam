open Type.System
open Type.Context

type pol = Neg | Pos

let inv pol =
  match pol with
  | Neg -> Pos
  | Pos -> Neg

type pols = {
  neg: Type.type' option;
  pos: Type.type' option;
}

let none = { neg = None; pos = None }

let merge_pols state a b =
  let neg = Util.option_join a.neg b.neg (fun a b -> meet a b state |> fst) in
  let pos = Util.option_join a.pos b.pos (fun a b -> join a b state |> fst) in
  { neg; pos }

(* POLARITY *)

let occurs_neg bind type' =
  List.for_all (List.exists ((=) (Type.Var { bind }))) type'.Type.dnf

let occurs_pos bind type' =
  List.exists ((=) [Type.Var { bind }]) type'.Type.dnf

let get_pos_coocurrences bind type' state =
  List.filter (fun types -> match types with
    | [Type.Var var] when var.bind != bind ->
      true
    | _ ->
      false
  ) type'.Type.dnf
  |> List.map (fun types -> { Type.dnf = [types] })
  |> List.fold_left (fun type' var -> meet type' var state |> fst) Type.top

let get_neg_coocurrences bind type' state =
  List.map (List.filter (fun type' -> match type' with
    | Type.Var var when var.bind != bind ->
      true
    | _ ->
      false
  )) type'.Type.dnf
  |> List.flatten
  |> List.map Type.base
  |> List.fold_left (fun type' var -> join type' var state |> fst) Type.bot

(**
  Get the polarities at which an inference variable occurs in a type, as well
  as the other variables it co-occurs with.
*)
let rec get_pols state bind pol (type': Type.type') =
  let pols_1 = match pol with
  | Pos ->
    if occurs_pos bind type' then
      { neg = None; pos = Some (get_pos_coocurrences bind type' state) }
    else
      none
  | Neg ->
    if occurs_neg bind type' then
      { neg = Some (get_neg_coocurrences bind type' state); pos = None }
    else
      none
  in
  let pols_2 = List.fold_left (merge_pols state) none (List.map (fun types ->
    List.fold_left (merge_pols state) none (List.map (get_pols_base state bind pol) types)
  ) type'.dnf) in
  merge_pols state pols_1 pols_2

and get_pols_base state bind pol type' =
  match type' with
  | Tuple tuple ->
    List.map (get_pols state bind pol) tuple.elems
    |> List.fold_left (merge_pols state) none
  | Record record ->
    Util.NameMap.to_list record.attrs
    |> List.map snd
    |> List.map (fun (attr: Type.attr) -> get_pols_attr state bind pol attr)
    |> List.fold_left (merge_pols state) none
  | Lam lam ->
    merge_pols state
      (get_pols state bind (inv pol) lam.param)
      (get_pols state bind pol lam.ret)
  | Univ univ ->
    let param = get_pols_param state bind pol univ.param in
    let ret, state = with_param_rigid univ.param
      (fun ctx -> get_pols ctx bind pol univ.ret, ctx) state in
    merge_pols state param ret
  | _ ->
    none

and get_pols_attr state bind pol attr =
  get_pols state bind pol attr.type'

and get_pols_param state bind _pol param =
  merge_pols state
    (get_pols state bind Pos param.lower)
    (get_pols state bind Neg param.upper)
