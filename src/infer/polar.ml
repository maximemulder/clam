open State

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
  let neg = Util.option_join a.neg b.neg (fun a b -> join a b state |> fst) in
  let pos = Util.option_join a.pos b.pos (fun a b -> meet a b state |> fst) in
  { neg; pos }

(* EXTRACT *)

(*
  Extracts co-occurences. This algorithm is an ugly proof-of-concept for now but
  is should easily be simplificable in the future.
*)

let has_var bind types =
  List.exists (fun type' -> type'.Type.inter = [Type.Var { bind }]) types

let get_vars state bind types =
  List.filter (fun type' -> match type'.Type.inter with
    | [Type.Var var] when is_infer var.bind state && var.bind != bind ->
      true
    | _ ->
      false
  ) types |> List.map (fun type' -> { Type.union = [type'] })

let extract_pos state bind types =
  if has_var bind types then
    let vars = get_vars state bind types in
    let var = List.fold_left (fun type' var -> join type' var state |> fst) Type.bot vars in
    { neg = None; pos = Some var }
  else
    none

let has_var bind types =
  List.exists (fun type' -> type' = Type.Var { bind }) types

let get_vars state bind types =
  List.filter (fun type' -> match type' with
    | Type.Var var when is_infer var.bind state && var.bind != bind ->
      true
    | _ ->
      false
  ) types |> List.map Type.base

let extract_neg state bind types =
  if has_var bind types then
    let vars = get_vars state bind types in
    let var = List.fold_left (fun type' var -> meet type' var state |> fst) Type.top vars in
    { neg = Some var; pos = None }
  else
    none

(* POLARITY *)

(**
  Get the polarities at which an inference variable occurs in a type, as well
  as the types it co-occurs with.
*)
let rec get_pols state bind pol (type': Type.type') =
  get_pols_union state bind pol type'

and get_pols_union state bind pol union =
  let pols = if pol = Pos then extract_pos state bind union.union else none in
  let types = List.map (get_pols_inter state bind pol) union.union in
  List.fold_left (merge_pols state) pols types

and get_pols_inter state bind pol inter =
  let pols = if pol = Neg then extract_neg state bind inter.inter else none in
  let types = List.map (get_pols_base state bind pol) inter.inter in
  List.fold_left (merge_pols state) pols types

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
    merge_pols state
      (get_pols_param state bind pol univ.param)
      (get_pols state bind pol univ.ret)
  | _ ->
    none

and get_pols_attr state bind pol attr =
  get_pols state bind pol attr.type'

and get_pols_param state bind _pol param =
  merge_pols state
    (get_pols state bind Pos param.lower)
    (get_pols state bind Neg param.upper)
