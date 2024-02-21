open State

let rec occurs_directly bind (type': Type.type') =
  occurs_directly_union bind type'

and occurs_directly_union bind union =
  List.exists (occurs_directly_inter bind) union.union

and occurs_directly_inter bind inter =
  List.exists (occurs_directly_base bind) inter.inter

and occurs_directly_base bind type' =
  match type' with
  | Var var ->
    var.bind == bind
  | _ ->
    false

type pol = Pos | Neg

let inv pol =
  match pol with
  | Pos -> Neg
  | Neg -> Pos

let rec occurs (type': Type.type') bind =
  list_any (fun type' -> occurs_inter type' bind) type'.union

and occurs_inter (inter: Type.inter) bind =
  list_any (fun type' -> occurs_base type' bind) inter.inter

and occurs_base (type': Type.base) bind =
  match type' with
  | Var var ->
    occurs_bind var bind
  | Tuple tuple ->
    list_any (fun elem -> occurs elem bind) tuple.elems
  | Record record ->
    Util.NameMap.to_list record.attrs
    |> List.map snd
    |> list_any (fun (attr: Type.attr) -> occurs attr.type' bind)
  | Lam lam ->
    let* param = occurs lam.param bind in
    let* ret = occurs lam.ret bind in
    return (param || ret)
  | Univ univ ->
    let* param = occurs_param univ.param bind in
    let* ret = with_type univ.param.bind univ.param.bound (occurs univ.ret bind) in
    return (param || ret)
  | _ ->
    return false

and occurs_bind var bind =
  if var.bind == bind then
    return true
  else
    return false

and occurs_param param bind =
  occurs param.bound bind

(* EXTRACT *)

(*
  Extracts co-occurences. This algorithm is an ugly proof-of-concept for now but
  is should easily be simplificable in the future.
*)

let extract_pos bind types =
  List.map (fun type' -> match type' with Type.Var var when var.bind != bind -> Some var.bind | _ -> None) types
  |> List.filter Option.is_some
  |> List.map Option.get

let extract_neg bind types =
  List.map (fun type' -> match type' with { Type.inter = [Type.Var var] } when var.bind != bind -> Some var.bind | _ -> None) types
  |> List.filter Option.is_some
  |> List.map Option.get

let merge_occs a b =
  List.filter (fun a -> List.exists (fun b -> a == b) b) a

(* POLARITY *)

(* Returns the polarities in which a type variable occurs in a type, used to know whether to
  inline or quantify this variable *)

type pols = {
  pos: Abt.bind_type list option;
  neg: Abt.bind_type list option;
}

let none = {
  pos = None;
  neg = None;
}

let from_pol pol neg_occs pol_occs =
  match pol with
  | Neg ->
    {
      pos = None;
      neg = Some neg_occs;
    }
  | Pos ->
    {
      pos = Some pol_occs;
      neg = None;
    }

let from_pols left right =
  {
    pos = Util.option_join left.pos right.pos merge_occs;
    neg = Util.option_join left.neg right.neg merge_occs;
  }

let rec get_pols (type': Type.type') bind pol =
  get_pols_union type' bind pol

and get_pols_union union bind pol =
  let neg_occs = extract_neg bind union.union in
  let types = List.map (fun type' -> get_pols_inter type' bind pol neg_occs) union.union in
  Util.list_reduce from_pols types

and get_pols_inter inter bind pol neg_occs =
  let pol_occs = extract_pos bind inter.inter in
  let types = List.map (fun type' -> get_pols_base type' bind pol neg_occs pol_occs) inter.inter in
  Util.list_reduce from_pols types

and get_pols_base type' bind pol neg_occs pol_occs =
  match type' with
  | Type.Var var when var.bind == bind ->
    from_pol pol neg_occs pol_occs
  | Tuple tuple ->
    List.map (fun elem -> get_pols elem bind pol) tuple.elems
    |> List.fold_left from_pols none
  | Record record ->
    Util.NameMap.to_list record.attrs
    |> List.map snd
    |> List.map (fun (attr: Type.attr) -> get_pols_attr attr bind pol)
    |> List.fold_left from_pols none
  | Lam lam ->
    from_pols
      (get_pols lam.param bind (inv pol))
      (get_pols lam.ret bind pol)
  | Univ univ ->
    from_pols
      (get_pols_param univ.param bind pol)
      (get_pols univ.ret bind pol)
  | _ ->
    none

and get_pols_attr attr bind pol =
  get_pols attr.type' bind pol

and get_pols_param param bind pol =
  get_pols param.bound bind (inv pol)
