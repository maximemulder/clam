open TypeState

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
    Utils.NameMap.to_list record.attrs
    |> List.map snd
    |> list_any (fun (attr: Type.attr) -> occurs attr.type' bind)
  | AbsExpr abs ->
    let* param = occurs abs.param bind in
    let* ret = occurs abs.ret bind in
    return (param || ret)
  | AbsTypeExpr abs ->
    let* param = occurs_param abs.param bind in
    let* ret = with_type abs.param.bind abs.param.bound (occurs abs.ret bind) in
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

(* POLARITY *)

(* Returns the polarities in which a type variable occurs in a type, used to know whether to
  inline or quantify this variable *)

type pols = {
  pos: bool;
  neg: bool;
}

let none = {
  pos = false;
  neg = false;
}

let from_pol pol =
  match pol with
  | Pos ->
    {
      pos = true;
      neg = false;
    }
  | Neg ->
    {
      pos = false;
      neg = true;
    }

let from_pols left right =
  {
    pos = left.pos || right.pos;
    neg = left.neg || right.neg;
  }

let rec get_pols (type': Type.type') bind pol =
  get_pols_union type' bind pol

and get_pols_union union bind pol =
  let types = List.map (fun type' -> get_pols_inter type' bind pol) union.union in
  Utils.list_reduce from_pols types

and get_pols_inter inter bind pol =
  let types = List.map (fun type' -> get_pols_base type' bind pol) inter.inter in
  Utils.list_reduce from_pols types

and get_pols_base type' bind pol =
  match type' with
  | Type.Var var when var.bind == bind ->
    from_pol pol
  | Tuple tuple ->
    List.map (fun elem -> get_pols elem bind pol) tuple.elems
    |> List.fold_left from_pols none
  | Record record ->
    Utils.NameMap.to_list record.attrs
    |> List.map snd
    |> List.map (fun (attr: Type.attr) -> get_pols_attr attr bind pol)
    |> List.fold_left from_pols none
  | AbsExpr abs ->
    from_pols
      (get_pols abs.param bind (inv pol))
      (get_pols abs.ret bind pol)
  | AbsTypeExpr abs ->
    from_pols
      (get_pols_param abs.param bind pol)
      (get_pols abs.ret bind pol)
  | _ ->
    none

and get_pols_attr attr bind pol =
  get_pols attr.type' bind pol

and get_pols_param param bind pol =
  get_pols param.bound bind (inv pol)
