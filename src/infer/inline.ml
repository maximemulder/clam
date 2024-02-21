open Polar
open State

let rec inline (type': Type.type') bind pol =
  inline_union type' bind pol

and inline_union union bind pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> inline_inter type' bind pol) union.union in
  return (Util.list_reduce (Type.System.join ctx) types)

and inline_inter inter bind pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> inline_base type' bind pol) inter.inter in
  return (Util.list_reduce (Type.System.meet ctx) types)

and inline_base type' bind pol =
  match type' with
  | Type.Var var when var.bind == bind -> (
    match pol with
    | Pos ->
      get_var_upper var.bind
    | Neg ->
      get_var_lower var.bind
    )
  | Tuple tuple ->
    let* elems = map_list (fun elem -> inline elem bind pol) tuple.elems in
    return (Type.tuple elems)
  | Record record ->
    let* attrs = map_map (fun attr -> inline_attr attr bind pol) record.attrs in
    return (Type.record attrs)
  | Lam lam ->
    let* param = inline lam.param bind (inv pol) in
    let* ret = inline lam.ret bind pol in
    return (Type.lam param ret)
  | Univ univ ->
    let* param: Type.param  = inline_param univ.param bind pol in
    let* ret = with_type param.bind param.bound (inline univ.ret bind pol) in
    return (Type.univ param ret)
  | _ ->
    return (Type.base type')

and inline_attr attr bind pol =
  let* type' = inline attr.type' bind pol in
  return { attr with type' }

and inline_param param bind pol =
  let* bound = inline param.bound bind (inv pol) in
  return { param with bound }
