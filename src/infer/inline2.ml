open Polar
open State

let rec inline bind pol (type': Type.type')=
  inline_union bind pol type'

and inline_union bind pol union =
  let* ctx = get_context in
  let* types = list_map (inline_inter bind pol) union.union in
  return (Util.list_reduce (Type.System.join ctx) types)

and inline_inter bind pol inter =
  let* ctx = get_context in
  let* types = list_map (inline_base bind pol) inter.inter in
  return (Util.list_reduce (Type.System.meet ctx) types)

and inline_base bind pol type' =
  match type' with
  | Type.Var var when var.bind == bind -> (
    match pol with
    | Pos ->
      get_var_upper var.bind
    | Neg ->
      get_var_lower var.bind
    )
  | Tuple tuple ->
    let* elems = list_map (inline bind pol) tuple.elems in
    return (Type.tuple elems)
  | Record record ->
    let* attrs = map_map (inline_attr bind pol) record.attrs in
    return (Type.record attrs)
  | Lam lam ->
    let* param = inline bind (inv pol) lam.param in
    let* ret = inline bind pol lam.ret in
    return (Type.lam param ret)
  | Univ univ ->
    let* param: Type.param = inline_param univ.param bind pol in
    let* ret = with_type param.bind param.lower param.upper (inline bind pol univ.ret) in
    return (Type.univ param ret)
  | _ ->
    return (Type.base type')

and inline_attr bind pol attr =
  let* type' = inline bind pol attr.type' in
  return { attr with type' }

and inline_param param bind pol =
  let* lower = inline bind pol param.lower in
  let* upper = inline bind (inv pol) param.upper in
  return { param with lower; upper }
