open TypePolar
open TypeState

let rec inline (type': Type.type') bind pol =
  inline_union type' bind pol

and inline_union union bind pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> inline_inter type' bind pol) union.union in
  return (Utils.list_reduce (TypeSystem.join ctx) types)

and inline_inter inter bind pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> inline_base type' bind pol) inter.inter in
  return (Utils.list_reduce (TypeSystem.meet ctx) types)

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
  | AbsExpr abs ->
    let* param = inline abs.param bind (inv pol) in
    let* ret = inline abs.ret bind pol in
    return (Type.abs_expr param ret)
  | AbsTypeExpr abs ->
    let* param: Type.param  = inline_param abs.param bind pol in
    let* ret = with_type param.bind param.bound (inline abs.ret bind pol) in
    return (Type.abs_type_expr param ret)
  | _ ->
    return (Type.base type')

and inline_attr attr bind pol =
  let* type' = inline attr.type' bind pol in
  return { attr with type' }

and inline_param param bind pol =
  let* bound = inline param.bound bind (inv pol) in
  return { param with bound }

let inline_state bind state =
  let vars = List.map (fun entry -> { entry with
    upper = fst(inline entry.upper bind Pos state);
    lower = fst(inline entry.lower bind Neg state);
  }) state.vars in
  let exprs =  List.map (fun entry -> {
    entry with type' = fst(inline entry.type' bind Neg state)
  })  state.exprs in
  (), { state with vars; exprs }
