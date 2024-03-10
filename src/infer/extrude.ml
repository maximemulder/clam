open State

let rec extrude (type': Type.type') =
  extrude_union type'

and extrude_union union =
  let* vars = list_map extrude_inter union.union in
  return (List.concat vars)

and extrude_inter inter =
  let* vars = list_map extrude_base inter.inter in
  return (List.concat vars)

and extrude_base type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return []
  | Var var ->
    let* entry = get_var_entry_opt var.bind in
    (match entry with
    | Some entry ->
      let* state = get_state in
      if entry.level_low >= state.level then
        return [var.bind]
      else
        return []
    | None ->
      return [])
  | Tuple tuple ->
    let* elems = list_map extrude tuple.elems in
    return (List.concat elems)
  | Record record ->
    let* attrs = map_map extrude_attr record.attrs in
    let attrs = Util.NameMap.bindings attrs |> List.map snd in
    return (List.concat (attrs))
  | Lam lam ->
    let* param = extrude lam.param in
    let* ret = extrude lam.ret in
    return (List.append param ret)
  | Univ univ ->
    let* param = extrude_param univ.param in
    let* ret = extrude univ.ret in
    return (List.append param ret)
  | Abs abs ->
    let* param = extrude_param abs.param in
    let* body = extrude abs.body in
    return (List.append param body)
  | App app ->
    let* abs = extrude app.abs in
    let* arg = extrude app.arg in
    return (List.append abs arg)

and extrude_attr attr =
  extrude attr.type'

and extrude_param param =
  let* lower = extrude param.lower in
  let* upper = extrude param.upper in
  return (List.append lower upper)
