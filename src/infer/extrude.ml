open State
open Polar

let rec extrude (type': Type.type') (pol: pol) =
  extrude_union type' pol

and extrude_union union pol =
  let* types = map_list (fun type' -> extrude_inter type' pol) union.union in
  return (List.concat types)

and extrude_inter inter pol =
  let* types = map_list (fun type' -> extrude_base type' pol) inter.inter in
  return (List.concat types)

and extrude_base type' pol =
  match type' with
  | Type.Var var ->
    let* entry = get_var_entry_opt var.bind in (
    match entry with
    | Some entry ->
      let* state = get_state in
      if entry.level >= state.level then
        return [var.bind]
      else
        return []
    | None ->
      return [])
  | Tuple tuple ->
    let* elems = map_list (fun elem -> extrude elem pol) tuple.elems in
    return (List.concat elems)
  | Record record ->
    let* attrs = map_map (fun attr -> extrude_attr attr pol) record.attrs in
    return (attrs |> Util.NameMap.to_list |> List.map snd |> List.concat)
  | Lam lam ->
    let* param = extrude lam.param (inv pol) in
    let* ret = extrude lam.ret pol in
    return (List.append param ret)
  | Univ univ ->
    let* param = extrude_param univ.param pol in
    let* ret = extrude univ.ret pol in
    return (List.append param ret)
  | _ ->
    return []

and extrude_attr attr pol =
  extrude attr.type' pol

and extrude_param param pol =
  extrude param.bound (inv pol)
