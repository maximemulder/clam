open TypeState

type pol = Pos | Neg

let inv pol =
  match pol with
  | Pos -> Neg
  | Neg -> Pos

let exists_in_lower_bounds bind state =
  let level, _ = get_level bind state in
  List.exists (fun (entry: entry_bounds) -> if entry.level < level then
    TypeUtils.contains entry.upper bind || TypeUtils.contains entry.lower bind
  else
    false
  ) state.bounds, state

let rec get_type (type': Type.type') pol =
  get_union type' pol

and get_union union pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> get_inter type' pol) union.union in
  return (Utils.list_reduce (TypeSystem.join ctx) types)

and get_inter inter pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> get_base type' pol) inter.inter in
  return (Utils.list_reduce (TypeSystem.meet ctx) types)

and get_base type' pol =
  match type' with
  | Type.Var var -> (
    match pol with
    | Pos ->
      let* bound = get_upper_bound var.bind in
      let* cond = exists_in_lower_bounds var.bind in
      if not cond then
        get_type bound Pos
      else
        return bound
    | Neg ->
      let* bound = get_lower_bound var.bind in
      let* cond = exists_in_lower_bounds var.bind in
      if not cond then
        get_type bound Neg
      else
        return bound
  )
  | AbsExpr abs ->
    let* param = get_type abs.param (inv pol) in
    let* ret = get_type abs.ret pol in
    return (Type.base (Type.AbsExpr { param; ret }))
  | _ ->
    return (Type.base type')

let get_pos type' = get_type type' Pos

let get_neg type' = get_type type' Neg
