open State

let rec levelize bind (type': Type.type') =
  levelize_union bind type'

and levelize_union bind union =
  list_iter (levelize_inter bind) union.union

and levelize_inter bind inter =
  list_iter (levelize_base bind) inter.inter

and levelize_base bind type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return ()
  | Var var ->
    let* entry = get_var_entry_opt var.bind in
    (match entry with
    | Some entry ->
      let* level = get_var_entry bind in
      let level = level.level_low in
      if entry.level_low > level then
        let* () = reorder level entry.bind in
        let* () = levelize var.bind entry.lower in
        let* () = levelize var.bind entry.upper in
        return ()
      else
        return ()
    | None ->
      return ())
  | Tuple tuple ->
    list_iter (levelize bind) tuple.elems
  | Record record ->
    map_iter (levelize_attr bind) record.attrs
  | Lam lam ->
    let* () = levelize bind lam.param in
    let* () = levelize bind lam.ret in
    return ()
  | Univ univ ->
    let* () = levelize_param bind univ.param in
    let* () = levelize bind univ.ret in
    return ()
  | Abs abs ->
    let* () = levelize_param bind abs.param in
    let* () = levelize bind abs.body in
    return ()
  | App app ->
    let* () = levelize bind app.abs in
    let* () = levelize bind app.arg in
    return ()

and levelize_attr bind attr =
  levelize bind attr.type'

and levelize_param bind param =
  let* () = levelize bind param.lower in
  let* () = levelize bind param.upper in
  return ()
