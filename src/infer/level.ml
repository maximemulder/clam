open State

let rec levelize bind (type': Type.type') =
  match type' with
  | Dnf dnf ->
    list_iter (list_iter (levelize_base bind)) dnf
  | Cnf cnf ->
    list_iter (list_iter (levelize_base bind)) cnf

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

let join_level a b =
  Util.option_join a b min

let fold_level f a b =
  let* b = f b in
  return (join_level a b)

let rec get_level (type': Type.type') =
  match type' with
  | Dnf dnf ->
    list_fold (fold_level (list_fold (fold_level get_level_base) None)) None dnf
  | Cnf cnf ->
    list_fold (fold_level (list_fold (fold_level get_level_base) None)) None cnf

and get_level_base type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return None
  | Var var ->
    let* entry = get_var_entry_opt var.bind in
    (match entry with
    | Some entry ->
      return (Some entry.level_low)
    | None ->
      return None)
  | Tuple tuple ->
    list_fold (fold_level get_level) None tuple.elems
  | Record record ->
    map_fold (fold_level get_level_attr) None record.attrs
  | Lam lam ->
    let* param = get_level lam.param in
    let* ret = get_level lam.ret in
    return (join_level param ret)
  | Univ univ ->
    let* param = get_level_param univ.param in
    let* ret = get_level univ.ret in
    return (join_level param ret)
  | Abs abs ->
    let* param = get_level_param abs.param in
    let* body = get_level abs.body in
    return (join_level param body)
  | App app ->
    let* abs = get_level app.abs in
    let* arg = get_level app.arg in
    return (join_level abs arg)

and get_level_attr attr =
  get_level attr.type'

and get_level_param param =
  let* lower = get_level param.lower in
  let* upper = get_level param.upper in
  return (join_level lower upper)
