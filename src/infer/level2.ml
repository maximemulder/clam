open State

let rec levelize level (type': Type.type') =
  levelize_union level type'

and levelize_union level union =
  list_iter (levelize_inter level) union.union

and levelize_inter level inter =
  list_iter (levelize_base level) inter.inter

and levelize_base level type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return ()
  | Var var ->
    let* entry = get_var_entry_opt var.bind in
    (match entry with
    | Some entry ->
      if entry.level_low > level then
        let* () = update_var_entry var.bind (fun entry -> { entry with level_low = level }) in
        let* () = levelize level entry.lower in
        let* () = levelize level entry.upper in
        return ()
      else
        return ()
    | None ->
      return ())
  | Tuple tuple ->
    list_iter (levelize level) tuple.elems
  | Record record ->
    map_iter (levelize_attr level) record.attrs
  | Lam lam ->
    let* () = levelize level lam.param in
    let* () = levelize level lam.ret in
    return ()
  | Univ univ ->
    let* () = levelize_param level univ.param in
    let* () = levelize level univ.ret in
    return ()
  | Abs abs ->
    let* () = levelize_param level abs.param in
    let* () = levelize level abs.body in
    return ()
  | App app ->
    let* () = levelize level app.abs in
    let* () = levelize level app.arg in
    return ()

and levelize_attr level attr =
  levelize level attr.type'

and levelize_param level param =
  let* () = levelize level param.lower in
  let* () = levelize level param.upper in
  return ()
