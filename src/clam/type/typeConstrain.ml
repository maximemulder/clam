open TypeState

let unwrap_base type' = List.nth (List.nth (type'.Type.union) 0).inter 0

let rec constrain sub sup =
  match unwrap_base sub, unwrap_base sup with
  | Type.Var sub_var, Type.Var sup_var ->
    let* sub_level = get_level sub_var.bind in
    let* sup_level = get_level sup_var.bind in
    if sub_level < sup_level then
      constrain_var_sup sup_var sub
    else
    if sub_level > sup_level then
      constrain_var_sub sub_var sup
    else
      return ()
    (* let* () = constrain_var_sup sup_var sub in
    let* () = constrain_var_sub sub_var sup in
    return () *)
  | Type.Var sub_var, _ ->
    constrain_var_sub sub_var sup
  | _, Type.Var sup_var ->
    constrain_var_sup sup_var sub
  | Type.AbsExpr sub_abs, Type.AbsExpr sup_abs ->
    let* () = constrain sup_abs.param sub_abs.param in
    let* () = constrain sub_abs.ret sup_abs.ret in
    return ()
  | _, _ ->
    return ()

and constrain_var_sub sub_var sup =
  let* () = set_upper_bound sub_var.bind sup in
  let* sub_lower = get_lower_bound sub_var.bind in
  constrain sub_lower sup

and constrain_var_sup sup_var sub =
  let* () = set_lower_bound sup_var.bind sub in
  let* sup_upper = get_upper_bound sup_var.bind in
  constrain sub sup_upper
