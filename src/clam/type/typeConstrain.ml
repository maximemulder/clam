open TypeState

let rec constrain (sub: Type.type') (sup: Type.type') =
  if not (sub = Type.bot) && not (sup = Type.top) then
    print_endline("constrain " ^ TypeDisplay.display sub ^ "  <  " ^ TypeDisplay.display sup ^ "")
  else
    ();
  constrain_union_1 sub sup

and constrain_union_1 sub sup =
  list_iter (Utils.flip constrain_union_2 sup) sub.union

and constrain_union_2 sub sup =
  list_iter (constrain_inter_1 sub) sup.union

and constrain_inter_1 sub sup =
  list_iter (constrain_inter_2 sub) sup.inter

and constrain_inter_2 sub sup =
  list_iter (Utils.flip constrain_base sup) sub.inter

and constrain_base sub sup =
  match sub, sup with
  | Var sub_var, Var sup_var ->
    let* sub_level = get_level sub_var.bind in
    let* sup_level = get_level sup_var.bind in
    if sub_level < sup_level then
      constrain_var_sup sup_var (Type.base sub)
    else
    if sub_level > sup_level then
      constrain_var_sub sub_var (Type.base sup)
    else
      return ()
    (* let* () = constrain_var_sup sup_var sub in
    let* () = constrain_var_sub sub_var sup in
    return () *)
  | Type.Var sub_var, _ ->
    constrain_var_sub sub_var (Type.base sup)
  | _, Type.Var sup_var ->
    constrain_var_sup sup_var (Type.base sub)
  | Record sub_record, Record sup_record ->
    iter_map (fun sup_attr -> constrain_record_attr sub_record sup_attr) sup_record.attrs
  | AbsExpr sub_abs, AbsExpr sup_abs ->
    let* () = constrain sup_abs.param sub_abs.param in
    let* () = constrain sub_abs.ret sup_abs.ret in
    return ()
  | _, _ ->
    return ()

and constrain_record_attr sub_record sup_attr =
  match Utils.NameMap.find_opt sup_attr.name sub_record.attrs with
  | Some sub_attr ->
    constrain sub_attr.type' sup_attr.type'
  | None ->
    return ()

and constrain_var_sub sub_var sup =
  let* () = set_upper_bound sub_var.bind sup in
  let* sub_lower = get_lower_bound sub_var.bind in
  constrain sub_lower sup

and constrain_var_sup sup_var sub =
  let* () = set_lower_bound sup_var.bind sub in
  let* sup_upper = get_upper_bound sup_var.bind in
  constrain sub sup_upper
