open TypeLevel
open TypePolar
open TypeState
open TypeSolve

let rec direct_sup (type': Type.type') bind =
  direct_sup_union type' bind

and direct_sup_union union bind =
  list_any (fun type' -> direct_sup_inter type' bind) union.union

and direct_sup_inter inter bind =
  list_any (fun type' -> direct_sup_base type' bind) inter.inter

and direct_sup_base type' bind =
  match type' with
  | Var var ->
    if var.bind == bind then
      return true
    else
      let* upper = get_var_upper var.bind in
      direct_sup upper bind
  | _ ->
    return false

let rec direct_sub (type': Type.type') bind =
  direct_sub_union type' bind

and direct_sub_union union bind =
  list_any (fun type' -> direct_sub_inter type' bind) union.union

and direct_sub_inter inter bind =
  list_any (fun type' -> direct_sub_base type' bind) inter.inter

and direct_sub_base type' bind =
  match type' with
  | Var var ->
    if var.bind == bind then
      return true
    else
      let* lower = get_var_lower var.bind in
      direct_sub lower bind
  | _ ->
    return false

let rec constrain (sub: Type.type') (sup: Type.type') =
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
    let* cond = direct_sub_base sub sup_var.bind in
    let* cond2 = direct_sup_base sup sub_var.bind in
    if not (cond || cond2) then
      let* () = constrain_sub_var sub_var (Type.base sup) in
      let* () = constrain_sup_var sup_var (Type.base sub) in
      return ()
    else
      (* TODO: Two variables are equal here. We probably need to treat that
        without forming a direct cycle if possible. *)
      return ()
  | Var sub_var, _ ->
    constrain_sub_var sub_var (Type.base sup)
  | _, Var sup_var ->
    constrain_sup_var sup_var (Type.base sub)
  | Tuple sub_tuple, Tuple sup_tuple ->
    iter_list2 (fun sub sup -> constrain sub sup) sub_tuple.elems sup_tuple.elems
  | Record sub_record, Record sup_record ->
    iter_map (fun sup_attr -> constrain_record_attr sub_record sup_attr) sup_record.attrs
  | AbsExpr sub_abs, AbsExpr sup_abs ->
    let* () = constrain sup_abs.param sub_abs.param in
    let* () = constrain sub_abs.ret sup_abs.ret in
    return ()
  | AbsTypeExpr sub_abs, _ ->
    let* _ = with_var (fun var ->
      let* () = constrain var sub_abs.param.bound in
      let* ctx = get_context in
      let ret = TypeSystem.substitute_arg ctx sub_abs.param.bind var sub_abs.ret in
      let* () = constrain ret (Type.base sup) in
      return ret
    ) in
    return ()
  | _, AbsTypeExpr sup_abs ->
    let* _ = with_var (fun var ->
      let* () = constrain var sup_abs.param.bound in
      let* ctx = get_context in
      let ret = TypeSystem.substitute_arg ctx sup_abs.param.bind var sup_abs.ret in
      let* () = constrain (Type.base sub) ret in
      return ret
    ) in
    return ()
  | _, Top | Bot, _ | Unit, Unit | Bool, Bool | Int, Int | String, String ->
    return ()
  | _, _ ->
    print_endline ("TYPE ERROR " ^ TypeDisplay.display_base sub ^ " " ^ TypeDisplay.display_base sup);
    return ()

and constrain_record_attr sub_record sup_attr =
  match Utils.NameMap.find_opt sup_attr.name sub_record.attrs with
  | Some sub_attr ->
    constrain sub_attr.type' sup_attr.type'
  | None ->
    return ()

and constrain_sub_var sub_var sup =
  let* entry = get_var_entry sub_var.bind in
  let* () = levelize sup entry.level_low in
  let* () = update_var_upper sub_var.bind sup in
  let* sub_lower = get_var_lower sub_var.bind in
  constrain sub_lower sup

and constrain_sup_var sup_var sub =
  let* entry = get_var_entry sup_var.bind in
  let* () = levelize sub entry.level_low in
  let* () = update_var_lower sup_var.bind sub in
  let* sup_upper = get_var_upper sup_var.bind in
  constrain sub sup_upper
