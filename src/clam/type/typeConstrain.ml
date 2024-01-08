open TypeLevel
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
  | Var var -> (
    if var.bind == bind then
      return true
    else
    let* entry = get_var var.bind in
    match entry with
    | Param _ ->
      return false
    | Infer entry ->
      direct_sup entry.upper bind
    )
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
  | Var var -> (
    if var.bind == bind then
      return true
    else
    let* entry = get_var var.bind in
    match entry with
    | Param _ ->
      return false
    | Infer entry ->
      direct_sub entry.lower bind
    )
  | _ ->
    return false

let rec constrain pos (sub: Type.type') (sup: Type.type') =
  constrain_union_1 pos sub sup

and constrain_union_1 pos sub sup =
  list_all (Utils.flip (constrain_union_2 pos) sup) sub.union

and constrain_union_2 pos sub sup =
  match sub with
  | { inter = [Var sub_var] } -> (
    let* entry = get_var sub_var.bind in
    match entry with
    | Param _ ->
      return true
    | Infer _ ->
      let* cond = direct_sup sup sub_var.bind in
      if not cond then
        constrain_sub_var pos sub_var sup
      else
        return true)
  | _ ->
  list_any (constrain_inter_1 pos sub) sup.union

and constrain_inter_1 pos sub sup =
  list_all (constrain_inter_2 pos sub) sup.inter

and constrain_inter_2 pos sub sup =
  match sup with
  | Var sup_var -> (
    let* entry = get_var sup_var.bind in
    match entry with
    | Param _ ->
      (* TODO ? *)
      return true
    | Infer _ ->
      let* cond = direct_sub_inter sub sup_var.bind in
      if not cond then
        constrain_sup_var pos sup_var { Type.union = [sub] }
      else
        return true)
  | _ ->
  list_any (Utils.flip (constrain_base pos) sup) sub.inter

and constrain_base pos sub sup =
  match sub, sup with
  | _, Var sup_var -> (
    let* entry = get_var sup_var.bind in
    match entry with
    | Param _ ->
      (* TODO ? *)
      return true
    | Infer _ ->
      let* cond = direct_sub_base sub sup_var.bind in
      if not cond then
        constrain_sup_var pos sup_var (Type.base sub)
      else
        return true)
  | Var sub_var, _ -> (
    let* entry = get_var sub_var.bind in
    match entry with
    | Param _ ->
      (* TODO ? *)
      return true
    | Infer _ ->
      let* cond = direct_sup_base sup sub_var.bind in
      if not cond then
        constrain_sub_var pos sub_var (Type.base sup)
      else
        return true)
  | Tuple sub_tuple, Tuple sup_tuple ->
    List.combine sub_tuple.elems sup_tuple.elems
    |> list_all (fun (sub, sup) -> constrain pos sub sup)
  | Record sub_record, Record sup_record ->
    map_all (fun sup_attr -> constrain_record_attr pos sub_record sup_attr) sup_record.attrs
  | AbsExpr sub_abs, AbsExpr sup_abs ->
    let* param = constrain pos sup_abs.param sub_abs.param in
    let* ret = constrain pos sub_abs.ret sup_abs.ret in
    return (param && ret)
  | AbsTypeExpr sub_abs, _ ->
    let* var = make_var in
    let* param = constrain pos var sub_abs.param.bound in
    let* ctx = get_context in
    let ret = TypeSystem.substitute_arg ctx sub_abs.param.bind var sub_abs.ret in
    let* ret = constrain pos ret (Type.base sup) in
    return (param && ret)
  | _, AbsTypeExpr sup_abs ->
    let* var = make_var in
    let* param = constrain pos var sup_abs.param.bound in
    let* ctx = get_context in
    let ret = TypeSystem.substitute_arg ctx sup_abs.param.bind var sup_abs.ret in
    let* ret = constrain pos (Type.base sub) ret in
    return (param && ret)
  | _, _ ->
    let* ctx = get_context in
    let result = TypeSystem.isa ctx (Type.base sub) (Type.base sup) in
    return result

and constrain_record_attr pos sub_record sup_attr =
  match Utils.NameMap.find_opt sup_attr.name sub_record.attrs with
  | Some sub_attr ->
    constrain pos sub_attr.type' sup_attr.type'
  | None ->
    return true

and constrain_sub_var pos sub_var sup =
  let* entry = get_var_entry sub_var.bind in
  let* () = levelize sup entry.level_low in
  let* () = update_var_upper sub_var.bind sup in
  let* sub_lower = get_var_lower sub_var.bind in
  constrain pos sub_lower sup

and constrain_sup_var pos sup_var sub =
  let* entry = get_var_entry sup_var.bind in
  let* () = levelize sub entry.level_low in
  let* () = update_var_lower sup_var.bind sub in
  let* sup_upper = get_var_upper sup_var.bind in
  constrain pos sub sup_upper

let constrain pos sub sup =
  let* result = constrain pos sub sup in
  if result then
    return ()
  else
    TypeError.infer_constrain pos sub sup
