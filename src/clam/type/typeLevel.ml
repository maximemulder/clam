(*
  This file contains the tree walker used to lower the level of inference variables
  to the lowest context level in which they appear, allowing to know when to infer them
*)

open TypeState

let rec levelize (type': Type.type') level =
  levelize_union type' level

and levelize_union union level =
  list_iter (fun type' -> levelize_inter type' level) union.union

and levelize_inter inter level =
  list_iter (fun type' -> levelize_base type' level) inter.inter

and levelize_base type' level =
  match type' with
  | Var var ->
    levelize_var var level
  | Tuple tuple ->
    list_iter (fun elem -> levelize elem level) tuple.elems
  | Record record ->
    iter_map (fun (attr: Type.attr) -> levelize attr.type' level) record.attrs
  | AbsExpr abs ->
    let* () = levelize abs.param level in
    let* () = levelize abs.ret level in
    return ()
  | AbsTypeExpr abs ->
    let* () = levelize abs.param.bound level in
    let* () = levelize abs.ret level in
    return ()
  | _ ->
    return ()

and levelize_var var level =
  let* entry = get_var_entry_opt var.bind in
  match entry with
  | Some entry ->
    if entry.level_low > level then
      let* () = update_var_entry var.bind (fun entry -> { entry with level_low = level }) in
      let* () = levelize entry.lower level in
      let* () = levelize entry.upper level in
      return ()
    else
      return ()
  | None ->
    return ()
