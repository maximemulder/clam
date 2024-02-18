(*
  This file contains the tree walker used to lower the level of the inference variables that occur
  in a given type to a given level. A subtlety is that the algorithm differenciates between direct
  and indirect occurences. A direct occurence happens when an inference variable appears directly
  in the type, that is, without an intermediary type such as a tuple, a function... Only indirect
  occurences are lowered.
*)

open TypeState

let rec levelize (type': Type.type') level direct =
  levelize_union type' level direct

and levelize_union union level direct =
  list_iter (fun type' -> levelize_inter type' level direct) union.union

and levelize_inter inter level direct =
  list_iter (fun type' -> levelize_base type' level direct) inter.inter

and levelize_base type' level direct =
  match type' with
  | Var var ->
    levelize_var var level direct
  | Tuple tuple ->
    list_iter (fun elem -> levelize elem level false) tuple.elems
  | Record record ->
    iter_map (fun (attr: Type.attr) -> levelize attr.type' level false) record.attrs
  | Lam lam ->
    let* () = levelize lam.param level false in
    let* () = levelize lam.ret level false in
    return ()
  | Univ univ ->
    let* () = levelize univ.param.bound level false in
    let* () = levelize univ.ret level false in
    return ()
  | _ ->
    return ()

and levelize_var var level direct =
  let* entry = get_var_entry_opt var.bind in
  match entry with
  | Some entry ->
    if entry.level > level then
      if not direct then
        let* () = update_var_entry var.bind (fun entry -> { entry with level }) in
        let* () = levelize entry.lower level direct in
        let* () = levelize entry.upper level direct in
        return ()
      else
        return ()
    else
      return ()
  | None ->
    return ()

let levelize type' level =
  levelize type' level true
