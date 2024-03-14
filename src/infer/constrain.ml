open Level
open State

(*
  This file contains the type constraining algorithm, which updates the environment
  constraints and ensures that they remain coherent, raising an error if it is not possible.
*)

(* These functions are used to check whether a type is a single inference variable *)

let get_infer_var_sub sub_inter =
  let* env = get_state in
  match sub_inter with
  | { Type.inter = [Var sub_var] } when is_infer sub_var.bind env ->
    return (Some sub_var)
  | _ ->
    return None

let get_infer_var_sup sup =
  let* env = get_state in
  match sup with
  | Type.Var sup_var when is_infer sup_var.bind env ->
    return (Some sup_var)
  | _ ->
    return None

let rec constrain (sub: Type.type') (sup: Type.type') =
  constrain_union_1 sub sup

and constrain_union_1 sub sup =
  list_all (fun sub -> constrain_union_2 sub sup) sub.union

and constrain_union_2 sub sup =
  let* sub_var = get_infer_var_sub sub in
  match sub_var with
  | Some sub_var when List.length sup.union > 1 ->
    constrain_sub_var sub_var sup
  | _ ->
    list_any (constrain_inter_1 sub) sup.union

and constrain_inter_1 sub sup =
  list_all (constrain_inter_2 sub) sup.inter

and constrain_inter_2 sub sup =
  let* sup_var = get_infer_var_sup sup in
  match sup_var with
  | Some sup_var when List.length sub.inter > 1 ->
    let sub = { Type.union = [sub] } in
    constrain_sup_var sup_var sub
  | _ ->
    list_any (fun sub -> constrain_base sub sup) sub.inter

and constrain_base sub sup =
  let* state = get_state in
  match sub, sup with
  | _, Top | Bot, _ | Unit, Unit | Bool, Bool | Int, Int | String, String ->
    (*
      We can assume the type is a proper type here
      (although still checking it may not be a bad idea)
    *)
    return true
  | Var sub_var, Var sup_var when is_infer sub_var.bind state && is_infer sup_var.bind state ->
    constrain_var sub_var sup_var
  | Var sub_var, _ when is_infer sub_var.bind state ->
    let sup = Type.base sup in
    constrain_sub_var sub_var sup
  | _, Var sup_var when is_infer sup_var.bind state ->
    let sub = Type.base sub in
    constrain_sup_var sup_var sub
  | Tuple sub_tuple, Tuple sup_tuple ->
    constrain_tuple sub_tuple sup_tuple
  | Record sub_record, Record sup_record ->
    constrain_record sub_record sup_record
  | Lam sub_lam, Lam sup_lam ->
    constrain_lam sub_lam sup_lam
  | Univ sub_univ, _ ->
    let* var = make_var in
    let* level = Level.get_level (Type.base sup) in
    let* () = match level with
    | Some level ->
      reorder level var
    | None ->
      return ()
    in
    let var = Type.var var in
    let* lower = constrain sub_univ.param.lower var in
    let* upper = constrain var sub_univ.param.upper in
    let* ret = substitute sub_univ.param.bind var sub_univ.ret in
    let* ret = constrain ret (Type.base sup) in
    return (lower && upper && ret)
  | _, Univ sup_univ ->
    with_type sup_univ.param.bind sup_univ.param.lower sup_univ.param.upper
      (constrain (Type.base sub) sup_univ.ret)
  | _, _ ->
    (* TODO: Constrain type applications without using simple subtyping *)
    let* ctx = get_context in
    let result = Type.System.isa ctx (Type.base sub) (Type.base sup) in
    return result

and constrain_sub_var sub_var sup =
  let* () = levelize sub_var.bind sup in
  let* () = update_var_upper sub_var.bind sup in
  let* sub_lower = get_var_lower sub_var.bind in
  constrain sub_lower sup

and constrain_sup_var sup_var sub =
  let* () = levelize sup_var.bind sub in
  let* () = update_var_lower sup_var.bind sub in
  let* sup_upper = get_var_upper sup_var.bind in
  constrain sub sup_upper

and constrain_var sub_var sup_var =
  let sub = Type.var sub_var.bind in
  let sup = Type.var sup_var.bind in
  let* sub_entry = get_var_entry sub_var.bind in
  let* sup_entry = get_var_entry sup_var.bind in
  let sub_level = sub_entry.level_low in
  let sup_level = sup_entry.level_low in
  if sub_level > sup_level then
    let* () = update_var_upper sub_var.bind sup in
    let* sub_lower = get_var_lower sub_var.bind in
    constrain sub_lower sup
  else if sup_level > sub_level then
    let* () = update_var_lower sup_var.bind sub in
    let* sup_upper = get_var_upper sup_var.bind in
    constrain sub sup_upper
  else
    return true

and constrain_tuple sub_tuple sup_tuple =
  List.combine sub_tuple.elems sup_tuple.elems
  |> list_all (fun (sub, sup) -> constrain sub sup)

and constrain_record sub_record sup_record =
  map_all (fun sup_attr -> constrain_record_attr sub_record sup_attr) sup_record.attrs

and constrain_record_attr sub_record sup_attr =
  match Util.NameMap.find_opt sup_attr.label sub_record.attrs with
  | Some sub_attr ->
    constrain sub_attr.type' sup_attr.type'
  | None ->
    return true

and constrain_lam sub_abs sup_abs =
  let* param = constrain sup_abs.param sub_abs.param in
  let* ret = constrain sub_abs.ret sup_abs.ret in
  return (param && ret)

let constrain span sub sup =
  let* () = print ("constrain " ^ Type.display sub ^ " < " ^ Type.display sup) in
  let* result = constrain sub sup in
  let* () = print_vars in
  if result then
    return ()
  else
    Error.raise_constrain span sub sup
