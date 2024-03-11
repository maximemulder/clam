open Level
open State

(*
  This file contains the type constraining algorithm, which updates the environment constraints
  and ensures that they remain coherent, raising an error if it is not possible.
  There are several major challenges, which are not all solved:

  1. How to fully handle unions and intersections, such as in '1 | '2 <= '3 | '4 ?
  It does not look fully feasible with bounds, but can such situations even happen at all ?
  (it can with explicit type annotations, but what about inference variables ?)

  2. How to handle cycles, where a variable appears in its own bounds (like '1 <= '2 <= '1) ?

  3. Reciprocity, updates bounds of both variables whenever '1 <= '2
*)

(*
  These functions are used to know if an inference variable appears directly in the lower or upper
  bounds of another type, which allows to prevent creating cycles
*)

let rec is_direct_sup bind (type': Type.type') =
  is_direct_sup_union bind type'

and is_direct_sup_union bind union =
  list_any (is_direct_sup_inter bind) union.union

and is_direct_sup_inter bind inter =
  list_any (is_direct_sup_base bind) inter.inter

and is_direct_sup_base bind type' =
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
      is_direct_sup bind entry.upper
    )
  | _ ->
    return false

let rec is_direct_sub bind (type': Type.type') =
  is_direct_sub_union bind type'

and is_direct_sub_union bind union =
  list_any (is_direct_sub_inter bind) union.union

and is_direct_sub_inter bind inter =
  list_any (is_direct_sub_base bind) inter.inter

and is_direct_sub_base bind type' =
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
      is_direct_sub bind entry.lower
    )
  | _ ->
    return false

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
  | Var sub_var, Var sup_var when is_infer sub_var.bind state && is_infer sup_var.bind state ->
    (* let* sub_res = constrain_sub_var sub_var (Type.base sup) in
    let* sup_res = constrain_sup_var sup_var (Type.base sub) in
    return (sub_res && sup_res) *)
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
    let* () =  print("var_univ " ^ Type.display var) in
    let* lower = constrain sub_univ.param.lower var in
    let* upper = constrain var sub_univ.param.upper in
    let* ctx = get_context in
    let ret = Type.System.substitute ctx sub_univ.ret sub_univ.param.bind var in
    let* ret = constrain ret (Type.base sup) in
    return (lower && upper && ret)
  | _, Univ sup_univ ->
    with_type sup_univ.param.bind sup_univ.param.lower sup_univ.param.upper
      (constrain (Type.base sub) sup_univ.ret)
  | _, _ ->
    let* ctx = get_context in
    let result = Type.System.isa ctx (Type.base sub) (Type.base sup) in
    return result

and constrain_sub_var sub_var sup =
  let* cond = is_direct_sup sub_var.bind sup in
  if not cond then
    let* entry = get_var_entry sub_var.bind in
    let* () = Level2.levelize entry.level_low sup in
    let* () = levelize sup entry.level in
    let* () = update_var_upper sub_var.bind sup in
    let* sub_lower = get_var_lower sub_var.bind in
    constrain sub_lower sup
  else
    (* TODO: Handle cycle *)
    return true

and constrain_sup_var sup_var sub =
  let* cond = is_direct_sub sup_var.bind sub in
  if not cond then
    let* entry = get_var_entry sup_var.bind in
    let* () = Level2.levelize entry.level_low sub in
    let* () = levelize sub entry.level in
    let* () = update_var_lower sup_var.bind sub in
    let* sup_upper = get_var_upper sup_var.bind in
    constrain sub sup_upper
  else
    (* TODO: Handle cycle *)
    return true

and constrain_var sub_var sup_var =
  let sub = Type.var sub_var.bind in
  let sup = Type.var sup_var.bind in
  let* sub_entry = get_var_entry sub_var.bind in
  let* sup_entry = get_var_entry sup_var.bind in
  let sub_level = sub_entry.level_low in
  let sup_level = sup_entry.level_low in
  if sub_level > sup_level then
    (* let* () = update_var_lower sup_var.bind sub in *)
    (*  *)
    let* () = update_var_upper sub_var.bind sup in
    let* () = Level2.levelize sub_entry.level_low sub in
    let* () = levelize sub sub_entry.level in
    let* sub_lower = get_var_lower sub_var.bind in
    constrain sub_lower sup
  else if sup_level > sub_level then
    (* let* () = update_var_upper sub_var.bind sup in *)
    (*  *)
    let* () = update_var_lower sup_var.bind sub in
    let* () = Level2.levelize sup_entry.level_low sub in
    let* () = levelize sub sup_entry.level in
    let* sup_upper = get_var_upper sup_var.bind in
    constrain sub sup_upper
  else
    let* () = print ("EQUAL " ^ sub_var.bind.name ^ " " ^ sup_var.bind.name) in
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

let constrain pos sub sup =
  (* let* () = print_vars in *)
  let* () = print("constrain " ^ Type.display sub ^ " < " ^ Type.display sup) in
  let* result = constrain sub sup in
  (* let* () = print_vars in *)
  if result then
    return ()
  else
    Error.raise_constrain pos sub sup
