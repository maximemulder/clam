(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently a little ugly and almost certainly buggy. The biggest challenge is to figure
  out in which order the type variables should be treated.
*)

open Polar
open State
open Inline
open Constrain

let solve type' bind =
  let* state = get_state in
  let pols = get_pols state bind Pos type' in
  let* () = if Option.is_some pols.neg then
    print ("NEG" ^ bind.name ^ " IN " ^ Type.display type')
  else
      return () in
    let* () = if Option.is_some pols.pos then
    print ("POS" ^ bind.name ^ " IN " ^ Type.display type')
  else
      return () in
  match pols.neg, pols.pos with
  | Some neg, _ when neg <> Type.top ->
    let* () = print("co_neg " ^ bind.name ^ " by " ^ Type.display neg ^ " in " ^ Type.display type') in
    let* lower = get_var_lower bind in
    let* upper = get_var_upper bind in
    let* () = constrain (Code.span_primitive) lower neg in
    let* () = constrain (Code.span_primitive) neg upper in
    inline bind (Type.top) neg Pos type'
  | _, Some pos when pos <> Type.bot ->
    let* () = print("co_pos " ^ bind.name ^ " by " ^ Type.display pos ^ " in " ^ Type.display type') in
    let* lower = get_var_lower bind in
    let* upper = get_var_upper bind in
    let* () = constrain (Code.span_primitive) lower pos in
    let* () = constrain (Code.span_primitive) pos upper in
    inline bind pos (Type.bot) Pos type'
  | Some _, Some _ ->
    let* () = print("quantify " ^ bind.name ^ " in " ^ Type.display type') in
    let* lower = get_var_lower bind in
    let* upper = get_var_upper bind in
    let* cond = is lower upper in
    if cond then
      substitute bind lower type'
    else
      let param_bind = { Abt.name = bind.name } in
      let type' = Type.rename bind param_bind type' in
      return (Type.univ { bind = param_bind; lower; upper } type')
  | Some _, None ->
    let* () = print("inline_neg " ^ bind.name ^ " in " ^ Type.display type') in
    let* upper = get_var_upper bind in
    substitute bind upper type'
  | None, Some _ ->
    let* () = print("inline_pos " ^ bind.name ^ " in " ^ Type.display type') in
    let* lower = get_var_lower bind in
    substitute bind lower type'
  | None, None ->
    let* () = print("none " ^ bind.name ^ " in " ^ Type.display type') in
    return type'

let find_recursive span entry type' =
  let* level = Level.get_level type' in
  match level with
  | Some level when level = entry.level_low ->
    Error.raise_recursive span entry.bind type'
  | _ ->
    return ()

let solve_type span bind type' =
  let* type' = solve type' bind in
  let* var_entry = get_var_entry bind in
  let* () = find_recursive span var_entry type' in
  let* () = print ("= " ^ Type.display type') in
  return type'

let solve_expr bind var_expr =
  let* type' = solve var_expr.type' bind in
  let* () = update_expr_type var_expr.bind type' in
  let* var_entry = get_var_entry bind in
  let* () = find_recursive var_expr.span var_entry type' in
  let* () = print (var_expr.bind.name ^ ": " ^ Type.display type') in
  let* level = Level.get_level type' in
  match level with
  | Some level ->
    let* () = update_expr_level var_expr.bind level in
    return ()
  | None ->
    return ()

let solve_exprs bind =
  let* exprs = get_high_exprs in
  list_iter (solve_expr bind) exprs

let rec solve_bis span level type' =
  let* high = get_highest_variable in
  match high with
  | None ->
    return type'
  | Some high ->
    let* high_entry = get_var_entry high in
    if high_entry.level_orig >= level then
      let* type' = solve_type span high type'  in
      let* () = solve_exprs high in
      let* () = remove_var high in
      solve_bis span level type'
    else
      return type'

let with_var span f =
  let* var = make_var in
  let type' = Type.var var in
  let* type' = f type' in
  let* entry = get_var_entry var in
  solve_bis span entry.level_orig type'
