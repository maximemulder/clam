(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently a little ugly and almost certainly buggy. The biggest challenge is to figure
  out in which order the type variables should be treated.
*)

open Polar
open State

let solve_b type' bind =
  let pols = get_pols type' bind Neg in
  let* type' = match pols.neg, pols.pos with
  | Some (neg :: _), _ ->
    let* () = print("co_neg " ^ bind.name ^ " by " ^ neg.name ^ " in " ^ Type.display type') in
    let* type' = substitute bind (Type.var neg) type' in
    let* () = print("= " ^ Type.display type') in
    return type'
  | _, Some (pos :: _) ->
    let* () = print("co_pos " ^ bind.name ^ " by " ^ pos.name ^ " in " ^ Type.display type') in
    let* type' = substitute bind (Type.var pos) type' in
    let* () = print("= " ^ Type.display type') in
    return type'
  | Some [], Some [] ->
    let* () = print("quantify " ^ bind.name ^ " in " ^ Type.display type') in
    let* lower = get_var_lower bind in
    let* upper = get_var_upper bind in
    let type' = (Type.univ { bind; lower; upper } type') in
    let* () = print("= " ^ Type.display type') in
    return type'
  | Some [], None ->
    let* () = print("inline_neg " ^ bind.name ^ " in " ^ Type.display type') in
    let* lower = get_var_lower bind in
    let* type' = substitute bind lower type' in
    let* () = print("= " ^ Type.display type') in
    return type'
  | None, Some [] ->
    let* () = print("inline_pos " ^ bind.name ^ " in " ^ Type.display type') in
    let* upper = get_var_upper bind in
    let* type' = substitute bind upper type' in
    let* () = print("= " ^ Type.display type') in
    return type'
  | None, None ->
    let* () = print("none " ^ bind.name ^ " in " ^ Type.display type') in
    return type'
  in
  return type'

let rec solve_bis type' level =
  let* high = get_highest_variable in
  match high with
  | None ->
    return type'
  | Some high ->
    let* high_entry = get_var_entry high in
    if high_entry.level_orig >= level then
      let* type' = solve_b type' high in
      let* () = remove_var high in
        solve_bis type' level
    else
      return type'

let with_var f =
  let* var = make_var in
  let type' = Type.var var in
  let* type' = f type' in
  let* entry = get_var_entry var in
  solve_bis type' entry.level_orig
