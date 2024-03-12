(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently a little ugly and almost certainly buggy. The biggest challenge is to figure
  out in which order the type variables should be treated.
*)

open Inline
open Polar
open State

let inline_state bind state =
  let exprs =  List.map (fun entry -> {
    entry with type' = fst(inline bind Neg entry.type' state)
  }) state.exprs in
  (), { state with exprs }

let  solve_b type' bind =
  let pols = get_pols type' bind Neg in
  let* type' = match pols.neg, pols.pos with
  | Some ((_ :: _) as neg), _ ->
    let neg = List.map Type.var neg in
    let* neg = fold_list join Type.bot neg in
    let* () = print("co_neg " ^ bind.name ^ " by " ^ Type.display neg ^ " in " ^ Type.display type') in
    let* type' = substitute bind neg type' in
    let* () = print("= " ^ Type.display type') in
    return type'
  | _, Some ((_ :: _) as pos) ->
    let pos = List.map Type.var pos in
    let* pos = fold_list meet Type.top pos in
    let* () = print("co_pos " ^ bind.name ^ " by " ^ Type.display pos ^ " in " ^ Type.display type') in
    let* type' = substitute bind pos type' in
    let* () = print("= " ^ Type.display type') in
    return type'
  | Some [], Some [] ->
    let* () = print("quantify " ^ bind.name ^ " in " ^ Type.display type') in
    let* lower = get_var_lower bind in
    let* upper = get_var_upper bind in
    let type' = (Type.univ { bind; lower; upper } type') in
    let* () = print("= " ^ Type.display type') in
    return type'
  | _, _ ->
    let* () = print("inline " ^ bind.name ^ " in " ^ Type.display type') in
    let* type' = inline bind Neg type' in
    let* () = print("= " ^ Type.display type') in
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
  let* () = print ("var " ^ var.name) in
  let type' = Type.var var in
  let* type' = f type' in
  let* entry = get_var_entry var in
  solve_bis type' entry.level_orig
