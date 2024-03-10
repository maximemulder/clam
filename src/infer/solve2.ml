(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently a little ugly and almost certainly buggy. The biggest challenge is to figure
  out in which order the type variables should be treated.
*)

open Polar
open State

(* Returns variables that are equal or higher to the current state level and that do not appear in lower variables *)
let get_variables state =
  List.filter (fun (entry: entry_var) -> entry.level >= state.level) state.vars
  |> List.map (fun entry -> entry.bind), state

open Inline2

let inline_state bind state =
  let exprs =  List.map (fun entry -> {
    entry with type' = fst(inline bind Neg entry.type' state)
  }) state.exprs in
  (), { state with exprs }

let rec solve type' =
  let* vars = Extrude.extrude type' in
  match vars with
  | [] ->
    return type'
  | bind :: _ ->
    let* () = print_vars in
    let* entry = get_var_entry bind in
    let* state = get_state in
    let pols = get_pols type' bind Neg in
    let* type' = match pols.neg, pols.pos with
    | Some ((_ :: _) as neg), _ ->
      let neg = List.map Type.var neg in
      let* neg = fold_list join Type.bot neg in
      let* () = print("co_neg " ^ bind.name ^ " by " ^ Type.display neg ^ " in " ^ " in " ^ Type.display type') in
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
    | Some [], Some [] when entry.level_low = state.level ->
      let* () = print("quantify " ^ bind.name ^ " in " ^ Type.display type') in
      let* lower = get_var_lower bind in
      let* upper = get_var_upper bind in
      let type' = (Type.univ { bind; lower; upper } type') in
      let* () = print("= " ^ Type.display type') in
      let* () = remove_var bind in
      let* () = add_type bind lower upper in
      return type'
    | _, _ ->
      let* () = print("inline " ^ bind.name ^ " in " ^ Type.display type') in
      let* type' = inline bind Neg type' in
      let* () = print("= " ^ Type.display type') in
      return type'
    in
    (* if level_low = state.level, remove variable *)
    (* let* () = remove_var bind in *)
    solve type'

let remove_vars state =
  let vars = List.filter (fun var -> var.level_low != state.level) state.vars in
  { state with vars }

let with_level f state =
  let state = { state with level = state.level + 1 } in
  let x, state = f state in
  let x, state = solve x state in
  (* let state = remove_vars state in *)
  let state = { state with level = state.level - 1 } in
  x, state

let with_var f =
  let f = (let* var = make_var in let* () = print ("var " ^ Type.display var) in f var) in
  with_level f
