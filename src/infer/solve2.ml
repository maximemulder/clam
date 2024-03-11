(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently a little ugly and almost certainly buggy. The biggest challenge is to figure
  out in which order the type variables should be treated.
*)

open Polar
open State

(* Returns variables that are equal or higher to the current state level and that do not appear in lower variables *)
let get_variables state =
  List.filter (fun (entry: entry_var) -> entry.level_low >= state.level) state.vars
  |> List.map (fun entry -> entry.bind), state

open Inline2

let inline_state bind state =
  let exprs =  List.map (fun entry -> {
    entry with type' = fst(inline bind Neg entry.type' state)
  }) state.exprs in
  (), { state with exprs }

let state_contains bind state =
  List.exists (fun entry ->
    entry.bind != bind &&
    (Contain.contains entry.lower bind || Contain.contains entry.upper bind)
  ) state.vars

let rec solve type' =
  let* vars = Extrude.extrude type' in (* TODO: Order seems to change the result, investigate why *)
  let vars = List.rev vars in
  let* () = print("extrude " ^ (String.concat ", " (List.map (fun (bind: Abt.bind_type) -> bind.name) vars))) in
  match vars with
  | [] ->
    return type'
  | bind :: _ ->
    let* () = print_vars in
    let* entry = get_var_entry bind in
    let* state = get_state in
    let pols = get_pols type' bind Neg in
    let pols = { pols with neg = Option.map (fun neg -> List.filter (fun bind -> List.exists (fun b -> b.bind == bind) state.vars) neg) pols.neg } in
    let pols = { pols with pos = Option.map (fun pos -> List.filter (fun bind -> List.exists (fun b -> b.bind == bind) state.vars) pos) pols.pos } in
    let* type' = match pols.neg, pols.pos with
    | Some ((_ :: _) as neg), _ ->
      let neg = List.map Type.var neg in
      let* () = print("co_neg " ^ bind.name ^ " by " ^ (Type.display (List.nth neg 0)) ^ " in " ^ Type.display type') in
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
    | Some [], Some [] when entry.level_low = state.level && not (state_contains bind state) ->
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
    let* state = get_state in
    let contains = state_contains bind state in
    let* () = if not contains then
      let* () = print ("remove " ^ bind.name) in
      remove_var bind
    else
      return ()
    in
    solve type'

let remove_vars state =
  let vars = List.filter (fun var ->
    let contains = state_contains var.bind state in
    if var.level_low != state.level || contains then
      true
    else
      let _ = print("remove other " ^ var.bind.name) state in
      false
  ) state.vars in
  { state with vars }

let with_level f state =
  let state = { state with level = state.level + 1 } in
  let x, state = f state in
  let x, state = solve x state in
  let state = remove_vars state in
  let state = { state with level = state.level - 1 } in
  x, state

let with_var f =
  let f = (let* var = make_var in let* () = print ("var " ^ Type.display var) in f var) in
  with_level f
