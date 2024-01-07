(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently quite ugly and almost certainly buggy. The biggest challenge is to figure out in
  which order the type variables should be treated.
*)

open TypeInline
open TypePolar
open TypeState

let prioritize f vars =
  let news = List.filter f vars in
  if List.is_empty news then
    vars
  else
    news

(* Prioritize variables that occur in the type *)
let prioritize_occur type' vars state =
  prioritize (fun var -> fst (occurs type' var state)) vars

(* Prioritize variables that do not occur in other bounds *)
let prioritize_bound vars state =
  prioritize (fun var ->
    List.for_all (fun (entry: entry_var) ->
      if entry.bind == var then
        true
      else
        let a, _ = occurs entry.lower var state in
        let b, _ = occurs entry.upper var state in
        (not a && not b)
    ) state.vars
  ) vars

(* Prioritize variables that occur directly in other bounds *)
let prioritize_direct vars state =
  prioritize (fun var ->
    List.exists (fun (entry: entry_var) ->
      if entry.bind == var then
        false
      else
        let a = occurs_directly var entry.lower in
        let b = occurs_directly var entry.upper in
        (a || b)
    ) state.vars
  ) vars

(* Returns variables that are equal or higher to the current state level and that do not appear in lower variables *)
let get_variables type' state =
  let vars = List.filter (fun (entry: entry_var) -> entry.level_low = state.level) state.vars
  |> List.sort (fun (a: entry_var) b -> compare b.level a.level)
  |> List.map (fun entry -> entry.bind) in

  let vars = prioritize_occur type' vars state in
  let vars = prioritize_bound vars state in
  let vars = prioritize_direct vars state in

  vars, state

let should_quantify type' bind =
  let occurence = get_pols type' bind Neg in
  occurence.pos && occurence.neg

let rec solve type' =
  let* vars = get_variables type' in
  match vars with
  | [] ->
    return type'
  | bind :: _ ->
    if should_quantify type' bind then
      let* bound = get_var_upper bind in
      let type' = (Type.abs_type_expr { bind; bound } type') in
      let* () = remove_var bind in
      with_type bind bound
        (solve type')
    else
      let* () = inline_state bind in
      let* type' = (inline type' bind Neg) in
      let* () = remove_var bind in
      solve type'

let with_level f state =
  let state = { state with level = state.level + 1 } in
  let x, state = f state in
  let x, state = solve x state in
  let state = { state with level = state.level - 1 } in
  x, state

let with_var f =
  let* type' = make_var in
  with_level (f type')
