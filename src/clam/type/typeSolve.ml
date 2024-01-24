(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently a little ugly and almost certainly buggy. The biggest challenge is to figure
  out in which order the type variables should be treated.
*)

open TypePolar
open TypeState

let prioritize f vars =
  let news = List.filter f vars in
  if List.is_empty news then
    vars
  else
    news

(* Prioritize variables that do not occur in other bounds *)
(* Ideally this should be determined when variable levels are being lowered *)
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

(* Returns variables that are equal or higher to the current state level and that do not appear in lower variables *)
let get_variables state =
  let vars = List.filter (fun (entry: entry_var) -> entry.level = state.level) state.vars
  |> List.map (fun entry -> entry.bind) in
  prioritize_bound vars state, state

let inline_state bind state =
  let ctx, _ = get_context state in
  let lower, _ = get_var_lower bind state in
  let upper, _ = get_var_upper bind state in
  let vars = List.map (fun entry -> { entry with
    lower = TypeSystem.substitute_arg ctx bind lower entry.lower;
    upper = TypeSystem.substitute_arg ctx bind upper entry.upper;
  }) state.vars in
  let exprs =  List.map (fun entry -> {
    entry with type' = TypeSystem.substitute_arg ctx bind lower entry.type'
  }) state.exprs in
  (), { state with vars; exprs }

let rec solve type' =
  let* vars = get_variables in
  match vars with
  | [] ->
    return type'
  | bind :: _ ->
    let pols = get_pols type' bind Neg in
    let* type' = match pols.neg, pols.pos with
    | true, true ->
      let* bound = get_var_upper bind in
      let type' = (Type.abs_type_expr { bind; bound } type') in
      let* () = add_type bind bound in
      return type'
    | true, false ->
      let* () = inline_state bind in
      let* lower = get_var_lower bind in
      substitute bind lower type'
    | false, true ->
      let* () = inline_state bind in
      let* upper = get_var_upper bind in
      substitute bind upper type'
    | false, false ->
      let* () = inline_state bind in
      return type' in
    let* () = remove_var bind in
    solve type'

let with_level f state =
  let state = { state with level = state.level + 1 } in
  let x, state = f state in
  let x, state = solve x state in
  let state = { state with level = state.level - 1 } in
  x, state

let with_var f =
  let f = (let* type' = make_var in f type') in
  with_level f
