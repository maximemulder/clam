(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently a little ugly and almost certainly buggy. The biggest challenge is to figure
  out in which order the type variables should be treated.
*)

open Polar
open Inline
open Type.Context
open State

let solve (fresh: fresh) type' =
  let* pols = with_ctx (get_pols fresh Pos type') in
  match pols.neg, pols.pos with
  | Some neg, _ when neg <> Type.bot ->
    let* () = show_infer ("co_neg " ^ fresh.bind.name ^ " by " ^ Type.display neg ^ " in " ^ Type.display type') in
    (* let* lower = get_var_lower bind in
    let* upper = get_var_upper bind in
    let* neg = join neg lower in *)
    with_ctx (inline fresh (Type.top) neg Pos type')
  | _, Some pos when pos <> Type.top ->
    let* () = show_infer ("co_pos " ^ fresh.bind.name ^ " by " ^ Type.display pos ^ " in " ^ Type.display type') in
    (* let* lower = get_var_lower bind in
    let* upper = get_var_upper bind in
    let* pos = meet pos upper in *)
    with_ctx (inline fresh pos (Type.bot) Pos type')
  | Some _, Some _ ->
    let* () = show_infer ("quantify " ^ fresh.bind.name ^ " in " ^ Type.display type') in
    let* cond = is fresh.lower fresh.upper in
    if cond then
      substitute fresh.bind fresh.lower type'
    else
      let param_bind = { Abt.name = fresh.bind.name } in
      let type' = Type.rename fresh.bind param_bind type' in
      return (Type.univ { bind = param_bind; lower = fresh.lower; upper = fresh.upper } type')
  | Some _, None ->
    let* () = show_infer ("inline_neg " ^ fresh.bind.name ^ " in " ^ Type.display type') in
    substitute fresh.bind fresh.upper type'
  | None, Some _ ->
    let* () = show_infer ("inline_pos " ^ fresh.bind.name ^ " in " ^ Type.display type') in
    substitute fresh.bind fresh.lower type'
  | None, None ->
    let* () = show_infer ("none " ^ fresh.bind.name ^ " in " ^ Type.display type') in
    return type'

let find_recursive span fresh type' =
  (*let* level = Level.get_level type' in
  match level with
  | Some level when level = entry.level_low ->
    Error.raise_recursive span entry.bind type'
  | _ -> *)
  (* TODO: Port this. *)
    return ()

let solve_type span fresh type' =
  let* type' = solve fresh type' in
  let* () = find_recursive span fresh type' in
  let* () = show_infer ("= " ^ Type.display type') in
  return type'

let solve_expr (fresh: fresh) (var_expr: entry_expr) =
  let* type' = solve fresh var_expr.type' in
  let* () = update_expr_type var_expr.bind type' in
  let* () = find_recursive var_expr.span fresh type' in
  let* () = show_infer (var_expr.bind.name ^ ": " ^ Type.display type') in
  (* let* level = Level.get_level type' in
  match level with
  | Some level ->
    let* () = update_expr_level var_expr.bind level in
    return ()
  | None -> *)
  (* TODO: Port this. *)
    return ()

let solve_exprs fresh =
  let* exprs = get_high_exprs in
  list_iter (solve_expr fresh) exprs

let rec solve_bis span fresh type' =
  let* type' = solve_type span fresh type'  in
  let* () = solve_exprs fresh in
  return type'

let rec collect_freshs f x state =
  let fresh = List.nth_opt state.ctx.freshs 0 in
  match fresh with
  | Some fresh when fresh.level >= state.ctx.level ->
    let x, state = f fresh x state in
    let ctx = { state.ctx with freshs = List.tl state.ctx.freshs } in
    let state = { state with ctx } in
    collect_freshs f x state
  | _ ->
    x, state

let with_var span f state =
  let bind = { Abt.name = "'" ^ string_of_int state.ctx.id } in
  let _ = show_infer ("fresh_infer " ^ bind.name) state in
  let var = { bind; level = state.ctx.level + 1; lower = Type.bot; upper = Type.top } in
  let type' = Type.var bind in
  let ctx = { state.ctx with id = state.ctx.id + 1; level = state.ctx.level + 1; freshs = var :: state.ctx.freshs } in
  let state = { state with ctx } in
  let x, state = f type' state in
  let x, state = collect_freshs (solve_bis span) x state in
  let ctx = { state.ctx with level = state.ctx.level - 1 } in
  let state = { state with ctx } in
  x, state
