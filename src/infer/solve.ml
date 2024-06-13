(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently a little ugly and almost certainly buggy. The biggest challenge is to figure
  out in which order the type variables should be treated.
*)

open Abt.Display
open Cooccur
open Polar
open Type.Context
open Type.Rename
open State
open State.Monad

let inline bind bound type' =
  let rec' = Type.appears bind bound in
  let span = Abt.Span.type_span type' in
  let bound = if rec' then Abt.Type.Rec { span; bind; body = bound } else bound in
  substitute bind bound type'

let solve (fresh: fresh) type' =
  let* occs = with_ctx (occurs fresh.bind Pos type') in
  match occs.neg, occs.pos with
  | true, true ->
    let* () = show !Global.show_infer ("quantify " ^ fresh.bind.name ^ " in " ^ display type') in
    let* cond = is fresh.lower fresh.upper in
    if cond then
      substitute fresh.bind fresh.lower type'
    else
    let* type' = with_ctx (simplify fresh Pos type') in
    let param_bind = { Abt.Type.name = fresh.bind.name } in
    let type' = rename fresh.bind param_bind type' in
    let span = Abt.Span.type_span type' in
    return (Abt.Type.Univ { span; param = { span; bind = param_bind; lower = fresh.lower; upper = fresh.upper }; ret = type' })
  | true, false ->
    let* () = show !Global.show_infer ("inline_neg " ^ fresh.bind.name ^ " in " ^ display type') in
    inline fresh.bind fresh.upper type'
  | false, true ->
    let* () = show !Global.show_infer ("inline_pos " ^ fresh.bind.name ^ " in " ^ display type') in
    inline fresh.bind fresh.lower type'
  | false, false ->
    let* () = show !Global.show_infer ("none " ^ fresh.bind.name ^ " in " ^ display type') in
    return type'

let solve_type fresh type' =
  let* type' = solve fresh type' in
  let* () = show !Global.show_infer ("= " ^ display type') in
  return type'

let solve_expr (fresh: fresh) (var_expr: entry_expr) =
  let* type' = solve fresh var_expr.type' in
  let* () = update_expr_type var_expr.bind type' in
  let* () = show !Global.show_infer (var_expr.bind.name ^ ": " ^ display type') in
  (* let* level = Level.get_level type' in
  match level with
  | Some level ->
    let* () = update_expr_level var_expr.bind level in
    return ()
  | None -> *)
  (* TODO: Why does mutual recursion seem to work without this code. *)
    return ()

let solve_exprs fresh =
  let* exprs = get_high_exprs in
  list_iter (solve_expr fresh) exprs

let solve_bis fresh type' =
  let* type' = solve_type fresh type'  in
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

(* TODO: Remove span parameter *)
let with_var span f state =
  let bind = { Abt.Type.name = "'" ^ string_of_int state.ctx.id } in
  let _ = show !Global.show_infer ("infer_fresh " ^ bind.name) state in
  let var = { bind; level = state.ctx.level + 1; lower = Type.Build.bot; upper = Type.Build.top } in
  let type' = Abt.Type.Var { span; bind } in
  let ctx = { state.ctx with id = state.ctx.id + 1; level = state.ctx.level + 1; freshs = var :: state.ctx.freshs } in
  let state = { state with ctx } in
  let x, state = f type' state in
  let x, state = collect_freshs solve_bis x state in
  let ctx = { state.ctx with level = state.ctx.level - 1 } in
  let state = { state with ctx } in
  x, state
