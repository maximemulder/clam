(*
  This file contains the environment used for type inference, which itself contains information
  about the current inference type variables, parameter type variables and expression types.
*)

open Type.Context

let cmp_bind a b =
  a.Abt.id = b.Abt.id

type entry_def = {
  bind: Abt.bind_expr;
  def: Abt.def_expr;
}

type entry_expr = {
  span: Code.span;
  bind: Abt.bind_expr;
  level: int;
  type': Type.type';
}

(* STATE *)

type state = {
  defs: entry_def list;
  exprs: entry_expr list;
  ctx: ctx;
}

module Monad = Util.Monad.StateMonad(struct
  type s = state
end)

open Monad

(* FUNCTIONS *)

let make_state defs exprs =
  let defs = List.map (fun (def: Abt.def_expr) -> { bind = def.bind; def }) defs in
  { defs; exprs; ctx = empty }

(* Adapter functions to directly use the type context monadic functions. *)

let with_ctx f state =
  let x, ctx = f state.ctx in
  x, { state with ctx }

let get_var bind =
  with_ctx (get_var bind)

let update_fresh fresh =
  with_ctx (update_fresh fresh)

let validate type' =
  with_ctx (Type.Validate.validate type')

let validate_param param =
  with_ctx (Type.Validate.validate_param param)

let validate_proper type' =
  with_ctx (Type.Validate.validate_proper type')

let substitute bind arg type' =
  with_ctx (Type.System.substitute bind arg type')

let is left right =
  with_ctx (Type.Context.with_freeze (Type.System.is left right))

let isa sub sup =
  with_ctx (Type.System.isa sub sup)

let join left right =
  with_ctx (Type.System.join left right)

let meet left right =
  with_ctx (Type.System.meet left right)

let with_param_rigid (param: Type.param) f state =
  let var = { bind = param.bind; lower = param.lower; upper = param.upper } in
  let ctx = { state.ctx with rigids = var :: state.ctx.rigids } in
  let state = { state with ctx } in
  let x, state = f state in
  let ctx = { state.ctx with rigids = List.tl state.ctx.rigids } in
  let state = { state with ctx } in
  x, state

(* PRINT STATE CONTEXT *)

let show_infer string state =
  if !Global.show_infer then
    with_ctx (show string) state
  else
    (), state

let show_infer_ctx state =
  if !Global.show_infer then
    with_ctx show_ctx state
  else
    (), state

(* STATE FUNCTION *)

let get_def_entry bind state =
  List.find (fun (entry: entry_def) -> cmp_bind entry.bind bind) state.defs, state

let get_expr_entry bind state =
  List.find_opt (fun (entry: entry_expr) -> cmp_bind entry.bind bind) state.exprs, state

let update_expr f bind state =
  let exprs = List.map (fun (var_expr: entry_expr) ->
    if var_expr.bind == bind then f var_expr else var_expr
  ) state.exprs in
  (), { state with exprs }

let update_expr_level bind level =
  update_expr (fun (var_expr: entry_expr) -> { var_expr with level }) bind

let update_expr_type bind type' =
  update_expr (fun (var_expr: entry_expr) -> { var_expr with type' }) bind

let get_def bind =
  let* entry = get_def_entry bind in
  return entry.def

let get_expr_type bind =
  let* entry = get_expr_entry bind in
  let type' = Option.map (fun entry -> entry.type') entry in
  return type'

let add_expr span bind type' state =
  let exprs = { span; bind; level = state.ctx.level; type' } :: state.exprs in
  (), { state with exprs }

let remove_expr bind state =
  let exprs = List.filter (fun (entry: entry_expr) -> not (cmp_bind entry.bind bind)) state.exprs in
  (), { state with exprs }

let with_expr span bind type' f =
  let* () = add_expr span bind type' in
  let* x = f in
  let* () = remove_expr bind in
  return x

let get_high_exprs state =
  List.filter (fun (var_expr: entry_expr) -> var_expr.level = state.ctx.level) state.exprs, state
