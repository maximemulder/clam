(* UTILS *)

let todo = (Failure "TODO")

let cmp_bind a b =
  a.Abt.id = b.Abt.id

(* CONTEXTS *)

type entry_def = {
  bind: Abt.bind_expr;
  def: Abt.def_expr;
}

type entry_type = {
  bind: Abt.bind_expr;
  type': Type.type';
}

type entry_bounds = {
  bind: Abt.bind_type;
  lower: Type.type';
  upper: Type.type';
}

(* STATE *)

type state = {
  defs: entry_def list;
  types: entry_type list;
  bounds: entry_bounds list;
}

let make_state defs types =
  let defs = List.map (fun def -> { bind = def.Abt.bind; def }) defs in
  { defs; types; bounds = [] }

let get_context state =
  (* TODO: bounds ??? *)
  let assumptions = List.map (fun entry -> { TypeContext.bind = entry.bind; bound = entry.upper }) state.bounds in
  { TypeContext.assumptions }, state

let get_bind_def bind state =
  let entry = List.find (fun (entry: entry_def) -> cmp_bind entry.bind bind) state.defs in
  entry.def, state

let get_bind_type bind state =
  let entry = List.find_opt (fun (entry: entry_type) -> cmp_bind entry.bind bind) state.types in
  let type' = Option.map (fun entry -> entry.type')  entry in
  type', state

let get_lower_bound bind state =
  let entry = List.find (fun (entry: entry_bounds) -> entry.bind == bind) state.bounds in
  entry.lower, state

let get_upper_bound bind state =
  let entry = List.find (fun (entry: entry_bounds) -> entry.bind == bind) state.bounds in
  entry.upper, state

let update_upper_bound bind bound state =
  let bounds = List.map (fun (entry: entry_bounds) ->
    if entry.bind == bind then
      let ctx, _ = get_context state in
      { entry with upper = TypeSystem.meet ctx entry.upper bound }
    else
      entry
    ) state.bounds in
  (), { state with bounds }

let update_lower_bound bind bound state =
  let bounds = List.map (fun (entry: entry_bounds) ->
    if entry.bind == bind then
      let ctx, _ = get_context state in
      { entry with lower = TypeSystem.join ctx entry.lower bound }
    else
      entry
    ) state.bounds in
  (), { state with bounds }

let remove_def bind state =
  let defs = List.filter (fun (entry: entry_def) -> not (cmp_bind entry.bind bind)) state.defs in
  let state = { state with defs } in
  (), state

let add_bind bind type' state =
  let types = { bind; type' } :: state.types in
  let state = { state with types } in
  (), state

let with_bind bind type' f state =
  let types = { bind; type' } :: state.types in
  let state = { state with types } in
  let x, state = f state in
  let types = List.filter (fun (entry: entry_type) -> not (cmp_bind entry.bind bind)) state.types in
  let state = { state with types } in
  x, state

open Monad.Monad(Monad.StateMonad(struct
  type s = state
end))

let counter = ref 0

(* TODO: Use a "with" function instead to constrain each bound to its scope *)
let make_var state =
  let name = "'" ^ string_of_int counter.contents in
  counter := counter.contents + 1;
  let bind = { Abt.name } in
  let type' = Type.base (Type.Var { bind }) in
  let bound = { bind; lower = TypePrimitive.bot; upper = TypePrimitive.top } in
  let state = { state with bounds = bound :: state.bounds } in
  (bind, type'), state

let unwrap_base type' = List.nth (List.nth (type'.Type.union) 0).inter 0

(* VALIDATE *)

let validate_proper type' =
  let* ctx = get_context in
  return (TypeValidate.validate_proper ctx type')

(* TYPE INFERENCE *)

let constrain sub sup =
  print_endline("constrain " ^ TypeDisplay.display sub ^ " " ^ TypeDisplay.display sup);
  match unwrap_base sub, unwrap_base sup with
  | Type.Var sub_var, Type.Var sup_var when sub_var.bind == sup_var.bind ->
    return ()
  | Type.Var var, _ ->
    update_upper_bound var.bind sup
  | _, Type.Var var ->
    update_lower_bound var.bind sub
  | _, _ ->
    return ()

let return_void _ state = (), state

let rec infer (expr: Abt.expr) =
  infer_return expr return_void

and infer_return expr returner =
  match expr with
  | ExprUnit unit ->
    infer_unit unit
  | ExprBool bool ->
    infer_bool bool
  | ExprInt int ->
    infer_int int
  | ExprString string ->
    infer_string string
  | ExprBind bind ->
    infer_bind bind
  | ExprTuple tuple ->
    infer_tuple tuple
  | ExprIf if' ->
    infer_if if'
  | ExprAbs abs ->
    infer_abs abs returner
  | ExprApp app ->
    infer_app app
  | _ ->
    raise todo

and infer_unit _ =
  return (Type.base Type.Unit)

and infer_bool _ =
  return (Type.base Type.Bool)

and infer_int _ =
  return (Type.base Type.Int)

and infer_string _ =
  return (Type.base Type.String)

and infer_bind bind =
  let bind = Option.get !(bind.bind) in
  let* type' = get_bind_type bind in
  match type' with
  | Some type' ->
    return type'
  | None ->
  let* def = get_bind_def bind in
  infer_def def

and infer_tuple tuple =
  let* elems = map_list infer tuple.elems in
  return (Type.base (Type.Tuple { elems }))

and infer_if if' =
  (* TODO: Do not join but constrain context to both then and else *)
  let* cond' = infer if'.cond in
  let* () = constrain cond' (Type.base Type.Bool) in
  let* then' = infer if'.then' in
  let* else' = infer if'.else' in
  let* ctx = get_context in
  return (TypeSystem.join ctx then' else')

and infer_abs abs returner =
  match abs.param.type' with
  | Some type' ->
    let* type' = validate_proper type' in
    let* ret = with_bind abs.param.bind type'
      (infer abs.body) in
    return (Type.base (Type.AbsExpr { param = type'; ret }))
  | None ->
    let* param_bind, param_type = make_var in
    let* ret_bind, ret_type = make_var in
    let abs_type = (Type.base (Type.AbsExpr { param = param_type; ret = ret_type })) in
    let* _ = returner abs_type in
    let* ret = with_bind abs.param.bind param_type
      (infer abs.body) in
    let* param_bound = get_upper_bound param_bind in
    let* lower = get_lower_bound param_bind in
    let* upper = get_upper_bound param_bind in
    print_endline("lower param " ^ TypeDisplay.display lower);
    print_endline("upper param " ^ TypeDisplay.display upper);
    let* lower = get_lower_bound ret_bind in
    let* upper = get_upper_bound ret_bind in
    print_endline("ret " ^ TypeDisplay.display ret);
    print_endline("lower ret " ^ TypeDisplay.display lower);
    print_endline("upper ret " ^ TypeDisplay.display upper);
    if TypeUtils.contains ret param_bind then
      let abs = Type.base (Type.AbsExpr { param = param_type; ret }) in
      return (Type.base (Type.AbsTypeExpr { param = { bind = param_bind; bound = param_bound }; ret = abs }))
    else
      return (Type.base (Type.AbsExpr { param = param_bound; ret }))

and infer_app app =
  let* abs = infer app.expr in
  let* arg = infer app.arg in
  infer_app_type abs arg

and infer_app_type abs arg =
  match unwrap_base abs with
  | AbsExpr abs ->
    let* () = constrain arg abs.param in
    return abs.ret
  | Var _ ->
    let* ctx = get_context in
    let abs = TypeSystem.promote ctx abs in
    infer_app_type abs arg
  | _ ->
    raise todo

and infer_def def =
  print_endline("");
  print_endline("infer def " ^ def.bind.name);
  let* () = remove_def def.bind in
  let* type' = infer_def_type def in
  let* () = add_bind def.bind type' in
  return type'

and infer_def_type def =
  match def.type' with
  | Some def_type ->
    let* def_type = validate_proper def_type in
    let* body_type = with_bind def.bind def_type
      (infer def.expr) in
    let* () = constrain body_type def_type in
    return def_type
  | None ->
    let* var_bind, var_type = make_var in
    let* body_type = with_bind def.bind var_type
      (infer_return def.expr (update_upper_bound var_bind)) in
    let* lower_bound = get_lower_bound var_bind in
    let* upper_bound = get_upper_bound var_bind in
    let* () = constrain lower_bound body_type in
    let* () = constrain body_type upper_bound in
    if TypeUtils.contains body_type var_bind then
      TypeError.infer_recursive_type def
    else
      return body_type

let rec check_defs state =
  match state.defs with
  | entry :: _ ->
    let _, state = infer_def entry.def state in
    check_defs state
  | [] ->
    state

let check_defs defs primitives =
  let primitives = List.map (fun primitive -> { bind = fst primitive; type' = snd primitive }) primitives in
  let state = make_state defs primitives in
  let state = check_defs state in
  List.iter (fun (e: entry_type) -> print_endline(e.bind.name ^ ": " ^ TypeDisplay.display e.type')) state.types
