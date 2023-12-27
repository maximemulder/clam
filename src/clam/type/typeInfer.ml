(* UTILS *)

let todo = (Failure "TODO")

let cmp_bind a b =
  a.Abt.id = b.Abt.id

(* CONTEXTS *)

type polarity = Positive | Negative

type def_entry = {
  bind: Abt.bind_expr;
  def: Abt.def_expr;
}

type bind_entry = {
  bind: Abt.bind_expr;
  type': Type.type';
}

type bound_entry = {
  bind: Abt.bind_type;
  polarity: polarity;
  bound: Type.type';
}

(* STATE *)

type state = {
  defs: def_entry list;
  binds: bind_entry list;
  bounds: bound_entry list;
}

let make_state defs binds =
  let defs = List.map (fun def -> { bind = def.Abt.bind; def }) defs in
  { defs; binds; bounds = [] }

let get_context state =
  let assumptions = List.map (fun entry -> { TypeContext.bind = entry.bind; bound = entry.bound }) state.bounds in
  { TypeContext.assumptions }, state

let get_bind_def bind state =
  let entry = List.find (fun (entry: def_entry) -> cmp_bind entry.bind bind) state.defs in
  entry.def, state

let get_bind_type bind state =
  let entry = List.find_opt (fun (entry: bind_entry) -> cmp_bind entry.bind bind) state.binds in
  let type' = Option.map (fun entry -> entry.type')  entry in
  type', state

let get_bound bind state =
  let entry = List.find (fun (entry: bound_entry) -> entry.bind == bind) state.bounds in
  entry.bound, state

let add_bound bind bound state =
  let bounds = List.map (fun (entry: bound_entry) -> if entry.bind == bind then
    let ctx, _ = get_context state in
    let bound = match entry.polarity with
    | Positive -> TypeSystem.join ctx entry.bound bound
    | Negative -> TypeSystem.meet ctx entry.bound bound
    in
    { entry with bound }
  else
    entry) state.bounds in
  (), { state with bounds }

let remove_def bind state =
  let defs = List.filter (fun (entry: def_entry) -> not (cmp_bind entry.bind bind)) state.defs in
  let state = { state with defs } in
  (), state

let add_bind bind type' state =
  let binds = { bind; type' } :: state.binds in
  let state = { state with binds } in
  (), state

let add_var bind polarity state =
  let bound = match polarity with
  | Positive -> Type.base (Type.Bot)
  | Negative -> Type.base (Type.Top)
  in
  let bounds = { bind; polarity; bound } :: state.bounds in
  let state = { state with bounds } in
  (), state

let with_bind bind type' f state =
  let binds = { bind; type' } :: state.binds in
  let state = { state with binds } in
  let x, state = f state in
  let binds = List.filter (fun (entry: bind_entry) -> not (cmp_bind entry.bind bind)) state.binds in
  let state = { state with binds } in
  x, state

open Monad.Monad(Monad.StateMonad(struct
  type s = state
end))

let counter = ref 0

let fresh_var (_: unit) =
  let name = "'" ^ string_of_int counter.contents in
  counter := counter.contents + 1;
  let bind = { Abt.name } in
  bind, Type.base (Type.Var { bind })

let unwrap_base type' = List.nth (List.nth (type'.Type.union) 0).inter 0

(* VALIDATE *)

let validate_proper type' =
  let* ctx = get_context in
  return (TypeValidate.validate_proper ctx type')

(* TYPE INFERENCE *)

let constrain sub sup =
  match unwrap_base sub, sup with
  | Type.Var var, _ ->
    add_bound var.bind sup
  | _, _ ->
    return ()

let rec infer (expr: Abt.expr) =
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
    infer_abs abs
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
  let* cond' = infer if'.cond in
  let* () = constrain cond' (Type.base Type.Bool) in
  let* then' = infer if'.then' in
  let* else' = infer if'.else' in
  let* ctx = get_context in
  return (TypeSystem.join ctx then' else')

and infer_abs abs =
  match abs.param.type' with
  | Some type' ->
    let* type' = validate_proper type' in
    let* ret = with_bind abs.param.bind type'
      (infer abs.body) in
    return (Type.base (Type.AbsExpr { param = type'; ret }))
  | None ->
    let param_bind, param_type = fresh_var () in
    let* () = add_var param_bind Negative in
    let* ret = with_bind abs.param.bind param_type
      (infer abs.body) in
    let* param_bound = get_bound param_bind in
    if TypeUtils.contains ret param_bind then
      let abs = Type.base (Type.AbsExpr { param = param_type; ret }) in
      return (Type.base (Type.AbsTypeExpr { param = { bind = param_bind; bound = param_bound }; ret = abs }))
    else
      return (Type.base (Type.AbsExpr { param = param_bound; ret }))

and infer_app app =
  let* abs = infer app.expr in
  let* arg = infer app.arg in
  match unwrap_base abs with
  | AbsExpr abs ->
    let* () = constrain arg abs.param in
    return abs.ret
  | _ ->
    raise todo

and infer_def def =
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
    let var_bind, var_type = fresh_var () in
    let* () = add_var var_bind Positive in
    let* body_type = with_bind def.bind var_type
      (infer def.expr) in
    let* bound_type = get_bound var_bind in
    let* () = constrain bound_type body_type in
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
  List.iter (fun (e: bind_entry) -> print_endline(e.bind.name ^ ": " ^ TypeDisplay.display e.type')) state.binds
