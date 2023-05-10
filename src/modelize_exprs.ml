type state = {
  remains: Ast.def_expr list;
  currents: Ast.def_expr list;
  scope: Scope.scope;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let find_remain name state =
  (List.find_opt (fun def -> def.Ast.expr_name = name) state.remains, state)

let find_current name state =
  (List.find_opt (fun def -> def.Ast.expr_name = name) state.currents, state)

let find_scope name state =
  (Scope.find_expr name state.scope, state)

let init_state remains scope =
  { remains; currents = []; scope }

module NameSet = Set.Make(Scope.NameKey)

let check_duplicates defs =
  let names = NameSet.empty in
  let _ = List.fold_left (fun names def ->
    let name = def.Ast.expr_name in
    if NameSet.mem name names
      then Modelize_errors.raise ("duplicate expr `" ^ name ^ "`")
      else NameSet.add name names
    ) names defs in
  ()

let parse_int (value: string) =
  match int_of_string_opt value with
  | Some int -> int
  | None     -> Modelize_errors.raise ("invalid integer `" ^ value ^ "`")

let parse_char (value: string) =
  value.[0]

let parse_string (value: string) =
  value

let enter name state =
  let (defs, remains) = List.partition (fun def -> def.Ast.expr_name == name) state.remains in
  let def = List.nth defs 0 in
  let currents = def :: state.currents in
  (def.expr, { state with remains; currents })

let exit name expr state =
  let currents = List.filter (fun def -> def.Ast.expr_name != name) state.currents in
  let scope = Scope.add_expr name expr state.scope in
  ((), { state with currents; scope })

let modelize_type (type': Ast.type') state =
  (Modelize_types.modelize_type_expr type' state.scope, state)

let rec modelize_name name =
  let* expr = find_remain name in
  match expr with
  | Some def -> modelize_def def
  | None     ->
  let* expr = find_current name in
  match expr with
  | Some _ -> Modelize_errors.raise ("recursive expr `" ^ name ^ "`")
  | None   ->
  let* expr = find_scope name in
  match expr with
  | Some expr -> return expr
  | None      -> Modelize_errors.raise ("unbound expr `" ^ name ^ "`")

and modelize_def (node: Ast.def_expr) =
  let name = node.expr_name in
  let* node = enter name in
  let* expr = modelize_expr node in
  let* _ = exit name expr in
  return expr

and modelize_expr (expr: Ast.expr) =
  match expr with
  | Ast.ExprIdent name   -> modelize_name name
  | Ast.ExprVoid         -> return Model.ExprVoid
  | Ast.ExprTrue         -> return (Model.ExprBool true)
  | Ast.ExprFalse        -> return (Model.ExprBool false)
  | Ast.ExprInt    value -> return (Model.ExprInt (parse_int value))
  | Ast.ExprChar   value -> return (Model.ExprChar (parse_char value))
  | Ast.ExprString value -> return (Model.ExprString (parse_string value))
  | ExprTuple exprs ->
    let* exprs = map_list modelize_expr exprs in
    return (Model.ExprTuple exprs)
  | ExprRecord attrs ->
    let* attrs = map_list modelize_attr attrs in
    return (Model.ExprRecord attrs)
  | ExprPreop (op, expr) ->
    let* expr = modelize_expr expr in
    return (Model.ExprPreop (op, expr))
  | ExprBinop  (left, op, right) ->
    let* left = modelize_expr left in
    let* right = modelize_expr right in
    return (Model.ExprBinop (left, op, right))
  | ExprAscr (expr, type') ->
    let* expr = modelize_expr expr in
    let* type' = modelize_type type' in
    return (Model.ExprAscr (expr, type'))
  | ExprBlock block -> modelize_block block
  | ExprIf (cond, then', else') ->
    let* cond = modelize_expr cond in
    let* then' = modelize_expr then' in
    let* else' = modelize_expr else' in
    return (Model.ExprIf (cond, then', else'))
  | ExprAbs (params, type', expr) ->
    let* params = map_list modelize_param params in
    let* type' = map_option modelize_type type' in
    let* expr = modelize_expr expr in
    return (Model.ExprAbs (params, type', expr))
  | ExprApp (expr, args) ->
    let* expr = modelize_expr expr in
    let* args = map_list modelize_expr args in
    return (Model.ExprApp (expr, args))
  | ExprTypeAbs (params, expr) ->
    let* params = map_list modelize_param params in
    let* expr = modelize_expr expr in
    return (Model.ExprTypeAbs (params, expr))
  | ExprTypeApp (expr, args) ->
    let* expr = modelize_expr expr in
    let* args = map_list modelize_type args in
    return (Model.ExprTypeApp (expr, args))

and modelize_param (param: Ast.param) =
  let* type' = modelize_type param.param_type in
  return { Model.param_name = param.param_name; Model.param_type = type' }

and modelize_attr (attr: Ast.attr_expr) =
  let* expr = modelize_expr attr.attr_expr in
  return { Model.attr_expr_name = attr.attr_expr_name; Model.attr_expr = expr }

and modelize_block (block: Ast.block) =
  modelize_expr block.block_expr

let rec modelize_defs state =
  match state.remains with
  | [] -> state
  | remain :: _ ->
    let (_, state) = modelize_def remain state in
    modelize_defs state

let modelize_program (program: Ast.program) (scope: Scope.scope) =
  let defs = Ast.get_exprs program in
  let _ = check_duplicates defs in
  let state = init_state defs scope in
  (modelize_defs state).scope
