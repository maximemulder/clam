open Modelize_state

type state = {
  parent: state option;
  types: Model.type' NameMap.t;
  remains: Ast.expr NameMap.t;
  currents: Ast.expr NameMap.t;
  dones: Model.expr NameMap.t;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let find_remain name state =
  NameMap.find_opt name state.remains

let find_current name state =
  NameMap.find_opt name state.currents

let find_done name state =
  NameMap.find_opt name state.dones

let check_duplicates names set =
  List.fold_left (fun set name ->
    if NameSet.mem name set
      then Modelize_errors.raise ("duplicate expr `" ^ name ^ "`")
      else NameSet.add name set
    ) set names

let fold_remain map remain =
  NameMap.add remain.Ast.expr_name remain.Ast.expr map

let new_state parent types remains =
  let _ = check_duplicates (List.map (fun remain -> remain.Ast.expr_name) remains) NameSet.empty in
  let remains = List.fold_left fold_remain NameMap.empty remains in
  { parent; remains; types; currents = NameMap.empty; dones = NameMap.empty }

let parse_int (value: string) =
  match int_of_string_opt value with
  | Some int -> int
  | None     -> Modelize_errors.raise ("invalid integer `" ^ value ^ "`")

let parse_char (value: string) =
  value.[0]

let parse_string (value: string) =
  value

let extract key map =
  let value = NameMap.find key map in
  let map = NameMap.remove key map in
  (value, map)

let with_name name call state =
  let (expr, remains) = extract name state.remains in
  let currents = NameMap.add name expr state.currents in
  let state = { state with remains; currents } in
  let (expr, state) = call expr state in
  let currents = NameMap.remove name state.currents in
  let dones = NameMap.add name expr state.dones in
  (expr, { state with currents; dones })

let with_scope call types defs state =
  let parent = state in
  let state = new_state (Some parent) types defs in
  let (result, state) = call state in
  let state = Option.get state.parent in
  (result, state)

let rec translate_state state =
  {
    Modelize_types.parent = Option.map translate_state state.parent;
    Modelize_types.remains = NameMap.empty;
    Modelize_types.currents =  NameMap.empty;
    Modelize_types.dones = state.types
  }

let modelize_type (type': Ast.type') state =
  (Modelize_types.modelize_type_expr type' (translate_state state), state)

let rec modelize_name name state =
  match find_remain name state with
  | Some expr -> modelize_def name expr state
  | None     ->
  match find_remain name state with
  | Some _ -> Modelize_errors.raise ("recursive expr `" ^ name ^ "`")
  | None   ->
  match find_done name state with
  | Some expr -> (expr, state)
  | None      ->
  match state.parent with
  | Some parent ->
    let (type', parent) = modelize_name name parent in
    (type', { state with parent = Some parent })
  | None -> Modelize_errors.raise ("unbound expr `" ^ name ^ "`")

and modelize_def (name: string) (_expr: Ast.expr) =
  with_name name modelize_expr

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
    modelize_abs params expr
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

and modelize_block (block: Ast.block) state =
  let types = Modelize_types.modelize_block block (translate_state state) in
  with_scope (fun state ->
    let state = modelize_defs state in
    modelize_expr block.block_expr state
  ) types (Ast.get_block_exprs block) state

and modelize_abs params expr state =
let (params, state) = map_list modelize_param params state in
let types = Modelize_types.modelize_abs params (translate_state state) in
with_scope (fun state ->
  modelize_expr expr state
) types [] state

and modelize_defs state =
  match NameMap.choose_opt state.remains with
  | None -> state
  | Some (name, remain) ->
    let (_, state) = modelize_def name remain state in
    modelize_defs state

let modelize_program (program: Ast.program) (types: Model.type' NameMap.t) =
  let defs = Ast.get_program_exprs program in
  let state = new_state None types defs in
  (modelize_defs state).dones
