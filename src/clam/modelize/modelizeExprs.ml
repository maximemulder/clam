open Utils

type scope = {
  parent: scope option;
  types: Model.type' NameMap.t;
  remains: Ast.def_expr NameMap.t;
  currents: Model.bind_expr option ref NameMap.t;
  dones: Model.bind_expr option ref NameMap.t;
}

type state = {
  scope: scope;
  all_types: Model.type' list;
  all_exprs: Model.def_expr list;
  id: int;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let find_remain name state =
  NameMap.find_opt name state.scope.remains

let find_current name state =
  NameMap.find_opt name state.scope.currents

let find_done name state =
  NameMap.find_opt name state.scope.dones

let next_id state =
  (state.id, { state with id = state.id + 1 })

let fold_remain map (remain: Ast.def_expr) =
  NameMap.add remain.Ast.name remain map

let fold_done map done' =
  NameMap.add (fst done') { contents = Some (snd done') } map

let make_state types remains dones =
  let remains = List.fold_left fold_remain NameMap.empty remains in
  let dones = List.fold_left fold_done NameMap.empty dones in
  let scope = { parent = None; types; remains; currents = NameMap.empty; dones } in
  { scope; all_exprs = []; all_types = []; id = 0 }

let make_child types remains dones state =
  let remains = List.fold_left fold_remain NameMap.empty remains in
  let dones = List.fold_left fold_done NameMap.empty dones in
  let scope = { parent = Some state.scope; types; remains; currents = NameMap.empty; dones } in
  ((), { state with scope })

let parse_int expr (value: string) =
  match int_of_string_opt value with
  | Some int -> int
  | None     -> ModelizeErrors.raise_expr_integer expr value

let parse_char (value: string) =
  value.[0]

let parse_string (value: string) =
  value

let with_scope call types defs dones state =
  let (_, state) = make_child types defs dones state in
  let (result, state) = call state in
  (result, { state with scope = Option.get state.scope.parent })

let rec translate_scope state =
  {
    ModelizeTypes.parent = Option.map translate_scope state.parent;
    ModelizeTypes.remains = NameMap.empty;
    ModelizeTypes.currents =  NameMap.empty;
    ModelizeTypes.dones = state.types;
  }

let translate_state state =
  {
    ModelizeTypes.scope = translate_scope state.scope;
    ModelizeTypes.all = [];
  }

(* TODO: Can I remove state from the return ? *)
let modelize_type (type': Ast.type') state =
  (ModelizeTypes.modelize_type_expr type' (translate_state state), state)

let rec modelize_name expr name state =
  match find_remain name state with
  | Some def -> modelize_def def state
  | None     ->
  match find_current name state with
  | Some bind -> (bind, state)
  | None      ->
  match find_done name state with
  | Some bind -> (bind, state)
  | None      ->
  match state.scope.parent with
  | Some scope ->
    let parent = { state with scope } in
    let (expr, parent) = modelize_name expr name parent in
    (expr, { parent with scope = { state.scope with parent = Some parent.scope } })
  | None -> ModelizeErrors.raise_expr_bound expr name

and modelize_def def state =
  let name = def.Ast.name in
  let (remain, remains) = extract name state.scope.remains in
  let currents = NameMap.add name { contents = None } state.scope.currents in
  let state = { state with scope = { state.scope with remains; currents } } in
  let type' = Option.map (fun type' -> fst (modelize_type type' state)) remain.Ast.type' in
  let (expr, state) = modelize_expr remain.Ast.expr state in
  let (current, currents) = extract name state.scope.currents in
  let (id, state) = next_id state in
  let def = { Model.pos = def.pos; id; name; type'; expr } in
  let _ = current := Some (Model.BindExprDef def) in
  let dones = NameMap.add name current state.scope.dones in
  let state = { state with scope = { state.scope with currents; dones}; all_exprs = def :: state.all_exprs } in
  (current, state)

and modelize_expr (expr: Ast.expr): state -> Model.expr * state =
  match snd expr with
  | ExprUnit ->
    modelize_unit expr
  | ExprTrue ->
    modelize_bool expr true
  | ExprFalse ->
    modelize_bool expr false
  | ExprInt value ->
    modelize_int expr value
  | ExprChar value ->
    modelize_char expr value
  | ExprString value ->
    modelize_string expr value
  | ExprBind name ->
    modelize_bind expr name
  | ExprTuple exprs ->
    modelize_tuple expr exprs
  | ExprProduct fields ->
    modelize_product expr fields
  | ExprElem (expr, index) ->
    modelize_elem expr index
  | ExprAttr (expr, name) ->
    modelize_attr expr name
  | ExprPreop (op, operand) ->
    modelize_preop expr op operand
  | ExprBinop  (left, op, right) ->
    modelize_binop expr left op right
  | ExprAscr (expr, type') ->
    modelize_ascr expr type'
  | ExprIf (cond, then', else') ->
    modelize_if expr cond then' else'
  | ExprAbs (params, body) ->
    modelize_abs expr params body
  | ExprApp (expr, args) ->
    modelize_app expr args
  | ExprTypeAbs (params, body) ->
    let* params = map_list modelize_type_param params in
    modelize_type_abs_expr expr params body
  | ExprTypeApp (expr, args) ->
    modelize_type_app expr args
  | ExprStmt (stmt, expr) ->
    modelize_stmt stmt expr

and modelize_unit expr =
  return (Model.ExprUnit { pos = fst expr })

and modelize_bool expr value =
  return (Model.ExprBool { pos = fst expr; value })

and modelize_int expr value =
  let value = parse_int expr value in
  return (Model.ExprInt { pos = fst expr; value })

and modelize_char expr value =
  let value = parse_char value in
  return (Model.ExprChar { pos = fst expr; value })

and modelize_string expr value =
  let value = parse_string value in
  return (Model.ExprString { pos = fst expr; value })

and modelize_bind expr name =
  let* bind = modelize_name expr name in
  return (Model.ExprBind { pos = fst expr; bind })

and modelize_tuple expr elems =
  let* elems = map_list modelize_expr elems in
  return (Model.ExprTuple { pos = fst expr; elems })

and modelize_product expr fields =
  if (List.length fields) == 0 then
    return (Model.ExprRecord { pos = fst expr; attrs = [] })
  else if List.for_all (fun (field: Ast.field_expr) -> Option.is_some field.name) fields then
    let* attrs = map_list modelize_record_attr fields in
    return (Model.ExprRecord { pos = fst expr; attrs })
  else
    let* elems = map_list modelize_tuple_elem fields in
    return (Model.ExprTuple { pos = fst expr; elems })

and modelize_tuple_elem field =
  modelize_expr field.expr

and modelize_record_attr field =
  let* expr = modelize_expr field.expr in
  return { Model.pos = field.pos; Model.name = Option.get field.name; Model.expr = expr }

and modelize_elem expr index =
  let* expr2 = modelize_expr expr in
  let index = parse_int expr index in
  return (Model.ExprElem { pos = fst expr; expr = expr2; index })

and modelize_attr expr name =
  let* expr2 = modelize_expr expr in
  return (Model.ExprAttr { pos = fst expr; expr = expr2; name })

and modelize_preop expr op operand =
  let* operand = modelize_expr operand in
  return (Model.ExprPreop { pos = fst expr; op; expr = operand })

and modelize_binop expr left op right =
  let* left = modelize_expr left in
  let* right = modelize_expr right in
  return (Model.ExprBinop { pos = fst expr; left; op; right })

and modelize_param (param: Ast.param) =
  let* type' = map_option modelize_type param.type' in
  let* id = next_id in
  return { Model.pos = param.pos; id; name = param.name; type' }

and modelize_type_param (param: Ast.param) =
  let* type' = map_option modelize_type param.type' in
  let type' = Option.value type' ~default:(Model.TypeTop { pos = param.pos }) in
  return { Model.name = param.name; type' }

and modelize_stmt (stmt: Ast.stmt) (expr: Ast.expr) =
  match stmt with
  | StmtVar (var_name, var_type, var_expr) ->
    let* var_expr = modelize_expr var_expr in
    let* var_type = map_option modelize_type var_type in
    let* id = next_id in
    let var = { Model.id = id; Model.name = var_name } in
    let body = Model.StmtVar (var, var_type, var_expr) in
    let* expr = with_scope (modelize_expr expr) NameMap.empty [] [(var_name, Model.BindExprVar var)] in
    return (Model.ExprStmt { pos = Model.expr_pos expr; stmt = body; expr })
  | StmtExpr stmt_expr ->
    let* stmt_expr = modelize_expr stmt_expr in
    let stmt = Model.StmtExpr stmt_expr in
    let* expr = modelize_expr expr in
    return (Model.ExprStmt { pos = Model.expr_pos stmt_expr; stmt; expr })

and modelize_ascr expr type' =
  let* expr2 = modelize_expr expr in
  let* type' = modelize_type type' in
  return (Model.ExprAscr { pos = fst expr; expr = expr2; type' = type' })

and modelize_if expr cond then' else' =
  let* cond = modelize_expr cond in
  let* then' = modelize_expr then' in
  let* else' = modelize_expr else' in
  return (Model.ExprIf { pos = fst expr; cond = cond; then'; else' })

and modelize_abs expr params body =
  let* params = map_list modelize_param params in
  let pairs = List.map (fun (param: Model.param_expr) -> (param.Model.name, Model.BindExprParam param)) params in
  let* body = with_scope (modelize_expr body) NameMap.empty [] pairs in
  return (Model.ExprAbs { pos = fst expr; params; body })

and modelize_app expr args =
  let* expr2 = modelize_expr expr in
  let* args = map_list modelize_expr args in
  return (Model.ExprApp { pos = fst expr; expr = expr2; args })

and modelize_type_abs_expr expr params body state =
  let types = ModelizeTypes.modelize_abs params (translate_state state) in
  let (body, state) = with_scope (modelize_expr body) types [] [] state in
  (Model.ExprTypeAbs { pos = fst expr; params; body }, state)

and modelize_type_app expr args =
  let* expr2 = modelize_expr expr in
  let* args = map_list modelize_type args in
  return (Model.ExprTypeApp { pos = fst expr; expr = expr2; args })

let rec modelize_defs state =
  match NameMap.choose_opt state.scope.remains with
  | None -> state
  | Some (_, def) ->
    let (_, state) = modelize_def def state in
    modelize_defs state

let modelize_program (program: Ast.program) (types: Model.type' NameMap.t) (all_types: Model.type' list)=
  let defs = Ast.get_program_exprs program in
  let state = make_state types defs [("print", Model.BindExprPrint)] in
  let state = { state with all_types = all_types } in
  let state = modelize_defs state in
  (state.all_exprs, state.all_types)
