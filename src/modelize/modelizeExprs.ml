open Collection

type state = {
  parent: state option;
  types: Model.type' NameMap.t;
  remains: Ast.def_expr NameMap.t;
  currents: Model.expr_bind NameMap.t;
  dones: Model.expr_bind NameMap.t;
  all_types: Model.type' list;
  all_exprs: Model.def_expr list;
  id: int;
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
      then ModelizeErrors.raise_expr_duplicate name
      else NameSet.add name set
    ) set names

let fold_remain map remain =
  NameMap.add remain.Ast.expr_name remain map

let fold_done map done' =
  NameMap.add (fst done') { Model.bind_expr = Some (snd done') } map

let make_state parent types (remains: Ast.def_expr list) dones id =
  let names = NameSet.empty in
  let names = check_duplicates (List.map (fun remain -> remain.Ast.expr_name) remains) names in
  let _ = check_duplicates (List.map (fun done' -> fst done') dones) names in
  let remains = List.fold_left fold_remain NameMap.empty remains in
  let dones = List.fold_left fold_done NameMap.empty dones in
  { parent; remains; types; currents = NameMap.empty; dones; all_exprs = []; all_types = []; id }

let parse_int expr (value: string) =
  match int_of_string_opt value with
  | Some int -> int
  | None     -> ModelizeErrors.raise_expr_integer expr value

let parse_char (value: string) =
  value.[0]

let parse_string (value: string) =
  value

let with_scope call types defs dones state =
  let parent = state in
  let state = make_state (Some parent) types defs dones state.id in
  let (result, state) = call state in
  let all_types = state.all_types in
  let all_exprs = state.all_exprs in
  let state = Option.get state.parent in
  (result, { state with all_exprs = List.append state.all_exprs all_exprs; all_types = List.append state.all_types all_types; id = state.id })

let rec translate_state state =
  {
    ModelizeTypes.parent = Option.map translate_state state.parent;
    ModelizeTypes.remains = NameMap.empty;
    ModelizeTypes.currents =  NameMap.empty;
    ModelizeTypes.dones = state.types;
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
  | Some bind -> ((Model.ExprBind bind), state)
  | None      ->
  match find_done name state with
  | Some bind -> ((Model.ExprBind bind), state)
  | None      ->
  match state.parent with
  | Some parent ->
    let (type', parent) = modelize_name expr name parent in
    (type', { state with parent = Some parent })
  | None -> ModelizeErrors.raise_expr_bound expr name

and modelize_def def state =
  let name = def.Ast.expr_name in
  let (remain, remains) = Collection.extract name state.remains in
  let currents = NameMap.add name { Model.bind_expr = None } state.currents in
  let state = { state with remains; currents } in
  let type' = Option.map (fun type' -> fst (modelize_type type' state)) remain.Ast.expr_type in
  let (expr, state) = modelize_expr remain.Ast.expr state in
  let (current, currents) = Collection.extract name state.currents in
  let def = Model.make_def_expr def.Ast.expr_pos state.id name type' expr in
  let _ = current.bind_expr <- Some (Model.BindExprDef def) in
  let dones = NameMap.add name current state.dones in
  (snd expr, { state with currents; dones; all_exprs = def :: state.all_exprs; id = state.id + 1 })

and modelize_expr_data (expr: Ast.expr): state -> Model.expr_data * state =
  match snd expr with
  | ExprIdent name ->
    modelize_name expr name
  | ExprVoid ->
    return Model.ExprVoid
  | ExprTrue ->
    return (Model.ExprBool true)
  | ExprFalse ->
    return (Model.ExprBool false)
  | ExprInt value ->
    return (Model.ExprInt (parse_int expr value))
  | ExprChar value ->
    return (Model.ExprChar (parse_char value))
  | ExprString value ->
    return (Model.ExprString (parse_string value))
  | ExprTuple exprs ->
    let* exprs = list_map modelize_expr exprs in
    return (Model.ExprTuple exprs)
  | ExprRecord attrs ->
    let* attrs = list_map modelize_attr attrs in
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
  | ExprBlock block ->
    modelize_block block
  | ExprIf (cond, then', else') ->
    let* cond = modelize_expr cond in
    let* then' = modelize_expr then' in
    let* else' = modelize_expr else' in
    return (Model.ExprIf (cond, then', else'))
  | ExprAbs (params, type', expr) ->
    let* params = list_map modelize_param params in
    let* type' = option_map modelize_type type' in
    let* expr = modelize_abs_expr params expr in
    return (Model.ExprAbs (params, type', expr))
  | ExprApp (expr, args) ->
    let* expr = modelize_expr expr in
    let* args = list_map modelize_expr args in
    return (Model.ExprApp (expr, args))
  | ExprTypeAbs (params, expr) ->
    let* params = list_map modelize_type_param params in
    modelize_type_abs_expr params expr
  | ExprTypeApp (expr, args) ->
    let* expr = modelize_expr expr in
    let* args = list_map modelize_type args in
    return (Model.ExprTypeApp (expr, args))

and modelize_expr (expr: Ast.expr): state -> Model.expr * state =
  let* expr_data = modelize_expr_data expr in
  return (fst expr, expr_data)

and modelize_param (param: Ast.param) state =
  let (type', state) = option_map modelize_type param.param_type state in
  (Model.make_param_expr param.param_pos state.id param.param_name type', { state with id = state.id + 1 })

and modelize_type_param (param: Ast.param) =
  let* type' = option_map modelize_type param.param_type in
  let type' = Option.value type' ~default:(param.param_pos, Model.TypeAny) in
  return { Model.param_type_name = param.param_name; Model.param_type = type' }

and modelize_attr (attr: Ast.attr_expr) =
  let* expr = modelize_expr attr.attr_expr in
  return { Model.attr_expr_name = attr.attr_expr_name; Model.attr_expr = expr }

and modelize_block (block: Ast.block) state =
  let (types, all) = ModelizeTypes.modelize_block block (translate_state state) in
  let defs = Ast.get_block_exprs block in
  with_scope (fun state ->
    let state = modelize_defs state in
    let (expr, state) = modelize_expr block.block_expr state in
    ((Model.ExprBlock { Model.block_expr = expr }), state)
  ) types defs [] { state with all_types = List.append state.all_types all }

and modelize_abs_expr params expr state =
  let params = List.map (fun param -> (param.Model.param_expr_name, Model.BindExprParam param)) params in
  with_scope (modelize_expr expr) NameMap.empty [] params state

and modelize_type_abs_expr params expr state =
  let types = ModelizeTypes.modelize_abs params (translate_state state) in
  let (expr, state) = with_scope (modelize_expr expr) types [] [] state in
  ((Model.ExprTypeAbs (params, expr)), state)

and modelize_defs state =
  match NameMap.choose_opt state.remains with
  | None -> state
  | Some (_, def) ->
    let (_, state) = modelize_def def state in
    modelize_defs state

let modelize_program (program: Ast.program) (types: Model.type' NameMap.t) (all_types: Model.type' list)=
  let defs = Ast.get_program_exprs program in
  let state = make_state None types defs [] 0 in
  let state = { state with all_types = all_types } in
  let state = modelize_defs state in
  (state.all_exprs, state.all_types)
