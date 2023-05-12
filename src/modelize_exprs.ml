open Modelize_state

type remain = {
  remain_expr: Ast.expr;
  remain_type: Ast.type' option;
}

let remain_from_def def =
  { remain_expr = def.Ast.expr; remain_type = def.Ast.expr_type }

type done' = {
  done_expr: Model.expr;
  done_type: Model.type' option;
}

let done_from_components expr type' =
  { done_expr = expr; done_type = type'}

let done_from_param param =
  { done_expr = Model.ExprParam (param.Model.expr_param_name); done_type = (param.Model.expr_param_type) }

type state = {
  parent: state option;
  types: Model.type' NameMap.t;
  remains: remain NameMap.t;
  currents: Model.bind NameMap.t;
  dones: done' NameMap.t;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let find_remain name state =
  let remain = NameMap.find_opt name state.remains in
  Option.map (fun remain -> remain.remain_expr) remain

let find_current name state =
  NameMap.find_opt name state.currents

let find_done name state =
  let done' = NameMap.find_opt name state.dones in
  Option.map (fun done' -> done'.done_expr) done'

let check_duplicates names set =
  List.fold_left (fun set name ->
    if NameSet.mem name set
      then Modelize_errors.raise ("duplicate expr `" ^ name ^ "`")
      else NameSet.add name set
    ) set names

let fold_remain map remain =
  NameMap.add (fst remain) (snd remain) map

let fold_done map done' =
  NameMap.add (fst done') (snd done') map

let new_state parent types remains dones =
  let names = NameSet.empty in
  let names = check_duplicates (List.map fst remains) names in
  let _ = check_duplicates (List.map (fun done' -> fst done') dones) names in
  let remains = List.fold_left fold_remain NameMap.empty remains in
  let dones = List.fold_left fold_done NameMap.empty dones in
  { parent; remains; types; currents = NameMap.empty; dones }

let parse_int (value: string) =
  match int_of_string_opt value with
  | Some int -> int
  | None     -> Modelize_errors.raise ("invalid integer `" ^ value ^ "`")

let parse_char (value: string) =
  value.[0]

let parse_string (value: string) =
  value

let with_scope call types defs dones state =
  let parent = state in
  let state = new_state (Some parent) types defs dones in
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

(* TODO: Can I remove state from the return ? *)
let modelize_type (type': Ast.type') state =
  (Modelize_types.modelize_type_expr type' (translate_state state), state)

let rec modelize_name name state =
  match find_remain name state with
  | Some _ -> modelize_def name state
  | None      ->
  match find_current name state with
  | Some bind -> (Model.ExprBind bind, state)
  | None      ->
  match find_done name state with
  | Some expr -> (expr, state)
  | None      ->
  match state.parent with
  | Some parent ->
    let (type', parent) = modelize_name name parent in
    (type', { state with parent = Some parent })
  | None -> Modelize_errors.raise ("unbound expr `" ^ name ^ "`")

and modelize_def name state =
  let (remain, remains) = Modelize_state.extract name state.remains in
  let currents = NameMap.add name { Model.expr = None } state.currents in
  let state = { state with remains; currents } in
  let type' = Option.map (fun type' -> fst (modelize_type type' state)) remain.remain_type in
  let (expr, state) = modelize_expr remain.remain_expr state in
  let (current, currents) = Modelize_state.extract name state.currents in
  let _ = current.expr <- Some expr in
  let done' = done_from_components expr type' in
  let dones = NameMap.add name done' state.dones in
  (expr, { state with currents; dones })

and modelize_expr (expr: Ast.expr) =
  match expr with
  | ExprIdent name   -> modelize_name name
  | ExprVoid         -> return Model.ExprVoid
  | ExprTrue         -> return (Model.ExprBool true)
  | ExprFalse        -> return (Model.ExprBool false)
  | ExprInt    value -> return (Model.ExprInt (parse_int value))
  | ExprChar   value -> return (Model.ExprChar (parse_char value))
  | ExprString value -> return (Model.ExprString (parse_string value))
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
    let* expr = modelize_abs_expr params expr in
    return (Model.ExprAbs (params, type', expr))
  | ExprApp (expr, args) ->
    let* expr = modelize_expr expr in
    let* args = map_list modelize_expr args in
    return (Model.ExprApp (expr, args))
  | ExprTypeAbs (params, expr) ->
    let* params = map_list modelize_type_param params in
    modelize_type_abs_expr params expr
  | ExprTypeApp (expr, args) ->
    let* expr = modelize_expr expr in
    let* args = map_list modelize_type args in
    return (Model.ExprTypeApp (expr, args))

and modelize_param (param: Ast.param) =
  let* type' = map_option modelize_type param.param_type in
  return { Model.expr_param_name = param.param_name; Model.expr_param_type = type' }

and modelize_type_param (param: Ast.param) =
  let* type' = map_option modelize_type param.param_type in
  let type' = Option.value type' ~default:Model.TypeAny in
  return { Model.type_param_name = param.param_name; Model.type_param_type = type' }

and modelize_attr (attr: Ast.attr_expr) =
  let* expr = modelize_expr attr.attr_expr in
  return { Model.attr_expr_name = attr.attr_expr_name; Model.attr_expr = expr }

and modelize_block (block: Ast.block) state =
  let types = Modelize_types.modelize_block block (translate_state state) in
  let defs = List.map (fun def -> (def.Ast.expr_name, remain_from_def def)) (Ast.get_block_exprs block) in
  with_scope (fun state ->
    let state = modelize_defs state in
    modelize_expr block.block_expr state
  ) types defs [] state

and modelize_abs_expr params expr state =
  let params = List.map (fun param -> (param.Model.expr_param_name, done_from_param param)) params in
  with_scope (modelize_expr expr) NameMap.empty [] params state

and modelize_type_abs_expr params expr state =
  let types = Modelize_types.modelize_abs params (translate_state state) in
  with_scope (modelize_expr expr) types [] [] state

and modelize_defs state =
  match NameMap.choose_opt state.remains with
  | None -> state
  | Some (name, _) ->
    let (_, state) = modelize_def name state in
    modelize_defs state

let modelize_program (program: Ast.program) (types: Model.type' NameMap.t) =
  let defs = Ast.get_program_exprs program in
  let defs = List.map (fun def -> (def.Ast.expr_name, remain_from_def def)) defs in
  let state = new_state None types defs [] in
  (modelize_defs state).dones
