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

let fold_remain map remain =
  NameMap.add remain.Ast.expr_name remain map

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
  let name = def.Ast.expr_name in
  let (remain, remains) = extract name state.scope.remains in
  let currents = NameMap.add name { contents = None } state.scope.currents in
  let state = { state with scope = { state.scope with remains; currents } } in
  let type' = Option.map (fun type' -> fst (modelize_type type' state)) remain.Ast.expr_type in
  let (expr, state) = modelize_expr remain.Ast.expr state in
  let (current, currents) = extract name state.scope.currents in
  let (id, state) = next_id state in
  let def = Model.make_def_expr def.Ast.expr_pos id name type' expr in
  let _ = current := Some (Model.BindExprDef def) in
  let dones = NameMap.add name current state.scope.dones in
  let state = { state with scope = { state.scope with currents; dones}; all_exprs = def :: state.all_exprs } in
  (current, state)

and modelize_expr (expr: Ast.expr): state -> Model.expr * state =
  match snd expr with
  | ExprVoid ->
    modelize_void expr
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
  | ExprRecord attrs ->
    modelize_record expr attrs
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
  | ExprBlock block ->
    modelize_block expr block
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

and modelize_void expr =
  return (Model.ExprVoid {
    expr_void_pos = fst expr;
  })

and modelize_bool expr value =
  return (Model.ExprBool {
    expr_bool_pos = fst expr;
    expr_bool = value;
  })

and modelize_int expr value =
  let value = parse_int expr value in
  return (Model.ExprInt {
    expr_int_pos = fst expr;
    expr_int = value;
  })

and modelize_char expr value =
  let value = parse_char value in
  return (Model.ExprChar {
    expr_char_pos = fst expr;
    expr_char = value;
  })

and modelize_string expr value =
  let value = parse_string value in
  return (Model.ExprString {
    expr_string_pos = fst expr;
    expr_string = value;
  })

and modelize_bind expr name =
  let* bind = modelize_name expr name in
  return (Model.ExprBind {
    expr_bind_pos = fst expr;
    expr_bind = bind;
  })

and modelize_tuple expr exprs =
  let* exprs = map_list modelize_expr exprs in
  return (Model.ExprTuple {
    expr_tuple_pos = fst expr;
    expr_tuple_exprs = exprs;
  })

and modelize_record expr attrs =
  let* attrs = map_list modelize_attr2 attrs in
  return (Model.ExprRecord {
    expr_record_pos = fst expr;
    expr_record_attrs = attrs;
  })

and modelize_elem expr index =
  let* expr2 = modelize_expr expr in
  let index = parse_int expr index in
  return (Model.ExprElem {
    expr_elem_pos = fst expr;
    expr_elem_expr = expr2;
    expr_elem_index = index;
  })

and modelize_attr expr name =
  let* expr2 = modelize_expr expr in
  return (Model.ExprAttr {
    expr_attr_pos = fst expr;
    expr_attr_expr = expr2;
    expr_attr_name = name;
  })

and modelize_preop expr op operand =
  let* operand = modelize_expr operand in
  return (Model.ExprPreop {
    expr_preop_pos = fst expr;
    expr_preop_op = op;
    expr_preop_expr = operand;
  })

and modelize_binop expr left op right =
  let* left = modelize_expr left in
  let* right = modelize_expr right in
  return (Model.ExprBinop {
    expr_binop_pos = fst expr;
    expr_binop_left = left;
    expr_binop_op = op;
    expr_binop_right = right;
  })

and modelize_param (param: Ast.param) =
  let* type' = map_option modelize_type param.param_type in
  let* id = next_id in
  return (Model.make_param_expr param.param_pos id param.param_name type')

and modelize_type_param (param: Ast.param) =
  let* type' = map_option modelize_type param.param_type in
  let type' = Option.value type' ~default:(Model.TypeAny { type_any_pos = param.param_pos }) in
  return { Model.param_type_name = param.param_name; Model.param_type = type' }

and modelize_attr2 (attr: Ast.attr_expr) =
  let* expr = modelize_expr attr.attr_expr in
  return {
    Model.attr_expr_pos = attr.attr_expr_pos;
    Model.attr_expr_name = attr.attr_expr_name;
    Model.attr_expr = expr
  }

and modelize_block expr (block: Ast.block) =
  let* (stmts, expr2) = modelize_block_stmts block.block_stmts block.block_expr in
  return (Model.ExprBlock {
    expr_block_pos = fst expr;
    expr_block_stmts = stmts;
    expr_block_expr = expr2;
  })

and modelize_block_stmts stmts ret state =
  match stmts with
  | [] ->
    let (ret, state) = map_option modelize_expr ret state in
    (([], ret), state)
  | stmt :: stmts -> (
    match stmt with
    | StmtVar (name, type', expr) ->
      let (expr, state) = modelize_expr expr state in
      let (type', state) = map_option modelize_type type' state in
      let (id, state) = next_id state in
      let var = { Model.var_expr_id = id; Model.var_expr_name = name } in
      let stmt = Model.StmtVar (var, type', expr) in
      let ((stmts, ret), state) = with_scope (modelize_block_stmts stmts ret) NameMap.empty [] [(name, Model.BindExprVar var)] state in
      let stmts = stmt :: stmts in
      ((stmts, ret), state)
    | StmtExpr expr ->
      let (expr, state) = modelize_expr expr state in
      let stmt = Model.StmtExpr expr in
      let ((stmts, ret), state) = modelize_block_stmts stmts ret state in
      let stmts = stmt :: stmts in
    ((stmts, ret), state)
    )

and modelize_ascr expr type' =
  let* expr2 = modelize_expr expr in
  let* type' = modelize_type type' in
  return (Model.ExprAscr {
    expr_ascr_pos = fst expr;
    expr_ascr_expr = expr2;
    expr_ascr_type = type';
  })

and modelize_if expr cond then' else' =
  let* cond = modelize_expr cond in
  let* then' = modelize_expr then' in
  let* else' = modelize_expr else' in
  return (Model.ExprIf {
    expr_if_pos = fst expr;
    expr_if_cond = cond;
    expr_if_then = then';
    expr_if_else = else';
  })

and modelize_abs expr params body =
  let* params = map_list modelize_param params in
  let pairs = List.map (fun param -> (param.Model.param_expr_name, Model.BindExprParam param)) params in
  let* body = with_scope (modelize_expr body) NameMap.empty [] pairs in
  return (Model.ExprAbs {
    expr_abs_pos = fst expr;
    expr_abs_params = params;
    expr_abs_body = body;
  })

and modelize_app expr args =
  let* expr2 = modelize_expr expr in
  let* args = map_list modelize_expr args in
  return (Model.ExprApp {
    expr_app_pos = fst expr;
    expr_app_expr = expr2;
    expr_app_args = args;
  })

and modelize_type_abs_expr expr params body state =
  let types = ModelizeTypes.modelize_abs params (translate_state state) in
  let (body, state) = with_scope (modelize_expr body) types [] [] state in
  (Model.ExprTypeAbs {
    expr_type_abs_pos = fst expr;
    expr_type_abs_params = params;
    expr_type_abs_body = body;
  }, state)

and modelize_type_app expr args =
  let* expr2 = modelize_expr expr in
  let* args = map_list modelize_type args in
  return (Model.ExprTypeApp {
    expr_type_app_pos = fst expr;
    expr_type_app_expr = expr2;
    expr_type_app_args = args;
  })

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
