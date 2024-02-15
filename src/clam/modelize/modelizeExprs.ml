open Util

type scope = {
  parent: scope option;
  types: Abt.type' NameMap.t;
  remains: Ast.def_expr NameMap.t;
  currents: Abt.bind_expr option ref NameMap.t;
  dones: Abt.bind_expr option ref NameMap.t;
}

type state = {
  scope: scope;
  all_types: Abt.type' list;
  all_exprs: Abt.def_expr list;
  id: int;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let get_preop_name span op =
  match op with
  | "+" -> "__pos__"
  | "-" -> "__neg__"
  | "!" -> "__not__"
  | _ -> ModelizeErrors.raise_expr_operator span op

let get_binop_name span op =
  match op with
  | "+"  -> "__add__"
  | "-"  -> "__sub__"
  | "*"  -> "__mul__"
  | "/"  -> "__div__"
  | "%"  -> "__mod__"
  | "++" -> "__concat__"
  | "==" -> "__eq__"
  | "!=" -> "__ne__"
  | "<"  -> "__lt__"
  | ">"  -> "__gt__"
  | "<=" -> "__le__"
  | ">=" -> "__ge__"
  | "&"  -> "__and__"
  | "|"  -> "__or__"
  | _ -> ModelizeErrors.raise_expr_operator span op

let find_remain name state =
  NameMap.find_opt name state.scope.remains

let find_current name state =
  NameMap.find_opt name state.scope.currents

let find_done name state =
  NameMap.find_opt name state.scope.dones

let new_id state =
  (state.id, { state with id = state.id + 1 })

let new_bind name =
  let* id = new_id in
  return { Abt.id; name }

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

let parse_int span value =
  match int_of_string_opt value with
  | Some int -> int
  | None     -> ModelizeErrors.raise_expr_integer span value

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

let rec modelize_bind span name state =
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
    let (expr, parent) = modelize_bind span name parent in
    (expr, { parent with scope = { state.scope with parent = Some parent.scope } })
  | None -> ModelizeErrors.raise_expr_bound span name

and modelize_def def state =
  let name = def.Ast.name in
  let (remain, remains) = extract name state.scope.remains in
  let currents = NameMap.add name { contents = None } state.scope.currents in
  let state = { state with scope = { state.scope with remains; currents } } in
  let type' = Option.map (fun type' -> fst (modelize_type type' state)) remain.Ast.type' in
  let (expr, state) = modelize_expr remain.Ast.expr state in
  let (current, currents) = extract name state.scope.currents in
  let bind, state = new_bind name state in
  let def = { Abt.span = def.span; bind; type'; expr } in
  let _ = current := Some def.bind in
  let dones = NameMap.add name current state.scope.dones in
  let state = { state with scope = { state.scope with currents; dones}; all_exprs = def :: state.all_exprs } in
  (current, state)

and modelize_expr (expr: Ast.expr): state -> Abt.expr * state =
  match expr with
  | ExprUnit    expr -> modelize_unit     expr
  | ExprTrue    expr -> modelize_true     expr
  | ExprFalse   expr -> modelize_false    expr
  | ExprInt     expr -> modelize_int      expr
  | ExprString  expr -> modelize_string   expr
  | ExprName    expr -> modelize_name     expr.span expr.name
  | ExprProduct expr -> modelize_product  expr
  | ExprElem    expr -> modelize_elem     expr
  | ExprAttr    expr -> modelize_attr     expr
  | ExprPreop   expr -> modelize_preop    expr
  | ExprBinop   expr -> modelize_binop    expr
  | ExprAscr    expr -> modelize_ascr     expr
  | ExprStmt    expr -> modelize_stmt     expr
  | ExprIf      expr -> modelize_if       expr
  | ExprLamAbs  expr -> modelize_lam_abs  expr
  | ExprLamApp  expr -> modelize_lam_app  expr
  | ExprUnivAbs expr -> modelize_univ_abs expr
  | ExprUnivApp expr -> modelize_univ_app expr

and modelize_unit expr =
  return (Abt.ExprUnit { span = expr.span })

and modelize_true expr =
  return (Abt.ExprBool { span = expr.span; value = true })

and modelize_false expr =
  return (Abt.ExprBool { span = expr.span; value = false })

and modelize_int expr =
  let value = parse_int expr.span expr.value in
  return (Abt.ExprInt { span = expr.span; value })

and modelize_string expr =
  let value = parse_string expr.value in
  return (Abt.ExprString { span = expr.span; value })

and modelize_name span name =
  let* bind = modelize_bind span name in
  return (Abt.ExprBind { span = span; bind })

and modelize_product expr =
  let fields = List.partition_map partition_field expr.fields in
  match fields with
  | ([], []) ->
    return (Abt.ExprRecord { span = expr.span; attrs = [] })
  | (fields, []) ->
    let* elems = map_list modelize_tuple_elem fields in
    return (Abt.ExprTuple { span = expr.span; elems })
  | ([], fields) ->
    let* attrs = map_list modelize_record_attr fields in
    return (Abt.ExprRecord { span = expr.span; attrs })
  | _ ->
    ModelizeErrors.raise_expr_product expr

and partition_field field =
  match field with
  | Ast.FieldExprElem elem -> Either.Left elem
  | Ast.FieldExprAttr attr -> Either.Right attr

and modelize_tuple_elem field =
  modelize_expr field.Ast.expr

and modelize_record_attr field =
  let* expr = modelize_expr field.expr in
  return { Abt.span = field.span; Abt.label = field.Ast.label; Abt.expr = expr }

and modelize_elem expr =
  let* tuple = modelize_expr expr.tuple in
  let index = parse_int expr.span expr.index in
  return (Abt.ExprElem { span = expr.span; tuple; index })

and modelize_attr expr =
  let* record = modelize_expr expr.record in
  return (Abt.ExprAttr { span = expr.span; record; label = expr.label })

and modelize_preop expr =
  let* arg = modelize_expr expr.expr in
  let name = get_preop_name expr.span expr.op in
  let* abs = modelize_name expr.span name in
  return (Abt.ExprLamApp { span = expr.span; abs; arg })

and modelize_binop expr =
  let* left  = modelize_expr expr.left  in
  let* right = modelize_expr expr.right in
  let name = get_binop_name expr.span expr.op in
  let* abs = modelize_name expr.span name in
  return (Abt.ExprLamApp { span = expr.span; abs = (Abt.ExprLamApp { span = expr.span; abs; arg = left }); arg = right })

and modelize_ascr ascr =
  let* expr  = modelize_expr ascr.expr  in
  let* type' = modelize_type ascr.type' in
  return (Abt.ExprAscr { span = ascr.span; expr; type' })

and modelize_stmt stmt =
  match stmt.stmt with
  | StmtVar { span; name; type'; expr } ->
    let* bind = new_bind name in
    let* type' = map_option modelize_type type' in
    let param = { Abt.span = span; bind; type' } in
    let* body = with_scope (modelize_expr stmt.expr) NameMap.empty [] [name, bind] in
    let abs = Abt.ExprLamAbs { span = span; param; body } in
    let* arg = modelize_expr expr in
    return (Abt.ExprLamApp { span = span; abs; arg})
  | StmtExpr { span; expr } ->
    let* bind = new_bind "_" in
    let param = { Abt.span = span; bind; type' = None } in
    let* body = modelize_expr stmt.expr in
    let abs = Abt.ExprLamAbs { span = span; param; body } in
    let* arg = modelize_expr expr in
    return (Abt.ExprLamApp { span = span; abs; arg })

and modelize_if if' =
  let* cond  = modelize_expr if'.cond in
  let* then' = modelize_expr if'.then' in
  let* else' = modelize_expr if'.else' in
  return (Abt.ExprIf { span = if'.span; cond; then'; else' })

and modelize_lam_abs abs =
  modelize_lam_abs_curry abs.span abs.params abs.body

and modelize_lam_abs_curry span params body =
  match params with
  | [] ->
    modelize_expr body
  | (param :: params) ->
    let* param = modelize_param_expr param in
    let entries = [param.Abt.bind.name, param.bind] in
    let* body = with_scope (modelize_lam_abs_curry span params body) NameMap.empty [] entries in
    return (Abt.ExprLamAbs { span = span; param; body })

and modelize_lam_app app =
  let* abs = modelize_expr app.abs in
  modelize_lam_app_curry app.span abs app.args

and modelize_lam_app_curry span abs args =
  match args with
  | [] ->
    return abs
  | (arg :: args) ->
    let* arg = modelize_expr arg in
    let app = (Abt.ExprLamApp { span = span; abs; arg }) in
    modelize_lam_app_curry span app args

and modelize_univ_abs abs =
  modelize_univ_abs_curry abs.span abs.params abs.body

and modelize_univ_abs_curry span params body state =
  match params with
  | [] ->
    modelize_expr body state
  | (param :: params) ->
    let (param, state) = modelize_param_type param state in
    let entries = ModelizeTypes.modelize_abs param (translate_state state) in
    let (body, state) = with_scope (modelize_univ_abs_curry span params body) entries [] [] state in
    (Abt.ExprUnivAbs { span = span; param; body }, state)

and modelize_univ_app app =
  let* abs = modelize_expr app.abs in
  modelize_univ_app_curry app.span abs app.args

and modelize_univ_app_curry span abs args =
  match args with
  | [] ->
    return abs
  | (arg :: args) ->
    let* arg = modelize_type arg in
    let app = (Abt.ExprUnivApp { span = span; abs; arg }) in
    modelize_univ_app_curry span app args

and modelize_param_expr (param: Ast.param_expr): Abt.param_expr t =
  let* type' = map_option modelize_type param.type' in
  let* bind = new_bind param.name in
  return { Abt.span = param.span; bind; type' }

and modelize_param_type (param: Ast.param_type) =
  let* bound = (match param.type' with
    | Some type' ->
      modelize_type type'
    | None ->
      return (Abt.TypeTop { span = param.span })
  ) in
  return { Abt.bind = { name = param.name }; Abt.bound }

let rec modelize_defs state =
  match NameMap.choose_opt state.scope.remains with
  | None -> state
  | Some (_, def) ->
    let (_, state) = modelize_def def state in
    modelize_defs state

let modelize_program (program: Ast.program) (types: Abt.type' NameMap.t) (all_types: Abt.type' list)=
  let defs = Ast.get_program_exprs program in
  let state = make_state types defs Primitive.binds in
  let state = { state with all_types = all_types } in
  let state = modelize_defs state in
  (state.all_exprs, state.all_types)
