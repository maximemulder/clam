open Util

type scope = {
  parent: scope option;
  types: Abt.type' NameMap.t;
  exprs: Abt.bind_expr NameMap.t;
}

type state = {
  id: int;
  scope: scope;
  ast_defs: Ast.def_expr list;
  abt_defs: Abt.def_expr IntMap.t;
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
  | _ -> Errors.raise_expr_operator span op

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
  | _ -> Errors.raise_expr_operator span op

let find_ast_def name state =
  list_find_entry_opt (fun (def: Ast.def_expr) -> def.name = name) state.ast_defs

let find_expr name state =
  NameMap.find_opt name state.scope.exprs

let new_id state =
  (state.id, { state with id = state.id + 1 })

let new_bind name =
  let* id = new_id in
  return { Abt.id; name }

let fold_remain map (remain: Ast.def_expr) =
  NameMap.add remain.Ast.name remain map

let make_state ast_defs types exprs =
  let scope = { parent = None; types; exprs } in
  { id = 0; scope; ast_defs; abt_defs = IntMap.empty }

let make_child types exprs state =
  let scope = { parent = Some state.scope; types; exprs } in
  ((), { state with scope })

let with_scope types exprs f state =
  let (_, state) = make_child types exprs state in
  let (result, state) = f state in
  (result, { state with scope = Option.get state.scope.parent })

let parse_int span value =
  match int_of_string_opt value with
  | Some int -> int
  | None     -> Errors.raise_expr_integer span value

let parse_string (value: string) =
  value

let rec translate_scope state =
  {
    Types.parent = Option.map translate_scope state.parent;
    Types.remains = NameMap.empty;
    Types.currents = NameMap.empty;
    Types.dones = state.types;
  }

let translate_state state =
  {
    Types.scope = translate_scope state.scope;
    Types.all = [];
  }

(* TODO: Can I remove state from the return ? *)
let desugar_type (type': Ast.type') state =
  (Types.desugar_type_expr type' (translate_state state), state)

let rec desugar_bind span name state =
  match find_expr name state with
  | Some bind -> (bind, state)
  | None      ->
  match state.scope.parent with
  | Some scope ->
    let parent = { state with scope } in
    let (expr, parent) = desugar_bind span name parent in
    (expr, { parent with scope = { state.scope with parent = Some parent.scope } })
  | None ->
  match find_ast_def name state with
  | Some (id, def) -> desugar_def (id, def) state
  | None     ->
    Errors.raise_expr_bound span name

and desugar_def (id, def) state =
  let bind, state = new_bind def.name state in
  let exprs = NameMap.add def.name bind state.scope.exprs in
  let state = { state with scope = { state.scope with exprs } } in
  let type' = Option.map (fun type' -> fst (desugar_type type' state)) def.type' in
  let (expr, state) = desugar_expr def.expr state in
  let def = { Abt.span = def.span; bind; type'; expr } in
  let abt_def = { Abt.bind; Abt.type'; Abt.expr; span = def.span } in
  let abt_defs = IntMap.add id abt_def state.abt_defs in
  let state = { state with abt_defs } in
  (bind, state)

and desugar_expr (expr: Ast.expr): state -> Abt.expr * state =
  match expr with
  | ExprUnit    expr -> desugar_unit     expr
  | ExprTrue    expr -> desugar_true     expr
  | ExprFalse   expr -> desugar_false    expr
  | ExprInt     expr -> desugar_int      expr
  | ExprString  expr -> desugar_string   expr
  | ExprName    expr -> desugar_name     expr.span expr.name
  | ExprProduct expr -> desugar_product  expr
  | ExprElem    expr -> desugar_elem     expr
  | ExprAttr    expr -> desugar_attr     expr
  | ExprPreop   expr -> desugar_preop    expr
  | ExprBinop   expr -> desugar_binop    expr
  | ExprAscr    expr -> desugar_ascr     expr
  | ExprStmt    expr -> desugar_stmt     expr
  | ExprIf      expr -> desugar_if       expr
  | ExprLamAbs  expr -> desugar_lam_abs  expr
  | ExprLamApp  expr -> desugar_lam_app  expr
  | ExprUnivAbs expr -> desugar_univ_abs expr
  | ExprUnivApp expr -> desugar_univ_app expr

and desugar_unit expr =
  return (Abt.ExprUnit { span = expr.span })

and desugar_true expr =
  return (Abt.ExprBool { span = expr.span; value = true })

and desugar_false expr =
  return (Abt.ExprBool { span = expr.span; value = false })

and desugar_int expr =
  let value = parse_int expr.span expr.value in
  return (Abt.ExprInt { span = expr.span; value })

and desugar_string expr =
  let value = parse_string expr.value in
  return (Abt.ExprString { span = expr.span; value })

and desugar_name span name =
  let* bind = desugar_bind span name in
  return (Abt.ExprBind { span = span; bind })

and desugar_product expr =
  let fields = List.partition_map partition_field expr.fields in
  match fields with
  | ([], []) ->
    return (Abt.ExprRecord { span = expr.span; attrs = [] })
  | (fields, []) ->
    let* elems = list_map desugar_tuple_elem fields in
    return (Abt.ExprTuple { span = expr.span; elems })
  | ([], fields) ->
    let* attrs = list_map desugar_record_attr fields in
    return (Abt.ExprRecord { span = expr.span; attrs })
  | _ ->
    Errors.raise_expr_product expr

and partition_field field =
  match field with
  | Ast.FieldExprElem elem -> Either.Left elem
  | Ast.FieldExprAttr attr -> Either.Right attr

and desugar_tuple_elem field =
  desugar_expr field.Ast.expr

and desugar_record_attr field =
  let* expr = desugar_expr field.expr in
  return { Abt.span = field.span; Abt.label = field.Ast.label; Abt.expr = expr }

and desugar_elem expr =
  let* tuple = desugar_expr expr.tuple in
  let index = parse_int expr.span expr.index in
  return (Abt.ExprElem { span = expr.span; tuple; index })

and desugar_attr expr =
  let* record = desugar_expr expr.record in
  return (Abt.ExprAttr { span = expr.span; record; label = expr.label })

and desugar_preop expr =
  let* arg = desugar_expr expr.expr in
  let name = get_preop_name expr.span expr.op in
  let* abs = desugar_name expr.span name in
  return (Abt.ExprLamApp { span = expr.span; abs; arg })

and desugar_binop expr =
  let* left  = desugar_expr expr.left  in
  let* right = desugar_expr expr.right in
  let name = get_binop_name expr.span expr.op in
  let* abs = desugar_name expr.span name in
  return (Abt.ExprLamApp { span = expr.span; abs = (Abt.ExprLamApp { span = expr.span; abs; arg = left }); arg = right })

and desugar_ascr ascr =
  let* expr  = desugar_expr ascr.expr  in
  let* type' = desugar_type ascr.type' in
  return (Abt.ExprAscr { span = ascr.span; expr; type' })

and desugar_stmt stmt =
  match stmt.stmt with
  | StmtVar { span; name; type'; expr } ->
    let* bind = new_bind name in
    let* type' = map_option desugar_type type' in
    let param = { Abt.span = span; bind; type' } in
    let* body = with_scope NameMap.empty (NameMap.singleton name bind)
      (desugar_expr stmt.expr) in
    let abs = Abt.ExprLamAbs { span = span; param; body } in
    let* arg = desugar_expr expr in
    return (Abt.ExprLamApp { span = span; abs; arg})
  | StmtExpr { span; expr } ->
    let* bind = new_bind "_" in
    let param = { Abt.span = span; bind; type' = None } in
    let* body = desugar_expr stmt.expr in
    let abs = Abt.ExprLamAbs { span = span; param; body } in
    let* arg = desugar_expr expr in
    return (Abt.ExprLamApp { span = span; abs; arg })

and desugar_if if' =
  let* cond  = desugar_expr if'.cond in
  let* then' = desugar_expr if'.then' in
  let* else' = desugar_expr if'.else' in
  return (Abt.ExprIf { span = if'.span; cond; then'; else' })

and desugar_lam_abs abs =
  desugar_lam_abs_curry abs.span abs.params abs.body

and desugar_lam_abs_curry span params body =
  match params with
  | [] ->
    desugar_expr body
  | (param :: params) ->
    let* param = desugar_param_expr param in
    let* body = with_scope NameMap.empty (NameMap.singleton param.bind.name param.bind)
      (desugar_lam_abs_curry span params body) in
    return (Abt.ExprLamAbs { span = span; param; body })

and desugar_lam_app app =
  let* abs = desugar_expr app.abs in
  desugar_lam_app_curry app.span abs app.args

and desugar_lam_app_curry span abs args =
  match args with
  | [] ->
    return abs
  | (arg :: args) ->
    let* arg = desugar_expr arg in
    let app = (Abt.ExprLamApp { span = span; abs; arg }) in
    desugar_lam_app_curry span app args

and desugar_univ_abs abs =
  desugar_univ_abs_curry abs.span abs.params abs.body

and desugar_univ_abs_curry span params body state =
  match params with
  | [] ->
    desugar_expr body state
  | (param :: params) ->
    let (param, state) = desugar_param_type param state in
    let entries = Types.desugar_abs param (translate_state state) in
    let (body, state) = with_scope entries NameMap.empty
      (desugar_univ_abs_curry span params body) state in
    (Abt.ExprUnivAbs { span = span; param; body }, state)

and desugar_univ_app app =
  let* abs = desugar_expr app.abs in
  desugar_univ_app_curry app.span abs app.args

and desugar_univ_app_curry span abs args =
  match args with
  | [] ->
    return abs
  | (arg :: args) ->
    let* arg = desugar_type arg in
    let app = (Abt.ExprUnivApp { span = span; abs; arg }) in
    desugar_univ_app_curry span app args

and desugar_param_expr (param: Ast.param_expr): Abt.param_expr t =
  let* type' = map_option desugar_type param.type' in
  let* bind = new_bind param.name in
  return { Abt.span = param.span; bind; type' }

and desugar_param_type (param: Ast.param_type) =
  let* bound = (match param.type' with
    | Some type' ->
      desugar_type type'
    | None ->
      return (Abt.TypeTop { span = param.span })
  ) in
  return { Abt.bind = { name = param.name }; Abt.bound }

let desugar_defs state defs =
  List.fold_left (fun state (def: Ast.def_expr) ->
    desugar_bind def.span def.name state |> snd
  ) state defs

let desugar_program (program: Ast.program) (types: Abt.type' NameMap.t) primitives =
  let defs = Ast.get_program_exprs program in
  let state = make_state defs types primitives in
  let state = desugar_defs state defs in
  state.abt_defs |> IntMap.bindings |> List.map snd
