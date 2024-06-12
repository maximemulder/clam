open Util

type scope = {
  parent: scope option;
  currents: (Abt.bind_type * bool) NameMap.t;
  types: Abt.type' NameMap.t;
  exprs: Abt.bind_expr NameMap.t;
}

type state = {
  id: int;
  scope: scope;
  ast_types: Ast.def_type list;
  abt_types: Abt.def_type IntMap.t;
  ast_exprs: Ast.def_expr list;
  abt_exprs: Abt.def_expr IntMap.t;
}

let types = [
  "Top",    Abt.TypeTop    { span = Code.span_primitive };
  "Bot",    Abt.TypeBot    { span = Code.span_primitive };
  "Unit",   Abt.TypeUnit   { span = Code.span_primitive };
  "Bool",   Abt.TypeBool   { span = Code.span_primitive };
  "Int",    Abt.TypeInt    { span = Code.span_primitive };
  "String", Abt.TypeString { span = Code.span_primitive };
]

let make_state ast primitives =
  let ast_types = Ast.get_program_types ast in
  let ast_exprs = Ast.get_program_exprs ast in
  let scope = { parent = None; types = NameMap.of_list types; currents = NameMap.empty; exprs = primitives } in
  { id = 0; scope; ast_types; ast_exprs; abt_types = IntMap.empty; abt_exprs = IntMap.empty }

let find_ast_type name state =
  list_find_entry_opt (fun (def: Ast.def_type) -> def.name = name) state.ast_types

let find_ast_expr name state =
  list_find_entry_opt (fun (def: Ast.def_expr) -> def.name = name) state.ast_exprs

let find_current name state =
  NameMap.find_opt name state.scope.currents

let find_type name state =
  NameMap.find_opt name state.scope.types

let find_expr name state =
  NameMap.find_opt name state.scope.exprs

let make_child_type name type' state =
  let types = NameMap.singleton name type' in
  let scope = { parent = Some state.scope; exprs = NameMap.empty; currents = NameMap.empty; types } in
  ((), { state with scope })

let with_scope_type name type' f state =
  let ((), state) = make_child_type name type' state in
  let (result, state) = f state in
  let scope = Option.get state.scope.parent in
  (result, { state with scope })

let make_child_expr name expr state =
  let exprs = NameMap.singleton name expr in
  let scope = { parent = Some state.scope; currents = NameMap.empty; types = NameMap.empty; exprs } in
  ((), { state with scope })

let with_scope_expr name expr f state =
  let ((), state) = make_child_expr name expr state in
  let (result, state) = f state in
  (result, { state with scope = Option.get state.scope.parent })
