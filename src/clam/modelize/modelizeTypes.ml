open Utils

type scope = {
  parent: scope option;
  remains: Ast.type' NameMap.t;
  currents: Ast.type' NameMap.t;
  dones: Model.type' NameMap.t;
}

type state = {
  scope: scope;
  all: Model.type' list
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let fold_remain map remain =
  NameMap.add remain.Ast.type_name remain.Ast.type' map

let fold_done map done' =
  NameMap.add (fst done') (snd done') map

let make_state remains dones =
  let all = List.map snd dones in
  let remains = List.fold_left fold_remain NameMap.empty remains in
  let dones = List.fold_left fold_done NameMap.empty dones in
  let scope = { parent = None; remains; currents = NameMap.empty; dones } in
  { scope; all }

let make_child dones state =
  let all = List.map snd dones in
  let dones = List.fold_left fold_done NameMap.empty dones in
  let scope = { parent = Some state.scope; remains = NameMap.empty; currents = NameMap.empty; dones } in
  ((), { scope; all })

let find_remain name state =
  NameMap.find_opt name state.scope.remains

let find_current name state =
  NameMap.find_opt name state.scope.currents

let find_done name state =
  NameMap.find_opt name state.scope.dones

let make_attrs attrs =
  List.fold_left (fun map attr ->
    let name = attr.Model.attr_type_name in
    if NameMap.mem name map
      then ModelizeErrors.raise_type_duplicate_attribute attr
      else NameMap.add name attr map
  ) NameMap.empty attrs

let with_name name call state =
  let (type', remains) = extract name state.scope.remains in
  let currents = NameMap.add name type' state.scope.currents in
  let state = { state with scope = { state.scope with remains; currents} } in
  let (type', state) = call type' state in
  let currents = NameMap.remove name state.scope.currents in
  let dones = NameMap.add name type' state.scope.dones in
  let state = { state with scope = { state.scope with currents; dones} } in
  (type', state)

let with_scope call types state =
  let (_, state) = make_child types state in
  let (result, state) = call state in
  let scope = Option.get state.scope.parent in
  (result, { state with scope })

let rec modelize_name type' name state =
  match find_remain name state with
  | Some def -> modelize_def name def state
  | None     ->
  match find_current name state with
  | Some _ -> ModelizeErrors.raise_type_recursive type' name
  | None   ->
  match find_done name state with
  | Some type' -> (type', state)
  | None       ->
  match state.scope.parent with
  | Some scope ->
    let parent = { state with scope } in
    let (type', parent) = modelize_name type' name parent in
    (type', { parent with scope = { state.scope with parent = Some parent.scope } })
  | None -> ModelizeErrors.raise_type_bound type' name

and modelize_def name _type' =
  with_name name modelize_type

and modelize_type (type': Ast.type') =
  let pos = fst type' in
  match snd type' with
  | TypeIdent name ->
    modelize_name type' name
  | TypeAbsExpr (params, ret) ->
    let* params = map_list modelize_type params in
    let* body = modelize_type ret in
    return (Model.TypeAbsExpr {
      type_abs_expr_pos = pos;
      type_abs_expr_params = params;
      type_abs_expr_body = body;
    })
  | TypeAbsExprType (params, body) ->
    let* (params, body) = modelize_params params body in
    return (Model.TypeAbsExprType {
      type_abs_expr_type_pos = pos;
      type_abs_expr_type_params = params;
      type_abs_expr_type_body = body;
    })
  | TypeTuple (types) ->
    let* types = map_list modelize_type types in
    return (Model.TypeTuple {
      type_tuple_pos = pos;
      type_tuple_types = types;
    })
  | TypeRecord (attrs) ->
    let* attrs = map_list modelize_attr attrs in
    return (Model.TypeRecord {
      type_record_pos = pos;
      type_record_attrs = make_attrs attrs;
    })
  | TypeInter (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Model.TypeInter {
      type_inter_pos = pos;
      type_inter_left = left;
      type_inter_right = right;
    })
  | TypeUnion (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Model.TypeUnion {
      type_union_pos = pos;
      type_union_left = left;
      type_union_right = right;
    })
  | TypeAbs (params, body) ->
    let* (params, body) = modelize_params params body in
    return (Model.TypeAbs {
      type_abs_pos = pos;
      type_abs_params = params;
      type_abs_body = body;
    })
  | TypeApp (type', args) ->
    let* type' = modelize_type type' in
    let* args = map_list modelize_type args in
    return (Model.TypeApp {
      type_app_pos = pos;
      type_app_type = type';
      type_app_args = args;
    })

and modelize_params params type' =
  let* params = map_list modelize_param params in
  let types = List.map (fun param -> (param.Model.param_type_name, Model.TypeVar { type_var_pos = Model.type_pos param.param_type; type_var_param = param})) params in
  let* type' = with_scope (modelize_type type') types in
  return (params, type')

and modelize_param param =
  let* type' = map_option modelize_type param.param_type in
  let type' = Option.value type' ~default:(Model.TypeAny { type_any_pos = param.param_pos }) in
  return { Model.param_type_name = param.param_name; Model.param_type = type' }

and modelize_attr attr =
  let* type' = modelize_type attr.attr_type in
  return {
    Model.attr_type_pos = attr.attr_type_pos;
    Model.attr_type_name = attr.attr_type_name;
    Model.attr_type = type'
  }

let modelize_type_expr type' state =
  let (type', _) = modelize_type type' state in
  type'

let rec modelize_defs state =
  match NameMap.choose_opt state.scope.remains with
  | None -> state
  | Some (name, remain) ->
    let (_, state) = modelize_def name remain state in
    modelize_defs state

let primitives = [
  ("Any",    Model.type_any);
  ("Void",   Model.type_void);
  ("Bool",   Model.type_bool);
  ("Int",    Model.type_int);
  ("Char",   Model.type_char);
  ("String", Model.type_string);
]

let modelize_program (program: Ast.program) =
  let defs = Ast.get_program_types program in
  let state = make_state defs primitives in
  let state = modelize_defs state in
  (state.scope.dones, state.all)

let modelize_abs params state =
  let types = List.map (fun param -> (param.Model.param_type_name, Model.TypeVar { type_var_pos = Model.type_pos param.param_type; type_var_param = param})) params in
  let (_, state) = make_child types state in
  state.scope.dones
