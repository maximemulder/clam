type state = {
  remains: Ast.def_type list;
  currents: Ast.def_type list;
  scope: Scope.scope;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let find_remain name state =
  (List.find_opt (fun def -> def.Ast.type_name = name) state.remains, state)

let find_current name state =
  (List.find_opt (fun def -> def.Ast.type_name = name) state.currents, state)

let find_scope name state =
  (Scope.find_type name state.scope, state)

let init_scope = Scope.empty
  |> Scope.add_type "Any"    Model.TypeAny
  |> Scope.add_type "Void"   Model.TypeVoid
  |> Scope.add_type "Int"    Model.TypeInt
  |> Scope.add_type "Char"   Model.TypeChar
  |> Scope.add_type "String" Model.TypeString

let init_state remains =
  { remains; currents = []; scope = init_scope }

module NameSet = Set.Make(Scope.NameKey)

let check_duplicates defs =
  let names = NameSet.empty in
  let _ = List.fold_left (fun names def ->
    let name = def.Ast.type_name in
    if NameSet.mem name names
      then Modelize_error.raise ("duplicate type `" ^ name ^ "`")
      else NameSet.add name names
    ) names defs in
  ()

let enter name state =
  let (defs, remains) = List.partition (fun def -> def.Ast.type_name == name) state.remains in
  let def = List.nth defs 0 in
  let currents = def :: state.currents in
  (def.type', { state with remains; currents })

let exit name type' state =
  let currents = List.filter (fun def -> def.Ast.type_name != name) state.currents in
  let scope = Scope.add_type name type' state.scope in
  (0, { state with currents; scope })

let add_param param state =
  let type' = Model.TypeVar param in
  let scope = Scope.add_type param.Model.param_name type' state.scope in
  ((), { state with scope })

let with_scope call state =
  let parent = state.scope in
  let state = { state with scope = Scope.empty_child state.scope } in
  let (result, state) = call state in
  let state = { state with scope = parent } in
  (result, state)

let rec modelize_name name =
  let* type' = find_remain name in
  match type' with
  | Some def -> modelize_def def
  | None     ->
  let* type' = find_current name in
  match type' with
  | Some _ -> Modelize_error.raise ("recursive type `" ^ name ^ "`")
  | None   ->
  let* type' = find_scope name in
  match type' with
  | Some type' -> return type'
  | None       -> Modelize_error.raise ("unbound type `" ^ name ^ "`")

and modelize_params params type' =
  let* _ = map_list add_param params in
  modelize_type type'

and modelize_def (node: Ast.def_type) =
  let name = node.type_name in
  let* node = enter name in
  let* type' = modelize_type node in
  let* _ = exit name type' in
  return type'

and modelize_type (type': Ast.type') =
  match type' with
  | TypeIdent name -> modelize_name name
  | TypeAbsExpr (params, type') ->
    let* params = map_list modelize_type params in
    let* type' = modelize_type type' in
    return (Model.TypeAbsExpr (params, type'))
  | TypeAbsExprType (params, type') ->
    let* params = map_list modelize_param params in
    let* type' = with_scope (modelize_params params type') in
    return (Model.TypeAbsExprType (params, type'))
  | TypeTuple (types) ->
    let* types = map_list modelize_type types in
    return (Model.TypeTuple types)
  | TypeRecord (attrs) ->
    let* attrs = map_list modelize_attr attrs in
    return (Model.TypeRecord attrs)
  | TypeInter (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Model.TypeInter (left, right))
  | TypeUnion (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Model.TypeUnion (left, right))
  | TypeAbs (params, type') ->
    let* params = map_list modelize_param params in
    let* type' = with_scope (modelize_params params type') in
    return (Model.TypeAbs (params, type'))
  | TypeApp (type', args) ->
    let* type' = modelize_type type' in
    let* args = map_list modelize_type args in
    return (Model.TypeApp (type', args))

and modelize_param (param: Ast.param)=
  let* type' = modelize_type param.param_type in
  return { Model.param_name = param.param_name; Model.param_type = type' }

and modelize_attr (attr: Ast.attr_type) =
  let* type' = modelize_type attr.attr_type in
  return { Model.attr_type_name = attr.attr_type_name; Model.attr_type = type' }

let modelize_type_expr (type': Ast.type') scope =
  let state = { remains = []; currents =  []; scope } in
  let (type', _) = modelize_type type' state in
  type'

let rec modelize_defs state =
  match state.remains with
  | [] -> state
  | remain :: _ ->
    let (_, state) = modelize_def remain state in
    modelize_defs state

let modelize_program (program: Ast.program) =
  let defs = Ast.get_types program in
  let _ = check_duplicates defs in
  let state = init_state defs in
  (modelize_defs state).scope
