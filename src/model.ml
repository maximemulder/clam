exception ModelError of string

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
  |> Scope.add_type "Void"   Type.TypeVoid
  |> Scope.add_type "Int"    Type.TypeInt
  |> Scope.add_type "Char"   Type.TypeChar
  |> Scope.add_type "String" Type.TypeString

let init_state remains =
  { remains; currents = []; scope = init_scope }

module NameSet = Set.Make(Scope.NameKey)

let check_duplicates defs =
  let names = NameSet.empty in
  let _ = List.fold_left (fun names def -> 
    let name = def.Ast.type_name in
    if NameSet.mem name names
      then raise (ModelError ("duplicate type `" ^ name ^ "`"))
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

let rec modelize_name name =
  let* type' = find_remain name in
  match type' with
  | Some def -> modelize_def def
  | None     ->
  let* type' = find_current name in
  match type' with
  | Some _ -> raise (ModelError ("recursive type `" ^ name ^ "`"))
  | None   ->
  let* type' = find_scope name in
  match type' with
  | Some type' -> return type'
  | None       -> raise (ModelError ("unbound type `" ^ name ^ "`"))

and modelize_def (node: Ast.def_type) =
  let name = node.type_name in
  let* node = enter name in
  let* type' = modelize_type node in
  let* _ = exit name type' in
  return type'

and modelize_type (type': Ast.type') =
  match type' with
  | TypeIdent name -> modelize_name name 
  | TypeFun (params, type') ->
    let* params = map modelize_type params in
    let* type' = modelize_type type' in
    return (Type.TypeFun (params, type'))
  | TypeTuple (types) ->
    let* types = map modelize_type types in
    return (Type.TypeTuple types)
  | TypeRecord (attrs) ->
    let* attrs = map modelize_attr attrs in
    return (Type.TypeRecord attrs)
  | Ast.TypeInter (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Type.TypeInter (left, right))
  | Ast.TypeUnion (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Type.TypeUnion (left, right))
  | TypeAbs (params, type') ->
    let* params = map modelize_param params in
    let* type' = modelize_type type' in
    return (Type.TypeAbs (params, type'))
  | TypeApp (type', args) ->
    let* type' = modelize_type type' in
    let* args = map modelize_type args in
    return (Type.TypeApp (type', args))

and modelize_param (param: Ast.param)=
  let* type' = modelize_type param.param_type in
  return { Type.param_name = param.param_name; Type.param_type = type' }

and modelize_attr (attr: Ast.attr_type) =
  let* type' = modelize_type attr.attr_type in
  return { Type.attr_name = attr.attr_type_name; Type.attr_type = type' }

let rec modelize_defs state =
  match state.remains with 
  | [] -> state
  | remain :: _ ->
    let (_, state) = modelize_def remain state in
    modelize_defs state

let modelize (program: Ast.program) =
  let defs = Ast.get_types program in
  let _ = check_duplicates defs in
  let state = init_state defs in
  (modelize_defs state).scope
