exception ModelError of string

let init_scope = Scope.empty
  |> Scope.add_type "Void"   Type.TypeVoid
  |> Scope.add_type "Int"    Type.TypeInt
  |> Scope.add_type "Char"   Type.TypeChar
  |> Scope.add_type "String" Type.TypeString

type state = {
  remains: Ast.def_type list;
  currents: Ast.def_type list;
  scope: Scope.scope;
}

let enter name state =
  let (defs, remains) = List.partition (fun def -> def.Ast.type_name == name) state.remains in
  let def = List.nth defs 0 in
  let currents = def :: state.currents in
  (def.type', { state with remains; currents })

let exit name type' state =
  let currents = List.filter (fun def -> def.Ast.type_name != name) state.currents in
  let scope = Scope.add_type name type' state.scope in
  { state with currents; scope }

let find_remain name state =
  List.find_opt (fun def -> def.Ast.type_name = name) state.remains

let find_current name state = 
  List.find_opt (fun def -> def.Ast.type_name = name) state.currents

let find_scope name state = 
  Scope.find_type name state.scope

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

let init_state remains =
  { remains; currents = []; scope = init_scope }

let map modelize = fun a b -> let (c, d) = (modelize b a) in (d, c)

let rec modelize_name name state =
  match find_remain name state with
  | Some def -> modelize_def def state
  | None     ->
  match find_current name state with
  | Some _ -> raise (ModelError ("recursive type `" ^ name ^ "`"))
  | None   ->
  match find_scope name state with
  | Some type' -> (type', state)
  | None       -> raise (ModelError ("unbound type `" ^ name ^ "`"))

and modelize_def (node: Ast.def_type) state =
  let name = node.type_name in
  let (node, state) = enter name state in
  let (type', state) = modelize_type node state in
  let state = exit name type' state in
  (type', state)

and modelize_type (type': Ast.type') state =
  match type' with
  | TypeIdent  name -> modelize_name name state
  | TypeFun (params, return) ->
    let (state, params) = List.fold_left_map (map modelize_type) state params in
    let (return, state) = modelize_type return state in
    (Type.TypeFun (params, return), state)
  | TypeTuple (types) ->
    let (state, types) = List.fold_left_map (map modelize_type) state types in
    (Type.TypeTuple types, state)
  | TypeRecord (attrs) ->
    let (state, attrs) = List.fold_left_map (map modelize_attr) state attrs in
    (Type.TypeRecord attrs, state)
  | TypeInter  (left, right) ->
    let (left, state) = modelize_type left state in
    let (right, state) = modelize_type right state in
    (Type.TypeInter (left, right), state)
  | TypeUnion  (left, right) ->
    let (left, state) = modelize_type left state in
    let (right, state) = modelize_type right state in
    (Type.TypeUnion (left, right), state)
  | TypeAbs (params, type') ->
    let (state, params) = List.fold_left_map (map modelize_param) state params in
    let (type', state) = modelize_type type' state in
    (Type.TypeAbs (params, type'), state)
  | TypeApp (type', args) ->
    let (type', state) = modelize_type type' state in
    let (state, args) = List.fold_left_map (map modelize_type) state args in
    (Type.TypeApp (type', args), state)

and modelize_param (param: Ast.param) state =
  let (type', state) = modelize_type param.param_type state in
  ({ Type.param_name = param.param_name; Type.param_type = type' }, state)

and modelize_attr (attr: Ast.attr_type) state =
  let (type', state) = modelize_type attr.attr_type state in
  ({ Type.attr_name = attr.attr_type_name; Type.attr_type = type' }, state)

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
