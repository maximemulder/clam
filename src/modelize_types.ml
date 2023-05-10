module NameKey = struct
  type t = string
  let compare = String.compare
end

module NameMap = Map.Make(NameKey)

type state = {
  parent: state option;
  remains: Ast.type' NameMap.t;
  currents: Ast.type' NameMap.t;
  dones: Model.type' NameMap.t;
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

module NameSet = Set.Make(Scope.NameKey)

let check_duplicates names set =
  List.fold_left (fun set name ->
    if NameSet.mem name set
      then Modelize_error.raise ("duplicate type `" ^ name ^ "`")
      else NameSet.add name set
    ) set names

let fold_remain map remain =
  NameMap.add remain.Ast.type_name remain.Ast.type' map

let fold_done map done' =
  NameMap.add (fst done') (snd done') map

let new_state parent remains dones =
  let names = NameSet.empty in
  let names = check_duplicates (List.map (fun remain -> remain.Ast.type_name) remains) names in
  let _ = check_duplicates (List.map (fun done' -> fst done') dones) names in
  let remains = List.fold_left fold_remain NameMap.empty remains in
  let dones = List.fold_left fold_done NameMap.empty dones in
  { parent; remains; currents = NameMap.empty; dones }

let extract key map =
  let value = NameMap.find key map in
  let map = NameMap.remove key map in
  (value, map)

let enter name state =
  let (type', remains) = extract name state.remains in
  let currents = NameMap.add name type' state.currents in
  (type', { state with remains; currents })

let exit name type' state =
  let currents = NameMap.remove name state.currents in
  let dones = NameMap.add name type' state.dones in
  ((), { state with currents; dones })

let add_param param state =
  let type' = Model.TypeVar param in
  let dones = NameMap.add param.Model.param_name type' state.dones in
  ((), { state with dones })

let with_scope call state =
  let parent = state in
  let state = new_state (Some parent) [] [] in
  let (result, state) = call state in
  let state = Option.get state.parent in
  (result, state)

let rec modelize_name (name: string) (state: state) : Model.type' * state =
  match find_remain name state with
  | Some def -> modelize_def name def state
  | None     ->
  match find_current name state with
  | Some _ -> Modelize_error.raise ("recursive type `" ^ name ^ "`")
  | None   ->
  match find_done name state with
  | Some type' -> (type', state)
  | None       ->
  match state.parent with
  | Some parent -> let (type', parent) = modelize_name name parent in
    (type', { state with parent = Some parent })
  | None        -> Modelize_error.raise ("unbound type `" ^ name ^ "`")

and modelize_def (name: string) (_type': Ast.type') : state -> (Model.type' * state) =
  let* type' = enter name in
  let* type' = modelize_type type' in
  let* _ = exit name type' in
  return type'

and modelize_params params type' =
  let* _ = map_list add_param params in
  modelize_type type'

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

let rec translate_scope scope =
  let dones = NameMap.of_seq (Scope.NameMap.to_seq scope.Scope.types) in
  { parent = Option.map translate_scope scope.parent; remains = NameMap.empty; currents =  NameMap.empty; dones }

let modelize_type_expr (type': Ast.type') scope =
  let state = translate_scope scope in
  let (type', _) = modelize_type type' state in
  type'

let rec modelize_defs state =
  match NameMap.choose_opt state.remains with
  | None -> state
  | Some (name, remain) ->
    let (_, state) = modelize_def name remain state in
    modelize_defs state


let primitives = [
  ("Any", Model.TypeAny);
  ("Void", Model.TypeVoid);
  ("Int", Model.TypeInt);
  ("Char", Model.TypeChar);
  ("String", Model.TypeString);
]

let modelize_program (program: Ast.program) =
  let defs = Ast.get_types program in
  let state = new_state None defs primitives in
  (modelize_defs state)
