open Collection

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

let check_duplicates names set =
  List.fold_left (fun set name ->
    if NameSet.mem name set
      then Modelize_errors.raise ("duplicate type `" ^ name ^ "`")
      else NameSet.add name set
    ) set names

let fold_remain map remain =
  NameMap.add remain.Ast.type_name remain.Ast.type' map

let fold_done map done' =
  NameMap.add (fst done') (snd done') map

let make_state parent remains dones =
  let names = NameSet.empty in
  let names = check_duplicates (List.map (fun remain -> remain.Ast.type_name) remains) names in
  let _ = check_duplicates (List.map (fun done' -> fst done') dones) names in
  let remains = List.fold_left fold_remain NameMap.empty remains in
  let dones = List.fold_left fold_done NameMap.empty dones in
  { parent; remains; currents = NameMap.empty; dones }

let make_attrs attrs =
  List.fold_left (fun map attr ->
    let name = attr.Model.attr_type_name in
    if NameMap.mem name map
      then Modelize_errors.raise ("duplicate attribute `" ^ name ^ "`")
      else NameMap.add name attr map
  ) NameMap.empty attrs

let with_name name call state =
  let (type', remains) = Collection.extract name state.remains in
  let currents = NameMap.add name type' state.currents in
  let state = { state with remains; currents } in
  let (type', state) = call type' state in
  let currents = NameMap.remove name state.currents in
  let dones = NameMap.add name type' state.dones in
  (type', { state with currents; dones })

let with_scope call types state =
  let parent = state in
  let state = make_state (Some parent) [] types in
  let (result, state) = call state in
  let state = Option.get state.parent in
  (result, state)

let rec modelize_name name state =
  match find_remain name state with
  | Some def -> modelize_def name def state
  | None     ->
  match find_current name state with
  | Some _ -> Modelize_errors.raise ("recursive type `" ^ name ^ "`")
  | None   ->
  match find_done name state with
  | Some type' -> (type', state)
  | None       ->
  match state.parent with
  | Some parent ->
    let (type', parent) = modelize_name name parent in
    (type', { state with parent = Some parent })
  | None -> Modelize_errors.raise ("unbound type `" ^ name ^ "`")

and modelize_def name _type' =
  with_name name modelize_type

and modelize_params params type' =
  let* params = map_list modelize_param params in
  let types = List.map (fun param -> (param.Model.type_param_name, Model.TypeVar param)) params in
  let* type' = with_scope (modelize_type type') types in
  return (params, type')

and modelize_type (type': Ast.type') =
  match type' with
  | TypeIdent name -> modelize_name name
  | TypeAbsExpr (params, type') ->
    let* params = map_list modelize_type params in
    let* type' = modelize_type type' in
    return (Model.TypeAbsExpr (params, type'))
  | TypeAbsExprType (params, type') ->
    let* (params, type') = modelize_params params type' in
    return (Model.TypeAbsExprType (params, type'))
  | TypeTuple (types) ->
    let* types = map_list modelize_type types in
    return (Model.TypeTuple types)
  | TypeRecord (attrs) ->
    let* attrs = map_list modelize_attr attrs in
    return (Model.TypeRecord (make_attrs attrs))
  | TypeInter (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Model.TypeInter (left, right))
  | TypeUnion (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Model.TypeUnion (left, right))
  | TypeAbs (params, type') ->
    let* (params, type') = modelize_params params type' in
    return (Model.TypeAbs (params, type'))
  | TypeApp (type', args) ->
    let* type' = modelize_type type' in
    let* args = map_list modelize_type args in
    return (Model.TypeApp (type', args))

and modelize_param param =
  let* type' = map_option modelize_type param.param_type in
  let type' = Option.value type' ~default:Model.TypeAny in
  return { Model.type_param_name = param.param_name; Model.type_param_type = type' }

and modelize_attr (attr: Ast.attr_type) =
  let* type' = modelize_type attr.attr_type in
  return { Model.attr_type_name = attr.attr_type_name; Model.attr_type = type' }

let modelize_type_expr (type': Ast.type') state =
  let (type', _) = modelize_type type' state in
  type'

let rec modelize_defs state =
  match NameMap.choose_opt state.remains with
  | None -> state
  | Some (name, remain) ->
    let (_, state) = modelize_def name remain state in
    modelize_defs state

let primitives = [
  ("Any",    Model.TypeAny);
  ("Void",   Model.TypeVoid);
  ("Bool",   Model.TypeBool);
  ("Int",    Model.TypeInt);
  ("Char",   Model.TypeChar);
  ("String", Model.TypeString);
]

let modelize_program (program: Ast.program) =
  let defs = Ast.get_program_types program in
  let state = make_state None defs primitives in
  (modelize_defs state).dones

let modelize_block (block: Ast.block) parent =
  let defs = Ast.get_block_types block in
  let state = make_state (Some parent) defs [] in
  (modelize_defs state).dones

let modelize_abs (params: Model.type_param list) parent =
  let types = List.map (fun param -> (param.Model.type_param_name, param.Model.type_param_type)) params in
  let state = make_state (Some parent) [] types in
  state.dones
