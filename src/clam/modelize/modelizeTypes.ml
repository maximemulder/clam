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

let fold_remain map (remain: Ast.def_type) =
  NameMap.add remain.name remain.type' map

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
  List.fold_left (fun map (attr: Model.attr_type) ->
    let name = attr.name in
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
  | TypeProduct (fields) ->
    modelize_product type' fields
  | TypeInter (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Model.TypeInter { pos; left; right })
  | TypeUnion (left, right) ->
    let* left = modelize_type left in
    let* right = modelize_type right in
    return (Model.TypeUnion { pos; left; right })
  | TypeAbsExpr (params, body) ->
    modelize_abs_expr pos params body
  | TypeAbsExprType (params, body) ->
    modelize_abs_expr_type pos params body
  | TypeAbs (params, body) ->
    modelize_abs pos params body
  | TypeApp (type', args) ->
    let* type' = modelize_type type' in
    modelize_app pos type' args

and modelize_abs_expr pos params body =
  match params with
  | [] ->
    modelize_type body
  | (param :: params) ->
    let* param = modelize_type param in
    let* body = modelize_abs_expr pos params body in
    return (Model.TypeAbsExpr { pos; param; body })

and modelize_abs_expr_type pos params body =
  match params with
  | [] ->
    modelize_type body
  | (param :: params) ->
    let* param = modelize_param param in
    let type' = (param.name, Model.TypeVar { pos = Model.type_pos param.bound; param }) in
    let* body = with_scope (modelize_abs_expr_type pos params body) [type'] in
    return (Model.TypeAbsExprType { pos; param; body })

and modelize_abs pos params body =
  match params with
  | [] ->
    modelize_type body
  | (param :: params) ->
    let* param = modelize_param param in
    let type' = (param.name, Model.TypeVar { pos = Model.type_pos param.bound; param }) in
    let* body = with_scope (modelize_abs pos params body) [type'] in
    return (Model.TypeAbs { pos; param; body })

and modelize_app pos type' args =
  match args with
  | [] ->
    return type'
  | (arg :: args) ->
    let* arg = modelize_type arg in
    let app = (Model.TypeApp { pos; type'; arg }) in
    modelize_app pos app args

and modelize_param (param: Ast.param): Model.param_type t =
  let* bound = (match param.type' with
    | Some type' ->
      modelize_type type'
    | None ->
      return (Model.TypeTop { pos = param.pos })
  ) in
  return { Model.name = param.name; Model.bound }

and modelize_product type' fields =
  let fields = List.partition_map partition_field fields in
  match fields with
  | ([], []) ->
    return (Model.TypeRecord { pos = fst type'; attrs = NameMap.empty })
  | (fields, []) ->
    let* elems = map_list modelize_tuple_elem fields in
    return (Model.TypeTuple { pos = fst type'; elems })
  | ([], fields) ->
    let* attrs = map_list modelize_record_attr fields in
    let attrs = make_attrs attrs in
    return (Model.TypeRecord { pos = fst type'; attrs })
  | _ ->
    ModelizeErrors.raise_type_product type'

and partition_field field =
  match field with
  | Ast.FieldTypeElem elem -> Either.Left elem
  | Ast.FieldTypeAttr attr -> Either.Right attr

and modelize_tuple_elem field =
  modelize_type field.type'

and modelize_record_attr field =
  let* type' = modelize_type field.type' in
  return {
    Model.pos = field.pos;
    Model.name = field.name;
    Model.type' = type'
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
  ("Top",    Primitive.top);
  ("Bot",    Primitive.bot);
  ("Unit",   Primitive.unit);
  ("Bool",   Primitive.bool);
  ("Int",    Primitive.int);
  ("Char",   Primitive.char);
  ("String", Primitive.string);
]

let modelize_program (program: Ast.program) =
  let defs = Ast.get_program_types program in
  let state = make_state defs primitives in
  let state = modelize_defs state in
  (state.scope.dones, state.all)

let modelize_abs (param: Model.param_type) state =
  let type' = (param.name, Model.TypeVar { pos = Model.type_pos param.bound; param}) in
  let (_, state) = make_child [type'] state in
  state.scope.dones
