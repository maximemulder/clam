open Util
open Abt

type scope = {
  parent: scope option;
  remains: Ast.type' NameMap.t;
  currents: Ast.type' NameMap.t;
  dones: Abt.type' NameMap.t;
}

type state = {
  scope: scope;
  all: Abt.type' list
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
  List.fold_left (fun map (attr: Abt.attr_type) ->
    let name = attr.label in
    if NameMap.mem name map
      then Errors.raise_type_duplicate_attribute attr
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

let rec desugar_type (type': Ast.type') =
  match type' with
  | TypeName    type' -> desugar_name    type'
  | TypeProduct type' -> desugar_product type'
  | TypeLam     type' -> desugar_lam     type'
  | TypeUniv    type' -> desugar_univ    type'
  | TypeAbs     type' -> desugar_abs     type'
  | TypeApp     type' -> desugar_app     type'
  | TypeUnion   type' -> desugar_union   type'
  | TypeInter   type' -> desugar_inter   type'

and desugar_name type' state =
  let name = type'.name in
  match find_remain name state with
  | Some def -> desugar_def name def state
  | None     ->
  match find_current name state with
  | Some _ -> Errors.raise_type_recursive type'
  | None   ->
  match find_done name state with
  | Some type' -> (type', state)
  | None       ->
  match state.scope.parent with
  | Some scope ->
    let parent = { state with scope } in
    let (type', parent) = desugar_name type' parent in
    (type', { parent with scope = { state.scope with parent = Some parent.scope } })
  | None -> Errors.raise_type_bound type'

and desugar_def name _type' =
  with_name name desugar_type

and desugar_product product =
  let fields = List.partition_map partition_field product.fields in
  match fields with
  | ([], []) ->
    return (Abt.TypeRecord { span = product.span; attrs = NameMap.empty })
  | (fields, []) ->
    let* elems = map_list desugar_tuple_elem fields in
    return (Abt.TypeTuple { span = product.span; elems })
  | ([], fields) ->
    let* attrs = map_list desugar_record_attr fields in
    let attrs = make_attrs attrs in
    return (Abt.TypeRecord { span = product.span; attrs })
  | _ ->
    Errors.raise_type_product product

and desugar_lam lam =
  desugar_lam_curry lam.span lam.params lam.ret

and desugar_lam_curry span params ret =
  match params with
  | [] ->
    desugar_type ret
  | (param :: params) ->
    let* param = desugar_type param in
    let* ret = desugar_lam_curry span params ret in
    return (Abt.TypeLam { span = span; param; ret })

and desugar_univ univ =
  desugar_univ_curry univ.span univ.params univ.ret

and desugar_univ_curry span params ret =
  match params with
  | [] ->
    desugar_type ret
  | (param :: params) ->
    let* param = desugar_param_type param in
    let type' = (param.bind.name, Abt.TypeVar { span = Abt.type_span param.bound; bind = param.bind }) in
    let* ret = with_scope (desugar_univ_curry span params ret) [type'] in
    return (Abt.TypeUniv { span = span; param; ret })

and desugar_abs abs =
  desugar_abs_curry abs.span abs.params abs.body

and desugar_abs_curry span params body =
  match params with
  | [] ->
    desugar_type body
  | (param :: params) ->
    let* param = desugar_param_type param in
    let type' = (param.bind.name, Abt.TypeVar { span = Abt.type_span param.bound; bind = param.bind }) in
    let* body = with_scope (desugar_abs_curry span params body) [type'] in
    return (Abt.TypeAbs { span = span; param; body })

and desugar_app app =
  let* abs = desugar_type app.abs in
  desugar_app_curry app.span abs app.args

and desugar_app_curry span abs args =
  match args with
  | [] ->
    return abs
  | (arg :: args) ->
    let* arg = desugar_type arg in
    let app = (Abt.TypeApp { span = span; abs; arg }) in
    desugar_app_curry span app args

and desugar_param_type param: Abt.param_type t =
  let* bound = (match param.type' with
    | Some type' ->
      desugar_type type'
    | None ->
      return (Abt.TypeTop { span = param.span })
  ) in
  return { Abt.bind = { name = param.name }; Abt.bound }

and partition_field field =
  match field with
  | Ast.FieldTypeElem elem -> Either.Left elem
  | Ast.FieldTypeAttr attr -> Either.Right attr

and desugar_tuple_elem field =
  desugar_type field.type'

and desugar_record_attr field =
  let* type' = desugar_type field.type' in
  return {
    Abt.span = field.span;
    Abt.label = field.label;
    Abt.type' = type'
  }

and desugar_union union =
  let* left  = desugar_type union.left  in
  let* right = desugar_type union.right in
  return (Abt.TypeUnion { span = union.span; left; right })

and desugar_inter inter =
  let* left  = desugar_type inter.left  in
  let* right = desugar_type inter.right in
  return (Abt.TypeInter { span = inter.span; left; right })

let desugar_type_expr type' state =
  let (type', _) = desugar_type type' state in
  type'

let rec desugar_defs state =
  match NameMap.choose_opt state.scope.remains with
  | None -> state
  | Some (name, remain) ->
    let (_, state) = desugar_def name remain state in
    desugar_defs state

let span = {
  Code.code = {
    name = "primitives.clam";
    text = "";
  };
  start = 0;
  end' = 0;
}

let primitives = [
  ("Top",    Abt.TypeTop    { span });
  ("Bot",    Abt.TypeBot    { span });
  ("Unit",   Abt.TypeUnit   { span });
  ("Bool",   Abt.TypeBool   { span });
  ("Int",    Abt.TypeInt    { span });
  ("String", Abt.TypeString { span });
]

let desugar_program (program: Ast.program) =
  let defs = Ast.get_program_types program in
  let state = make_state defs primitives in
  let state = desugar_defs state in
  (state.scope.dones, state.all)

let desugar_abs (param: Abt.param_type) state =
  let type' = (param.bind.name, Abt.TypeVar { span = Abt.type_span param.bound; bind = param.bind }) in
  let (_, state) = make_child [type'] state in
  state.scope.dones
