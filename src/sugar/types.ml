open Util
open Abt
open State

open Monad.StateMonad(struct
  type s = state
end)

let make_attrs attrs =
  List.fold_left (fun map (attr: Abt.attr_type) ->
    let name = attr.label in
    if NameMap.mem name map
      then Errors.raise_type_duplicate_attribute attr
      else NameMap.add name attr map
  ) NameMap.empty attrs

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
  match find_current name state with
  | Some (bind, _) ->
    let state = { state with scope = {
      state.scope with currents = NameMap.add type'.name (bind, true) state.scope.currents
    }} in
    (TypeVar { span = type'.span; bind }, state)
  | None   ->
  match find_type name state with
  | Some type' -> (type', state)
  | None       ->
  match state.scope.parent with
  | Some scope ->
    let parent = { state with scope } in
    let (type', parent) = desugar_name type' parent in
    (type', { parent with scope = { state.scope with parent = Some parent.scope } })
  | None ->
  match find_ast_type name state with
  | Some (id, def) -> desugar_def id def state
  | None     ->
    Errors.raise_type_bound type'

and desugar_def id def state =
  let bind = { name = def.name } in
  let currents = NameMap.add def.name (bind, false) state.scope.currents in
  let state = { state with scope = { state.scope with currents} } in
  let (type', state) = desugar_type def.type' state in
  let current = NameMap.find def.name state.scope.currents in
  let type' = if snd current then (TypeRec { span = def.span; bind = fst current; body = type' }) else type' in
  let currents = NameMap.remove def.name state.scope.currents in
  let types = NameMap.add def.name type' state.scope.types in
  let abt_def = { span = def.span; name = def.name; type' } in
  let abt_types = IntMap.add id abt_def state.abt_types in
  let state = { state with abt_types; scope = { state.scope with currents; types } } in
  (type', state)

and desugar_product product =
  let fields = List.partition_map partition_field product.fields in
  match fields with
  | ([], []) ->
    return (Abt.TypeRecord { span = product.span; attrs = NameMap.empty })
  | (fields, []) ->
    let* elems = list_map desugar_tuple_elem fields in
    return (Abt.TypeTuple { span = product.span; elems })
  | ([], fields) ->
    let* attrs = list_map desugar_record_attr fields in
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
    let* param = desugar_param param in
    let var = Abt.TypeVar { span = param.interval.span; bind = param.bind } in
    let* ret = with_scope_type param.bind.name var (desugar_univ_curry span params ret) in
    return (Abt.TypeUniv { span = span; param; ret })

and desugar_abs abs =
  desugar_abs_curry abs.span abs.params abs.body

and desugar_abs_curry span params body =
  match params with
  | [] ->
    desugar_type body
  | (param :: params) ->
    let* param = desugar_param param in
    let var = Abt.TypeVar { span = param.interval.span; bind = param.bind } in
    let* body = with_scope_type param.bind.name var (desugar_abs_curry span params body) in
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

and desugar_param param: Abt.param_type t =
  let* interval = desugar_interval param in
  return { Abt.bind = { name = param.name }; interval }

and desugar_interval param =
  match param.interval with
  | Some interval ->
    let* lower = option_map desugar_type interval.lower in
    let* upper = option_map desugar_type interval.upper in
    return { span = interval.span; lower; upper }
  | None ->
    return {
      span = param.span; lower = None; upper = None }

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

let desugar_defs state =
  List.fold_left (fun state (def: Ast.def_type) ->
    desugar_name { span = def.span; name = def.name } state |> snd
  ) state state.ast_types

let desugar_program state =
  let state = desugar_defs state in
  state.abt_types |> IntMap.bindings |> List.map snd, state
