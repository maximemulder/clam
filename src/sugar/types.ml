open Util
open State

open Monad.StateMonad(struct
  type s = state
end)

let make_attrs attrs =
  List.fold_left (fun map (attr: Abt.Type.attr) ->
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
    (Abt.Type.Var { span = type'.span; bind }, state)
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
  let bind = { Abt.Type.name = def.name } in
  let currents = NameMap.add def.name (bind, false) state.scope.currents in
  let state = { state with scope = { state.scope with currents} } in
  let (type', state) = desugar_type def.type' state in
  let current = NameMap.find def.name state.scope.currents in
  let type' = if snd current then (Abt.Type.Rec { span = def.span; bind = fst current; body = type' }) else type' in
  let currents = NameMap.remove def.name state.scope.currents in
  let types = NameMap.add def.name type' state.scope.types in
  let abt_def = { Abt.Program.span = def.span; name = def.name; type' } in
  let abt_types = IntMap.add id abt_def state.abt_types in
  let state = { state with abt_types; scope = { state.scope with currents; types } } in
  (type', state)

and desugar_product product =
  let fields = List.partition_map partition_field product.fields in
  match fields with
  | ([], []) ->
    return (Abt.Type.Record { span = product.span; attrs = NameMap.empty })
  | (fields, []) ->
    let* elems = list_map desugar_tuple_elem fields in
    return (Abt.Type.Tuple { span = product.span; elems })
  | ([], fields) ->
    let* attrs = list_map desugar_record_attr fields in
    let attrs = make_attrs attrs in
    return (Abt.Type.Record { span = product.span; attrs })
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
    return (Abt.Type.Lam { span; param; ret })

and desugar_univ univ =
  desugar_univ_curry univ.span univ.params univ.ret

and desugar_univ_curry span params ret =
  match params with
  | [] ->
    desugar_type ret
  | (param :: params) ->
    let* param = desugar_param param in
    let var = Abt.Type.Var { span = param.span; bind = param.bind } in
    let* ret = with_scope_type param.bind.name var (desugar_univ_curry span params ret) in
    return (Abt.Type.Univ { span = span; param; ret })

and desugar_abs abs =
  desugar_abs_curry abs.span abs.params abs.body

and desugar_abs_curry span params body =
  match params with
  | [] ->
    desugar_type body
  | (param :: params) ->
    let* param = desugar_param param in
    let var = Abt.Type.Var { span = param.span; bind = param.bind } in
    let* body = with_scope_type param.bind.name var (desugar_abs_curry span params body) in
    return (Abt.Type.Abs { span; param; body })

and desugar_app app =
  let* abs = desugar_type app.abs in
  desugar_app_curry app.span abs app.args

and desugar_app_curry span abs args =
  match args with
  | [] ->
    return abs
  | (arg :: args) ->
    let* arg = desugar_type arg in
    let app = (Abt.Type.App { span; abs; arg }) in
    desugar_app_curry span app args

and desugar_param param: Abt.Type.param t =
  let* lower, upper = desugar_interval param in
  return { Abt.Type.span = param.span; bind = { name = param.name }; lower; upper }

and desugar_interval param =
  let lower = Option.bind param.interval (fun interval -> interval.Ast.lower) in
  let upper = Option.bind param.interval (fun interval -> interval.Ast.upper) in
  match lower, upper with
  | Some lower, Some upper ->
    let* lower = desugar_type lower in
    let* upper = desugar_type upper in
    return (lower, upper)
  | Some lower, None ->
    let* lower = desugar_type lower in
    (* let upper = Type.Kind.get_kind_max param.span lower in *)
    let upper = Abt.Type.Top { span = param.span } in
    return (lower, upper)
  | None, Some upper ->
    let* upper = desugar_type upper in
    (* let lower = Type.Kind.get_kind_max param.span upper in *)
    let lower = Abt.Type.Bot { span = param.span } in
    return (lower, upper)
  | _, _ ->
    return (Abt.Type.Bot { span = param.span }, Abt.Type.Top { span = param.span })

and partition_field field =
  match field with
  | Ast.FieldTypeElem elem -> Either.Left elem
  | Ast.FieldTypeAttr attr -> Either.Right attr

and desugar_tuple_elem field =
  desugar_type field.type'

and desugar_record_attr field =
  let* type' = desugar_type field.type' in
  return {
    Abt.Type.span = field.span;
    label = field.label;
    type'
  }

and desugar_union union =
  let* left  = desugar_type union.left  in
  let* right = desugar_type union.right in
  return (Abt.Type.Union { span = union.span; left; right })

and desugar_inter inter =
  let* left  = desugar_type inter.left  in
  let* right = desugar_type inter.right in
  return (Abt.Type.Inter { span = inter.span; left; right })

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
