(* UTILS *)

let todo = (Failure "TODO")

let unwrap_base type' = List.nth (List.nth (type'.Type.union) 0).inter 0

open TypeState
open TypeConstrain

(* VALIDATE *)

let validate_proper type' =
  let* ctx = get_context in
  return (TypeValidate.validate_proper ctx type')

(* SEARCH *)

let rec search ctx f (type': Type.type') =
  search_union ctx f type'

and search_union ctx f union =
  let types = List.map (search_inter ctx f) union.union in
  Utils.list_option_meet types (TypeSystem.join ctx)

and search_inter ctx f inter =
  let types = List.map (search_base ctx f) inter.inter in
  Utils.list_option_join types (TypeSystem.meet ctx)

and search_base ctx f type' =
  match type' with
  | Type.Bot ->
    Some TypePrimitive.bot
  | Type.Var var ->
    let bound = TypeContext.get_bind_type ctx var.bind in
    search ctx f bound
  | Type.App app ->
    let abs = TypeSystem.promote ctx app.abs in
    let type' = TypeSystem.compute ctx abs app.arg in
    search ctx f type'
  | _ ->
    f type'

(* TYPE INFERENCE *)

let rec infer (expr: Abt.expr) =
  match expr with
  | ExprUnit unit ->
    infer_unit unit
  | ExprBool bool ->
    infer_bool bool
  | ExprInt int ->
    infer_int int
  | ExprString string ->
    infer_string string
  | ExprBind bind ->
    infer_bind bind
  | ExprTuple tuple ->
    infer_tuple tuple
  | ExprRecord record ->
    infer_record record
  | ExprElem elem ->
    infer_elem elem
  | ExprAttr attr ->
    infer_attr attr
  | ExprIf if' ->
    infer_if if'
  | ExprAbs abs ->
    infer_abs abs
  | ExprApp app ->
    infer_app app
  | ExprAscr ascr ->
    infer_ascr ascr
  | _ ->
    print_endline "TODO INFER";
    raise todo

and infer_unit _ =
  return TypePrimitive.unit

and infer_bool _ =
  return TypePrimitive.bool

and infer_int _ =
  return TypePrimitive.int

and infer_string _ =
  return TypePrimitive.string

and infer_bind bind =
  let bind = Option.get !(bind.bind) in
  let* type' = get_bind_type bind in
  match type' with
  | Some type' ->
    return type'
  | None ->
    let* def = get_bind_def bind in
    let* type' = infer_def def in
    return type'

and infer_tuple tuple =
  let* elems = map_list infer tuple.elems in
  return (Type.base (Type.Tuple { elems }))

and infer_record record =
  let* attrs = map_list infer_record_attr record.attrs in
  let attrs = List.fold_left (fun map (attr: Type.attr) -> Utils.NameMap.add attr.name attr map) Utils.NameMap.empty attrs in
  return (Type.base (Type.Record { attrs }))

and infer_record_attr attr =
  let* type' = infer attr.expr in
  return { Type.name = attr.name; type' }

and infer_elem elem =
  let* tuple = infer elem.expr in
  let* ctx = get_context in
  match search ctx (infer_elem_type elem.index) tuple with
  | Some type' ->
    return type'
  | None ->
    TypeError.infer_elem elem tuple

and infer_elem_type index tuple  =
  match tuple with
  | Tuple tuple ->
    List.nth_opt tuple.elems index
  | _ ->
    None

and infer_attr attr =
  let* record = infer attr.expr in
  let* ctx = get_context in
  match search ctx (infer_attr_type attr.name) record with
  | Some type' ->
    return type'
  | None ->
    TypeError.infer_attr attr record

and infer_attr_type name record =
  match record with
  | Record record ->
    Utils.NameMap.find_opt name record.attrs
    |> Option.map (fun (attr: Type.attr) -> attr.type')
  | _ ->
    None

and infer_abs abs =
  match abs.param.type' with
  | Some type' ->
    let* type' = validate_proper type' in
    let* ret = with_bind abs.param.bind type'
      (infer abs.body) in
    return (Type.base (Type.AbsExpr { param = type'; ret }))
  | None ->
    with_var (fun param_bind param_type ->
      with_var (fun ret_bind ret_type ->
        let* body = with_bind abs.param.bind param_type
          (infer abs.body) in
        let* () = constrain body ret_type in
        let* param_bound = get_upper_bound param_bind in
        let* ret_bound = get_lower_bound ret_bind in
        if TypeUtils.contains ret_bound param_bind then
          let abs = Type.base (Type.AbsExpr { param = param_type; ret = ret_bound }) in
          return (Type.base (Type.AbsTypeExpr { param = { bind = param_bind; bound = param_bound }; ret = abs }))
        else
          return (Type.base (Type.AbsExpr { param = param_bound; ret = ret_bound }))
      )
    )

and infer_app app =
  with_var (fun ret_bind ret_type ->
    let* abs = infer app.expr in
    let* arg = infer app.arg in
    let abs_type = Type.base (Type.AbsExpr { param = arg; ret = ret_type }) in
    let* () = constrain abs abs_type in
    let* ret = get_lower_bound ret_bind in
    return ret
  )

and infer_ascr ascr =
  let* type' = validate_proper ascr.type' in
  let* body = infer ascr.expr in
  let* () = constrain body type' in
  return type'

and infer_if if' =
  let* cond' = infer if'.cond in
  let* () = constrain cond' TypePrimitive.bool in
  let* then' = infer if'.then' in
  let* else' = infer if'.else' in
  let* ctx = get_context in
  let type' = TypeSystem.join ctx then' else' in
  return type'

and infer_def def =
  let* () = remove_def def.bind in
  let* type' = infer_def_type def in
  let* () = add_bind def.bind type' in
  return type'

and infer_def_type def =
  print_endline("");
  print_endline("infer def " ^ def.bind.name);
  with_level 0 (
    match def.type' with
    | Some def_type ->
      let* def_type = validate_proper def_type in
      let* body_type = with_bind def.bind def_type
        (infer def.expr) in
      let* () = constrain body_type def_type in
      return def_type
    | None ->
      with_var (fun var_bind var_type ->
        let* body = with_bind def.bind var_type
          (infer def.expr) in
        let* () = constrain body var_type in
        let* lower_bound = get_lower_bound var_bind in
        if TypeUtils.contains lower_bound var_bind then
          TypeError.infer_recursive_type def
        else
          return lower_bound
      )
  )

let rec check_defs state =
  match state.defs with
  | entry :: _ ->
    let _, state = infer_def entry.def state in
    check_defs state
  | [] ->
    state

let check_defs defs primitives =
  let primitives = List.map (fun primitive -> { bind = fst primitive; type' = snd primitive }) primitives in
  let state = make_state defs primitives in
  let state = check_defs state in
  print_endline("");
  List.iter (fun (e: entry_type) -> print_endline(e.bind.name ^ ": " ^ TypeDisplay.display e.type')) state.types
