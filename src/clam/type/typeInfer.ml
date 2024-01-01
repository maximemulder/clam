(* UTILS *)

let todo = (Failure "TODO")

let unwrap_base type' = List.nth (List.nth (type'.Type.union) 0).inter 0

open TypeState
open TypeConstrain
open TypePolar

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

let with_type type' parent =
  constrain type' parent

let with_constrain f parent =
  let* type' = f in
  constrain type' parent

let rec infer (expr: Abt.expr) =
  with_var (fun var_type ->
    let* () = infer_with expr var_type in
    return var_type
  )

and infer_with (expr: Abt.expr) =
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
  with_type TypePrimitive.unit

and infer_bool _ =
  with_type TypePrimitive.bool

and infer_int _ =
  with_type TypePrimitive.int

and infer_string _ =
  with_type TypePrimitive.string

and infer_bind bind =
  with_constrain (
    let bind = Option.get !(bind.bind) in
    let* type' = get_bind_type bind in
    match type' with
    | Some type' ->
      return type'
    | None ->
      let* def = get_bind_def bind in
      let* type' = infer_def def in
      return type'
  )

and infer_tuple tuple =
  with_constrain (
    let* elems = map_list infer tuple.elems in
    return (Type.base (Type.Tuple { elems }))
  )

and infer_record record =
  with_constrain (
    let* attrs = map_list infer_record_attr record.attrs in
    let attrs = List.fold_left (fun map (attr: Type.attr) -> Utils.NameMap.add attr.name attr map) Utils.NameMap.empty attrs in
    return (Type.base (Type.Record { attrs }))
  )

and infer_record_attr attr =
  let* type' = infer attr.expr in
  return { Type.name = attr.name; type' }

and infer_elem elem =
  with_constrain (
    let* tuple = infer elem.expr in
    let* ctx = get_context in
    match search ctx (infer_elem_type elem.index) tuple with
    | Some type' ->
      return type'
    | None ->
      TypeError.infer_elem elem tuple
  )

and infer_elem_type index tuple  =
  match tuple with
  | Tuple tuple ->
    List.nth_opt tuple.elems index
  | _ ->
    None

and infer_attr attr =
  with_constrain (
    let* record = infer attr.expr in
    let* ctx = get_context in
    match search ctx (infer_attr_type attr.name) record with
    | Some type' ->
      return type'
    | None ->
      TypeError.infer_attr attr record
  )

and infer_attr_type name record =
  match record with
  | Record record ->
    Utils.NameMap.find_opt name record.attrs
    |> Option.map (fun (attr: Type.attr) -> attr.type')
  | _ ->
    None

and infer_abs abs =
  with_constrain (
    match abs.param.type' with
    | Some type' ->
      let* type' = validate_proper type' in
      let* ret = with_bind abs.param.bind type'
        (infer abs.body) in
      return (Type.base (Type.AbsExpr { param = type'; ret }))
    | None ->
      with_var (fun param_type ->
        with_var (fun ret_type ->
          let* () = with_bind abs.param.bind param_type
            (infer_with abs.body ret_type) in
          return (Type.base (Type.AbsExpr { param = param_type; ret = ret_type }))
        )
      )
  )

and infer_app app parent =
  let* _ = with_var (fun param_type ->
    with_var (fun ret_type ->
      let abs_type = Type.base (Type.AbsExpr { param = param_type; ret = ret_type }) in
      let* () = infer_with app.expr abs_type in
      let* () = infer_with app.arg param_type in
      let* () = constrain ret_type parent in
      return ret_type
    )
  ) in
  return ()

and infer_ascr ascr =
  with_constrain (
    let* type' = validate_proper ascr.type' in
    let* () = infer_with ascr.expr type' in
    return type'
  )

and infer_if if' =
  with_constrain (
    let* () = infer_with if'.cond TypePrimitive.bool in
    let* then' = infer if'.then' in
    let* else' = infer if'.else' in
    let* ctx = get_context in
    let type' = TypeSystem.join ctx then' else' in
    return type'
  )

and infer_def def =
  let* () = remove_def def.bind in
  let* type' = infer_def_type def in
  let* () = add_bind def.bind type' in
  return type'

and infer_def_type def =
  with_level 0 (
    print_endline("");
    print_endline("def " ^ def.bind.name);
    match def.type' with
    | Some def_type ->
      let* def_type = validate_proper def_type in
      let* _ = with_bind def.bind def_type
        (infer_with def.expr def_type) in
      return def_type
    | None ->
      with_var (fun var_type ->
        let* () = with_bind def.bind var_type
          (infer_with def.expr var_type) in
          return var_type
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
  let types = List.filter (fun (e: entry_type) -> not(List.exists (fun (p: entry_type) -> p.bind.name = e.bind.name) primitives)) state.types in
  List.iter (fun (e: entry_type) -> print_endline(e.bind.name ^ ": " ^ TypeDisplay.display e.type')) types
