(* UTILS *)

let todo = (Failure "TODO")

open TypeState
open TypeConstrain
open TypePolar

(* VALIDATE *)

let validate_proper type' =
  let* ctx = get_context in
  return (TypeValidate.validate_proper ctx type')

(* SEARCH *)

(* This is just a remnant of the old system that works decently with tuples
  however, tuples are fundamentally incompatible with type inference as envisioned
  and will eventually be removed in the future *)

let rec search state f (type': Type.type') =
  search_union state f type'

and search_union state f union =
  let ctx, _ = get_context state in
  let types = List.map (search_inter state f) union.union in
  Utils.list_option_meet types (TypeSystem.join ctx)

and search_inter state f inter =
  let ctx, _ = get_context state in
  let types = List.map (search_base state f) inter.inter in
  Utils.list_option_join types (TypeSystem.meet ctx)

and search_base state f type' =
  let ctx, _ = get_context state in
  match type' with
  | Type.Bot ->
    Some Type.bot
  | Type.Var var ->
    let bound, _ = get_lower_bound var.bind state in
    search state f bound
  | Type.App app ->
    let abs = TypeSystem.promote ctx app.abs in
    let type' = TypeSystem.compute ctx abs app.arg in
    search state f type'
  | _ ->
    f type'

(* TYPE INFERENCE *)

let with_type type' parent =
  constrain type' parent

let with_constrain f parent =
  let* type' = f in
  constrain type' parent

let rec infer (expr: Abt.expr) =
  with_var (fun var ->
    let* () = infer_with expr var in
    return var
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
  with_type Type.unit

and infer_bool _ =
  with_type Type.bool

and infer_int _ =
  with_type Type.int

and infer_string _ =
  with_type Type.string

and infer_bind bind =
  with_constrain (
    let bind = Option.get !(bind.bind) in
    let* type' = get_expr_type bind in
    match type' with
    | Some type' ->
      return type'
    | None ->
      let* def = get_def bind in
      let* type' = infer_def def in
      return type'
  )

and infer_tuple tuple =
  with_constrain (
    let* elems = map_list infer tuple.elems in
    return (Type.tuple elems)
  )

and infer_record record =
  with_constrain (
    let* attrs = map_list infer_record_attr record.attrs in
    let attrs = List.fold_left (fun map (attr: Type.attr) -> Utils.NameMap.add attr.name attr map) Utils.NameMap.empty attrs in
    return (Type.record attrs)
  )

and infer_record_attr attr =
  let* type' = infer attr.expr in
  return { Type.name = attr.name; type' }

and infer_elem elem =
  with_constrain (
    let* tuple = infer elem.expr in
    let* state = get_state in
    match search state (infer_elem_type elem.index) tuple with
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

and infer_attr attr parent =
  let* _ = with_var (fun ret ->
    let record = Type.record (Utils.NameMap.singleton attr.name { Type.name = attr.name; type' = ret }) in
    let* () = infer_with attr.expr record in
    let* () = constrain ret parent in
    return ret
  ) in
  return ()

and infer_abs abs =
  with_constrain (
    match abs.param.type' with
    | Some type' ->
      let* type' = validate_proper type' in
      let* ret = with_expr abs.param.bind type'
        (infer abs.body) in
      return (Type.abs_expr type' ret)
    | None ->
      with_var (fun param ->
        with_var (fun ret ->
          let* () = with_expr abs.param.bind param
            (infer_with abs.body ret) in
          return (Type.abs_expr param ret)
        )
      )
  )

and infer_app app parent =
  let* _ = with_var (fun param ->
    with_var (fun ret ->
      let abs = Type.abs_expr param ret in
      let* () = infer_with app.expr abs in
      let* () = infer_with app.arg param in
      let* () = constrain ret parent in
      return ret
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
    let* () = infer_with if'.cond Type.bool in
    let* then' = infer if'.then' in
    let* else' = infer if'.else' in
    let* ctx = get_context in
    let type' = TypeSystem.join ctx then' else' in
    return type'
  )

and infer_def def =
  let* () = remove_def def.bind in
  let* type' = infer_def_type def in
  let* () = add_expr def.bind type' in
  return type'

and infer_def_type def =
  with_level (
    match def.type' with
    | Some def_type ->
      let* def_type = validate_proper def_type in
      let* _ = with_expr def.bind def_type
        (infer_with def.expr def_type) in
      return def_type
    | None ->
      with_var (fun var ->
        let* () = with_expr def.bind var
          (infer_with def.expr var) in
        return var
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
  let types = List.filter (fun (e: entry_expr) -> not(List.exists (fun (p: entry_expr) -> p.bind.name = e.bind.name) primitives)) state.exprs in
  List.iter (fun (e: entry_expr) -> print_endline(e.bind.name ^ ": " ^ TypeDisplay.display e.type')) types
