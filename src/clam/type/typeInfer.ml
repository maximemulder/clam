(* UTILS *)

let todo = (Failure "TODO")

let cmp_bind a b =
  a.Abt.id = b.Abt.id

(* CONTEXTS *)

type entry_def = {
  bind: Abt.bind_expr;
  def: Abt.def_expr;
}

type entry_type = {
  bind: Abt.bind_expr;
  type': Type.type';
}

type entry_bounds = {
  bind: Abt.bind_type;
  level: int;
  lower: Type.type';
  upper: Type.type';
}

(* STATE *)

type state = {
  level: int;
  defs: entry_def list;
  types: entry_type list;
  bounds: entry_bounds list;
}

let make_state defs types =
  let defs = List.map (fun def -> { bind = def.Abt.bind; def }) defs in
  { level = 0; defs; types; bounds = [] }

let get_context state =
  (* TODO: bounds ??? *)
  let assumptions = List.map (fun entry -> { TypeContext.bind = entry.bind; bound = entry.upper }) state.bounds in
  { TypeContext.assumptions }, state

let get_bind_def bind state =
  let entry = List.find (fun (entry: entry_def) -> cmp_bind entry.bind bind) state.defs in
  entry.def, state

let get_bind_type bind state =
  let entry = List.find_opt (fun (entry: entry_type) -> cmp_bind entry.bind bind) state.types in
  let type' = Option.map (fun entry -> entry.type')  entry in
  type', state

let get_lower_bound bind state =
  let entry = List.find (fun (entry: entry_bounds) -> entry.bind == bind) state.bounds in
  entry.lower, state

let get_upper_bound bind state =
  let entry = List.find (fun (entry: entry_bounds) -> entry.bind == bind) state.bounds in
  entry.upper, state

let update_upper_bound bind bound state =
  let bounds = List.map (fun (entry: entry_bounds) ->
    if entry.bind == bind then
      let ctx, _ = get_context state in
      { entry with upper = TypeSystem.meet ctx entry.upper bound }
    else
      entry
    ) state.bounds in
  (), { state with bounds }

let update_lower_bound bind bound state =
  let bounds = List.map (fun (entry: entry_bounds) ->
    if entry.bind == bind then
      let ctx, _ = get_context state in
      { entry with lower = TypeSystem.join ctx entry.lower bound }
    else
      entry
    ) state.bounds in
  (), { state with bounds }

let remove_def bind state =
  let defs = List.filter (fun (entry: entry_def) -> not (cmp_bind entry.bind bind)) state.defs in
  let state = { state with defs } in
  (), state

let add_bind bind type' state =
  let types = { bind; type' } :: state.types in
  let state = { state with types } in
  (), state

let with_bind bind type' f state =
  let types = { bind; type' } :: state.types in
  let state = { state with types } in
  let x, state = f state in
  let types = List.filter (fun (entry: entry_type) -> not (cmp_bind entry.bind bind)) state.types in
  let state = { state with types } in
  x, state

open Monad.Monad(Monad.StateMonad(struct
  type s = state
end))

let counter = ref 0

(* TODO: Use a "with" function instead to constrain each bound to its scope *)
let make_var state =
  let name = "'" ^ string_of_int counter.contents in
  counter := counter.contents + 1;
  let bind = { Abt.name } in
  let type' = Type.base (Type.Var { bind }) in
  let bound = { bind; level = state.level; lower = TypePrimitive.bot; upper = TypePrimitive.top } in
  let state = { state with bounds = bound :: state.bounds } in
  (bind, type'), state

let remove_var bind state =
  let bounds = List.filter (fun entry -> entry.bind != bind) state.bounds in
  (), { state with bounds }

let with_var f =
  let* bind, type' = make_var in
  let* type' = f bind type' in
  let* type' = if TypeUtils.contains type' bind then
    let* bound = get_upper_bound bind in
    let param = { Type.bind; Type.bound } in
    return (Type.base (Type.AbsTypeExpr { param; ret = type' }))
  else
    return type'
  in
  (* TODO: Prevent variables from escaping and uncomment this *)
  (* let* () = remove_var bind in *)
  return type'

let unwrap_base type' = List.nth (List.nth (type'.Type.union) 0).inter 0

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

let constrain sub sup =
  print_endline("constrain `" ^ TypeDisplay.display sub ^ "` < `" ^ TypeDisplay.display sup ^ "`");
  match unwrap_base sub, unwrap_base sup with
  (* TODO: Var escape *)
  | Type.Var sub_var, Type.Var sup_var when sub_var.bind == sup_var.bind ->
    return ()
  | _, Type.Var var ->
    update_lower_bound var.bind sub
  | Type.Var var, _ ->
    update_upper_bound var.bind sup
  | _, _ ->
    return ()

let rec infer (expr: Abt.expr) =
  with_var (fun bind type' ->
    let* () = infer_parent expr type' in
    get_lower_bound bind
  )

and infer_parent expr parent =
  match expr with
  | ExprUnit unit ->
    infer_unit unit parent
  | ExprBool bool ->
    infer_bool bool parent
  | ExprInt int ->
    infer_int int parent
  | ExprString string ->
    infer_string string parent
  | ExprBind bind ->
    infer_bind bind parent
  | ExprTuple tuple ->
    infer_tuple tuple parent
  | ExprRecord record ->
    infer_record record parent
  | ExprElem elem ->
    infer_elem elem parent
  | ExprAttr attr ->
    infer_attr attr parent
  | ExprIf if' ->
    infer_if if' parent
  | ExprAbs abs ->
    infer_abs abs parent
  | ExprApp app ->
    infer_app app parent
  | ExprAscr ascr ->
    infer_ascr ascr parent
  | _ ->
    raise todo

and infer_unit _ parent =
  constrain TypePrimitive.unit parent

and infer_bool _ parent =
  constrain TypePrimitive.bool parent

and infer_int _ parent =
  constrain TypePrimitive.int parent

and infer_string _ parent =
  constrain TypePrimitive.string parent

and infer_bind bind parent =
  let bind = Option.get !(bind.bind) in
  let* type' = get_bind_type bind in
  match type' with
  | Some type' ->
    constrain type' parent
  | None ->
  let* def = get_bind_def bind in
  let* type' = infer_def def in
  constrain type' parent

and infer_tuple tuple parent =
  let* elems = map_list infer tuple.elems in
  constrain (Type.base (Type.Tuple { elems })) parent

and infer_record record parent =
  let* attrs = map_list infer_record_attr record.attrs in
  let attrs = List.fold_left (fun map (attr: Type.attr) -> Utils.NameMap.add attr.name attr map) Utils.NameMap.empty attrs in
  constrain (Type.base (Type.Record { attrs })) parent

and infer_record_attr attr =
  let* type' = infer attr.expr in
  return { Type.name = attr.name; type' }

and infer_elem elem parent =
  let* tuple = infer elem.expr in
  let* ctx = get_context in
  match search ctx (infer_elem_type elem.index) tuple with
  | Some type' ->
    constrain type' parent
  | None ->
    TypeError.infer_elem elem tuple

and infer_elem_type index tuple  =
  match tuple with
  | Tuple tuple ->
    List.nth_opt tuple.elems index
  | _ ->
    None

and infer_attr attr parent =
  let* record = infer attr.expr in
  let* ctx = get_context in
  match search ctx (infer_attr_type attr.name) record with
  | Some type' ->
    constrain type' parent
  | None ->
    TypeError.infer_attr attr record

and infer_attr_type name record =
    match record with
    | Record record ->
      Utils.NameMap.find_opt name record.attrs
      |> Option.map (fun (attr: Type.attr) -> attr.type')
    | _ ->
      None

(* TODO: It is probably wrong that the function is instanciated multiples times *)
(* Also the variables are not constrained to their scope *)
and infer_abs abs parent =
  let* type' = match abs.param.type' with
  | Some type' ->
    let* type' = validate_proper type' in
    let* ret = with_bind abs.param.bind type'
      (infer abs.body) in
    return (Type.base (Type.AbsExpr { param = type'; ret }))
  | None ->
    with_var (fun param_bind param_type ->
      with_var (fun ret_bind ret_type ->
        let abs_type = (Type.base (Type.AbsExpr { param = param_type; ret = ret_type })) in
        let* () = constrain parent abs_type in
        let* () = with_bind abs.param.bind param_type
          (infer_parent abs.body ret_type) in
        let* param_bound = get_upper_bound param_bind in
        let* ret_bound = get_lower_bound ret_bind in
        return (Type.base (Type.AbsExpr { param = param_bound; ret = ret_bound }))
      )
    )
  in
  constrain type' parent

and infer_app app parent =
  let* abs = infer app.expr in
  let* arg = infer app.arg in
  let* type' = infer_app_type abs arg in
  constrain type' parent

and infer_app_type abs arg =
  match unwrap_base abs with
  | AbsExpr abs ->
    let* () = constrain arg abs.param in
    return abs.ret
  | Var _ ->
    let* ctx = get_context in
    let abs = TypeSystem.promote ctx abs in
    infer_app_type abs arg
  | _ ->
    raise todo

and infer_ascr ascr parent =
  let* type' = validate_proper ascr.type' in
  let* () = infer_parent ascr.expr type' in
  constrain type' parent

and infer_if if' parent =
  let* cond' = infer if'.cond in
  let* () = constrain cond' TypePrimitive.bool in
  let* then' = infer if'.then' in
  let* else' = infer if'.else' in
  let* () = constrain then' parent in
  let* () = constrain else' parent in
  return ()

and infer_def def =
  let* () = remove_def def.bind in
  let* type' = infer_def_type def in
  let* () = add_bind def.bind type' in
  return type'

and infer_def_type def =
  print_endline("");
  print_endline("infer def " ^ def.bind.name);
  match def.type' with
  | Some def_type ->
    let* def_type = validate_proper def_type in
    let* body_type = with_bind def.bind def_type
      (infer def.expr) in
    let* () = constrain body_type def_type in
    return def_type
  | None ->
    let* var_bind, var_type = make_var in
    let* () = with_bind def.bind var_type
      (infer_parent def.expr var_type) in
    let* lower_bound = get_lower_bound var_bind in
    if TypeUtils.contains lower_bound var_bind then
      TypeError.infer_recursive_type def
    else
      return lower_bound

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
