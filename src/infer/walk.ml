open State
open Constrain
open Solve

(* TYPE INFERENCE *)

let rec infer_parent (expr: Abt.expr) parent =
  let* type' = infer expr in
  let span = Abt.expr_span expr in
  constrain span type' parent

and infer (expr: Abt.expr) =
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
  | ExprAscr ascr ->
    infer_ascr ascr
  | ExprIf if' ->
    infer_if if'
  | ExprLamAbs abs ->
    infer_lam_abs abs
  | ExprLamApp app ->
    infer_lam_app app
  | ExprUnivAbs abs ->
    infer_univ_abs abs
  | ExprUnivApp app ->
    infer_univ_app app

and infer_unit _ =
  return Type.unit

and infer_bool _ =
  return Type.bool

and infer_int _ =
  return Type.int

and infer_string _ =
  return Type.string

and infer_bind expr =
  infer_bind_bis expr.bind

and infer_tuple expr =
  let* elems = list_map infer expr.elems in
  return (Type.tuple elems)

and infer_record expr =
  let* attrs = list_map infer_record_attr expr.attrs in
  let attrs = List.fold_left (fun map (attr: Type.attr) -> Util.NameMap.add attr.label attr map) Util.NameMap.empty attrs in
  return (Type.record attrs)

and infer_record_attr attr =
  let* type' = infer attr.expr in
  return { Type.label = attr.label; type' }

and infer_elem expr =
  let* tuple = infer expr.tuple in
  let* state = get_state in
  let type' = Search.search_proj (infer_elem_base expr.index) tuple state.ctx in
  match type' with
  | Some type' ->
    return type'
  | None ->
    Error.raise_elem expr tuple

and infer_elem_base index tuple =
  match tuple with
  | Tuple tuple ->
    List.nth_opt tuple.elems index
  | _ ->
    None

and infer_attr expr =
  with_var expr.span (fun ret ->
    let record = Type.record (Util.NameMap.singleton expr.label { Type.label = expr.label; type' = ret }) in
    let* () = infer_parent expr.record record in
    return ret
  )

and infer_lam_abs expr =
  match expr.param.type' with
  | Some type' ->
    let* type' = validate_proper type' in
    let* ret = with_expr expr.param.span expr.param.bind type'
      (infer expr.body) in
    return (Type.lam type' ret)
  | None ->
    with_var expr.span (fun param ->
      with_var expr.span (fun ret ->
        let* () = with_expr expr.param.span expr.param.bind param
          (infer_parent expr.body ret) in
        return (Type.lam param ret)
      )
    )

and infer_lam_app expr =
  with_var expr.span (fun param ->
    with_var expr.span (fun ret ->
      let abs = Type.lam param ret in
      let* () = infer_parent expr.abs abs in
      let* () = infer_parent expr.arg param in
      return ret
    )
  )

and infer_univ_abs expr =
  let* param = validate_param expr.param in
  with_var expr.span (fun var ->
    let type' = Type.univ param var in
    let* () = with_param_rigid param
      (infer_parent expr.body var) in
    return type'
  )

and infer_univ_app expr =
  let* univ = infer expr.abs in
  let* state = get_state in
  let type' = Search.search_app_type infer_univ_app_base univ state.ctx in
  match type' with
  | Some { param; ret } ->
    let* arg = validate expr.arg in
    let* lower = isa param.lower arg in
    let* upper = isa arg param.upper in
    if not lower || not upper then
      Error.raise_univ_type expr arg param.upper
    else
    substitute param.bind arg ret
  | None ->
    Error.raise_univ_kind expr univ

and infer_univ_app_base univ =
  match univ with
  | Univ univ ->
    Some { param = univ.param; ret = univ.ret }
  | _ ->
    None

and infer_ascr expr =
  let* type' = validate_proper expr.type' in
  let* () = infer_parent expr.expr type' in
  return type'

and infer_if expr =
  let* () = infer_parent expr.cond Type.bool in
  let* then' = infer expr.then' in
  let* else' = infer expr.else' in
  join then' else'

and infer_def def =
  let* type' = infer_def_type def in
  let type' = Rename.rename type' in
  let* () = add_expr (def: Abt.def_expr).span def.bind type' in
  return type'

and infer_def_type def =
  match def.type' with
  | Some def_type ->
    let* def_type = validate_proper def_type in
    let* () = with_expr def.span def.bind def_type
      (infer_parent def.expr def_type) in
    return def_type
  | None ->
    with_var def.span (fun var ->
      let* () = with_expr def.span def.bind var
        (infer_parent def.expr var) in
      return var
    )

and infer_bind_bis bind =
  let* type' = get_expr_type bind in
  match type' with
  | Some type' ->
    return type'
  | None ->
    let* def = get_def bind in
    infer_def def

let check_defs state =
  List.fold_left (fun state entry ->
    infer_bind_bis entry.def.bind state |> snd
  ) state state.defs

let check_defs defs primitives =
  let primitives = List.map (fun primitive -> { span = Code.span_primitive; level = 0; bind = fst primitive; type' = snd primitive }) primitives in
  let state = make_state defs primitives in
  let state = check_defs state in
  List.map (fun (entry: entry_expr) -> entry.bind, entry.type') state.exprs

let check_types defs =
  List.map (fun (def: Abt.def_type) -> def.name,
    let ctx = Type.Context.empty in
    let type', _ = Type.Validate.validate def.type' ctx in
    let kind, _ = Type.Kind.get_kind type' ctx in
  kind) defs
