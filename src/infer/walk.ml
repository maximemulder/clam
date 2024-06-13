open Abt.Display
open Solve
open State
open State.Monad

module T = Abt.Type

(* TYPE CONSTRAIN *)

let constrain span sub sup =
  let* () = show
    (!Global.show_infer && not !Global.show_constrain)
    ("constrain " ^ display sub ^ " < " ^ display sup) in
  let* result = isa sub sup in
  let* () = show_ctx !Global.show_infer in
  if result then
    return ()
  else
    Error.raise_constrain span sub sup

(* TYPE INFERENCE *)

let rec infer_parent (expr: Abt.Expr.expr) parent =
  let* type' = infer expr in
  let span = Abt.Span.expr_span expr in
  constrain span type' parent

and infer (expr: Abt.Expr.expr) =

  match expr with
  | Unit unit     -> infer_unit unit
  | Bool bool     -> infer_bool bool
  | Int int       -> infer_int int
  | String string -> infer_string string
  | Bind bind     -> infer_bind bind
  | Tuple tuple   -> infer_tuple tuple
  | Record record -> infer_record record
  | Elem elem     -> infer_elem elem
  | Attr attr     -> infer_attr attr
  | Ascr ascr     -> infer_ascr ascr
  | If if'        -> infer_if if'
  | LamAbs abs    -> infer_lam_abs abs
  | LamApp app    -> infer_lam_app app
  | UnivAbs abs   -> infer_univ_abs abs
  | UnivApp app   -> infer_univ_app app

and infer_unit expr =
  return (T.Unit { span = expr.span })

and infer_bool expr =
  return (T.Bool { span = expr.span })

and infer_int expr =
  return (T.Int { span = expr.span })

and infer_string expr =
  return (T.String { span = expr.span })

and infer_bind expr =
  infer_bind_bis expr.bind

and infer_tuple expr =
  let* elems = list_map infer expr.elems in
  return (T.Tuple { span = expr.span; elems })

and infer_record expr =
  let* attrs = list_map infer_record_attr expr.attrs in
  let attrs = List.fold_left (fun map (attr: Abt.Type.attr) -> Util.NameMap.add attr.label attr map) Util.NameMap.empty attrs in
  return (T.Record { span = expr.span; attrs })

and infer_record_attr attr =
  let* type' = infer attr.expr in
  return { T.span = attr.span; label = attr.label; type' }

and infer_elem expr =
  let* tuple = infer expr.tuple in
  let* state = get in
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
    let record = T.Record { span = expr.span; attrs = (Util.NameMap.singleton expr.label { span = expr.span; T.label = expr.label; type' = ret }) } in
    let* () = infer_parent expr.record record in
    return ret
  )

and infer_lam_abs expr =
  match expr.param.type' with
  | Some type' ->
    let* () = validate_proper type' in
    let* ret = with_expr expr.param.span expr.param.bind type'
      (infer expr.body) in
    return (T.Lam { span = expr.span; param = type'; ret })
  | None ->
    with_var expr.span (fun param ->
      with_var expr.span (fun ret ->
        let* () = with_expr expr.param.span expr.param.bind param
          (infer_parent expr.body ret) in
        return (T.Lam { span = expr.span; param; ret })
      )
    )

and infer_lam_app expr =
  with_var expr.span (fun param ->
    with_var expr.span (fun ret ->
      let abs = T.Lam { span = expr.span; param; ret } in
      let* () = infer_parent expr.abs abs in
      let* () = infer_parent expr.arg param in
      return ret
    )
  )

and infer_univ_abs expr =
  let* () = validate_param expr.param in
  with_var expr.span (fun var ->
    let type' = T.Univ { span = expr.span; param = expr.param; ret = var } in
    let* () = with_param_rigid expr.param
      (infer_parent expr.body var) in
    return type'
  )

and infer_univ_app expr =
  let* univ = infer expr.abs in
  let* state = get in
  let type' = Search.search_app_type infer_univ_app_base univ state.ctx in
  match type' with
  | Some { param; ret } ->
    let* () = validate expr.arg in
    let* lower = isa param.lower expr.arg in
    let* upper = isa expr.arg param.upper in
    if not lower || not upper then
      Error.raise_univ_type expr expr.arg param.upper
    else
    substitute param.bind expr.arg ret
  | None ->
    Error.raise_univ_kind expr univ

and infer_univ_app_base univ =
  match univ with
  | Univ univ ->
    Some { param = univ.param; ret = univ.ret }
  | _ ->
    None

and infer_ascr expr =
  let* () = validate_proper expr.type' in
  let* () = infer_parent expr.expr expr.type' in
  return expr.type'

and infer_if expr =
  let* () = infer_parent expr.cond (T.Bool { span = expr.span }) in
  let* then' = infer expr.then' in
  let* else' = infer expr.else' in
  join then' else'

and infer_def def =
  let* type' = infer_def_type def in
  let type' = Rename.rename type' in
  let* () = add_expr (def: Abt.Program.def_expr).span def.bind type' in
  return type'

and infer_def_type def =
  match def.type' with
  | Some def_type ->
    let* () = validate_proper def_type in
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
  List.map (fun (def: Abt.Program.def_type) -> def.name,
    let ctx = Type.Context.empty in
    let (), _ = Type.Validate.validate def.type' ctx in
    let kind, _ = Type.Kind.get_kind def.type' ctx in
  kind) defs
