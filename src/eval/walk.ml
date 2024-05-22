open Abt
open Value

type context = {
  out: Util.writer;
  defs: def_expr BindMap.t;
  primitives: (bind_expr * value) list;
  frame: frame;
}

let root primitives =
  { parent = None; binds = BindMap.of_list primitives }

let new_empty defs primitives out =
  { out; defs; primitives; frame = { parent = Some (root primitives); binds = BindMap.empty } }

let new_scope context binds =
  { context with frame = { parent = Some context.frame; binds } }

let new_frame context frame binds =
  { context with frame = { parent = Some frame; binds } }

let get_frame context =
  context.frame

let rec get_bind bind stack =
  match BindMap.find_opt bind stack.binds with
  | Some value -> value
  | None -> get_bind bind (Option.get stack.parent)

open Util.Monad.ReaderMonad(struct
  type r = context
end)

let rec eval (expr: Abt.expr) =
  match expr with
  | ExprUnit unit ->
    eval_unit unit
  | ExprBool bool ->
    eval_bool bool
  | ExprInt int ->
    eval_int int
  | ExprString string ->
    eval_string string
  | ExprBind bind ->
    eval_bind bind
  | ExprTuple tuple ->
    let* values = list_map eval tuple.elems in
    return (VTuple values)
  | ExprRecord record ->
    let* attrs = fold_list (fun map (attr: attr_expr) ->
      let* value = (eval attr.expr) in
      return (Util.NameMap.add attr.label value map)
    ) Util.NameMap.empty record.attrs in
    return (VRecord attrs)
  | ExprElem elem ->
    let* elems = eval_tuple elem.tuple in
    return (List.nth elems elem.index)
  | ExprAttr attr ->
    let* attrs = eval_record attr.record in
    return (Util.NameMap.find attr.label attrs)
  | ExprAscr ascr ->
    eval ascr.expr
  | ExprIf if' ->
    let* cond = eval if'.cond in
    if value_bool if'.cond cond then eval if'.then' else eval if'.else'
  | ExprLamAbs abs ->
    let* frame = get_frame in
    return (VLam (VCode { abs; frame }))
  | ExprLamApp app ->
    eval_lam_app app
  | ExprUnivAbs abs ->
    eval_univ_abs abs
  | ExprUnivApp app ->
    eval_univ_app app

and eval_unit _ =
  return VUnit

and eval_bool bool =
  return (VBool bool.value)

and eval_int int =
  return (VInt int.value)

and eval_string string =
  return (VString string.value)

and eval_bind expr context =
  match BindMap.find_opt expr.bind context.defs with
  | Some def ->
    eval def.expr (new_empty context.defs context.primitives context.out)
  | None ->
    get_bind expr.bind context.frame

and eval_tuple expr =
  let* value = eval expr in
  match value with
  | VTuple values -> return values
  | _ -> Error.raise_tuple expr

and eval_record expr =
  let* value = eval expr in
  match value with
  | VRecord attrs -> return attrs
  | _ -> Error.raise_record expr

and eval_lam_app app context =
  let value = eval app.abs context in
  match value with
  | VLam abs -> eval_lam_app_abs abs app.arg context
  | _ -> Error.raise_lam app.abs

and eval_lam_app_abs abs arg context =
  let value = eval arg context in
  match abs with
  | VPrim prim ->
    prim { expr = arg; value; out = context.out }
  | VCode abs ->
    let binds = BindMap.singleton abs.abs.param.bind value in
    let context = new_frame context abs.frame binds in
    eval abs.abs.body context

and eval_univ_abs abs =
  eval abs.body

and eval_univ_app app =
  eval app.abs

let eval_def def defs primitives stdout =
  let defs = List.map (fun (def: Abt.def_expr) -> def.bind, def) defs in
  let defs = BindMap.of_list defs in
  eval (def: Abt.def_expr).expr (new_empty defs primitives stdout)
