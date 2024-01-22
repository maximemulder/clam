open Abt
open Utils
open RuntimeValue

type context = {
  out: writer;
  defs: def_expr BindMap.t;
  frame: frame;
}

let root =
  { parent = None; binds = BindMap.of_list Primitive.values }

let new_empty defs out =
  { out; defs; frame = { parent = Some root; binds = BindMap.empty } }

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

module Reader = struct
  type r = context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

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
    let* values = map_list eval tuple.elems in
    return (VTuple values)
  | ExprRecord record ->
    let* attrs = fold_list (fun map (attr: attr_expr) ->
      let* value = (eval attr.expr) in
      return (NameMap.add attr.name value map)
    ) NameMap.empty record.attrs in
    return (VRecord attrs)
  | ExprElem elem ->
    let* elems = eval_tuple elem.tuple in
    return (List.nth elems elem.index)
  | ExprAttr attr ->
    let* attrs = eval_record attr.record in
    return (NameMap.find attr.name attrs)
  | ExprAscr ascr ->
    eval ascr.expr
  | ExprIf if' ->
    let* cond = eval if'.cond in
    if value_bool cond then eval if'.then' else eval if'.else'
  | ExprAbs abs ->
    let* frame = get_frame in
    return (VExprAbs (VCode { abs; frame }))
  | ExprApp app ->
    eval_expr_app app
  | ExprTypeAbs abs ->
    eval_abs_type abs
  | ExprTypeApp app ->
    eval_app_type app

and eval_unit unit =
  let _ = unit.pos in
  return VUnit

and eval_bool bool =
  return (VBool bool.value)

and eval_int int =
  return (VInt int.value)

and eval_string string =
  return (VString string.value)

and eval_bind bind context =
  let bind = Option.get !(bind.bind) in
  match BindMap.find_opt bind context.defs with
  | Some def -> eval def.expr (new_empty context.defs context.out)
  | None ->
  get_bind bind context.frame

and eval_tuple expr =
  let* value = eval expr in
  match value with
  | VTuple values -> return values
  | _ -> RuntimeErrors.raise_value ()

and eval_record expr =
  let* value = eval expr in
  match value with
  | VRecord attrs -> return attrs
  | _ -> RuntimeErrors.raise_value ()

and eval_expr_app app context =
  let value = eval app.abs context in
  match value with
  | VExprAbs abs -> eval_expr_app_abs abs app.arg context
  | _ -> RuntimeErrors.raise_value ()

and eval_expr_app_print arg context =
  let value = eval arg context in
  let string = RuntimeDisplay.display value in
  let _ = context.out string in
  VUnit

and eval_expr_app_abs abs arg context =
  let value = eval arg context in
  match abs with
  | VPrim prim ->
    prim { value; out = context.out }
  | VCode abs ->
    let binds = BindMap.singleton abs.abs.param.bind value in
    let context = new_frame context abs.frame binds in
    eval abs.abs.body context

and eval_abs_type abs =
  eval abs.body

and eval_app_type app =
  eval app.abs

let eval_def def defs stdout =
  let defs = List.map (fun def -> def.bind, def) defs in
  let defs = BindMap.of_list defs in
  eval def.expr (new_empty defs stdout)
