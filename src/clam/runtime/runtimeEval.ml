open Model
open Utils
open RuntimeValue

module BindKey = struct
  type t = Model.bind_expr

  let compare x y  = Stdlib.compare (Model.bind_expr_id x) (Model.bind_expr_id y)
end

module BindMap = Map.Make(BindKey)

type stack = {
  parent: stack option;
  binds: value BindMap.t;
}

type writer = string -> unit

type context = {
  out_handler: writer;
  stack: stack;
}

let new_empty out_handler =
  { out_handler; stack = { parent = None; binds = BindMap.empty } }

let new_frame context binds =
  { context with stack = { parent = Some context.stack; binds } }

let rec get_param param stack =
  match BindMap.find_opt (Model.BindExprParam param) stack.binds with
  | Some value -> value
  | None -> get_param param (Option.get stack.parent)

let rec get_var var stack =
  match BindMap.find_opt (Model.BindExprVar var) stack.binds with
  | Some value -> value
  | None -> get_var var (Option.get stack.parent)

module Reader = struct
  type r = context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec eval (expr: Model.expr) =
  match expr with
  | ExprUnit unit ->
    eval_unit unit
  | ExprBool bool ->
    eval_bool bool
  | ExprInt int ->
    eval_int int
  | ExprChar char ->
    eval_char char
  | ExprString string ->
    eval_string string
  | ExprBind bind ->
    eval_bind bind
  | ExprTuple tuple ->
    let* values = map_list eval tuple.expr_tuple_exprs in
    return (VTuple values)
  | ExprRecord record ->
    let* attrs = fold_list (fun map attr ->
      let* value = (eval attr.Model.attr_expr) in
      return (NameMap.add attr.Model.attr_expr_name value map)
    ) NameMap.empty record.expr_record_attrs in
    return (VRecord attrs)
  | ExprElem elem ->
    let* values = eval_tuple elem.expr_elem_expr in
    return (List.nth values elem.expr_elem_index)
  | ExprAttr attr ->
    let* attrs = eval_record attr.expr_attr_expr in
    return (NameMap.find attr.expr_attr_name attrs)
  | ExprPreop preop ->
    eval_preop preop
  | ExprBinop binop ->
    eval_binop binop
  | ExprAscr ascr ->
    eval ascr.expr_ascr_expr
  | ExprBlock block ->
    eval_expr_block block
  | ExprIf if' ->
    let* cond = eval_value_bool if'.expr_if_cond in
    if cond then eval if'.expr_if_then else eval if'.expr_if_else
  | ExprAbs abs ->
    return (VExprAbs abs)
  | ExprApp app ->
    eval_expr_app app
  | ExprTypeAbs abs ->
    return (VTypeAbs abs)
  | ExprTypeApp app ->
    eval_type_app app.expr_type_app_expr

and eval_unit unit =
  let _ = unit.expr_unit_pos in
  return VUnit

and eval_bool bool =
  return (VBool bool.expr_bool)

and eval_int int =
  return (VInt int.expr_int)

and eval_char char =
  return (VChar char.expr_char)

and eval_string string =
  return (VString string.expr_string)

and eval_bind bind context =
  match (Option.get !(bind.expr_bind)) with
  | BindExprDef def -> eval def.def_expr (new_empty context.out_handler)
  | BindExprParam param -> get_param param context.stack
  | BindExprPrint -> VPrint
  | BindExprVar var -> get_var var context.stack

and eval_expr_app app context =
  let value = eval app.expr_app_expr context in
  let args = app.expr_app_args in
  match value with
  | VPrint -> eval_expr_app_print args context
  | VExprAbs abs -> eval_expr_app_abs abs args context
  | _ -> RuntimeErrors.raise_value ()

and eval_expr_app_print args context =
  let value = eval (List.nth args 0) context in
  let string = RuntimeDisplay.display value in
  let _ = context.out_handler string in
  VUnit

and eval_expr_app_abs abs args context =
  let args = map_list eval args context in
  let pairs = List.combine abs.expr_abs_params args in
  let binds = List.fold_left (fun map (param, value) -> BindMap.add (Model.BindExprParam param) value map) BindMap.empty pairs in
  let context = new_frame context binds in
  eval abs.expr_abs_body context

and eval_type_app expr =
  let* value = eval expr in
  match value with
  | VTypeAbs app -> eval app.expr_type_abs_body
  | _ -> RuntimeErrors.raise_value ()

and eval_expr_block block =
  eval_block_stmts block.expr_block_stmts block.expr_block_expr

and eval_block_stmts stmts expr context =
  match stmts with
  | [] ->
    eval_block_expr expr context
  | stmt :: stmts ->
    let context = eval_block_stmt stmt context in
    eval_block_stmts stmts expr context

and eval_block_stmt stmt context =
  match stmt with
  | StmtVar (var, _, expr) ->
    let value = eval expr context in
    new_frame context (BindMap.singleton (BindExprVar var) value)
  | StmtExpr expr ->
    let _ = eval expr context in
    context

and eval_block_expr expr =
  match expr with
  | Some expr -> eval expr
  | None -> return VUnit

and eval_preop preop =
  let expr = preop.expr_preop_expr in
  match preop.expr_preop_op with
  | "+" ->
    let* value = eval_value_int expr in
    return (VInt value)
  | "-" ->
    let* value = eval_value_int expr in
    return (VInt ~- value)
  | "!" ->
    let* value = eval_value_bool expr in
    return (VBool (not value))
  | _ -> RuntimeErrors.raise_operator preop.expr_preop_op

and eval_binop binop =
  let left = binop.expr_binop_left in
  let right = binop.expr_binop_right in
  match binop.expr_binop_op with
  | "+"  ->
    let* left = eval_value_int left in
    let* right = eval_value_int right in
    return (VInt (left + right))
  | "-"  ->
    let* left = eval_value_int left in
    let* right = eval_value_int right in
    return (VInt (left - right))
  | "*"  ->
    let* left = eval_value_int left in
    let* right = eval_value_int right in
    return (VInt (left * right))
  | "/"  ->
    let* left = eval_value_int left in
    let* right = eval_value_int right in
    return (VInt (left / right))
  | "%"  ->
    let* left = eval_value_int left in
    let* right = eval_value_int right in
    return (VInt (left mod right))
  | "++" ->
    let* left = eval_value_string left in
    let* right = eval_value_string right in
    return (VString (left ^ right))
  | "==" ->
    let* left = eval left in
    let* right = eval right in
    return (VBool (RuntimeValue.compare left right))
  | "!=" ->
    let* left = eval left in
    let* right = eval right in
    return (VBool (Bool.not (RuntimeValue.compare left right)))
  | "<" ->
    let* left = eval_value_int left in
    let* right = eval_value_int right in
    return (VBool (left < right))
  | ">" ->
    let* left = eval_value_int left in
    let* right = eval_value_int right in
    return (VBool (left > right))
  | "<=" ->
    let* left = eval_value_int left in
    let* right = eval_value_int right in
    return (VBool (left <= right))
  | ">=" ->
    let* left = eval_value_int left in
    let* right = eval_value_int right in
    return (VBool (left >= right))
  | "|" ->
    let* left = eval_value_bool left in
    let* right = eval_value_bool right in
    return (VBool (left || right))
  | "&" ->
    let* left = eval_value_bool left in
    let* right = eval_value_bool right in
    return (VBool (left && right))
  | _ -> RuntimeErrors.raise_operator binop.expr_binop_op

and eval_value_bool (expr: Model.expr) =
  let* value = eval expr in
  match value with
  | VBool bool -> return bool
  | _ -> RuntimeErrors.raise_value ()

and eval_value_int (expr: Model.expr) =
  let* value = eval expr in
  match value with
  | VInt int -> return int
  | _ -> RuntimeErrors.raise_value ()

and eval_value_string (expr: Model.expr) =
  let* value = eval expr in
  match value with
  | VString string -> return string
  | _ -> RuntimeErrors.raise_value ()

and eval_tuple (expr: Model.expr) =
  let* value = eval expr in
  match value with
  | VTuple values -> return values
  | _ -> RuntimeErrors.raise_value ()

and eval_record (expr: Model.expr) =
  let* value = eval expr in
  match value with
  | VRecord attrs -> return attrs
  | _ -> RuntimeErrors.raise_value ()

let eval_def def stdout =
  eval def.Model.def_expr (new_empty stdout)

