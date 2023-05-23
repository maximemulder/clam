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
  match snd expr with
  | ExprVoid ->
    return VVoid
  | ExprBool bool ->
    return (VBool bool)
  | ExprInt int ->
    return (VInt int)
  | ExprChar char ->
    return (VChar char)
  | ExprString string ->
    return (VString string)
  | ExprBind bind ->
    eval_bind (Option.get bind.Model.bind_expr)
  | ExprTuple exprs ->
    let* values = list_map eval exprs in
    return (VTuple values)
  | ExprRecord attrs ->
    let* attrs = list_fold (fun map attr ->
      let* value = (eval attr.Model.attr_expr) in
      return (NameMap.add attr.Model.attr_expr_name value map)
    ) NameMap.empty attrs in
    return (VRecord attrs)
  | ExprVariant (expr, index) ->
    let* values = eval_tuple expr in
    return (List.nth values index)
  | ExprAttr (expr, attr) ->
    let* attrs = eval_record expr in
    return (NameMap.find attr attrs)
  | ExprPreop (op, expr) ->
    eval_preop op expr
  | ExprBinop (left, op, right) ->
    eval_binop left op right
  | ExprAscr (expr, _) ->
    eval expr
  | ExprBlock block ->
    eval_expr_block block
  | ExprIf (cond, then', else') ->
    let* cond = eval_bool cond in
    if cond then eval then' else eval else'
  | ExprAbs (params, _, expr) ->
    return (VExprAbs (params, expr))
  | ExprApp (expr, args) -> eval_expr_app expr args
  | ExprTypeAbs (params, expr) ->
    return (VTypeAbs (params, expr))
  | ExprTypeApp (expr, _) ->
    eval expr

and eval_bind bind context =
  match bind with
  | Model.BindExprDef def -> eval def.Model.def_expr (new_empty context.out_handler)
  | Model.BindExprParam param -> get_param param context.stack
  | Model.BindExprPrint -> VPrint
  | Model.BindExprVar var -> get_var var context.stack

and eval_preop op expr =
  match op with
  | "+" ->
    let* value = eval_int expr in
    return (VInt value)
  | "-" ->
    let* value = eval_int expr in
    return (VInt ~- value)
  | "!" ->
    let* value = eval_bool expr in
    return (VBool (not value))
  | _ -> RuntimeErrors.raise_operator op

and eval_binop left op right =
  match op with
  | "+"  ->
    let* left = eval_int left in
    let* right = eval_int right in
    return (VInt (left + right))
  | "-"  ->
    let* left = eval_int left in
    let* right = eval_int right in
    return (VInt (left - right))
  | "*"  ->
    let* left = eval_int left in
    let* right = eval_int right in
    return (VInt (left * right))
  | "/"  ->
    let* left = eval_int left in
    let* right = eval_int right in
    return (VInt (left / right))
  | "%"  ->
    let* left = eval_int left in
    let* right = eval_int right in
    return (VInt (left mod right))
  | "++" ->
    let* left = eval_string left in
    let* right = eval_string right in
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
    let* left = eval_int left in
    let* right = eval_int right in
    return (VBool (left < right))
  | ">" ->
    let* left = eval_int left in
    let* right = eval_int right in
    return (VBool (left > right))
  | "<=" ->
    let* left = eval_int left in
    let* right = eval_int right in
    return (VBool (left <= right))
  | ">=" ->
    let* left = eval_int left in
    let* right = eval_int right in
    return (VBool (left >= right))
  | "|" ->
    let* left = eval_bool left in
    let* right = eval_bool right in
    return (VBool (left || right))
  | "&" ->
    let* left = eval_bool left in
    let* right = eval_bool right in
    return (VBool (left && right))
  | _ -> RuntimeErrors.raise_operator op

and eval_expr_app expr args context =
  let value = eval expr context in
  match value with
  | VPrint -> eval_expr_app_print args context
  | VExprAbs (params, body) -> eval_expr_app_abs params args body context
  | _ -> RuntimeErrors.raise_value ()

and eval_expr_app_print args context =
  let value = eval (List.nth args 0) context in
  let string = RuntimeDisplay.display value in
  let _ = context.out_handler string in
  VVoid

and eval_expr_app_abs params args body context =
  let args = list_map eval args context in
  let pairs = List.combine params args in
  let binds = List.fold_left (fun map (param, value) -> BindMap.add (Model.BindExprParam param) value map) BindMap.empty pairs in
  let context = new_frame context binds in
  eval body context

and eval_expr_block block =
  eval_expr_stmt block.block_stmts block.block_expr

and eval_expr_stmt stmts expr context =
  match stmts with
  | [] ->
    (match expr with
    | Some expr -> eval expr context
    | None -> VVoid)
  | stmt :: stmts ->
    let context = (match stmt with
    | StmtVar (var, _, expr) ->
      let value = eval expr context in
      new_frame context (BindMap.singleton (BindExprVar var) value)
    | StmtExpr expr ->
      let _ = eval expr context in
      context
      ) in
    eval_expr_stmt stmts expr context

and eval_bool (expr: Model.expr) =
  let* value = eval expr in
  match value with
  | VBool bool -> return bool
  | _ -> RuntimeErrors.raise_value ()

and eval_int (expr: Model.expr) =
  let* value = eval expr in
  match value with
  | VInt int -> return int
  | _ -> RuntimeErrors.raise_value ()

and eval_string (expr: Model.expr) =
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

