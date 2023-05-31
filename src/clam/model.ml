open Utils

type pos = Lexing.position

type type' = pos * type_data

and type_data =
  | TypeAny
  | TypeVoid
  | TypeBool
  | TypeInt
  | TypeChar
  | TypeString
  | TypeVar         of param_type
  | TypeAbsExpr     of (type' list) * type'
  | TypeAbsExprType of (param_type list) * type'
  | TypeTuple       of type' list
  | TypeRecord      of attr_type NameMap.t
  | TypeInter       of type' * type'
  | TypeUnion       of type' * type'
  | TypeAbs         of (param_type list) * type'
  | TypeApp         of type' * (type' list)

and attr_type = {
  attr_type_pos: pos;
  attr_type_name: string;
  attr_type: type';
}

and param_type = {
  param_type_name: string;
  param_type: type';
}

and expr =
  | ExprVoid    of expr_void
  | ExprBool    of expr_bool
  | ExprInt     of expr_int
  | ExprChar    of expr_char
  | ExprString  of expr_string
  | ExprBind    of expr_bind
  | ExprTuple   of expr_tuple
  | ExprRecord  of expr_record
  | ExprElem    of expr_elem
  | ExprAttr    of expr_attr
  | ExprPreop   of expr_preop
  | ExprBinop   of expr_binop
  | ExprAscr    of expr_ascr
  | ExprBlock   of expr_block
  | ExprIf      of expr_if
  | ExprAbs     of expr_abs
  | ExprApp     of expr_app
  | ExprTypeAbs of expr_type_abs
  | ExprTypeApp of expr_type_app

and expr_void = {
  expr_void_pos: pos;
}

and expr_bool = {
  expr_bool_pos: pos;
  expr_bool: bool;
}

and expr_int = {
  expr_int_pos: pos;
  expr_int: int;
}

and expr_char = {
  expr_char_pos: pos;
  expr_char: char;
}

and expr_string = {
  expr_string_pos: pos;
  expr_string: string;
}

and expr_bind = {
  expr_bind_pos: pos;
  expr_bind: bind_expr option ref;
}

and expr_tuple = {
  expr_tuple_pos: pos;
  expr_tuple_exprs: expr list;
}

and expr_record = {
  expr_record_pos: pos;
  expr_record_attrs: attr_expr list;
}

and expr_elem = {
  expr_elem_pos: pos;
  expr_elem_expr: expr;
  expr_elem_index: int;
}

and expr_attr = {
  expr_attr_pos: pos;
  expr_attr_expr: expr;
  expr_attr_name: string;
}

and expr_preop = {
  expr_preop_pos: pos;
  expr_preop_op: string;
  expr_preop_expr: expr;
}

and expr_binop = {
  expr_binop_pos: pos;
  expr_binop_left: expr;
  expr_binop_op: string;
  expr_binop_right: expr;
}

and expr_ascr = {
  expr_ascr_pos: pos;
  expr_ascr_expr: expr;
  expr_ascr_type: type';
}

and expr_block = {
  expr_block_pos: pos;
  expr_block_stmts: stmt list;
  expr_block_expr: expr option;
}

and expr_if = {
  expr_if_pos: pos;
  expr_if_cond: expr;
  expr_if_then: expr;
  expr_if_else: expr;
}

and expr_abs = {
  expr_abs_pos: pos;
  expr_abs_params: param_expr list;
  expr_abs_ret: type' option;
  expr_abs_body: expr;
}

and expr_app = {
  expr_app_pos: pos;
  expr_app_expr: expr;
  expr_app_args: expr list;
}

and expr_type_abs = {
  expr_type_abs_pos: pos;
  expr_type_abs_params: param_type list;
  expr_type_abs_body: expr;
}

and expr_type_app = {
  expr_type_app_pos: pos;
  expr_type_app_expr: expr;
  expr_type_app_args: type' list;
}

and stmt =
  | StmtVar  of var_expr * (type' option) * expr
  | StmtExpr of expr

and def_expr = {
  def_expr_pos: pos;
  def_expr_id: int;
  def_expr_name: string;
  def_expr_type: type' option;
  def_expr: expr;
}

and bind_expr =
  | BindExprPrint
  | BindExprDef   of def_expr
  | BindExprParam of param_expr
  | BindExprVar   of var_expr

and param_expr = {
  param_expr_pos: pos;
  param_expr_id: int;
  param_expr_name: string;
  param_expr_type: type' option;
}

and attr_expr = {
  attr_expr_pos: pos;
  attr_expr_name: string;
  attr_expr: expr;
}

and var_expr = {
  var_expr_id: int;
  var_expr_name: string;
}

let make_def_expr pos id name type' expr =
  {
    def_expr_pos = pos;
    def_expr_id = id;
    def_expr_name = name;
    def_expr_type = type';
    def_expr = expr;
  }

let make_param_expr pos id name type' =
  {
    param_expr_pos = pos;
    param_expr_id = id;
    param_expr_name = name;
    param_expr_type = type';
  }

let make_attr_expr pos name expr =
  {
    attr_expr_pos = pos;
    attr_expr_name = name;
    attr_expr = expr;
  }

let make_attr_type pos name type' =
  {
    attr_type_pos = pos;
    attr_type_name = name;
    attr_type = type';
  }

let bind_expr_id bind =
  match bind with
  | BindExprPrint       -> -1
  | BindExprDef   def   -> def.def_expr_id
  | BindExprParam param -> param.param_expr_id
  | BindExprVar   var   -> var.var_expr_id

let bind_expr_name bind =
  match bind with
  | BindExprPrint       -> "print"
  | BindExprDef   def   -> def.def_expr_name
  | BindExprParam param -> param.param_expr_name
  | BindExprVar   var   -> var.var_expr_name

let expr_pos expr =
  match expr with
  | ExprVoid expr ->
    expr.expr_void_pos
  | ExprBool expr ->
    expr.expr_bool_pos
  | ExprInt expr ->
    expr.expr_int_pos
  | ExprChar expr ->
    expr.expr_char_pos
  | ExprString expr ->
    expr.expr_string_pos
  | ExprBind expr ->
    expr.expr_bind_pos
  | ExprTuple expr ->
    expr.expr_tuple_pos
  | ExprRecord expr ->
    expr.expr_record_pos
  | ExprElem expr ->
    expr.expr_elem_pos
  | ExprAttr expr ->
    expr.expr_attr_pos
  | ExprPreop expr ->
    expr.expr_preop_pos
  | ExprBinop expr ->
    expr.expr_binop_pos
  | ExprAscr expr ->
    expr.expr_ascr_pos
  | ExprBlock expr ->
    expr.expr_block_pos
  | ExprIf expr ->
    expr.expr_if_pos
  | ExprAbs expr ->
    expr.expr_abs_pos
  | ExprApp expr ->
    expr.expr_app_pos
  | ExprTypeAbs expr ->
    expr.expr_type_abs_pos
  | ExprTypeApp expr ->
    expr.expr_type_app_pos
