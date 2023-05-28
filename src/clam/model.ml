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

type expr = pos * expr_data

and expr_data =
  | ExprVoid
  | ExprBool    of bool
  | ExprInt     of int
  | ExprChar    of char
  | ExprString  of string
  | ExprBind    of expr_bind
  | ExprTuple   of expr list
  | ExprRecord  of attr_expr list
  | ExprElem    of expr_elem
  | ExprAttr    of expr_attr
  | ExprPreop   of expr_preop
  | ExprBinop   of expr_binop
  | ExprAscr    of expr_ascr
  | ExprBlock   of expr_block
  | ExprIf      of expr_if
  | ExprAbs     of expr_abs
  | ExprApp     of expr * (expr list)
  | ExprTypeAbs of (param_type list) * expr
  | ExprTypeApp of expr * (type' list)

and expr_bind = {
  mutable bind_expr: bind_expr option;
}

and expr_elem = {
  expr_elem_expr: expr;
  expr_elem_index: int;
}

and expr_attr = {
  expr_attr_expr: expr;
  expr_attr_name: string;
}

and expr_preop = {
  expr_preop_op: string;
  expr_preop_expr: expr;
}

and expr_binop = {
  expr_binop_left: expr;
  expr_binop_op: string;
  expr_binop_right: expr;
}

and expr_ascr = {
  expr_ascr_expr: expr;
  expr_ascr_type: type';
}

and expr_block = {
  expr_block_stmts: stmt list;
  expr_block_expr: expr option;
}

and expr_if = {
  expr_if_cond: expr;
  expr_if_then: expr;
  expr_if_else: expr;
}

and expr_abs = {
  expr_abs_params: param_expr list;
  expr_abs_ret: type' option;
  expr_abs_body: expr;
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
  | BindExprDef   of def_expr
  | BindExprParam of param_expr
  | BindExprPrint
  | BindExprVar   of var_expr

and param_expr = {
  param_expr_pos: pos;
  param_expr_id: int;
  param_expr_name: string;
  param_expr_type: type' option;
}

and attr_expr = {
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

let make_attr_expr name expr =
  {
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
  | BindExprDef   def   -> def.def_expr_id
  | BindExprParam param -> param.param_expr_id
  | BindExprPrint       -> -1
  | BindExprVar   var   -> var.var_expr_id
