open Collection

type type' =
  | TypeVar of type_param
  | TypeAny
  | TypeVoid
  | TypeBool
  | TypeInt
  | TypeChar
  | TypeString
  | TypeAbsExpr     of (type' list) * type'
  | TypeAbsExprType of (type_param list) * type'
  | TypeTuple       of type' list
  | TypeRecord      of attr_type NameMap.t
  | TypeInter       of type' * type'
  | TypeUnion       of type' * type'
  | TypeAbs         of (type_param list) * type'
  | TypeApp         of type' * (type' list)

and attr_type = {
  attr_type_name: string;
  attr_type: type';
}

and type_param = {
  type_param_name: string;
  type_param_type: type';
}

type expr =
  | ExprVoid
  | ExprBool    of bool
  | ExprInt     of int
  | ExprChar    of char
  | ExprString  of string
  | ExprBind    of expr_bind
  | ExprTuple   of expr list
  | ExprRecord  of attr_expr list
  | ExprPreop   of string * expr
  | ExprBinop   of expr * string * expr
  | ExprAscr    of expr * type'
  | ExprBlock   of expr_block
  | ExprIf      of expr * expr * expr
  | ExprAbs     of (param_expr list) * (type' option) * expr
  | ExprApp     of expr * (expr list)
  | ExprTypeAbs of (type_param list) * expr
  | ExprTypeApp of expr * (type' list)

and expr_block = {
  block_expr: expr;
}

and expr_bind = {
  mutable bind_expr: bind_expr option;
}

and def_expr = {
  def_expr_name: string;
  def_expr_type: type' option;
  def_expr: expr;
}

and param_expr = {
  param_expr_name: string;
  param_expr_type: type' option;
}

and bind_expr =
  | BindExprDef   of def_expr
  | BindExprParam of param_expr

and attr_expr = {
  attr_expr_name: string;
  attr_expr: expr;
}

let make_def_expr name type' expr =
  {
    def_expr_name = name;
    def_expr_type = type';
    def_expr = expr;
  }

let make_attr_expr name expr =
  {
    attr_expr_name = name;
    attr_expr = expr;
  }

let make_attr_type name type' =
  {
    attr_type_name = name;
    attr_type = type';
  }
