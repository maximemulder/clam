type type' =
  | TypeVar of param
  | TypeAny
  | TypeVoid
  | TypeInt
  | TypeChar
  | TypeString
  | TypeAbsExpr     of (type' list) * type'
  | TypeAbsExprType of (param list) * type'
  | TypeTuple       of type' list
  | TypeRecord      of attr_type list
  | TypeInter       of type' * type'
  | TypeUnion       of type' * type'
  | TypeAbs         of (param list) * type'
  | TypeApp         of type' * (type' list)

and attr_type = {
  attr_type_name: string;
  attr_type: type';
}

and param = {
  param_name: string;
  param_type: type';
}

type expr =
  | ExprVoid
  | ExprBool    of bool
  | ExprInt     of int
  | ExprChar    of char
  | ExprString  of string
  | ExprParam   of string
  | ExprBind    of bind
  | ExprTuple   of expr list
  | ExprRecord  of attr_expr list
  | ExprPreop   of string * expr
  | ExprBinop   of expr * string * expr
  | ExprAscr    of expr * type'
  | ExprBlock   of block
  | ExprIf      of expr * expr * expr
  | ExprAbs     of (param list) * (type' option) * block
  | ExprApp     of expr * (expr list)
  | ExprTypeAbs of (param list) * expr
  | ExprTypeApp of expr * (type' list)

and attr_expr = {
  attr_expr_name: string;
  attr_expr: expr;
}

and block = expr

and bind = {
  mutable expr: expr option;
}
