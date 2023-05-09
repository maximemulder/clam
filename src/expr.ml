open Type

type expr =
  | ExprName    of string * expr
  | ExprVoid
  | ExprBool    of bool
  | ExprInt     of int
  | ExprChar    of char
  | ExprString  of string
  | ExprTuple   of expr list
  | ExprRecord  of attr list
  | ExprPreop   of string * expr
  | ExprBinop   of expr * string * expr
  | ExprCast    of expr * type'
  | ExprAscr    of expr * type'
  | ExprBlock   of block
  | ExprIf      of expr * expr * expr
  | ExprAbs     of (param list) * (type' option) * block
  | ExprApp     of expr * (expr list)
  | ExprTypeAbs of (param list) * expr
  | ExprTypeApp of expr * (type' list)

and attr = {
  attr_name: string;
  attr_expr: expr;
}

and block = expr
