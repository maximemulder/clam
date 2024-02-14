type span = Ast.span

type bind_type = {
  name: string;
}

type bind_expr = {
  id: int;
  name: string;
}

type type' =
  | TypeTop         of type_top
  | TypeBot         of type_bot
  | TypeUnit        of type_unit
  | TypeBool        of type_bool
  | TypeInt         of type_int
  | TypeString      of type_string
  | TypeVar         of type_var
  | TypeTuple       of type_tuple
  | TypeRecord      of type_record
  | TypeInter       of type_inter
  | TypeUnion       of type_union
  | TypeAbsExpr     of type_abs_expr
  | TypeAbsExprType of type_abs_expr_type
  | TypeAbs         of type_abs
  | TypeApp         of type_app

and type_top = {
  span: span;
}

and type_bot = {
  span: span;
}

and type_unit = {
  span: span;
}

and type_bool = {
  span: span;
}

and type_int = {
  span: span;
}

and type_string = {
  span: span;
}

and type_var = {
  span: span;
  bind: bind_type;
}

and type_tuple = {
  span: span;
  elems: type' list;
}

and type_record = {
  span: span;
  attrs: attr_type Utils.NameMap.t;
}

and type_inter = {
  span: span;
  left: type';
  right: type';
}

and type_union = {
  span: span;
  left: type';
  right: type';
}

and type_abs_expr = {
  span: span;
  param: type';
  ret: type';
}

and type_abs_expr_type = {
  span: span;
  param: param_type;
  ret: type';
}

and type_abs = {
  span: span;
  param: param_type;
  body: type';
}

and type_app = {
  span: span;
  abs: type';
  arg: type';
}

and attr_type = {
  span: span;
  name: string;
  type': type';
}

and param_type = {
  bind: bind_type;
  bound: type';
}

let type_span type' =
  match type' with
  | TypeTop         type' -> type'.span
  | TypeBot         type' -> type'.span
  | TypeUnit        type' -> type'.span
  | TypeBool        type' -> type'.span
  | TypeInt         type' -> type'.span
  | TypeString      type' -> type'.span
  | TypeVar         type' -> type'.span
  | TypeTuple       type' -> type'.span
  | TypeRecord      type' -> type'.span
  | TypeInter       type' -> type'.span
  | TypeUnion       type' -> type'.span
  | TypeAbsExpr     type' -> type'.span
  | TypeAbsExprType type' -> type'.span
  | TypeAbs         type' -> type'.span
  | TypeApp         type' -> type'.span

type expr =
  | ExprUnit    of expr_unit
  | ExprBool    of expr_bool
  | ExprInt     of expr_int
  | ExprString  of expr_string
  | ExprBind    of expr_bind
  | ExprTuple   of expr_tuple
  | ExprRecord  of expr_record
  | ExprElem    of expr_elem
  | ExprAttr    of expr_attr
  | ExprAscr    of expr_ascr
  | ExprIf      of expr_if
  | ExprAbs     of expr_abs
  | ExprApp     of expr_app
  | ExprTypeAbs of expr_type_abs
  | ExprTypeApp of expr_type_app

and expr_unit = {
  span: span;
}

and expr_bool = {
  span: span;
  value: bool;
}

and expr_int = {
  span: span;
  value: int;
}

and expr_string = {
  span: span;
  value: string;
}

and expr_bind = {
  span: span;
  bind: bind_expr option ref;
}

and expr_tuple = {
  span: span;
  elems: expr list;
}

and expr_record = {
  span: span;
  attrs: attr_expr list;
}

and expr_elem = {
  span: span;
  tuple: expr;
  index: int;
}

and expr_attr = {
  span: span;
  record: expr;
  label: string;
}

and expr_ascr = {
  span: span;
  expr: expr;
  type': type';
}

and expr_if = {
  span: span;
  cond: expr;
  then': expr;
  else': expr;
}

and expr_abs = {
  span: span;
  param: param_expr;
  body: expr;
}

and expr_app = {
  span: span;
  abs: expr;
  arg: expr;
}

and expr_type_abs = {
  span: span;
  param: param_type;
  body: expr;
}

and expr_type_app = {
  span: span;
  abs: expr;
  arg: type';
}

and attr_expr = {
  span: span;
  name: string;
  expr: expr;
}

and param_expr = {
  span: span;
  bind: bind_expr;
  type': type' option;
}

and def_expr = {
  span: span;
  bind: bind_expr;
  type': type' option;
  expr: expr;
}

let expr_span expr =
  match expr with
  | ExprUnit    expr -> expr.span
  | ExprBool    expr -> expr.span
  | ExprInt     expr -> expr.span
  | ExprString  expr -> expr.span
  | ExprBind    expr -> expr.span
  | ExprTuple   expr -> expr.span
  | ExprRecord  expr -> expr.span
  | ExprElem    expr -> expr.span
  | ExprAttr    expr -> expr.span
  | ExprAscr    expr -> expr.span
  | ExprIf      expr -> expr.span
  | ExprAbs     expr -> expr.span
  | ExprApp     expr -> expr.span
  | ExprTypeAbs expr -> expr.span
  | ExprTypeApp expr -> expr.span
