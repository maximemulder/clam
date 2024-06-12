open Bind
open Expr
open Type

type span = Code.span

type abt = {
  types: def_type list;
  exprs: def_expr list;
}

and def_type = {
  span: span;
  name: string;
  type': type';
}

and def_expr = {
  span: span;
  bind: bind_expr;
  type': type' option;
  expr: expr;
}
