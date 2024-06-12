open Bind
open Type

type span = Code.span

type expr =
  | Unit    of unit'
  | Bool    of bool'
  | Int     of int'
  | String  of string'
  | Bind    of bind
  | Tuple   of tuple
  | Record  of record
  | Elem    of elem
  | Attr    of attr
  | Ascr    of ascr
  | If      of if'
  | LamAbs  of lam_abs
  | LamApp  of lam_app
  | UnivAbs of univ_abs
  | UnivApp of univ_app

and unit' = {
  span: span;
}

and bool' = {
  span: span;
  value: bool;
}

and int' = {
  span: span;
  value: int;
}

and string' = {
  span: span;
  value: string;
}

and bind = {
  span: span;
  bind: bind_expr;
}

and tuple = {
  span: span;
  elems: expr list;
}

and record = {
  span: span;
  attrs: attr_expr list;
}

and elem = {
  span: span;
  tuple: expr;
  index: int;
}

and attr = {
  span: span;
  record: expr;
  label: string;
}

and ascr = {
  span: span;
  expr: expr;
  type': type';
}

and if' = {
  span: span;
  cond: expr;
  then': expr;
  else': expr;
}

and lam_abs = {
  span: span;
  param: param_expr;
  body: expr;
}

and lam_app = {
  span: span;
  abs: expr;
  arg: expr;
}

and univ_abs = {
  span: span;
  param: param_type;
  body: expr;
}

and univ_app = {
  span: span;
  abs: expr;
  arg: type';
}

(* AUXILIARIES *)

and attr_expr = {
  span: span;
  label: string;
  expr: expr;
}

and param_expr = {
  span: span;
  bind: bind_expr;
  type': type' option;
}
