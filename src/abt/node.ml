type span = Code.span

(* BINDS *)

type bind_type = {
  name: string;
}

type bind_expr = {
  id: int;
  name: string;
}

(* ABT *)

type abt = {
  types: def_type list;
  exprs: def_expr list;
}

(* DEFINITIONS *)

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

(* TYPES *)

and type' =
  | TypeTop    of type_top
  | TypeBot    of type_bot
  | TypeUnit   of type_unit
  | TypeBool   of type_bool
  | TypeInt    of type_int
  | TypeString of type_string
  | TypeVar    of type_var
  | TypeTuple  of type_tuple
  | TypeRecord of type_record
  | TypeLam    of type_lam
  | TypeUniv   of type_univ
  | TypeAbs    of type_abs
  | TypeApp    of type_app
  | TypeUnion  of type_union
  | TypeInter  of type_inter

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
  attrs: attr_type Util.NameMap.t;
}

and type_lam = {
  span: span;
  param: type';
  ret: type';
}

and type_univ = {
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

and type_union = {
  span: span;
  left: type';
  right: type';
}

and type_inter = {
  span: span;
  left: type';
  right: type';
}

(* TYPE AUXILIARIES *)

and attr_type = {
  span: span;
  label: string;
  type': type';
}

and param_type = {
  bind: bind_type;
  interval: interval;
}

and interval = {
  span: span;
  lower: type' option;
  upper: type' option;
}

(* EXPRESSIONS *)

and expr =
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
  | ExprLamAbs  of expr_lam_abs
  | ExprLamApp  of expr_lam_app
  | ExprUnivAbs of expr_univ_abs
  | ExprUnivApp of expr_univ_app

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
  bind: bind_expr;
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

and expr_lam_abs = {
  span: span;
  param: param_expr;
  body: expr;
}

and expr_lam_app = {
  span: span;
  abs: expr;
  arg: expr;
}

and expr_univ_abs = {
  span: span;
  param: param_type;
  body: expr;
}

and expr_univ_app = {
  span: span;
  abs: expr;
  arg: type';
}

(* EXPRESSION AUXILIARIES *)

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
