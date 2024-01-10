type type' = union

and union = {
  union: inter list;
}

and inter = {
  inter: base list;
}

and base =
  | Top
  | Bot
  | Unit
  | Bool
  | Int
  | String
  | Var         of var
  | Tuple       of tuple
  | Record      of record
  | AbsExpr     of abs_expr
  | AbsTypeExpr of abs_type_expr
  | Abs         of abs
  | App         of app

and var = {
  bind: Abt.bind_type;
}

and tuple = {
  elems: type' list;
}

and record = {
  attrs: attr Utils.NameMap.t;
}

and abs_expr = {
  param: type';
  ret: type';
}

and abs_type_expr = {
  param: param;
  ret: type';
}

and abs = {
  param: param;
  body: type';
}

and app = {
  abs: type';
  arg: type';
}

and attr = {
  name: String.t;
  type': type';
}

and param = {
  bind: Abt.bind_type;
  bound: type';
}

let base base = { union = [{ inter = [base] }] }

let top    = base Top
let bot    = base Bot
let unit   = base Unit
let bool   = base Bool
let int    = base Int
let string = base String
let var bind = base (Var { bind })
let tuple elems = base (Tuple { elems })
let record attrs = base (Record { attrs })
let abs_expr param ret = base (AbsExpr { param; ret })
let abs_type_expr param ret = base (AbsTypeExpr { param; ret })
let abs param body = base (Abs { param; body })
let app abs arg = base (App { abs; arg })
