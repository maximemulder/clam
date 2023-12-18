type pos = Lexing.position

type type' = union

and union = {
  union: inter list;
}

and inter = {
  inter: base list;
}

and base =
  | Top         of top
  | Bot         of bot
  | Unit        of unit
  | Bool        of bool
  | Int         of int
  | Char        of char
  | String      of string
  | Var         of var
  | Tuple       of tuple
  | Record      of record
  | AbsExpr     of abs_expr
  | AbsTypeExpr of abs_type_expr
  | Abs         of abs
  | App         of app

and top = {
  pos: pos;
}

and bot = {
  pos: pos;
}

and unit = {
  pos: pos;
}

and bool = {
  pos: pos;
}

and int = {
  pos: pos;
}

and char = {
  pos: pos;
}

and string = {
  pos: pos;
}

and var = {
  pos: pos;
  bind: Model.bind_type;
}

and tuple = {
  pos: pos;
  elems: type' list;
}

and record = {
  pos: pos;
  attrs: attr Utils.NameMap.t;
}

and abs_expr = {
  pos: pos;
  param: type';
  ret: type';
}

and app_expr = {
  pos: pos;
  abs: type';
  arg: type';
}

and abs_type_expr = {
  pos: pos;
  param: param;
  ret: type';
}

and abs = {
  pos: pos;
  param: param;
  body: type';
}

and app = {
  pos: pos;
  abs: type';
  arg: type';
}

and attr = {
  pos: pos;
  name: String.t;
  type': type';
}

and param = {
  bind: Model.bind_type;
  bound: type';
}

let pos type' =
  match type' with
  | Top         type' -> type'.pos
  | Bot         type' -> type'.pos
  | Unit        type' -> type'.pos
  | Bool        type' -> type'.pos
  | Int         type' -> type'.pos
  | Char        type' -> type'.pos
  | String      type' -> type'.pos
  | Var         type' -> type'.pos
  | Tuple       type' -> type'.pos
  | Record      type' -> type'.pos
  | AbsExpr     type' -> type'.pos
  | AbsTypeExpr type' -> type'.pos
  | Abs         type' -> type'.pos
  | App         type' -> type'.pos

let pos_type type' = pos (List.nth (List.nth type'.union 0).inter 0)

let base base = { union = [{ inter = [base] }] }
