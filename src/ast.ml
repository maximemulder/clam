type program = {
  program_defs: def list
}

and def =
  | DefType of def_type
  | DefExpr of def_expr

and def_type = {
  type_name: string;
  type': type';
}

and def_expr = {
  expr_name: string;
  expr: expr;
}

and type' =
  | TypeIdent  of string
  | TypeFun    of (type' list) * type'
  | TypeTuple  of type' list
  | TypeRecord of (string * type') list
  | TypeInter  of type' * type'
  | TypeUnion  of type' * type'
  | TypeAbs    of ((string * type') list) * type'
  | TypeApp    of type' * (type' list)

and expr =
  | ExprIdent      of string
  | ExprVoid
  | ExprBool       of bool
  | ExprInt        of int
  | ExprChar       of char
  | ExprString     of string
  | ExprTuple      of expr list
  | ExprRecord     of (string * expr) list
  | ExprPreop      of string * expr
  | ExprBinop      of expr * string * expr
  | ExprCast       of expr * type'
  | ExprAscription of expr * type'
  | ExprBlock      of block
  | ExprIf         of expr * expr * expr
  | ExprAbs        of ((string * type') list) * (type' option) * block
  | ExprApp        of expr * (expr list)
  | ExprTypeAbs    of ((string * type') list) * expr
  | ExprTypeApp    of expr * (type' list)

and block = {
  block_defs: def list;
  block_expr: expr;
}
