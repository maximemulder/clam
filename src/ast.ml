type pos = Lexing.position

type program = {
  program_defs: def list
}

and def =
  | DefType of def_type
  | DefExpr of def_expr

and def_type = {
  type_pos: pos;
  type_name: string;
  type': type';
}

and def_expr = {
  expr_pos: pos;
  expr_name: string;
  expr_type: type' option;
  expr: expr;
}

and type' =
  | TypeIdent       of string
  | TypeAbsExpr     of (type' list) * type'
  | TypeAbsExprType of (param list) * type'
  | TypeTuple       of type' list
  | TypeRecord      of attr_type list
  | TypeInter       of type' * type'
  | TypeUnion       of type' * type'
  | TypeAbs         of (param list) * type'
  | TypeApp         of type' * (type' list)

and expr = pos * expr_data

and expr_data =
  | ExprIdent   of string
  | ExprVoid
  | ExprTrue
  | ExprFalse
  | ExprInt     of string
  | ExprChar    of string
  | ExprString  of string
  | ExprTuple   of expr list
  | ExprRecord  of attr_expr list
  | ExprPreop   of string * expr
  | ExprBinop   of expr * string * expr
  | ExprAscr    of expr * type'
  | ExprBlock   of block
  | ExprIf      of expr * expr * expr
  | ExprAbs     of (param list) * (type' option) * expr
  | ExprApp     of expr * (expr list)
  | ExprTypeAbs of (param list) * expr
  | ExprTypeApp of expr * (type' list)

and param = {
  param_pos: pos;
  param_name: string;
  param_type: type' option;
}

and attr_type = {
  attr_type_pos: pos;
  attr_type_name: string;
  attr_type: type';
}

and attr_expr = {
  attr_expr_name: string;
  attr_expr: expr;
}

and block = {
  block_defs: def list;
  block_expr: expr;
}

let get_program_types program =
  List.filter_map (fun(def) -> match def with
    | DefType def_type -> Some def_type
    | DefExpr _        -> None
  ) program.program_defs

let get_program_exprs program =
  List.filter_map (fun(def) -> match def with
    | DefType _        -> None
    | DefExpr def_expr -> Some def_expr
  ) program.program_defs

let get_block_types block =
  List.filter_map (fun(def) -> match def with
    | DefType def_type -> Some def_type
    | DefExpr _        -> None
  ) block.block_defs

let get_block_exprs block =
  List.filter_map (fun(def) -> match def with
    | DefType _        -> None
    | DefExpr def_expr -> Some def_expr
  ) block.block_defs
