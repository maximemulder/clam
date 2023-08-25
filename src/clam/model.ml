type pos = Lexing.position

type type' =
  | TypeTop         of type_top
  | TypeBot         of type_bot
  | TypeUnit        of type_unit
  | TypeBool        of type_bool
  | TypeInt         of type_int
  | TypeChar        of type_char
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
  pos: pos;
}

and type_bot = {
  pos: pos;
}

and type_unit = {
  pos: pos;
}

and type_bool = {
  pos: pos;
}

and type_int = {
  pos: pos;
}

and type_char = {
  pos: pos;
}

and type_string = {
  pos: pos;
}

and type_var = {
  pos: pos;
  param: param_type;
}

and type_tuple = {
  pos: pos;
  elems: type' list;
}

and type_record = {
  pos: pos;
  attrs: attr_type Utils.NameMap.t;
}

and type_inter = {
  pos: pos;
  left: type';
  right: type';
}

and type_union = {
  pos: pos;
  left: type';
  right: type';
}

and type_abs_expr = {
  pos: pos;
  params: type' list;
  body: type';
}

and type_abs_expr_type = {
  pos: pos;
  params: param_type list;
  body: type';
}

and type_abs = {
  pos: pos;
  params: param_type list;
  body: type';
}

and type_app = {
  pos: pos;
  type': type';
  args: type' list;
}

and attr_type = {
  pos: pos;
  name: string;
  type': type';
}

and param_type = {
  name: string;
  type': type';
}

let type_pos type' =
  match type' with
  | TypeTop         type' -> type'.pos
  | TypeBot         type' -> type'.pos
  | TypeUnit        type' -> type'.pos
  | TypeBool        type' -> type'.pos
  | TypeInt         type' -> type'.pos
  | TypeChar        type' -> type'.pos
  | TypeString      type' -> type'.pos
  | TypeVar         type' -> type'.pos
  | TypeTuple       type' -> type'.pos
  | TypeRecord      type' -> type'.pos
  | TypeInter       type' -> type'.pos
  | TypeUnion       type' -> type'.pos
  | TypeAbsExpr     type' -> type'.pos
  | TypeAbsExprType type' -> type'.pos
  | TypeAbs         type' -> type'.pos
  | TypeApp         type' -> type'.pos

let prim_pos = {
  Lexing.pos_fname = "primitives.clam";
  Lexing.pos_lnum = 0;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}

let prim_top    = TypeTop    { pos = prim_pos; }
let prim_bot    = TypeBot    { pos = prim_pos; }
let prim_unit   = TypeUnit   { pos = prim_pos; }
let prim_bool   = TypeBool   { pos = prim_pos; }
let prim_int    = TypeInt    { pos = prim_pos; }
let prim_char   = TypeChar   { pos = prim_pos; }
let prim_string = TypeString { pos = prim_pos; }

type expr =
  | ExprUnit    of expr_unit
  | ExprBool    of expr_bool
  | ExprInt     of expr_int
  | ExprChar    of expr_char
  | ExprString  of expr_string
  | ExprBind    of expr_bind
  | ExprTuple   of expr_tuple
  | ExprRecord  of expr_record
  | ExprElem    of expr_elem
  | ExprAttr    of expr_attr
  | ExprPreop   of expr_preop
  | ExprBinop   of expr_binop
  | ExprAscr    of expr_ascr
  | ExprIf      of expr_if
  | ExprAbs     of expr_abs
  | ExprApp     of expr_app
  | ExprTypeAbs of expr_type_abs
  | ExprTypeApp of expr_type_app
  | ExprStmt    of expr_stmt

and expr_unit = {
  pos: pos;
}

and expr_bool = {
  pos: pos;
  value: bool;
}

and expr_int = {
  pos: pos;
  value: int;
}

and expr_char = {
  pos: pos;
  value: char;
}

and expr_string = {
  pos: pos;
  value: string;
}

and expr_bind = {
  pos: pos;
  bind: bind_expr option ref;
}

and expr_tuple = {
  pos: pos;
  elems: expr list;
}

and expr_record = {
  pos: pos;
  attrs: attr_expr list;
}

and expr_elem = {
  pos: pos;
  expr: expr;
  index: int;
}

and expr_attr = {
  pos: pos;
  expr: expr;
  name: string;
}

and expr_preop = {
  pos: pos;
  op: string;
  expr: expr;
}

and expr_binop = {
  pos: pos;
  left: expr;
  op: string;
  right: expr;
}

and expr_ascr = {
  pos: pos;
  expr: expr;
  type': type';
}

and expr_if = {
  pos: pos;
  cond: expr;
  then': expr;
  else': expr;
}

and expr_abs = {
  pos: pos;
  params: param_expr list;
  body: expr;
}

and expr_app = {
  pos: pos;
  expr: expr;
  args: expr list;
}

and expr_type_abs = {
  pos: pos;
  params: param_type list;
  body: expr;
}

and expr_type_app = {
  pos: pos;
  expr: expr;
  args: type' list;
}

and expr_stmt = {
  pos: pos;
  stmt: stmt;
  expr: expr;
}

and attr_expr = {
  pos: pos;
  name: string;
  expr: expr;
}

and stmt =
  | StmtVar  of var_expr * (type' option) * expr
  | StmtExpr of expr

and var_expr = {
  id: int;
  name: string;
}

and param_expr = {
  pos: pos;
  id: int;
  name: string;
  type': type' option;
}

and def_expr = {
  pos: pos;
  id: int;
  name: string;
  type': type' option;
  expr: expr;
}

and bind_expr =
  | BindExprPrint
  | BindExprDef   of def_expr
  | BindExprParam of param_expr
  | BindExprVar   of var_expr

let expr_pos expr =
  match expr with
  | ExprUnit    expr -> expr.pos
  | ExprBool    expr -> expr.pos
  | ExprInt     expr -> expr.pos
  | ExprChar    expr -> expr.pos
  | ExprString  expr -> expr.pos
  | ExprBind    expr -> expr.pos
  | ExprTuple   expr -> expr.pos
  | ExprRecord  expr -> expr.pos
  | ExprElem    expr -> expr.pos
  | ExprAttr    expr -> expr.pos
  | ExprPreop   expr -> expr.pos
  | ExprBinop   expr -> expr.pos
  | ExprAscr    expr -> expr.pos
  | ExprIf      expr -> expr.pos
  | ExprAbs     expr -> expr.pos
  | ExprApp     expr -> expr.pos
  | ExprTypeAbs expr -> expr.pos
  | ExprTypeApp expr -> expr.pos
  | ExprStmt    expr -> expr.pos

let bind_expr_id bind =
  match bind with
  | BindExprPrint       -> -1
  | BindExprVar   var   -> var.id
  | BindExprParam param -> param.id
  | BindExprDef   def   -> def.id

let bind_expr_name bind =
  match bind with
  | BindExprPrint       -> "print"
  | BindExprDef   def   -> def.name
  | BindExprParam param -> param.name
  | BindExprVar   var   -> var.name
