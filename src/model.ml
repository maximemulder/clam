open Collection

type pos = Lexing.position

type type' = pos * type_data

and type_data =
  | TypeAny
  | TypeVoid
  | TypeBool
  | TypeInt
  | TypeChar
  | TypeString
  | TypeVar         of param_type
  | TypeAbsExpr     of (type' list) * type'
  | TypeAbsExprType of (param_type list) * type'
  | TypeTuple       of type' list
  | TypeRecord      of attr_type NameMap.t
  | TypeInter       of type' * type'
  | TypeUnion       of type' * type'
  | TypeAbs         of (param_type list) * type'
  | TypeApp         of type' * (type' list)

and attr_type = {
  attr_type_pos: pos;
  attr_type_name: string;
  attr_type: type';
}

and param_type = {
  param_type_name: string;
  param_type: type';
}

type expr = pos * expr_data

and expr_data =
  | ExprVoid
  | ExprBool    of bool
  | ExprInt     of int
  | ExprChar    of char
  | ExprString  of string
  | ExprBind    of expr_bind
  | ExprTuple   of expr list
  | ExprRecord  of attr_expr list
  | ExprVariant of expr * int
  | ExprAttr    of expr * string
  | ExprPreop   of string * expr
  | ExprBinop   of expr * string * expr
  | ExprAscr    of expr * type'
  | ExprBlock   of expr_block
  | ExprIf      of expr * expr * expr
  | ExprAbs     of (param_expr list) * (type' option) * expr
  | ExprApp     of expr * (expr list)
  | ExprTypeAbs of (param_type list) * expr
  | ExprTypeApp of expr * (type' list)

and expr_bind = {
  mutable bind_expr: bind_expr option;
}

and expr_block = {
  block_stmts: stmt list;
  block_expr: expr option;
}

and stmt =
  | StmtVar  of var_expr * expr
  | StmtExpr of expr

and def_expr = {
  def_expr_pos: pos;
  def_expr_id: int;
  def_expr_name: string;
  def_expr_type: type' option;
  def_expr: expr;
}

and bind_expr =
  | BindExprDef   of def_expr
  | BindExprParam of param_expr
  | BindExprPrint
  | BindExprVar   of var_expr

and param_expr = {
  param_expr_pos: pos;
  param_expr_id: int;
  param_expr_name: string;
  param_expr_type: type' option;
}

and attr_expr = {
  attr_expr_name: string;
  attr_expr: expr;
}

and var_expr = {
  var_expr_id: int;
  var_expr_name: string;
}

let make_def_expr pos id name type' expr =
  {
    def_expr_pos = pos;
    def_expr_id = id;
    def_expr_name = name;
    def_expr_type = type';
    def_expr = expr;
  }

let make_param_expr pos id name type' =
  {
    param_expr_pos = pos;
    param_expr_id = id;
    param_expr_name = name;
    param_expr_type = type';
  }

let make_attr_expr name expr =
  {
    attr_expr_name = name;
    attr_expr = expr;
  }

let make_attr_type pos name type' =
  {
    attr_type_pos = pos;
    attr_type_name = name;
    attr_type = type';
  }

let bind_expr_id bind =
  match bind with
  | BindExprDef   def   -> def.def_expr_id
  | BindExprParam param -> param.param_expr_id
  | BindExprPrint       -> -1
  | BindExprVar   var   -> var.var_expr_id
