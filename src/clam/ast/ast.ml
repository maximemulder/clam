type pos = Lexing.position

type program = {
  defs: def list
}

and def =
  | DefType of def_type
  | DefExpr of def_expr

and def_type = {
  pos: pos;
  name: string;
  type': type';
}

and def_expr = {
  pos: pos;
  name: string;
  type': type' option;
  expr: expr;
}

and type' = pos * type_data

and type_data =
  | TypeIdent       of string
  | TypeAbsExpr     of (type' list) * type'
  | TypeAbsExprType of (param list) * type'
  | TypeProduct     of field_type list
  | TypeInter       of type' * type'
  | TypeUnion       of type' * type'
  | TypeAbs         of (param list) * type'
  | TypeApp         of type' * (type' list)

and expr = pos * expr_data

and expr_data =
  | ExprUnit
  | ExprTrue
  | ExprFalse
  | ExprInt     of string
  | ExprString  of string
  | ExprBind    of string
  | ExprTuple   of expr list
  | ExprProduct of field_expr list
  | ExprElem    of expr * string
  | ExprAttr    of expr * string
  | ExprPreop   of string * expr
  | ExprBinop   of expr * string * expr
  | ExprAscr    of expr * type'
  | ExprIf      of expr * expr * expr
  | ExprAbs     of (param list) * expr
  | ExprApp     of expr * (expr list)
  | ExprTypeAbs of (param list) * expr
  | ExprTypeApp of expr * (type' list)
  | ExprStmt    of stmt * expr

and field_type =
| FieldTypeElem of field_type_elem
| FieldTypeAttr of field_type_attr

and field_type_elem = {
  pos: pos;
  type': type';
}

and field_type_attr = {
  pos: pos;
  name: string;
  type': type';
}

and field_expr =
| FieldExprElem of field_expr_elem
| FieldExprAttr of field_expr_attr

and field_expr_elem = {
  pos: pos;
  expr: expr;
}

and field_expr_attr = {
  pos: pos;
  name: string;
  expr: expr;
}

and param = {
  pos: pos;
  name: string;
  type': type' option;
}

and attr_type = {
  pos: pos;
  name: string;
  type': type';
}

and stmt =
  | StmtVar  of string * (type' option) * expr
  | StmtExpr of expr

let get_program_types program =
  List.filter_map (fun(def) -> match def with
    | DefType type' -> Some type'
    | DefExpr _     -> None
  ) program.defs

let get_program_exprs program =
  List.filter_map (fun(def) -> match def with
    | DefType _    -> None
    | DefExpr expr -> Some expr
  ) program.defs
