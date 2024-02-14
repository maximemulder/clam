type span = Code.span

(* PROGRAM *)

type program = {
  defs: def list
}

(* DEFINITIONS *)

and def =
  | DefType of def_type
  | DefExpr of def_expr

and def_type = {
  span: span;
  name: string;
  type': type';
}

and def_expr = {
  span: span;
  name: string;
  type': type' option;
  expr: expr;
}

(* TYPES *)

and type' =
  | TypeName    of type_name
  | TypeProduct of type_product
  | TypeLam     of type_lam
  | TypeUniv    of type_univ
  | TypeAbs     of type_abs
  | TypeApp     of type_app
  | TypeInter   of type_inter
  | TypeUnion   of type_union

and type_name = {
  span: span;
  name: string;
}

and type_product = {
  span: span;
  fields: field_type list;
}

and type_lam = {
  span: span;
  params: type' list;
  ret: type';
}

and type_univ = {
  span: span;
  params: param_type list;
  ret: type';
}

and type_abs = {
  span: span;
  params: param_type list;
  body: type';
}

and type_app = {
  span: span;
  abs: type';
  args: type' list;
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

and field_type =
  | FieldTypeElem of field_type_elem
  | FieldTypeAttr of field_type_attr

and field_type_elem = {
  span: span;
  type': type';
}

and field_type_attr = {
  span: span;
  label: string;
  type': type';
}

and param_type = {
  span: span;
  name: string;
  type': type' option;
}

(* EXPRESSIONS *)

and expr =
  | ExprUnit    of expr_unit
  | ExprTrue    of expr_true
  | ExprFalse   of expr_false
  | ExprInt     of expr_int
  | ExprString  of expr_string
  | ExprName    of expr_name
  | ExprProduct of expr_product
  | ExprElem    of expr_elem
  | ExprAttr    of expr_attr
  | ExprPreop   of expr_preop
  | ExprBinop   of expr_binop
  | ExprAscr    of expr_ascr
  | ExprStmt    of expr_stmt
  | ExprIf      of expr_if
  | ExprLamAbs  of expr_lam_abs
  | ExprLamApp  of expr_lam_app
  | ExprUnivAbs of expr_univ_abs
  | ExprUnivApp of expr_univ_app

and expr_unit = {
  span: span;
}

and expr_true = {
  span: span;
}

and expr_false = {
  span: span;
}

and expr_int = {
  span: span;
  value: string;
}

and expr_string = {
  span: span;
  value: string;
}

and expr_name = {
  span: span;
  name: string;
}

and expr_product = {
  span: span;
  fields: field_expr list;
}

and expr_elem = {
  span: span;
  tuple: expr;
  index: string;
}

and expr_attr = {
  span: span;
  record: expr;
  label: string;
}

and expr_preop = {
  span: span;
  op: string;
  expr: expr;
}

and expr_binop = {
  span: span;
  left: expr;
  op: string;
  right: expr;
}

and expr_ascr = {
  span: span;
  expr: expr;
  type': type';
}

and expr_stmt = {
  span: span;
  stmt: stmt;
  expr: expr;
}

and expr_if = {
  span: span;
  cond: expr;
  then': expr;
  else': expr;
}

and expr_lam_abs = {
  span: span;
  params: param_expr list;
  body: expr;
}

and expr_lam_app = {
  span: span;
  abs: expr;
  args: expr list;
}

and expr_univ_abs = {
  span: span;
  params: param_type list;
  body: expr;
}

and expr_univ_app = {
  span: span;
  abs: expr;
  args: type' list;
}

(* EXPRESSION AUXILIARIES *)

and stmt =
  | StmtVar  of stmt_var
  | StmtExpr of stmt_expr

and stmt_var = {
  span: span;
  name: string;
  type': type' option;
  expr: expr;
}

and stmt_expr = {
  span: span;
  expr: expr;
}

and field_expr =
  | FieldExprElem of field_expr_elem
  | FieldExprAttr of field_expr_attr

and field_expr_elem = {
  span: span;
  expr: expr;
}

and field_expr_attr = {
  span: span;
  label: string;
  expr: expr;
}

and param_expr = {
  span: span;
  name: string;
  type': type' option;
}

let type_span type' =
  match type' with
  | TypeName    type' -> type'.span
  | TypeProduct type' -> type'.span
  | TypeLam     type' -> type'.span
  | TypeUniv    type' -> type'.span
  | TypeAbs     type' -> type'.span
  | TypeApp     type' -> type'.span
  | TypeInter   type' -> type'.span
  | TypeUnion   type' -> type'.span

let get_program_types program =
  List.filter_map (fun def -> match def with
    | DefType type' -> Some type'
    | DefExpr _     -> None
  ) program.defs

let get_program_exprs program =
  List.filter_map (fun def -> match def with
    | DefType _    -> None
    | DefExpr expr -> Some expr
  ) program.defs
