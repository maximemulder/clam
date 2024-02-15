open Node

type node =
  | Program   of program
  | Def       of def
  | Type      of type'
  | ParamType of param_type
  | FieldType of field_type
  | Expr      of expr
  | Stmt      of stmt
  | ParamExpr of param_expr
  | FieldExpr of field_expr

type value =
  | VString of string
  | VMaybe  of value option
  | VList   of value list
  | VNode   of node

let v_string string    = VString string
let v_maybe node f     = VMaybe (Option.map f node)
let v_list  node f     = VList (List.map f node)
let v_program program  = VNode (Program program)
let v_def  def         = VNode (Def def)
let v_type type'       = VNode (Type type')
let v_field_type field = VNode (FieldType field)
let v_param_type param = VNode (ParamType param)
let v_expr expr        = VNode (Expr expr)
let v_stmt stmt        = VNode (Stmt stmt)
let v_field_expr field = VNode (FieldExpr field)
let v_param_expr param = VNode (ParamExpr param)

let get_name node =
  match node with
  | Program _ -> "program"
  | Def def -> "def." ^ (
    match def with
    | DefType _ -> "type"
    | DefExpr _ -> "expr")
  | Type type' -> "type." ^ (
    match type' with
    | TypeName    _ -> "name"
    | TypeProduct _ -> "product"
    | TypeLam     _ -> "lam"
    | TypeUniv    _ -> "univ"
    | TypeAbs     _ -> "abs"
    | TypeApp     _ -> "app"
    | TypeUnion   _ -> "union"
    | TypeInter   _ -> "inter")
  | FieldType field -> "field_type." ^ (
    match field with
    | FieldTypeElem _ -> "elem"
    | FieldTypeAttr _ -> "attr")
  | ParamType _ -> "param_type"
  | Expr expr -> "expr." ^ (
    match expr with
    | ExprUnit    _ -> "unit"
    | ExprTrue    _ -> "true"
    | ExprFalse   _ -> "false"
    | ExprInt     _ -> "int"
    | ExprString  _ -> "string"
    | ExprName    _ -> "name"
    | ExprProduct _ -> "product"
    | ExprElem    _ -> "elem"
    | ExprAttr    _ -> "attr"
    | ExprPreop   _ -> "preop"
    | ExprBinop   _ -> "binop"
    | ExprAscr    _ -> "ascr"
    | ExprStmt    _ -> "stmt"
    | ExprIf      _ -> "if"
    | ExprLamAbs  _ -> "lam_abs"
    | ExprLamApp  _ -> "lam_app"
    | ExprUnivAbs _ -> "univ_abs"
    | ExprUnivApp _ -> "univ_app")
  | Stmt stmt -> "stmt." ^ (
    match stmt with
    | StmtVar  _ -> "var"
    | StmtExpr _ -> "expr")
  | FieldExpr field -> "field_expr." ^ (
    match field with
    | FieldExprElem _ -> "elem"
    | FieldExprAttr _ -> "attr")
  | ParamExpr _ -> "param_expr"

let get_span node =
  match node with
  | Program _ -> { Code.code = { name = "dummy"; text = " " }; start = 0; end' = 0}
  | Def def -> (
    match def with
    | DefType def -> def.span
    | DefExpr def -> def.span)
  | Type type' -> Span.type_span type'
  | FieldType field -> (
    match field with
    | FieldTypeElem field -> field.span
    | FieldTypeAttr field -> field.span)
  | ParamType param -> param.span
  | Expr expr -> Span.expr_span expr
  | Stmt stmt -> (
    match stmt with
    | StmtVar  stmt -> stmt.span
    | StmtExpr stmt -> stmt.span)
  | FieldExpr field -> (
    match field with
    | FieldExprElem field -> field.span
    | FieldExprAttr field -> field.span)
  | ParamExpr param -> param.span

let get_attrs node =
  match node with
  | Program { defs } -> [
      "defs", v_list defs v_def;
    ]
  | Def def -> (
    match def with
    | DefType { name; type'; _ } -> [
        "name", v_string name;
        "type", v_type  type';
      ]
    | DefExpr { name; type'; expr; _ } -> [
        "name", v_string name;
        "type", v_maybe  type' v_type;
        "expr", v_expr   expr;
      ])
  | Type type' -> (
    match type' with
    | TypeName { name; _} -> [
        "name", v_string name;
      ]
    | TypeProduct { fields; _ } -> [
        "fields", v_list fields v_field_type;
      ]
    | TypeLam { params; ret; _ } -> [
        "params", v_list params v_type;
        "ret",    v_type ret;
      ]
    | TypeUniv { params; ret; _ } -> [
        "params", v_list params v_param_type;
        "ret",    v_type ret;
      ]
    | TypeAbs { params; body; _ } -> [
        "params", v_list params v_param_type;
        "body",   v_type body;
      ]
    | TypeApp { abs; args; _ } -> [
        "abs",  v_type abs;
        "args", v_list args v_type;
      ]
    | TypeUnion { left; right; _ } -> [
        "left",  v_type left;
        "right", v_type right;
      ]
    | TypeInter { left; right; _ } -> [
        "left",  v_type left;
        "right", v_type right;
      ])
  | FieldType field -> (
    match field with
    | FieldTypeElem { type'; _ } -> [
        "type", v_type type';
      ]
    | FieldTypeAttr { label; type'; _ } -> [
        "label", v_string label;
        "type",  v_type type';
      ])
  | ParamType { name; type'; _ } -> [
      "name", v_string name;
      "type", v_maybe type' v_type;
    ]
  | Expr expr -> (
    match expr with
    | ExprUnit  _ -> []
    | ExprTrue  _ -> []
    | ExprFalse _ -> []
    | ExprInt { value; _ } -> [
        "value", v_string value;
      ]
    | ExprString { value; _ } -> [
        "value", v_string value;
      ]
    | ExprName { name; _ } -> [
        "name", v_string name;
      ]
    | ExprProduct { fields; _ } -> [
        "fields", v_list fields v_field_expr;
      ]
    | ExprElem { tuple; index; _ } -> [
        "tuple", v_expr tuple;
        "index", v_string index;
      ]
    | ExprAttr { record; label; _ } -> [
        "record", v_expr record;
        "label",  v_string label;
      ]
    | ExprPreop { op; expr; _ } -> [
        "op",   v_string op;
        "expr", v_expr expr;
      ]
    | ExprBinop { left; op; right; _ } -> [
        "left",  v_expr left;
        "op",    v_string op;
        "right", v_expr right;
      ]
    | ExprAscr { expr; type'; _ } -> [
        "expr", v_expr expr;
        "type", v_type type';
      ]
    | ExprStmt { stmt; expr; _ } -> [
        "stmt", v_stmt stmt;
        "expr", v_expr expr;
      ]
    | ExprIf { cond; then'; else'; _ } -> [
        "cond", v_expr cond;
        "then", v_expr then';
        "else", v_expr else';
      ]
    | ExprLamAbs { params; body; _ } -> [
        "params", v_list params v_param_expr;
        "body",   v_expr body;
      ]
    | ExprLamApp { abs; args; _ } -> [
        "abs",  v_expr abs;
        "args", v_list args v_expr;
      ]
    | ExprUnivAbs { params; body; _ } -> [
        "params", v_list params v_param_type;
        "body",   v_expr body;
      ]
    | ExprUnivApp { abs; args; _ } -> [
        "abs",  v_expr abs;
        "args", v_list args v_type;
      ])
  | Stmt stmt -> (
    match stmt with
    | StmtVar { name; type'; expr; _ } -> [
        "name", v_string name;
        "type", v_maybe type' v_type;
        "expr", v_expr expr;
      ]
    | StmtExpr { expr; _ } -> [
        "expr", v_expr expr;
      ])
  | FieldExpr field -> (
    match field with
    | FieldExprElem { expr; _ } -> [
        "expr", v_expr expr;
      ]
    | FieldExprAttr { label; expr; _ } -> [
        "label", v_string label;
        "expr",  v_expr expr;
      ])
  | ParamExpr { name; type'; _ } ->[
      "name", v_string name;
      "type", v_maybe type' v_type;
    ]
