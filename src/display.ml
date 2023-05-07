open Ast

type node =
  | Prog     of Ast.program
  | Def      of Ast.def
  | Type     of Ast.type'
  | Expr     of Ast.expr
  | Param    of Ast.param
  | AttrType of Ast.attr_type
  | AttrExpr of Ast.attr_expr
  | Block    of Ast.block

type attr =
  | ANode   of node
  | AList   of node list
  | AOption of node option
  | AString of string

let node_name node =
  match node with
  | Prog _ -> "program"
  | Def def -> "def." ^ (match def with
    | DefType _ -> "type"
    | DefExpr _ -> "expr")
  | Type type' -> "type." ^ (match type' with
    | TypeIdent  _ -> "name"
    | TypeFun    _ -> "fun"
    | TypeTuple  _ -> "tuple"
    | TypeRecord _ -> "record"
    | TypeInter  _ -> "inter"
    | TypeUnion  _ -> "union"
    | TypeAbs    _ -> "abs"
    | TypeApp    _ -> "app")
  | Expr expr -> "expr." ^ (match expr with
    | ExprIdent   _ -> "name"
    | ExprVoid      -> "void"
    | ExprTrue      -> "true"
    | ExprFalse     -> "false"
    | ExprInt     _ -> "int"
    | ExprChar    _ -> "char"
    | ExprString  _ -> "string"
    | ExprTuple   _ -> "tuple"
    | ExprRecord  _ -> "record"
    | ExprPreop   _ -> "preop"
    | ExprBinop   _ -> "binop"
    | ExprCast    _ -> "cast"
    | ExprAscr    _ -> "ascr"
    | ExprBlock   _ -> "block"
    | ExprIf      _ -> "if"
    | ExprAbs     _ -> "abs"
    | ExprApp     _ -> "app"
    | ExprTypeAbs _ -> "type_abs"
    | ExprTypeApp _ -> "type_app")
  | Param _ -> "param"
  | AttrType _ -> "attr_type"
  | AttrExpr _ -> "attr_expr"
  | Block _-> "block"

let node_attrs node =
  match node with
  | Prog { program_defs } -> [("defs", AList (List.map (fun def -> Def def) program_defs))]
  | Def def -> (match def with
    | Ast.DefType type' -> [("name", AString type'.type_name); ("type", ANode (Type type'.type'))]
    | Ast.DefExpr expr  -> [("name", AString expr.expr_name); ("expr", ANode (Expr expr.expr))])
  | Type type' -> (match type' with
    | TypeIdent  name -> [("name", AString name)]
    | TypeFun    (params, return) -> [("params", AList (List.map (fun param -> Type param) params)); ("return", ANode (Type return))]
    | TypeTuple  types -> [("types", AList (List.map (fun type' -> Type type') types))]
    | TypeRecord attrs -> [("attrs", AList (List.map (fun attr -> AttrType attr) attrs))]
    | TypeInter  (left, right) -> [("left", ANode (Type left)); ("right", ANode (Type right))]
    | TypeUnion  (left, right) -> [("left", ANode (Type left)); ("right", ANode (Type right))]
    | TypeAbs    (params, return) -> [("params", AList (List.map (fun param -> Param param) params)); ("return", ANode (Type return))]
    | TypeApp    (type', args) -> [("type", ANode (Type type')); ("args", AList (List.map (fun arg -> Type arg) args))])
  | Expr expr -> (match expr with
    | ExprIdent name -> [("name", AString name)]
    | ExprVoid -> []
    | ExprTrue -> []
    | ExprFalse -> []
    | ExprInt value -> [("value", AString value)]
    | ExprChar value -> [("value", AString value)]
    | ExprString value -> [("value", AString value)]
    | ExprTuple exprs -> [("exprs", AList (List.map (fun expr -> Expr expr) exprs))]
    | ExprRecord attrs -> [("attrs", AList (List.map (fun attr -> AttrExpr attr) attrs))]
    | ExprPreop (op, expr) -> [("op", AString op); ("expr", ANode (Expr expr))]
    | ExprBinop (left, op, right) -> [("left", ANode (Expr left)); ("op", AString op); ("right", ANode (Expr right))]
    | ExprCast (expr, type') -> [("expr", ANode (Expr expr)); ("type", ANode (Type type'))]
    | ExprAscr (expr, type') -> [("expr", ANode (Expr expr)); ("type", ANode (Type type'))]
    | ExprBlock block -> [("block", ANode (Block block))]
    | ExprIf (cond, then', else') -> [("cond", ANode (Expr cond)); ("then", ANode (Expr then')); ("else", ANode (Expr else'))]
    | ExprAbs (params, return, block) -> [("params", AList (List.map (fun param -> Param param) params)); ("return", AOption (Option.map (fun return -> Type return) return)); ("block", ANode (Block block))]
    | ExprApp (expr, args) -> [("expr", ANode (Expr expr)); ("args", AList(List.map (fun arg -> Expr arg) args))]
    | ExprTypeAbs (params, expr) -> [("params", AList (List.map (fun param -> Param param) params)); ("expr", ANode (Expr expr))]
    | ExprTypeApp (expr, args) -> [("expr", ANode (Expr expr)); ("args", AList(List.map (fun arg -> Type arg) args))])
  | Param { param_name; param_type } -> [("name", AString param_name); ("type", ANode (Type param_type))]
  | AttrType { attr_type_name; attr_type } -> [("name", AString attr_type_name); ("type", ANode (Type attr_type))]
  | AttrExpr { attr_expr_name; attr_expr } -> [("name", AString attr_expr_name); ("expr", ANode (Expr attr_expr))]
  | Block block -> [("defs", AList (List.map (fun def -> Def def) block.block_defs)); ("expr", ANode (Expr block.block_expr))]

let rec indent tab =
  if tab == 0
    then ""
    else "  " ^ (indent (tab - 1))

let rec display_node tab node =
  let attrs = node_attrs node in
  (node_name node) ^ (if List.length attrs != 0
    then " {\n" ^ (String.concat "" (List.map (fun (name, attr) -> (indent (tab + 1)) ^ name ^ ": " ^ display_attr (tab + 1) attr) attrs)) ^ (indent tab) ^ "}"
    else "")
    ^ "\n"

and display_attr tab attr =
  match attr with
  | ANode node     -> display_node tab node
  | AList nodes    -> "[\n" ^ (String.concat "" (List.map (fun node -> (indent (tab + 1)) ^ display_node (tab + 1) node) nodes)) ^ (indent tab) ^ "]\n"
  | AOption option -> (match option with
    | Some node -> "some " ^ display_node tab node
    | None      -> "none\n")
  | AString string -> "\"" ^ string ^ "\"\n"

let display_program (prog: Ast.program) = "\n" ^ (display_node 0 (Prog prog))
