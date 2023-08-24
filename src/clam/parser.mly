%{
  open Ast
%}

%token <string> IDENT
%token UNIT
%token TRUE
%token FALSE
%token <string> INT
%token <string> CHAR
%token <string> STRING

%token AND
%token ARROW
%token ASSIGN
%token AT
%token BRACE_LEFT
%token BRACE_RIGHT
%token COLON
%token COMMA
%token CONCAT
%token CROTCHET_LEFT
%token CROTCHET_RIGHT
%token DIV
%token DOT
%token DOUBLE_ARROW
%token EQ
%token GE
%token GT
%token LE
%token LT
%token MINUS
%token MOD
%token MUL
%token NE
%token NOT
%token OR
%token PARENTHESIS_LEFT
%token PARENTHESIS_RIGHT
%token PLUS
%token SEMICOLON

%token DEF
%token ELSE
%token IF
%token THEN
%token TYPE
%token VAR

%token EOF

%start <Ast.program> program

%%

let program :=
  | defs = list(def); EOF;
    { { defs } }

let def :=
  | TYPE; name = IDENT; ASSIGN; type_ = type_; SEMICOLON;
    { DefType { pos = $startpos; name; type' = type_ } }
  | DEF; name = IDENT; type_ = option(COLON; type_); ASSIGN; expr = expr; SEMICOLON;
    { DefExpr { pos = $startpos; name; type' = type_; expr } }

let type_ :=
  | type_2
  | PARENTHESIS_LEFT; params = list_comma(type_); PARENTHESIS_RIGHT; ARROW; expr = type_;
    { $startpos, TypeAbsExpr (params, expr) }
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; ARROW; expr = type_;
    { $startpos, TypeAbsExprType (params, expr) }
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; DOUBLE_ARROW; type_ = type_;
    { $startpos, TypeAbs (params, type_) }

let type_2 :=
  | type_1
  | left = type_2; AND; right = type_1;
    { $startpos, TypeInter (left, right) }
  | left = type_2; OR; right = type_1;
    { $startpos, TypeUnion (left, right) }

let type_1 :=
  | name = IDENT;
    { $startpos, TypeIdent name }
  | LT; type_ = type_; GT;
    { type_ }
  | PARENTHESIS_LEFT; types = list_comma(type_); PARENTHESIS_RIGHT;
    { $startpos, TypeTuple types }
  | BRACE_LEFT; attrs = list_comma(attr_type); BRACE_RIGHT;
    { $startpos, TypeRecord attrs }
  | type_ = type_1; CROTCHET_LEFT; args = list_comma(type_); CROTCHET_RIGHT;
    { $startpos, TypeApp (type_, args) }

// Expressions

let expr :=
  | expr_7
  | PARENTHESIS_LEFT; params = list_comma(param); PARENTHESIS_RIGHT; ARROW; expr = expr;
    { $startpos, ExprAbs (params, expr) }
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; ARROW; expr = expr;
    { $startpos, ExprTypeAbs (params, expr) }
  | IF; cond = expr; THEN; then_ = expr; ELSE; else_ = expr;
    { $startpos, ExprIf (cond, then_, else_) }

let expr_7 :=
  | expr_6
  | left = expr_7; op = bin_op_5; right = expr_6;
    { $startpos, ExprBinop (left, op, right) }

let expr_6 :=
  | expr_5
  | left = expr_6; op = bin_op_4; right = expr_5;
    { $startpos, ExprBinop (left, op, right) }

let expr_5 :=
  | expr_4
  | left = expr_5; op = bin_op_3; right = expr_4;
    { $startpos, ExprBinop (left, op, right) }

let expr_4 :=
  | expr_3
  | left = expr_4; op = bin_op_2; right = expr_3;
    { $startpos, ExprBinop (left, op, right) }

let expr_3 :=
  | expr_2
  | left = expr_3; op = bin_op_1; right = expr_2;
    { $startpos, ExprBinop (left, op, right) }

let expr_2 :=
  | expr_05
  | op = pre_op; expr = expr_2;
    { $startpos, ExprPreop (op, expr) }

let expr_05 :=
  | expr_1
  | expr = expr_1; COLON; type_ = type_1;
    { $startpos, ExprAscr (expr, type_) }

let expr_1 :=
  | UNIT;
    { $startpos, ExprUnit }
  | TRUE;
    { $startpos, ExprTrue }
  | FALSE;
    { $startpos, ExprFalse }
  | int = INT;
    { $startpos, ExprInt int }
  | char = CHAR;
    { $startpos, ExprChar char }
  | string = STRING;
    { $startpos, ExprString string }
  | name = IDENT;
    { $startpos, ExprBind name }
  | AT; PARENTHESIS_LEFT; exprs = list_comma(expr); PARENTHESIS_RIGHT;
    { $startpos, ExprTuple exprs }
  | BRACE_LEFT; attrs = list_comma(attr_expr); BRACE_RIGHT;
    { $startpos, ExprRecord (attrs) }
  | expr = expr_1; DOT; index = INT;
    { $startpos, ExprElem (expr, index) }
  | expr = expr_1; DOT; name = IDENT;
    { $startpos, ExprAttr (expr, name) }
  | block = block;
    { $startpos, ExprBlock block }
  | expr = expr_1; PARENTHESIS_LEFT; args = list_comma(expr); PARENTHESIS_RIGHT;
    { $startpos, ExprApp (expr, args)}
  | expr = expr_1; CROTCHET_LEFT; args = list_comma(type_); CROTCHET_RIGHT;
    { $startpos, ExprTypeApp (expr, args) }

let param :=
  | name = IDENT; type_ = option(COLON; type_);
    { { pos = $startpos; name; type' = type_ } }

let attr_type :=
  | name = IDENT; COLON; type_ = type_;
    { { pos = $startpos; name; type' = type_ } }

let attr_expr :=
  | name = IDENT; ASSIGN; expr = expr;
    { { pos = $startpos; name; expr } }

let block :=
  | BRACE_LEFT; stmts = stmts; BRACE_RIGHT;
    { { stmts } }

let stmts :=
  | stmt = stmt;
    { StmtsStmt stmt }
  | expr = expr;
    { StmtsExpr expr }

let stmt :=
  | body = stmt_body; SEMICOLON; stmts = stmts;
    { { body; stmts } }

let stmt_body :=
  | VAR; name = IDENT; type_ = option(COLON; type_); ASSIGN; expr = expr;
  { StmtVar (name, type_, expr) }
  | expr = expr;
  { StmtExpr (expr) }

// Operators

let pre_op :=
  | PLUS;
    { "+" }
  | MINUS;
    { "-" }
  | NOT;
    { "!" }

let bin_op_1 :=
  | MUL;
    { "*" }
  | DIV;
    { "/"}
  | MOD;
    { "%" }

let bin_op_2 :=
  | PLUS;
    { "+" }
  | MINUS;
    { "-" }

let bin_op_3 :=
  | CONCAT;
    { "++" }

let bin_op_4 :=
  | EQ;
    { "==" }
  | NE;
    { "!=" }
  | LT;
    { "<" }
  | GT;
    { ">" }
  | LE;
    { "<=" }
  | GE;
    { ">=" }

let bin_op_5 :=
  | AND;
    { "&" }
  | OR;
    { "|" }

// Utilities

let list_comma(X) := separated_list(COMMA, X)
