%{
  open Ast
%}

%token <string> IDENT
%token VOID
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
%token RET
%token THEN
%token TYPE
%token VAR

%token EOF

%start <Ast.program> program

%%

let program :=
  | defs = list(def); EOF;
    { { program_defs = defs } }

let def :=
  | TYPE; name = IDENT; ASSIGN; type_ = type_; SEMICOLON;
    { DefType { type_pos = $startpos; type_name = name; type' = type_ } }
  | DEF; name = IDENT; type_ = option(COLON; type_); ASSIGN; expr = expr; SEMICOLON;
    { DefExpr { expr_pos = $startpos; expr_name = name; expr_type = type_; expr = expr } }

let type_ :=
  | type_2
  | PARENTHESIS_LEFT; params = list_comma(type_); PARENTHESIS_RIGHT; ARROW; expr = type_;
    { $startpos, TypeAbsExpr (params, expr) }
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; ARROW; expr = type_;
    { $startpos, TypeAbsExprType (params, expr) }
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; type_ = type_;
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
  | PARENTHESIS_LEFT; types = list_comma(type_); PARENTHESIS_RIGHT;
    { $startpos, TypeTuple types }
  | BRACE_LEFT; attrs = list_comma(attr_type); BRACE_RIGHT;
    { $startpos, TypeRecord attrs }
  | type_ = type_1; CROTCHET_LEFT; args = list_comma(type_); CROTCHET_RIGHT;
    { $startpos, TypeApp (type_, args) }

// Expressions

let expr :=
  | expr_7
  | PARENTHESIS_LEFT; params = list_comma(param); PARENTHESIS_RIGHT; return = option(ARROW; type_); DOUBLE_ARROW; expr = expr;
    { $startpos, ExprAbs (params, return, expr) }
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; DOUBLE_ARROW; expr = expr;
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
  | VOID;
    { $startpos, ExprVoid }
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
  | AT; BRACE_LEFT; attrs = list_comma(attr_expr); BRACE_RIGHT;
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
    { { param_pos = $startpos; param_name = name; param_type = type_ } }

let attr_type :=
  | name = IDENT; COLON; type_ = type_;
    { { attr_type_pos = $startpos; attr_type_name = name; attr_type = type_ } }

let attr_expr :=
  | name = IDENT; ASSIGN; expr = expr;
    { { attr_expr_name = name; attr_expr = expr } }

let block :=
  | BRACE_LEFT; stmts = list(stmt); expr = option(RET; expr); BRACE_RIGHT;
    { { block_stmts = stmts; block_expr = expr } }

let stmt :=
  | VAR; name = IDENT; type_ = option(COLON; type_); ASSIGN; expr = expr; SEMICOLON;
  { StmtVar (name, type_, expr) }
  | expr = expr; SEMICOLON;
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
