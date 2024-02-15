%{
open Ast

let span (start, end') =
  { Code.code = Option.get !Global.code; start = start.Lexing.pos_cnum; end' = end'.Lexing.pos_cnum }

%}

%token <string> IDENT
%token UNIT
%token TRUE
%token FALSE
%token <string> INT
%token <string> STRING

%token AND
%token ARROW
%token ASSIGN
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
%token PARENTHESIS_RIGHT_ARROW
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

// PROGRAM

let program :=
  | defs = list(def); EOF;
    { { defs } }

// DEFINITIONS

let def :=
  | TYPE; name = IDENT; ASSIGN; type_ = type_;
    { DefType { span = span $loc; name; type' = type_ } }
  | DEF; name = IDENT; type_ = option(COLON; type_); ASSIGN; expr = expr;
    { DefExpr { span = span $loc; name; type' = type_; expr } }

// TYPES

let type_ :=
  | type_2
  | PARENTHESIS_LEFT; params = list_comma(type_); PARENTHESIS_RIGHT_ARROW; ret = type_;
    { TypeLam { span = span $loc; params; ret } }
  | CROTCHET_LEFT; params = list_comma(param_type); CROTCHET_RIGHT; ARROW; ret = type_;
    { TypeUniv { span = span $loc; params; ret } }
  | CROTCHET_LEFT; params = list_comma(param_type); CROTCHET_RIGHT; DOUBLE_ARROW; body = type_;
    { TypeAbs { span = span $loc; params; body } }

let type_2 :=
  | type_1
  | left = type_2; AND; right = type_1;
    { TypeInter { span = span $loc; left; right } }
  | left = type_2; OR; right = type_1;
    { TypeUnion { span = span $loc; left; right } }

let type_1 :=
  | name = IDENT;
    { TypeName { span = span $loc; name } }
  | PARENTHESIS_LEFT; type_ = type_; PARENTHESIS_RIGHT;
    { type_ } // Maybe there should be a group type (and expression) for more precise positions
  | BRACE_LEFT; fields = list_comma(field_type); BRACE_RIGHT;
    { TypeProduct { span = span $loc; fields } }
  | abs = type_1; CROTCHET_LEFT; args = list_comma(type_); CROTCHET_RIGHT;
    { TypeApp { span = span $loc; abs; args } }

// TYPE UTILITIES

let field_type :=
  | type_ = type_;
    { FieldTypeElem { span = span $loc; type' = type_ } }
  | label = IDENT; COLON; type_ = type_;
    { FieldTypeAttr { span = span $loc; label; type' = type_ } }

let param_type :=
  | name = IDENT; type_ = option(COLON; type_);
    { { span = span $loc; name; type' = type_ } }

// EXPRESSIONS

let expr :=
  | expr_8
  | PARENTHESIS_LEFT; params = list_comma(param_expr); PARENTHESIS_RIGHT_ARROW; body = expr;
    { ExprLamAbs { span = span $loc; params; body } }
  | CROTCHET_LEFT; params = list_comma(param_type); CROTCHET_RIGHT; ARROW; body = expr;
    { ExprUnivAbs { span = span $loc; params; body } }
  | IF; cond = expr; THEN; then_ = expr; ELSE; else_ = expr;
    { ExprIf  { span = span $loc; cond; then' = then_; else' = else_ } }

let expr_8 :=
  | expr_7
  | stmt = stmt; SEMICOLON; expr = expr;
    { ExprStmt { span = span $loc; stmt; expr } }

let expr_7 :=
  | expr_6
  | left = expr_7; op = bin_op_5; right = expr_6;
    { ExprBinop { span = span $loc; left; op; right } }

let expr_6 :=
  | expr_5
  | left = expr_6; op = bin_op_4; right = expr_5;
    { ExprBinop { span = span $loc; left; op; right } }

let expr_5 :=
  | expr_4
  | left = expr_5; op = bin_op_3; right = expr_4;
    { ExprBinop { span = span $loc; left; op; right } }

let expr_4 :=
  | expr_3
  | left = expr_4; op = bin_op_2; right = expr_3;
    { ExprBinop { span = span $loc; left; op; right } }

let expr_3 :=
  | expr_2
  | left = expr_3; op = bin_op_1; right = expr_2;
    { ExprBinop { span = span $loc; left; op; right } }

let expr_2 :=
  | expr_05
  | op = pre_op; expr = expr_2;
    { ExprPreop { span = span $loc; op; expr } }

let expr_05 :=
  | expr_1
  | expr = expr_1; COLON; type_ = type_1;
    { ExprAscr { span = span $loc; expr; type' = type_ } }

let expr_1 :=
  | UNIT;
    { ExprUnit { span = span $loc } }
  | TRUE;
    { ExprTrue { span = span $loc } }
  | FALSE;
    { ExprFalse { span = span $loc } }
  | value = INT;
    { ExprInt { span = span $loc; value } }
  | value = STRING;
    { ExprString { span = span $loc; value } }
  | name = IDENT;
    { ExprName  { span = span $loc; name } }
  | PARENTHESIS_LEFT; expr = expr; PARENTHESIS_RIGHT;
    { expr }
  | BRACE_LEFT; fields = list_comma(field_expr); BRACE_RIGHT;
    { ExprProduct { span = span $loc; fields } }
  | tuple = expr_1; DOT; index = INT;
    { ExprElem { span = span $loc; tuple; index } }
  | record = expr_1; DOT; label = IDENT;
    { ExprAttr { span = span $loc; record; label } }
  | abs = expr_1; PARENTHESIS_LEFT; args = list_comma(expr); PARENTHESIS_RIGHT;
    { ExprLamApp { span = span $loc; abs; args } }
  | abs = expr_1; CROTCHET_LEFT; args = list_comma(type_); CROTCHET_RIGHT;
    { ExprUnivApp { span = span $loc; abs; args } }

// EXPRESSION AUXILIARIES

let stmt :=
  | VAR; name = IDENT; type_ = option(COLON; type_); ASSIGN; expr = expr_7;
    { StmtVar { span = span $loc; name; type' = type_; expr } }
  | expr = expr_7;
    { StmtExpr { span = span $loc; expr } }

let field_expr :=
  | expr = expr;
    { FieldExprElem { span = span $loc; expr }}
  | label = IDENT; ASSIGN; expr = expr;
    { FieldExprAttr { span = span $loc; label; expr } }

let param_expr :=
  | name = IDENT; type_ = option(COLON; type_);
    { { span = span $loc; name; type' = type_ } }

// OPERATORS

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

// UTILITIES

let list_comma(X) := separated_list(COMMA, X)
