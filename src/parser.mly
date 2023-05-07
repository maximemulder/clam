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

%token AMPERSAND
%token ARROW
%token AT
%token BRACE_LEFT
%token BRACE_RIGHT
%token COLON
%token COMMA
%token CROTCHET_LEFT
%token CROTCHET_RIGHT
%token EQUAL
%token MINUS
%token PARENTHESIS_LEFT
%token PARENTHESIS_RIGHT
%token PIPE
%token PLUS
%token SEMICOLON

%token AS
%token DEF
%token ELSE
%token IF
%token THEN
%token TYPE

%token EOF

%start <Ast.program> program

%%

let program :=
  | defs = list(def); EOF;
    { { program_defs = defs } }

let def :=
  | TYPE; name = IDENT; EQUAL; type_ = type_; SEMICOLON;
    { DefType {type_name = name; type' = type_} }
  | DEF; name = IDENT; EQUAL; expr = expr; SEMICOLON;
    { DefExpr {expr_name = name; expr = expr} }

let type_ :=
  | name = IDENT;
    { TypeIdent name }
  | PARENTHESIS_LEFT; types = list_comma(type_); PARENTHESIS_RIGHT;
    { TypeTuple types }
  | BRACE_LEFT; attrs = list_comma(attr_type); BRACE_RIGHT;
    { TypeRecord attrs }
  | PARENTHESIS_LEFT; params = list_comma(type_); PARENTHESIS_RIGHT; return = return;
    { TypeFun (params, return) }
  | left = type_; AMPERSAND; right = type_;
    { TypeInter (left, right) }
  | left = type_; PIPE; right = type_;
    { TypeUnion (left, right) }
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; type_ = type_;
    { TypeAbs (params, type_) }
  | type_ = type_; CROTCHET_LEFT; args = list_comma(type_); CROTCHET_RIGHT;
    { TypeApp (type_, args) }

// Expressions

let expr :=
  | name = IDENT;
    { ExprIdent name }
  | VOID;
    { ExprVoid }
  | TRUE;
    { ExprTrue }
  | FALSE;
    { ExprFalse }
  | int = INT;
    { ExprInt int }
  | char = CHAR;
    { ExprChar char }
  | string = STRING;
    { ExprString string }
  | AT; PARENTHESIS_LEFT; exprs = list_comma(expr); PARENTHESIS_RIGHT;
    { ExprTuple exprs }
  | AT; BRACE_LEFT; attrs = list_comma(attr_expr); BRACE_RIGHT;
    { ExprRecord (attrs) }
  | op = pre_op; expr = expr;
    { ExprPreop (op, expr) }
  | left = expr; op = bin_op; right = expr;
    { ExprBinop (left, op, right) }
  | expr = expr; COLON; type_ = type_;
    { ExprAscr (expr, type_) }
  | expr = expr; AS; type_ = type_;
    { ExprCast (expr, type_) }
  | block = block;
    { ExprBlock block }
  | IF; cond = expr; THEN; then_ = expr; ELSE; else_ = expr;
    { ExprIf (cond, then_, else_) }
  | PARENTHESIS_LEFT; params = list_comma(param); PARENTHESIS_RIGHT; return = option(return); block = block;
    { ExprAbs (params, return, block) }
  | expr = expr; PARENTHESIS_LEFT; args = list_comma(expr); PARENTHESIS_RIGHT;
    { ExprApp (expr, args)}
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; expr = expr;
    { ExprTypeAbs (params, expr) }
  | expr = expr; CROTCHET_LEFT; args = list_comma(type_); CROTCHET_RIGHT;
    { ExprTypeApp (expr, args) }

let param :=
  | name = IDENT; COLON; type_ = type_;
    { { param_name = name; param_type = type_ } }

let return :=
  | ARROW; type_ = type_;
    { type_ }

let attr_type :=
  | name = IDENT; COLON; type_ = type_;
    { { attr_type_name = name; attr_type = type_ } }

let attr_expr :=
  | name = IDENT; EQUAL; expr = expr;
    { { attr_expr_name = name; attr_expr = expr } }

let block :=
  | BRACE_LEFT; defs = list(def); expr = expr; BRACE_RIGHT;
    { { block_defs = defs; block_expr = expr } }

// Operators

let pre_op :=
  | PLUS;
    { "+" }
  | MINUS;
    { "-" }

let bin_op :=
  | PLUS;
    { "+" }
  | MINUS;
    { "-" }

// Utilities

let list_comma(X) := xs = separated_list(COMMA, X);
  { xs }
