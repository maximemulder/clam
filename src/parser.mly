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
    { DefType { type_pos = $startpos; type_name = name; type' = type_ } }
  | DEF; name = IDENT; type_ = option(typing); EQUAL; expr = expr; SEMICOLON;
    { DefExpr { expr_pos = $startpos; expr_name = name; expr_type = type_; expr = expr } }

let type_ :=
  | name = IDENT;
    { TypeIdent name }
  | PARENTHESIS_LEFT; types = list_comma(type_); PARENTHESIS_RIGHT;
    { TypeTuple types }
  | BRACE_LEFT; attrs = list_comma(attr_type); BRACE_RIGHT;
    { TypeRecord attrs }
  | PARENTHESIS_LEFT; params = list_comma(type_); PARENTHESIS_RIGHT; ARROW; expr = type_;
    { TypeAbsExpr (params, expr) }
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; ARROW; expr = type_;
    { TypeAbsExprType (params, expr) }
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
    { $startpos, ExprIdent name }
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
  | AT; PARENTHESIS_LEFT; exprs = list_comma(expr); PARENTHESIS_RIGHT;
    { $startpos, ExprTuple exprs }
  | AT; BRACE_LEFT; attrs = list_comma(attr_expr); BRACE_RIGHT;
    { $startpos, ExprRecord (attrs) }
  | op = pre_op; expr = expr;
    { $startpos, ExprPreop (op, expr) }
  | left = expr; op = bin_op; right = expr;
    { $startpos, ExprBinop (left, op, right) }
  | expr = expr; COLON; type_ = type_;
    { $startpos, ExprAscr (expr, type_) }
  | block = block;
    { $startpos, ExprBlock block }
  | IF; cond = expr; THEN; then_ = expr; ELSE; else_ = expr;
    { $startpos, ExprIf (cond, then_, else_) }
  | PARENTHESIS_LEFT; params = list_comma(param); PARENTHESIS_RIGHT; return = option(typing); ARROW; expr = expr;
    { $startpos, ExprAbs (params, return, expr) }
  | expr = expr; PARENTHESIS_LEFT; args = list_comma(expr); PARENTHESIS_RIGHT;
    { $startpos, ExprApp (expr, args)}
  | CROTCHET_LEFT; params = list_comma(param); CROTCHET_RIGHT; ARROW; expr = expr;
    { $startpos, ExprTypeAbs (params, expr) }
  | expr = expr; CROTCHET_LEFT; args = list_comma(type_); CROTCHET_RIGHT;
    { $startpos, ExprTypeApp (expr, args) }

let param :=
  | name = IDENT; type_ = option(typing);
    { { param_pos = $startpos; param_name = name; param_type = type_ } }

let typing :=
  | COLON; type_ = type_;
    { type_ }

let attr_type :=
  | name = IDENT; COLON; type_ = type_;
    { { attr_type_pos = $startpos; attr_type_name = name; attr_type = type_ } }

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

let list_comma(X) := separated_list(COMMA, X)
