# Grammar

This is the current "ideal" grammar I have in mind for Clam. It can be parsed in LR(1) except for lambdas.

## Grammar

```
program = list(def) eof

def =
    | 'type' ident '=' type
    | 'def' ident option(':' type) '=' expr

type =
    | ident
    | '(' type ')'
    | '{' list_comma(field_type) '}'
    | type '[' list_comma(type) ']'
    | type '|' type
    | type '&' type
    | '(' list_comma(type) ')' -> type
    | '[' list_comma(param_type) ']' '->' type
    | '[' list_comma(param_type) ']' '=>' type

param_type = ident option(':' type)

field_type = option(ident ':') type

expr =
    | ident
    | 'void'
    | 'true'
    | 'false'
    | int
    | char
    | string
    | '(' expr ')'
    | '{' list_comma(field_expr) '}'
    | expr '.' int
    | expr '.' ident
    | expr '(' list_comma(expr) ')'
    | expr '[' list_comma(type) ']'
    | preop expr
    | expr binop_1 expr
    | expr binop_2 expr
    | expr binop_3 expr
    | expr binop_4 expr
    | expr binop_5 expr
    | expr ':' type
    | stmt ';' expr
    | 'if' expr 'then' expr 'else' expr
    | '(' list_comma(param_expr) ')' '->' expr
    | '[' list_comma(param_type) ']' '->' expr

param_expr = ident option(':' type)

field_expr = option(ident '=') expr

preop = '+' '-' '!'
binop_1 = '*' '/' '%'
binop_2 = '+' '-'
binop_3 = '++'
binop_4 = '==' '!=' '<' '>' '<=' '>='
binop_5 = '|' '&'

stmt =
    | 'var' ident option(':' type) '=' expr
    | expr
```

## Conflicts

Currently, there are two conflicts with lambdas and expression groups:

The first conflict occurs at this point: `(a)`
While parsing `)`, the parser does not know whether to reduce `a` into a parameter or an expression. This is solved by hacking the lexer to lex a right parenthesis followed by an arrow (`) ->`) as a unique token so that no lookahead is needed.

The second conflict occurs at this point: `(a:`
While parsing `:`, the parser does not know whether to parse `a` to a parameter or an expression. This is currently solved by Menhir by giving priority to the parameter. I don't think this conflict can be solved in LR(1), except by forbidding type ascriptions in groups.

Also the precedences of the lowest-predecence expressions (ifs, applications statements) should be reviewed.

This grammar also has the disadvantage of not differentiating empty records and tuples. I am also not against using an identation-sensitive grammar in the future although I need more experience in that area.

## Future ideas

Various ideas for future extensions:

Bodyless type and expression definitions
- `type' ident option('=' type)`
- `def' ident option(':' type) option('=' type)`
This would allow to declare primitives
This would also allow to split expression definitions type and body

Syntactic sugar for generic type definitions
`'type' ident '[' list_comma(param_type)' ]' '=' type` for `'type' ident '=' '[' list_comma(param_type)' ]' type`

Syntactic sugar for function definitions
`'def' ident '(' list_comma(param_expr) ')' option(':' type) '=' expr` for `'def' ident '=' '(' list_comma(param_expr) ')' option(':' type) '->' expr`

Syntactic sugar for generic expression definitions and generic function definitions

Syntactic sugar for expression attributes
- New alternative `'=' ident` in `field_expr`, shordhand for `ident = ident` (where the second `ident` is an expression)

Pattern matching expression
- New alternative `expr 'is' pat` in `expr`
