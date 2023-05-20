# Grammar

This is the current "ideal" grammar I have in mind for Clam. I believe this grammar is context-free if we resolve its conflicts, it is however not LR(1). Alternatives are generally listed by order of precedecence.

## Grammar

```
program = list(def) eof

def =
    | 'type' ident '=' type ';'
    | 'def' ident option(':' type) '=' expr ';'

type =
    | ident
    | '(' list_comma(type) ')'
    | '{' list_comma(attr_type) '}'
    | type '[' list_comma(type) ']'
    | type '|' type
    | type '&' type
    | '(' list_comma(type) ')' -> type
    | '[' list_comma(param_type) ']' '->' type
    | '[' list_comma(param_type) ']' type

param_type = ident option(':' type)

attr_type = ident ':' type

expr =
    | ident
    | 'void'
    | 'true'
    | 'false'
    | int
    | char
    | string
    | '(' list_comma(expr) ')'
    | '{' list_comma(attr_expr) '}'
    | '{' list(def) expr '}'
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
    | 'if' expr 'then' expr 'else' expr
    | '(' list_comma(param_expr) ')' option(':' type) '->' expr
    | '[' list_comma(param_type) ']' '->' expr

param_expr = ident option(':' type)

attr_expr = ident '=' expr

preop = '+' '-' '!'
binop_1 = '*' '/' '%'
binop_2 = '+' '-'
binop_3 = '++'
binop_4 = '==' '!=' '<' '>' '<=' '>='
binop_5 = '|' '&'
```

## Notable conflicts

Type ascription and operators that are valid for both on expressions and types (type application and '|' / '&').
Example: `expr : ident | ident`
I think the type should take precedecence.

It is hard to differentiate between tuples and function types in return type annotations.
Example: `() : () -> ident -> expr`
A solution (which is currently used) is to use different arrows for function types and return expressions (such as `->` and `=>`).

Tuples and functions
Example: `( ident: type ) ->`
It is not possible to know whether what is left of the arrow is a tuple with a type ascription or a parameter group before advancing to the arrow symbol. This problem also exists with a return type annotation.

Records and blocks
Example: `{ ident = expr }`
It is not possible to know whether a production is a block or a record before advancing to the equal symbol. Which is problematic to reduce an empty definition list in LR(1).

## Future ideas

Various ideas for future extensions:

Bodyless type and expression definitions
- `type' ident option('=' type) ';'`
- `def' ident option(':' type) option('=' type) ';'`
This would allow to declare primitives
This would also allow to split expression definitions type and body

Syntactic sugar for generic type definitions
`'type' ident '[' list_comma(param_type)' ]' '=' type ';'` for `'type' ident '=' '[' list_comma(param_type)' ]' type ';'`

Syntactic sugar for function definitions
`'def' ident '(' list_comma(param_expr) ')' option(':' type) '=' expr ';'` for `'def' ident '=' '(' list_comma(param_expr) ')' option(':' type) '->' expr ';'`

Syntactic sugar for generic expression definitions and generic function definitions

Syntactic sugar for expression attributes
- New alternative `'=' ident` in `attr_expr`

Pattern matching expression
- New alternative `expr 'is' pat` in `expr`
