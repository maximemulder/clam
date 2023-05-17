open Collection

type value =
| VVoid
| VBool    of bool
| VInt     of int
| VChar    of char
| VString  of string
| VTuple   of value list
| VRecord  of value NameMap.t
| VExprAbs of (Model.param_expr list) * Model.expr
| VTypeAbs of (Model.type_param list) * Model.expr
