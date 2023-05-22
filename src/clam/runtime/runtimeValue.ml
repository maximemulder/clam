open Utils
open Model

type value =
| VPrint
| VVoid
| VBool    of bool
| VInt     of int
| VChar    of char
| VString  of string
| VTuple   of value list
| VRecord  of value NameMap.t
| VExprAbs of (param_expr list) * expr
| VTypeAbs of (param_type list) * expr
