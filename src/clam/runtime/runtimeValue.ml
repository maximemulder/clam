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

let rec compare value other =
  match (value, other) with
  | (VPrint, VPrint) ->
    true
  | (VVoid, VVoid) ->
    true
  | (VBool bool, VBool other) ->
    bool = other
  | (VInt int, VInt other) ->
    int = other
  | (VChar char, VChar other) ->
    char = other
  | (VString string, VString other) ->
    string = other
  | (VTuple values, VTuple others) ->
    compare_lists compare values others
  | (VRecord attrs, VRecord others) ->
    compare_maps compare attrs others
  | (VExprAbs (a, b), VExprAbs (c, d)) ->
    (a, b) = (c, d)
  | (VTypeAbs (a, b), VTypeAbs (c, d)) ->
    (a, b) = (c, d)
  | _ -> false
