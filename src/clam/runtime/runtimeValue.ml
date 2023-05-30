open Utils

type value =
| VPrint
| VVoid
| VBool    of bool
| VInt     of int
| VChar    of char
| VString  of string
| VTuple   of value list
| VRecord  of value NameMap.t
| VExprAbs of Model.expr_abs
| VTypeAbs of Model.expr_type_abs

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
  | (VExprAbs abs, VExprAbs other) ->
    abs = other
  | (VTypeAbs abs, VTypeAbs other) ->
    abs = other
  | _ -> false
