open Utils

module BindKey = struct
  type t = Model.bind_expr

  let compare x y  = Stdlib.compare (Model.bind_expr_id x) (Model.bind_expr_id y)
end

module BindMap = Map.Make(BindKey)

type frame = {
  parent: frame option;
  binds: value BindMap.t;
}

and value =
| VPrint
| VUnit
| VBool    of bool
| VInt     of int
| VChar    of char
| VString  of string
| VTuple   of value list
| VRecord  of value NameMap.t
| VExprAbs of abs_expr
| VTypeAbs of abs_type

and abs_expr = {
  abs: Model.expr_abs;
  frame: frame;
}

and abs_type = {
  abs: Model.expr_type_abs;
  frame: frame;
}

let rec compare value other =
  match (value, other) with
  | (VPrint, VPrint) ->
    true
  | (VUnit, VUnit) ->
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
