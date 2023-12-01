open Utils

module BindKey = struct
  type t = Model.bind_expr

  let compare x y  = Stdlib.compare (Model.bind_expr_id x) (Model.bind_expr_id y)
end

module BindMap = Map.Make(BindKey)

type writer = string -> unit

type value =
| VUnit
| VBool    of bool
| VInt     of int
| VChar    of char
| VString  of string
| VTuple   of value list
| VRecord  of value NameMap.t
| VExprAbs of abs_expr
| VTypeAbs of abs_type

and abs_expr =
  | VPrim of abs_expr_prim
  | VCode of abs_expr_code

and abs_expr_prim = context -> value

and abs_expr_code = {
  abs: Model.expr_abs;
  frame: frame
}

and abs_type = {
  abs: Model.expr_type_abs;
  frame: frame;
}

and context = { value: value; out: writer }

and frame = {
  parent: frame option;
  binds: value BindMap.t;
}

let rec compare left right =
  match (left, right) with
  | (VUnit, VUnit) ->
    true
  | (VBool left, VBool right) ->
    left = right
  | (VInt left, VInt right) ->
    left = right
  | (VChar left, VChar right) ->
    left = right
  | (VString left, VString right) ->
    left = right
  | (VTuple lefts, VTuple rights) ->
    compare_lists compare lefts rights
  | (VRecord lefts, VRecord rights) ->
    compare_maps compare lefts rights
  | (VExprAbs (VPrim left), VExprAbs (VPrim right)) ->
    left = right
  | (VExprAbs (VCode left), VExprAbs (VCode right)) ->
    left.abs = right.abs
  | (VTypeAbs left, VTypeAbs right) ->
    left = right
  | _ -> false

let value_int value =
  match value with
  | VInt int -> int
  | _ -> RuntimeErrors.raise_value ()

let value_bool value =
  match value with
  | VBool bool -> bool
  | _ -> RuntimeErrors.raise_value ()
