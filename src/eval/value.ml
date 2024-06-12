module BindKey = struct
  type t = Abt.Bind.bind_expr

  let compare x y = Stdlib.compare x.Abt.Bind.id y.Abt.Bind.id
end

module BindMap = Map.Make(BindKey)

type value =
| VUnit
| VBool   of bool
| VInt    of int
| VString of string
| VTuple  of value list
| VRecord of value Util.NameMap.t
| VLam    of lam

and lam =
  | VPrim of lam_prim
  | VCode of lam_code

and lam_prim = context -> value

and lam_code = {
  abs: Abt.Expr.lam_abs;
  frame: frame
}

and context = { expr: Abt.Expr.expr; value: value; out: Util.writer }

and frame = {
  parent: frame option;
  binds: value BindMap.t;
}

let rec compare left right =
  match (left, right) with
  | VUnit, VUnit ->
    true
  | VBool left, VBool right ->
    left = right
  | VInt left, VInt right ->
    left = right
  | VString left, VString right ->
    left = right
  | VTuple lefts, VTuple rights ->
    Util.compare_lists compare lefts rights
  | VRecord lefts, VRecord rights ->
    Util.compare_maps compare lefts rights
  | VLam (VPrim left), VLam (VPrim right) ->
    left = right
  | VLam (VCode left), VLam (VCode right) ->
    left.abs = right.abs
  | _ -> false

let value_bool expr value =
  match value with
  | VBool bool -> bool
  | _ -> Error.raise_bool expr

let value_int expr value =
  match value with
  | VInt int -> int
  | _ -> Error.raise_int expr

let value_string expr value =
  match value with
  | VString string -> string
  | _ -> Error.raise_string expr
