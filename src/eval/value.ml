module BindKey = struct
  type t = Abt.bind_expr

  let compare x y  = Stdlib.compare x.Abt.id y.Abt.id
end

module BindMap = Map.Make(BindKey)

type writer = string -> unit

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
  abs: Abt.expr_lam_abs;
  frame: frame
}

and context = { value: value; out: writer }

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

let value_bool value =
  match value with
  | VBool bool -> bool
  | _ -> Error.raise_value "bool"

let value_int value =
  match value with
  | VInt int -> int
  | _ -> Error.raise_value "int"

let value_string value =
  match value with
  | VString string -> string
  | _ -> Error.raise_value "string"
