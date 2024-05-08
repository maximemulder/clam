open Node
open Rename

let rec compare left right =
  match left, right with
  | Union left, Union right ->
    compare left.left  right.left &&
    compare left.right right.right
  | Inter left, Inter right ->
    compare left.left  right.left &&
    compare left.right right.right
  | Top    , Top    -> true
  | Bot    , Bot    -> true
  | Unit   , Unit   -> true
  | Bool   , Bool   -> true
  | Int    , Int    -> true
  | String , String -> true
  | Var left, Var right ->
    left.bind == right.bind
  | Tuple left, Tuple right ->
    Util.compare_lists compare left.elems right.elems
  | Record left, Record right ->
    Util.compare_maps compare_attr left.attrs right.attrs
  | Lam left, Lam right ->
    compare left.param right.param &&
    compare left.ret   right.ret
  | Univ left, Univ right ->
    compare_param left.param right.param &&
    let right_ret = rename right.param.bind left.param.bind right.ret in
    compare left.ret right_ret
  | Abs left, Abs right ->
    compare_param left.param right.param &&
    let right_body = rename right.param.bind left.param.bind right.body in
    compare left.body right_body
  | App left, App right ->
    compare left.abs right.abs &&
    compare left.arg right.arg
  | _ -> false

and compare_param left_param right_param =
  left_param.bind.name == right_param.bind.name &&
  compare left_param.lower right_param.lower &&
  compare left_param.upper right_param.upper

and compare_attr left_attr right_attr =
  compare left_attr.type' right_attr.type'
