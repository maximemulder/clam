open Abt.Display
open Abt.Span
open Abt.Type

type error = {
  message: string;
  span: Code.span;
}

exception Error of error

let raise message span =
  raise (Error { message; span })

let validate_proper (type': type') =
  let span = type_span type' in
  let type' = display type' in
  raise
    ("expected proper type but found type `" ^ type' ^ "`")
    span

let validate_inter_kind (inter: inter) =
  let span = inter.span in
  let inter = display (Inter inter) in
  raise
    ("both operands of intersection `" ^ inter ^ "` must be of the same kind")
    span

let validate_union_kind (union: union) =
  let span = union.span in
  let union = display (Union union) in
  raise
    ("both operands of union `" ^ union ^ "` must be of the same kind")
    span

let validate_app_arg (app: app) (lower: type') (upper: type') =
  let lower = display lower in
  let upper = display upper in
  let arg = display app.arg in
  raise
    ("expected type within interval ``" ^ lower ^ " .. " ^ upper ^ "` but found type `" ^ arg ^ "`")
    app.span

let validate_param (param: param) =
  let span = param.span in
  let lower = display param.lower in
  let upper = display param.upper in
  raise
    ("inconsistent type interval, expected the lower bound to be a subtype of the upper bound but found types `" ^ lower ^ "` and `" ^ upper ^ "`")
    span
