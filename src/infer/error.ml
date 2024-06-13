open Abt.Display

type error = {
  message: string;
  span: Code.span;
}

exception Error of error

let raise message span =
  raise (Error { message; span })

let raise_constrain span (sub: Abt.Type.type') (sup: Abt.Type.type') =
  let sub = display sub in
  let sup = display sup in
  raise
    ("cannot constrain `" ^ sub ^ "` as a subtype of `" ^ sup ^ "`")
    span

let raise_elem elem type' =
  let type' = display type' in
  let index = string_of_int elem.Abt.Expr.index in
  raise
    ("expected tuple expression with element `" ^ index ^"` but found expression of type `" ^ type' ^ "`")
    elem.span

let raise_univ_kind (app: Abt.Expr.univ_app) type' =
  let type' = display type' in
  raise
    ("expected type to expression abstraction but found type `" ^ type' ^ "`")
    app.span

let raise_univ_type (app: Abt.Expr.univ_app) arg bound =
  let arg = display arg in
  let bound = display bound in
  raise
    ("expected subtype of `" ^ bound ^ "` but found type `" ^ arg ^ "`")
    app.span

let raise_recursive span (bind: Abt.Type.bind_type) type' =
  let type' = display type' in
  raise
    ("found recursive type `" ^ bind.name ^ ". " ^ type' ^ "`")
    span
