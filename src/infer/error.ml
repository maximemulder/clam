type error = {
  message: string;
  span: Code.span;
}

exception Error of error

let raise message span =
  raise (Error { message; span })

let raise_constrain span (sub: Type.type') (sup: Type.type') =
  let sub = Type.display sub in
  let sup = Type.display sup in
  raise
    ("cannot constrain `" ^ sub ^ "` as a subtype of `" ^ sup ^ "`")
    span

let raise_elem elem type' =
  let type' = Type.display type' in
  let index = string_of_int elem.Abt.index in
  raise
    ("expected tuple expression with element `" ^ index ^"` but found expression of type `" ^ type' ^ "`")
    elem.span

let raise_univ_kind (app: Abt.expr_univ_app) type' =
  let type' = Type.display type' in
  raise
    ("expected type to expression abstraction but found type `" ^ type' ^ "`")
    app.span

let raise_univ_type (app: Abt.expr_univ_app) arg bound =
  let arg = Type.display arg in
  let bound = Type.display bound in
  raise
    ("expected subtype of `" ^ bound ^ "` but found type `" ^ arg ^ "`")
    app.span

let raise_recursive span (bind: Abt.bind_type) type' =
  let type' = Type.display type' in
  raise
    ("found recursive type `" ^ bind.name ^ ". " ^ type' ^ "`")
    span
