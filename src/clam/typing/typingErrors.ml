open TypingDisplay
open Model

let raise message pos =
  Error.raise "TYPE ERROR" (message ^ "\n" ^ Error.display_pos pos)

let raise_expr_recursive def =
  raise
    ("recursive definition `" ^ def.name ^ "`, type annotation needed")
    def.pos

let raise_expr_constraint expr type' constraint' =
  let type' = display type' in
  let constraint' = display constraint' in
  raise
    ("expected expression of type `" ^ constraint' ^ "` but found expression of type `" ^ type' ^ "`")
    (expr_pos expr)

let raise_expr_elem elem type' =
  let type' = display type' in
  let index = string_of_int elem.index in
  raise
    ("expected tuple expression with element `" ^ index ^"` but found expression of type `" ^ type' ^ "`")
    elem.pos

let raise_expr_attr (attr: expr_attr) type' =
  let type' = display type' in
  raise
    ("expected record expression with attribute `" ^ attr.name ^ "` but found expression of type `" ^ type' ^ "`")
    attr.pos

let raise_expr_app_kind (app: expr_app) type' =
  let type' = display type' in
  raise
    ("expected expression abstraction but found expression of type `" ^ type' ^ "`")
    app.pos

let raise_expr_type_app_kind (app: expr_type_app) type' =
  let type' = display type' in
  raise
    ("expected type to expression abstraction but found type `" ^ type' ^ "`")
    app.pos

let raise_param (param: param_expr) =
  raise
    ("require type annotation for parameter `" ^ param.name ^ "`")
    param.pos

let raise_subtype_constraint type' constr =
  let pos = type_pos type' in
  let type' = display type' in
  let constr = display constr in
  raise
    ("expected subtype of `" ^ constr ^ "` but found type `" ^ type' ^ "`")
    pos

let raise_suptype_constraint type' constr =
  let pos = type_pos type' in
  let type' = display type' in
  let constr = display constr in
  raise
    ("expected supertype of `" ^ constr ^ "` but found type `" ^ type' ^ "`")
    pos

let raise_type_app_kind type' =
  let pos = type_pos type' in
  let type' = display type' in
  raise
    ("expected type abstraction but found type `" ^ type' ^ "`")
    pos

let raise_type_proper type' =
  let pos = type_pos type' in
  let type' = display type' in
  raise
    ("expected proper type but found type `" ^ type' ^ "`")
    pos

let raise_unexpected _ =
  Error.raise "TYPE ERROR" "unexpected error"
