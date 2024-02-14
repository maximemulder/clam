open Abt

let raise message span =
  Error.raise "TYPE ERROR" (message ^ "\n" ^ Error.display_span span)

let validate_proper (type': Abt.type') =
  let span = type_span type' in
  let type' = AbtDisplay.display type' in
  raise
    ("expected proper type but found type `" ^ type' ^ "`")
    span

let validate_inter_kind (inter: Abt.type_inter) =
  let span = inter.span in
  let inter = AbtDisplay.display (TypeInter inter) in
  raise
    ("both operands of intersection `" ^ inter ^ "` must be of the same kind")
    span

let validate_union_kind (union: Abt.type_union) =
  let span = union.span in
  let union = AbtDisplay.display (TypeUnion union) in
  raise
    ("both operands of union `" ^ union ^ "` must be of the same kind")
    span

let validate_app_arg (app: Abt.type_app) (param: Type.type') (arg: Type.type') =
  let span = app.span in
  let param = TypeDisplay.display param in
  let arg = TypeDisplay.display arg in
  raise
    ("expected subtype of `" ^ param ^ "` but found type `" ^ arg ^ "`")
    span

let check_type expr (type': Type.type') (constr: Type.type') =
  let span = expr_span expr in
  let type' = TypeDisplay.display type' in
  let constr = TypeDisplay.display constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found expression of type `" ^ type' ^ "`")
    span

let check_tuple (expr: Abt.expr_tuple) (constr: Type.base) =
  let span = expr.span in
  let constr = TypeDisplay.display_base constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found a tuple")
    span

let check_tuple_arity (expr: Abt.expr_tuple) (constr: Type.tuple) =
  let span = expr.span in
  let arity = string_of_int (List.length expr.elems) in
  let constr_arity = string_of_int (List.length constr.elems) in
  raise
    ("expected tuple of " ^ constr_arity ^ " elements but found tuple of " ^ arity ^ " elements")
    span

let check_record (expr: expr_record) (constr: Type.base) =
  let span = expr.span in
  let constr = TypeDisplay.display_base constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found a record")
    span

let check_record_attr (expr: Abt.expr_record) (constr: Type.attr) =
  let span = expr.span in
  let name = constr.name in
  let type' = TypeDisplay.display constr.type' in
  raise
    ("expected attribute `" ^ name ^ "` of type `" ^ type' ^ "` but found no attribute with this name")
    span

let check_abs (expr: Abt.expr_abs) (constr: Type.base) =
  let span = expr.span in
  let constr = TypeDisplay.display_base constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found abstraction")
    span

let check_abs_param (param: Abt.param_expr) (param': Type.type') (constr: Type.type') =
  let span = param.span in
  let param' = TypeDisplay.display param' in
  let constr = TypeDisplay.display constr in
  raise
    ("expected parameter to be a supertype of `" ^ constr ^ "` but found type `" ^ param' ^ "`")
    span

let check_type_abs (expr: expr_type_abs) constr =
  let span = expr.span in
  let constr = TypeDisplay.display_base constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found type abstraction")
    span

let check_type_abs_param (expr: expr_type_abs) bound (constr: Type.param) =
  let span = expr.span in
  let bound = TypeDisplay.display bound in
  let constr = TypeDisplay.display constr.bound in
  raise
    ("expected type parameter of type `" ^ constr ^ "` but found parameter of type `" ^ bound ^ "`")
    span

let infer_recursive def =
  raise
    ("recursive definition `" ^ def.bind.name ^ "`, type annotation needed")
    def.span

let infer_elem elem type' =
  let type' = TypeDisplay.display type' in
  let index = string_of_int elem.index in
  raise
    ("expected tuple expression with element `" ^ index ^"` but found expression of type `" ^ type' ^ "`")
    elem.span

let infer_attr (attr: expr_attr) type' =
  let type' = TypeDisplay.display type' in
  raise
    ("expected record expression with attribute `" ^ attr.label ^ "` but found expression of type `" ^ type' ^ "`")
    attr.span

let infer_abs_param (param: param_expr) =
  raise
    ("require type annotation for parameter `" ^ param.bind.name ^ "`")
    param.span

let infer_app_kind (app: expr_app) type' =
  let type' = TypeDisplay.display type' in
  raise
    ("expected expression abstraction but found expression of type `" ^ type' ^ "`")
    app.span

let infer_type_app_kind (app: expr_type_app) type' =
  let type' = TypeDisplay.display type' in
  raise
    ("expected type to expression abstraction but found type `" ^ type' ^ "`")
    app.span

let infer_type_app_type (app: expr_type_app) arg bound =
  let arg = TypeDisplay.display arg in
  let bound = TypeDisplay.display bound in
  raise
    ("expected subtype of `" ^ bound ^ "` but found type `" ^ arg ^ "`")
    app.span

let infer_constrain span (sub: Type.type') (sup: Type.type') =
  let sub = TypeDisplay.display sub in
  let sup = TypeDisplay.display sup in
  raise
    ("cannot constrain `" ^ sub ^ "` as a subtype of `" ^ sup ^ "`")
    span

let infer_recursive_type (def: def_expr) =
  raise
    ("cannot infer recursive type for definition `" ^ def.bind.name ^ "`")
    def.span
