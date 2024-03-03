type error = {
  message: string;
  span: Code.span;
}

exception Error of error

let raise message span =
  raise (Error { message; span })

open Abt

let validate_proper (type': Abt.type') =
  let span = type_span type' in
  let type' = Abt.display type' in
  raise
    ("expected proper type but found type `" ^ type' ^ "`")
    span

let validate_inter_kind (inter: Abt.type_inter) =
  let span = inter.span in
  let inter = Abt.display (TypeInter inter) in
  raise
    ("both operands of intersection `" ^ inter ^ "` must be of the same kind")
    span

let validate_union_kind (union: Abt.type_union) =
  let span = union.span in
  let union = Abt.display (TypeUnion union) in
  raise
    ("both operands of union `" ^ union ^ "` must be of the same kind")
    span

let validate_app_arg (app: Abt.type_app) (param: Node.type') (arg: Node.type') =
  let span = app.span in
  let param = Display.display param in
  let arg = Display.display arg in
  raise
    ("expected subtype of `" ^ param ^ "` but found type `" ^ arg ^ "`")
    span

let check_type expr (type': Node.type') (constr: Node.type') =
  let span = expr_span expr in
  let type' = Display.display type' in
  let constr = Display.display constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found expression of type `" ^ type' ^ "`")
    span

let check_tuple (expr: Abt.expr_tuple) (constr: Node.base) =
  let span = expr.span in
  let constr = Display.display_base constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found a tuple")
    span

let check_tuple_arity (expr: Abt.expr_tuple) (constr: Node.tuple) =
  let span = expr.span in
  let arity = string_of_int (List.length expr.elems) in
  let constr_arity = string_of_int (List.length constr.elems) in
  raise
    ("expected tuple of " ^ constr_arity ^ " elements but found tuple of " ^ arity ^ " elements")
    span

let check_record (expr: expr_record) (constr: Node.base) =
  let span = expr.span in
  let constr = Display.display_base constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found a record")
    span

let check_record_attr (expr: Abt.expr_record) (constr: Node.attr) =
  let span = expr.span in
  let label = constr.label in
  let type' = Display.display constr.type' in
  raise
    ("expected attribute `" ^ label ^ "` of type `" ^ type' ^ "` but found no attribute with this name")
    span

let check_abs (expr: Abt.expr_lam_abs) (constr: Node.base) =
  let span = expr.span in
  let constr = Display.display_base constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found abstraction")
    span

let check_abs_param (param: Abt.param_expr) (param': Node.type') (constr: Node.type') =
  let span = param.span in
  let param' = Display.display param' in
  let constr = Display.display constr in
  raise
    ("expected parameter to be a supertype of `" ^ constr ^ "` but found type `" ^ param' ^ "`")
    span

let check_type_abs (expr: expr_univ_abs) constr =
  let span = expr.span in
  let constr = Display.display_base constr in
  raise
    ("expected expression of type `" ^ constr ^ "` but found type abstraction")
    span

let check_type_abs_param (expr: expr_univ_abs) bound (constr: Node.param) =
  let span = expr.span in
  let bound = Display.display bound in
  let constr = Display.display constr.upper in (* TODO lower *)
  raise
    ("expected type parameter of type `" ^ constr ^ "` but found parameter of type `" ^ bound ^ "`")
    span

let infer_recursive def =
  raise
    ("recursive definition `" ^ def.bind.name ^ "`, type annotation needed")
    def.span

let infer_attr (attr: expr_attr) type' =
  let type' = Display.display type' in
  raise
    ("expected record expression with attribute `" ^ attr.label ^ "` but found expression of type `" ^ type' ^ "`")
    attr.span

let infer_abs_param (param: param_expr) =
  raise
    ("require type annotation for parameter `" ^ param.bind.name ^ "`")
    param.span

let infer_app_kind (app: expr_univ_app) type' =
  let type' = Display.display type' in
  raise
    ("expected expression abstraction but found expression of type `" ^ type' ^ "`")
    app.span

let infer_recursive_type (def: def_expr) =
  raise
    ("cannot infer recursive type for definition `" ^ def.bind.name ^ "`")
    def.span
