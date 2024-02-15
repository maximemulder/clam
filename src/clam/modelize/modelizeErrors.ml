open Abt
open Ast

let raise message span =
  Error.raise "MODEL ERROR" (message ^ "\n" ^ (Error.display_span span))

let raise_expr_duplicate name =
  Error.raise "MODEL ERROR" ("duplicate expression definition `" ^ name ^ "`")

let raise_expr_bound span name =
  raise ("unbound expression `" ^ name ^ "`") span

let raise_expr_operator span op =
  raise ("unknown operator `" ^ op ^ "`") span

let raise_expr_integer span value =
  raise ("invalid integer literal `" ^ value ^ "`") span

let raise_expr_product (expr: expr_product) =
  raise "product expression cannot have both indexed and labeled fields" expr.span

let raise_type_bound (type': type_name) =
  raise ("unbound type `" ^ type'.name ^ "`") type'.span

let raise_type_recursive (type': type_name) =
  raise ("recursive type `" ^ type'.name ^ "`") type'.span

let raise_type_duplicate_attribute (attr: Abt.attr_type) =
  raise ("duplicate attribute `" ^ attr.label ^ "`") attr.span

let raise_type_product (type': type_product) =
  raise "product type cannot have both indexed and labeled fields" type'.span
